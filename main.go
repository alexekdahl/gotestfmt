package main

// gotestfmt is a small utility for formatting the JSON output from
// `go test -json`. It aggregates test events, groups them by
// package and test name, and then renders a better summary to
// standard output.
import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strings"
	"text/tabwriter"
	"time"
)

// Event mirrors the structure of objects emitted by `go test -json`.
// Ref https://pkg.go.dev/cmd/test2json
type Event struct {
	Time    time.Time `json:"Time"`
	Action  string    `json:"Action"`
	Package string    `json:"Package"`
	Test    string    `json:"Test,omitempty"`
	Elapsed float64   `json:"Elapsed,omitempty"`
	Output  string    `json:"Output,omitempty"`
	Error   string    `json:"Error,omitempty"`
}

// testResult tracks the aggregated state for a single test. We record the
// final status (pass, fail, skip), execution time and any output lines.
// Output lines are kept verbatim; trimming is deferred to rendering time.
type testResult struct {
	Package string
	Name    string
	Action  string
	Elapsed float64
	Output  []string
}

const (
	// Actions as emitted by the go tool. These constants ensure we
	// compare against a single canonical string rather than sprinkling
	// magic values throughout the code.
	actRun       = "run"
	actPass      = "pass"
	actFail      = "fail"
	actSkip      = "skip"
	actError     = "error"
	actBuildFail = "build-fail"
	actBuildOut  = "build-output"
)

// colors is a simple helper to optionally apply ANSI colour codes when
// printing.  When colours are disabled, the `style` method simply
// returns the original string. The zero value for colors (enabled
// false) is safe for use.
type colors struct {
	enabled bool
}

const (
	cReset  = "\033[0m"
	cRed    = "\033[31m"
	cGreen  = "\033[32m"
	cYellow = "\033[33m"
	cBold   = "\033[1m"
	cGray   = "\033[90m"
)

// newColors returns a Colors instance.
func newColors(enabled bool) colors { return colors{enabled: enabled} }

// style wraps the given string in any number of ANSI codes followed by a
// reset. When colour output is disabled the string is returned
// unchanged. An explicit reset is appended to ensure state does not
// bleed into subsequent print calls.
func (c colors) style(s string, codes ...string) string {
	if !c.enabled {
		return s
	}
	var b strings.Builder
	for _, code := range codes {
		b.WriteString(code)
	}
	b.WriteString(s)
	b.WriteString(cReset)
	return b.String()
}

func (c colors) gray(s string) string   { return c.style(s, cGray) }
func (c colors) bold(s string) string   { return c.style(s, cBold) }
func (c colors) red(s string) string    { return c.style(s, cRed) }
func (c colors) green(s string) string  { return c.style(s, cGreen) }
func (c colors) yellow(s string) string { return c.style(s, cYellow) }

// status returns a coloured representation for a test action. If an
// unrecognised action is provided it is returned unmodified.
func (c colors) status(action string) string {
	switch action {
	case actPass:
		return c.green("PASS")
	case actFail:
		return c.style("FAIL", cRed, cBold)
	case actSkip:
		return c.yellow("SKIP")
	default:
		return action
	}
}

// Aggregator collects test events and produces aggregated results on
// demand. It keeps insertion order per package to make the output
// deterministic across runs.
type Aggregator struct {
	results       map[string]*testResult   // key: pkg/test
	testsByPkg    map[string][]*testResult // pkg -> []*TestResult (in insertion order)
	pkgErrors     map[string][]string      // package build or runtime errors
	keepAllOutput bool                     // include INFO/WARN etc when showing fail details
}

// NewAggregator creates an empty aggregator. When keepAllOutput is
// true the collector preserves all output lines, even for tests that
// eventually pass; otherwise only output for failing tests is kept.
func NewAggregator(keepAllOutput bool) *Aggregator {
	return &Aggregator{
		results:       make(map[string]*testResult),
		testsByPkg:    make(map[string][]*testResult),
		pkgErrors:     make(map[string][]string),
		keepAllOutput: keepAllOutput,
	}
}

// Add processes a single Event. Depending on the action the event
// contributes to test status, stores build errors or updates existing
// records. Unknown actions are ignored.
func (a *Aggregator) Add(ev Event) {
	switch ev.Action {
	case actError:
		if ev.Error != "" {
			a.pkgErrors[ev.Package] = append(a.pkgErrors[ev.Package], ev.Error)
		}
		return
	case actBuildFail:
		a.pkgErrors[ev.Package] = append(a.pkgErrors[ev.Package], "build failed")
		return
	case actBuildOut:
		if ev.Output != "" {
			a.pkgErrors[ev.Package] = append(a.pkgErrors[ev.Package], ev.Output)
		}
		return
	}

	// Ignore non‑test events (such as output at the package level).
	if ev.Test == "" {
		return
	}

	key := ev.Package + "/" + ev.Test
	switch ev.Action {
	case actRun:
		// Starting a new test resets any previous state. This can
		// legitimately happen if multiple runs are interleaved (for
		// example via -run flags), though it is uncommon.
		tr := &testResult{Package: ev.Package, Name: ev.Test}
		a.results[key] = tr
		a.testsByPkg[ev.Package] = append(a.testsByPkg[ev.Package], tr)

	case "output":
		// Keep output if we haven't decided outcome yet, or if we are
		// keeping logs for passing tests. Output can include arbitrary
		// newlines; preserve the original line breaks by splitting on
		// newline boundaries when ingesting from the decoder.
		if tr, ok := a.results[key]; ok && (a.keepAllOutput || tr.Action == "" || tr.Action == actFail) {
			tr.Output = append(tr.Output, ev.Output)
		}

	case actPass, actFail, actSkip:
		if tr, ok := a.results[key]; ok {
			tr.Action = ev.Action
			tr.Elapsed = ev.Elapsed
		}
	}
}

// Packages returns a sorted slice of package names that have recorded
// tests. Packages with only build errors are not included in this list.
func (a *Aggregator) Packages() []string {
	pkgs := make([]string, 0, len(a.testsByPkg))
	for p := range a.testsByPkg {
		pkgs = append(pkgs, p)
	}
	sort.Strings(pkgs)
	return pkgs
}

// Failed returns a sorted list of TestResult pointers for any test
// whose final action is fail. The returned slice is sorted first by
// package name then by test name for deterministic output.
func (a *Aggregator) Failed() []*testResult {
	var out []*testResult
	for _, r := range a.results {
		if r.Action == actFail {
			out = append(out, r)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Package == out[j].Package {
			return out[i].Name < out[j].Name
		}
		return out[i].Package < out[j].Package
	})
	return out
}

// Timed returns all tests for which we recorded a duration. The slice
// is sorted in descending order by elapsed time, allowing callers to
// display the top N slowest tests without sorting again.
func (a *Aggregator) Timed() []*testResult {
	var out []*testResult
	for _, r := range a.results {
		if r.Elapsed > 0 {
			out = append(out, r)
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Elapsed > out[j].Elapsed })
	return out
}

// fileLineRe matches strings of the form "<file.go>:<line>: <message>". It is
// used to detect file/line pairs in test output so we can render them more
// succinctly or transform them into GitHub annotations. The three capture
// groups correspond to the file name, line number and message body.
// Ref: https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-commands
var fileLineRe = regexp.MustCompile(`^(.+\.go):(\d+):\s*(.*)$`)

// formatSecs formats a floating point number of seconds to three decimal
// places. We deliberately preserve the legacy behaviour of always
// printing exactly three decimal places to make output consistent with
// earlier versions of this tool.
func formatSecs(s float64) string {
	return fmt.Sprintf("%.3fs", s)
}

// printHeader prints a section header in bold. The caller can include
// colour codes in the title string (for example via Colors.red) and
// they will be preserved because we don't further style the string.
func printHeader(c colors, title string) {
	fmt.Println(c.bold("=== " + title + " ==="))
}

// printPackageBlock prints a table of tests within a package. When
// condense is true and there are no failing tests in the package, a
// single summary line is emitted instead of printing each test. It
// returns the total number of tests along with counts of passed,
// failed and skipped tests.
// Note that on early return from the condensed path we must
// explicitly assign to the named return
// variables so that callers receive accurate statistics.
func printPackageBlock(c colors, pkg string, tests []*testResult, condense bool, tw *tabwriter.Writer) (tot, pass, fail, skip int) {
	anyFailed, anyPassed := false, false
	pkgPassed, pkgSkipped := 0, 0

	// Sort test names for determinism
	sort.Slice(tests, func(i, j int) bool { return tests[i].Name < tests[j].Name })

	for _, r := range tests {
		tot++
		switch r.Action {
		case actPass:
			pass++
			anyPassed = true
			pkgPassed++
		case actFail:
			fail++
			anyFailed = true
		case actSkip:
			skip++
			pkgSkipped++
		}
	}

	if condense && !anyFailed && anyPassed {
		// When condensing successful packages we output a single summary
		// line. Assign to the named returns to ensure the caller
		// receives the correct counters.
		if pkgSkipped > 0 {
			fmt.Fprintf(tw, "%s\t%s%d tests passed%s\t(%d skipped)\n",
				c.bold(pkg), c.green(""), pkgPassed, "", pkgSkipped)
		} else {
			fmt.Fprintf(tw, "%s\t%s%d tests passed%s\n",
				c.bold(pkg), c.green(""), pkgPassed, "")
		}
		_ = tw.Flush()
		// Override totals for condensed branch. The variables tot,
		// pass, fail and skip are named return values and thus in
		// scope here.
		tot = len(tests)
		pass = pkgPassed
		fail = 0
		skip = pkgSkipped
		return
	}

	// Non‑condensed output prints each test on its own line. Elapsed
	// times are shown in parentheses when available.
	fmt.Println(c.bold(pkg))
	for _, r := range tests {
		timeStr := ""
		if r.Elapsed > 0 {
			timeStr = c.gray(" (" + formatSecs(r.Elapsed) + ")")
		}
		fmt.Fprintf(tw, "  %s\t%s\t%s\n", c.status(r.Action), r.Name, timeStr)
	}
	_ = tw.Flush()
	return
}

// printBuildErrors renders build and generic errors by package.  Each
// error line is trimmed and coloured red.  Empty strings are skipped.
// When runner is true, a corresponding GitHub Actions annotation is
// emitted for each line. File/line formatted errors are encoded
// using the annotation syntax "::error file=name,line=n::message"; all
// other lines use the simpler "::error ::message" form. This allows
// GitHub to surface the errors inline in the code view while still
// preserving human‑friendly output.
func printBuildErrors(c colors, pkgErrors map[string][]string, runner bool) {
	if len(pkgErrors) == 0 {
		return
	}
	printHeader(c, c.red("BUILD ERRORS"))
	pkgs := make([]string, 0, len(pkgErrors))
	for p := range pkgErrors {
		pkgs = append(pkgs, p)
	}
	sort.Strings(pkgs)
	for _, p := range pkgs {
		fmt.Println(c.bold(p))
		for _, line := range pkgErrors[p] {
			trimmed := strings.TrimSpace(strings.TrimRight(line, "\n"))
			if trimmed == "" {
				continue
			}
			// Print the human readable line
			fmt.Println("  " + c.red(trimmed))
			// Emit annotation for GitHub runner if requested
			if runner {
				if m := fileLineRe.FindStringSubmatch(trimmed); len(m) == 4 {
					file, lineNum, msg := m[1], m[2], m[3]
					fmt.Printf("::error file=%s,line=%s::%s\n", file, lineNum, strings.TrimSpace(msg))
				} else {
					fmt.Printf("::error ::%s\n", trimmed)
				}
			}
		}
		fmt.Println()
	}
}

// printFailures prints a high‑level summary table of failed tests followed by
// detailed log excerpts.  When showLogs is false only file/line
// information is displayed for each failure; when true, all output
// lines are rendered in grey.  When runner is true, the fail lines
// themselves are also echoed in GitHub Actions annotation format so
// that the runner can surface them inline.  The human readable
// output remains unchanged to aid local debugging.
func printFailures(c colors, failed []*testResult, showLogs bool, runner bool) {
	if len(failed) == 0 {
		return
	}
	fmt.Println()
	fmt.Printf("%s\n", c.bold(fmt.Sprintf("=== FAILURES (%d) ===", len(failed))))
	// Tabular header
	tw := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintf(tw, "Package\tTest\tStatus\n")
	for _, r := range failed {
		fmt.Fprintf(tw, "%s\t%s\t%s\n", r.Package, r.Name, c.status(r.Action))
	}
	_ = tw.Flush()
	fmt.Println()
	fmt.Println(c.bold("--- FAIL DETAILS ---"))
	for _, r := range failed {
		fmt.Printf("%s %s\n", r.Package, r.Name)
		for _, line := range r.Output {
			trim := strings.TrimSpace(line)
			if trim == "" {
				continue
			}
			// If the line looks like a file:line: message, render a coloured
			// summary and optionally emit a GitHub annotation. Otherwise
			// treat it as generic output.
			if m := fileLineRe.FindStringSubmatch(trim); len(m) == 4 {
				file, lineNum, msg := m[1], m[2], m[3]
				// Human friendly formatting
				fmt.Printf("%s\n", c.style("FAIL at "+file+":"+lineNum, cRed, cBold))
				fmt.Printf("    %s\n", c.red(strings.TrimSpace(msg)))
				// Annotation for runner if requested
				if runner {
					fmt.Printf("::error file=%s,line=%s::%s\n", file, lineNum, strings.TrimSpace(msg))
				}
				continue
			}
			// Generic output line.  Only show or annotate these lines when
			// the caller has asked for logs via --show-logs.  When
			// showLogs is false we skip both the grey output and
			// annotations so as not to clutter the GitHub runner output
			// with implementation details or unrelated log noise.
			if showLogs {
				fmt.Println(c.gray(trim))
				if runner {
					fmt.Printf("::error ::%s\n", trim)
				}
			}
		}
		fmt.Println()
	}
}

// printSlowest prints the N slowest tests. If no tests have elapsed
// times recorded or N <= 0, the function returns silently. The table
// header always uses the same column order: index, package, test and
// time. Callers must ensure timed is pre‑sorted by decreasing
// duration.
func printSlowest(c colors, timed []*testResult, topN int) {
	if len(timed) == 0 || topN <= 0 {
		return
	}
	printHeader(c, "SLOWEST TESTS")
	if topN > len(timed) {
		topN = len(timed)
	}
	tw := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "#\tPackage\tTest\tTime")
	for i, r := range timed[:topN] {
		fmt.Fprintf(tw, "%d\t%s\t%s\t%s\n", i+1, r.Package, r.Name, formatSecs(r.Elapsed))
	}
	_ = tw.Flush()
}

func main() {
	condense := flag.Bool("condense", false, "condense successful tests to package-level summary")
	noColor := flag.Bool("no-color", false, "disable coloured output")
	topN := flag.Int("top", 10, "number of slowest tests to show")
	showLogs := flag.Bool("show-logs", false, "include INFO/WARN logs in fail details")
	runner := flag.Bool("runner", false, "emit GitHub Actions annotations for errors and failures")
	flag.Parse()
	colors := newColors(!*noColor)
	agg := NewAggregator(*showLogs)
	dec := json.NewDecoder(bufio.NewReader(os.Stdin))
	for {
		var ev Event
		if err := dec.Decode(&ev); err != nil {
			if err == io.EOF {
				break
			}
			// Use fmt.Fprintf on stderr rather than log to avoid automatically
			// adding timestamps. Propagate the error by exiting with a
			// non‑zero status code.
			fmt.Fprintf(os.Stderr, "decode error: %v\n", err)
			os.Exit(1)
		}
		agg.Add(ev)
	}
	fmt.Println(colors.bold("=== TEST RESULTS ==="))
	total, passed, failedCount, skipped := 0, 0, 0, 0
	pkgs := agg.Packages()
	for _, pkg := range pkgs {
		// Skip packages that only contain build errors; those will be
		// rendered separately in printBuildErrors.
		if _, hasErr := agg.pkgErrors[pkg]; hasErr {
			continue
		}
		tests := agg.testsByPkg[pkg]
		tw := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
		tTot, tPass, tFail, tSkip := printPackageBlock(colors, pkg, tests, *condense, tw)
		total += tTot
		passed += tPass
		failedCount += tFail
		skipped += tSkip
	}
	fmt.Println()
	printBuildErrors(colors, agg.pkgErrors, *runner)
	failedTests := agg.Failed()
	if len(failedTests) > 0 {
		fmt.Println()
		printFailures(colors, failedTests, *showLogs, *runner)
	}
	printSlowest(colors, agg.Timed(), *topN)
	fmt.Println()
	printHeader(colors, "SUMMARY")
	fmt.Printf("Total: %d, %s%d passed%s, %s%d failed%s, %s%d skipped%s, %s%d build errors%s\n",
		total,
		colors.green(""), passed, "",
		colors.red(""), failedCount, "",
		colors.yellow(""), skipped, "",
		colors.red(""), len(agg.pkgErrors), "",
	)
	// Exit with non‑zero status if any failures or build errors were
	// encountered. When --runner is specified annotations will have
	// already been emitted in printFailures and printBuildErrors.
	if failedCount > 0 || len(agg.pkgErrors) > 0 {
		os.Exit(1)
	}
}
