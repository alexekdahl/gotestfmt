package main

// gotestfmt formats the JSON stream from `go test -json` into a readable,
// deterministic summary suitable for both local usage and CI logs.
// It aggregates events by package/test, renders per-package results (either
// detailed lines or condensed summaries), shows failures and build errors,
// lists the slowest tests, and exits non-zero if any failures or build errors
// were encountered.
//
// Usage:
//   go test -json ./... | gotestfmt [flags]
//
// Flags:
//   --summary     condense successful packages to a 1-line summary
//   --no-colors   disable ANSI colors
//   --top N       show the N slowest tests (default 10)
//   --show-logs   include generic logs for failing tests in details
//   --github      emit GitHub Actions ::error annotations

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"
)

const (
	// Actions emitted by `go test -json` / test2json
	actRun       = "run"
	actPass      = "pass"
	actFail      = "fail"
	actSkip      = "skip"
	actError     = "error"
	actOutput    = "output"       // stdout/stderr line for a package or a test
	actBuildFail = "build-fail"   // non-standard: build failure sentinel
	actBuildOut  = "build-output" // non-standard: build log/output line
)

const (
	cReset  = "\033[0m"
	cRed    = "\033[31m"
	cGreen  = "\033[32m"
	cYellow = "\033[33m"
	cBold   = "\033[1m"
	cGray   = "\033[90m"
)

// fileLineRe matches strings of the form "<file.go>:<line>: <message>"
var fileLineRe = regexp.MustCompile(`^(.+\.go):(\d+):\s*(.*)$`)

// ansiRe matches ANSI escape sequences used for colouring. We use it
// when computing visible string lengths for alignment. Without stripping
// ANSI sequences the padding will be off because the escapes count
// towards the string length.
var ansiRe = regexp.MustCompile(`\x1b\[[0-9;]*m`)

type Event struct {
	Time    time.Time `json:"Time"`
	Action  string    `json:"Action"`
	Package string    `json:"Package"`
	Test    string    `json:"Test,omitempty"`
	Elapsed float64   `json:"Elapsed,omitempty"`
	Output  string    `json:"Output,omitempty"`
	Error   string    `json:"Error,omitempty"`
}

// testResult tracks the aggregated state for a single test
type testResult struct {
	Package string
	Name    string
	Action  string   // pass|fail|skip (final)
	Elapsed float64  // seconds
	Output  []string // collected lines (policy depends on flags)
}

// Aggregator collects events and produces test/build error summaries.
type Aggregator struct {
	results       map[string]*testResult   // key = pkg + "/" + test
	testsByPkg    map[string][]*testResult // output order per package
	pkgErrors     map[string][]string      // build/runtime errors per package
	keepAllOutput bool                     // keep all output for passing tests if true
}

type colors struct {
	enabled bool
}

func newColors(enabled bool) colors { return colors{enabled: enabled} }

// style wraps the given string in any number of ANSI codes followed by a
// reset. An explicit reset is appended to ensure state does not
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

func stripANSI(s string) string {
	return ansiRe.ReplaceAllString(s, "")
}

// visibleLen returns the display width as rune-count (ANSI stripped).
// (We do not attempt full grapheme width calculations here.)
func visibleLen(s string) int {
	return utf8.RuneCountInString(stripANSI(s))
}

func pad(n int) string {
	if n <= 0 {
		return ""
	}
	return strings.Repeat(" ", n)
}

// formatSecs formats a floating point number of seconds to three decimal
// places.
func formatSecs(s float64) string {
	return fmt.Sprintf("%.3fs", s)
}

// printHeader prints a section header in bold. The caller can include
// colour codes in the title string (for example via Colors.red) and
// they will be preserved because we don't further style the string.
func printHeader(c colors, title string) {
	fmt.Println(c.bold("=== " + title + " ==="))
}

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

	// Ignore non-test events (package-level output etc.)
	if ev.Test == "" {
		return
	}

	key := ev.Package + "/" + ev.Test
	switch ev.Action {
	case actRun:
		tr := &testResult{Package: ev.Package, Name: ev.Test}
		a.results[key] = tr
		a.testsByPkg[ev.Package] = append(a.testsByPkg[ev.Package], tr)

	case actOutput:
		// Append per-line, preserving order; store only while undecided
		// or for failures; keep for passing tests only when keepAllOutput is true.
		if tr, ok := a.results[key]; ok && (a.keepAllOutput || tr.Action == "" || tr.Action == actFail) {
			// Split on newlines; keep non-empty trimmed lines.
			for ln := range strings.SplitSeq(ev.Output, "\n") {
				if ln == "" {
					continue
				}
				tr.Output = append(tr.Output, ln)
			}
		}

	case actPass, actFail, actSkip:
		if tr, ok := a.results[key]; ok {
			tr.Action = ev.Action
			tr.Elapsed = ev.Elapsed
			// Memory hygiene: if pass and we are not keeping all output, drop buffered lines.
			if ev.Action == actPass && !a.keepAllOutput {
				tr.Output = nil
			}
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

// printPackageBlock renders tests for a package.
// If condense==true and there are no failures, it prints one summary line.
// Returns totals: tot, pass, fail, skip.
func printPackageBlock(c colors, pkg string, tests []*testResult, condense bool) (tot, pass, fail, skip int) {
	// Deterministic order
	sort.Slice(tests, func(i, j int) bool { return tests[i].Name < tests[j].Name })

	// First pass: counts & failure detection
	anyFailed := false
	for _, r := range tests {
		tot++
		switch r.Action {
		case actPass:
			pass++
		case actFail:
			fail++
			anyFailed = true
		case actSkip:
			skip++
		}
	}

	// Condensed line for all-pass packages when requested
	if condense && !anyFailed && pass > 0 {
		if skip > 0 {
			fmt.Printf("%s  %s passed (%s skipped)\n",
				c.bold(pkg),
				c.green(strconv.Itoa(pass)),
				c.yellow(strconv.Itoa(skip)),
			)
		} else {
			fmt.Printf("%s  %s passed\n",
				c.bold(pkg),
				c.green(strconv.Itoa(pass)),
			)
		}
		return
	}

	// Detailed table: second pass computes widths across ALL tests.
	statusWidth, nameWidth, timeWidth := 0, 0, 0
	for _, r := range tests {
		sw := visibleLen(c.status(r.Action))
		if sw > statusWidth {
			statusWidth = sw
		}
		if ln := utf8.RuneCountInString(r.Name); ln > nameWidth {
			nameWidth = ln
		}
		if r.Elapsed > 0 {
			tStr := "(" + formatSecs(r.Elapsed) + ")"
			if lt := utf8.RuneCountInString(tStr); lt > timeWidth {
				timeWidth = lt
			}
		}
	}

	fmt.Println(c.bold(pkg))
	for _, r := range tests {
		statusStr := c.status(r.Action)
		fmt.Print("  ")
		fmt.Print(statusStr)
		fmt.Print(pad(statusWidth - visibleLen(statusStr)))
		fmt.Print("  ")

		// Name
		nameRunes := utf8.RuneCountInString(r.Name)
		fmt.Print(r.Name)
		fmt.Print(pad(nameWidth - nameRunes))

		// Time
		if r.Elapsed > 0 {
			timeStr := "(" + formatSecs(r.Elapsed) + ")"
			fmt.Print("  ")
			fmt.Print(c.gray(timeStr))
			fmt.Print(pad(timeWidth - utf8.RuneCountInString(timeStr)))
		}
		fmt.Println()
	}
	return
}

// printBuildErrors renders build and generic errors by package. Each
// error line is trimmed and coloured red. Empty strings are skipped.
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
			fmt.Println("  " + c.red(trimmed))
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
// detailed log excerpts.
func printFailures(c colors, failed []*testResult, showLogs, runner bool) {
	if len(failed) == 0 {
		return
	}

	fmt.Println()
	fmt.Printf("%s\n", c.bold(fmt.Sprintf("=== FAILURES (%d) ===", len(failed))))

	// Column widths for summary table
	pkgWidth, testWidth, statusWidth := 0, 0, 0
	for _, r := range failed {
		if lp := utf8.RuneCountInString(r.Package); lp > pkgWidth {
			pkgWidth = lp
		}
		if lt := utf8.RuneCountInString(r.Name); lt > testWidth {
			testWidth = lt
		}
		sw := visibleLen(c.status(r.Action))
		if sw > statusWidth {
			statusWidth = sw
		}
	}

	fmt.Printf("%s", pad(2))
	fmt.Printf("%-*s  %-*s  %-*s\n", pkgWidth, "Package", testWidth, "Test", statusWidth, "Status")
	for _, r := range failed {
		statusStr := c.status(r.Action)
		fmt.Printf("%s", pad(2))
		fmt.Printf("%-*s  ", pkgWidth, r.Package)
		fmt.Printf("%-*s  ", testWidth, r.Name)
		fmt.Print(statusStr)
		fmt.Print(pad(statusWidth - visibleLen(statusStr)))
		fmt.Println()
	}

	fmt.Println()
	fmt.Println(c.bold("--- FAIL DETAILS ---"))
	for _, r := range failed {
		fmt.Printf("%s %s\n", r.Package, r.Name)
		for _, line := range r.Output {
			trim := strings.TrimSpace(line)
			if trim == "" {
				continue
			}
			if m := fileLineRe.FindStringSubmatch(trim); len(m) == 4 {
				file, lineNum, msg := m[1], m[2], m[3]
				fmt.Printf("%s\n", c.style("FAIL at "+file+":"+lineNum, cRed, cBold))
				fmt.Printf("    %s\n", c.red(strings.TrimSpace(msg)))
				if runner {
					fmt.Printf("::error file=%s,line=%s::%s\n", file, lineNum, strings.TrimSpace(msg))
				}
				continue
			}
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

	indexWidth := len(strconv.Itoa(topN))
	pkgWidth, testWidth, timeWidth := 0, 0, 0
	for i, r := range timed[:topN] {
		if w := len(strconv.Itoa(i + 1)); w > indexWidth {
			indexWidth = w
		}
		if lp := utf8.RuneCountInString(r.Package); lp > pkgWidth {
			pkgWidth = lp
		}
		if lt := utf8.RuneCountInString(r.Name); lt > testWidth {
			testWidth = lt
		}
		tStr := formatSecs(r.Elapsed)
		if lt := utf8.RuneCountInString(tStr); lt > timeWidth {
			timeWidth = lt
		}
	}

	fmt.Printf("%-*s  %-*s  %-*s  %-*s\n",
		indexWidth, "#",
		pkgWidth, "Package",
		testWidth, "Test",
		timeWidth, "Time",
	)
	for i, r := range timed[:topN] {
		fmt.Printf("%-*d  %-*s  %-*s  %-*s\n",
			indexWidth, i+1,
			pkgWidth, r.Package,
			testWidth, r.Name,
			timeWidth, formatSecs(r.Elapsed),
		)
	}
}

// ===== main ===================================================================

func main() {
	summary := flag.Bool("summary", false, "condense successful packages to package-level summary")
	noColors := flag.Bool("no-colors", false, "disable coloured output")
	topN := flag.Int("top", 10, "number of slowest tests to show")
	showLogs := flag.Bool("show-logs", false, "include INFO/WARN logs in fail details")
	githubAnnotations := flag.Bool("github", false, "emit GitHub Actions annotations for errors and failures")
	flag.Parse()

	colors := newColors(!*noColors)
	annotate := *githubAnnotations

	agg := NewAggregator(*showLogs)

	dec := json.NewDecoder(bufio.NewReader(os.Stdin))
	for {
		var ev Event
		if err := dec.Decode(&ev); err != nil {
			if err == io.EOF {
				break
			}
			fmt.Fprintf(os.Stderr, "decode error: %v\n", err)
			os.Exit(1)
		}
		agg.Add(ev)
	}

	fmt.Println(colors.bold("=== TEST RESULTS ==="))

	total, passed, failedCount, skipped := 0, 0, 0, 0
	for _, pkg := range agg.Packages() {
		tests := agg.testsByPkg[pkg]
		tTot, tPass, tFail, tSkip := printPackageBlock(colors, pkg, tests, *summary)
		total += tTot
		passed += tPass
		failedCount += tFail
		skipped += tSkip
	}

	fmt.Println()
	printBuildErrors(colors, agg.pkgErrors, annotate)

	failedTests := agg.Failed()
	if len(failedTests) > 0 {
		fmt.Println()
		printFailures(colors, failedTests, *showLogs, annotate)
	}

	printSlowest(colors, agg.Timed(), *topN)

	fmt.Println()
	printHeader(colors, "SUMMARY")
	fmt.Printf(
		"Total: %d, %s passed, %s failed, %s skipped, %s build errors\n",
		total,
		colors.green(strconv.Itoa(passed)),
		colors.red(strconv.Itoa(failedCount)),
		colors.yellow(strconv.Itoa(skipped)),
		colors.red(strconv.Itoa(len(agg.pkgErrors))),
	)

	// Exit non-zero on any failure or build error.
	if failedCount > 0 || len(agg.pkgErrors) > 0 {
		os.Exit(1)
	}
}
