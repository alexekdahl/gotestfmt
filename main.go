package main

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

type Event struct {
	Time    time.Time `json:"Time"`
	Action  string    `json:"Action"`
	Package string    `json:"Package"`
	Test    string    `json:"Test,omitempty"`
	Elapsed float64   `json:"Elapsed,omitempty"`
	Output  string    `json:"Output,omitempty"`
	Error   string    `json:"Error,omitempty"`
}

type TestResult struct {
	Package string
	Name    string
	Action  string
	Elapsed float64
	Output  []string
}

const (
	actRun   = "run"
	actPass  = "pass"
	actFail  = "fail"
	actSkip  = "skip"
	actError = "error"

	actBuildFail   = "build-fail"
	actBuildOutput = "build-output"
)

// ----------------------------------------------------------------------------
// Colors

type Colors struct {
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

func NewColors(enabled bool) Colors { return Colors{enabled: enabled} }

func (c Colors) style(s string, codes ...string) string {
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

func (c Colors) gray(s string) string   { return c.style(s, cGray) }
func (c Colors) bold(s string) string   { return c.style(s, cBold) }
func (c Colors) red(s string) string    { return c.style(s, cRed) }
func (c Colors) green(s string) string  { return c.style(s, cGreen) }
func (c Colors) yellow(s string) string { return c.style(s, cYellow) }

func (c Colors) status(action string) string {
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

// ----------------------------------------------------------------------------
// Aggregation

type Aggregator struct {
	results    map[string]*TestResult   // key: pkg/test
	testsByPkg map[string][]*TestResult // pkg -> []*TestResult (kept in insertion order, later sorted)
	pkgErrors  map[string][]string

	keepAllOutput bool // include INFO/WARN etc when showing fail details
}

func NewAggregator(keepAllOutput bool) *Aggregator {
	return &Aggregator{
		results:       make(map[string]*TestResult),
		testsByPkg:    make(map[string][]*TestResult),
		pkgErrors:     make(map[string][]string),
		keepAllOutput: keepAllOutput,
	}
}

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
	case actBuildOutput:
		if ev.Output != "" {
			a.pkgErrors[ev.Package] = append(a.pkgErrors[ev.Package], ev.Output)
		}
		return
	}

	// Ignore non-test events.
	if ev.Test == "" {
		return
	}

	key := ev.Package + "/" + ev.Test
	switch ev.Action {
	case actRun:
		tr := &TestResult{Package: ev.Package, Name: ev.Test}
		a.results[key] = tr
		a.testsByPkg[ev.Package] = append(a.testsByPkg[ev.Package], tr)

	case "output":
		// Keep output if we haven't decided outcome yet, or if we are keeping logs.
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

func (a *Aggregator) Packages() []string {
	pkgs := make([]string, 0, len(a.testsByPkg))
	for p := range a.testsByPkg {
		pkgs = append(pkgs, p)
	}
	sort.Strings(pkgs)
	return pkgs
}

func (a *Aggregator) Failed() []*TestResult {
	var out []*TestResult
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

func (a *Aggregator) Timed() []*TestResult {
	var out []*TestResult
	for _, r := range a.results {
		if r.Elapsed > 0 {
			out = append(out, r)
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Elapsed > out[j].Elapsed })
	return out
}

// ----------------------------------------------------------------------------
// Rendering

var fileLineRe = regexp.MustCompile(`^(.+\.go):(\d+):\s*(.*)$`)

func formatSecs(s float64) string {
	// Keep your 3-decimals behavior.
	return fmt.Sprintf("%.3fs", s)
}

func printHeader(c Colors, title string) {
	fmt.Println(c.bold("=== " + title + " ==="))
}

func printPackageBlock(c Colors, pkg string, tests []*TestResult, condense bool, tw *tabwriter.Writer) (tot, pass, fail, skip int) {
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
		if pkgSkipped > 0 {
			fmt.Fprintf(tw, "%s\t%s%d tests passed%s\t(%d skipped)\n",
				c.bold(pkg), c.green(""), pkgPassed, c.enabledAs(""), pkgSkipped)
		} else {
			fmt.Fprintf(tw, "%s\t%s%d tests passed%s\n",
				c.bold(pkg), c.green(""), pkgPassed, c.enabledAs(""))
		}
		_ = tw.Flush()
		return
	}

	fmt.Println(c.bold(pkg))
	for _, r := range tests {
		timeStr := ""
		if r.Elapsed > 0 {
			timeStr = c.gray(" (" + formatSecs(r.Elapsed) + ")")
		}
		// status | test name | (time)
		fmt.Fprintf(tw, "  %s\t%s\t%s\n", c.status(r.Action), r.Name, timeStr)
	}
	_ = tw.Flush()
	return
}

// helper for places where we just want color enable/disable to be neutral
func (c Colors) enabledAs(s string) string { return s }

func printBuildErrors(c Colors, pkgErrors map[string][]string) {
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
			fmt.Println("  " + c.red(strings.TrimRight(line, "\n")))
		}
		fmt.Println()
	}
}

func printFailures(c Colors, failed []*TestResult, showLogs bool) {
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
			if m := fileLineRe.FindStringSubmatch(trim); len(m) == 4 {
				file, lineNum, msg := m[1], m[2], m[3]
				fmt.Printf("%s\n", c.style("FAIL at "+file+":"+lineNum, cRed, cBold))
				fmt.Printf("    %s\n", c.red(strings.TrimSpace(msg)))
				continue
			}
			if showLogs {
				fmt.Println(c.gray(trim))
			}
		}
		fmt.Println()
	}
}

func printSlowest(c Colors, timed []*TestResult, topN int) {
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
	noColor := flag.Bool("no-color", false, "disable colored output")
	topN := flag.Int("top", 10, "number of slowest tests to show")
	showLogs := flag.Bool("show-logs", false, "include INFO/WARN logs in fail details")
	flag.Parse()

	colors := NewColors(!*noColor)

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

	total, passed, failed, skipped := 0, 0, 0, 0
	pkgs := agg.Packages()

	for _, pkg := range pkgs {
		if _, hasErr := agg.pkgErrors[pkg]; hasErr {
			continue
		}
		tests := agg.testsByPkg[pkg]
		// Render package block
		tw := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
		tTot, tPass, tFail, tSkip := printPackageBlock(colors, pkg, tests, *condense, tw)
		total += tTot
		passed += tPass
		failed += tFail
		skipped += tSkip
	}

	fmt.Println()
	printBuildErrors(colors, agg.pkgErrors)

	failedTests := agg.Failed()
	if len(failedTests) > 0 {
		fmt.Println()
		printFailures(colors, failedTests, *showLogs)
	}

	printSlowest(colors, agg.Timed(), *topN)

	fmt.Println()
	printHeader(colors, "SUMMARY")
	fmt.Printf("Total: %d, %s%d passed%s, %s%d failed%s, %s%d skipped%s, %s%d build errors%s\n",
		total,
		colors.green(""), passed, colors.enabledAs(""),
		colors.red(""), failed, colors.enabledAs(""),
		colors.yellow(""), skipped, colors.enabledAs(""),
		colors.red(""), len(agg.pkgErrors), colors.enabledAs(""),
	)

	if failed > 0 || len(agg.pkgErrors) > 0 {
		os.Exit(1)
	}
}
