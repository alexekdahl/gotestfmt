# Go Test Formatter

A simple tool to parse and format Go test output with better readability and useful summaries.

## Usage

Pipe your Go test output through this tool:

```bash
go test -json ./... | go run main.go
```

## Options

- `-condense` - Show package-level summaries for passing tests instead of individual test lines
- `-no-color` - Disable colored output  
- `-top N` - Show top N slowest tests (default: 10)
- `-show-logs` - Include all output logs in failure details
- `-github` - Emit GitHub Actions ::error annotations

## Features

- Color-coded test results (PASS/FAIL/SKIP)
- Package-grouped test organization  
- Failed test details with file locations
- Build error reporting
- Slowest test identification
- Summary statistics

## Example

```bash
# Basic usage
go test -json ./... | go run main.go

# Condensed view with top 5 slowest tests
go test -json ./... | go run main.go -condense -top 5

# No colors (for CI/logging)
go test -json ./... | go run main.go -no-color
```

The tool reads JSON test events from stdin and outputs a formatted summary to stdout. It exits with code 1 if any tests fail or build errors occur.
