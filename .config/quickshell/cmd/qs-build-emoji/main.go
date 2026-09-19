package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"quickshell.local/helpers/internal/emojibuild"
)

func main() {
	home, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	cache := flag.String("cache", filepath.Join(home, ".cache/qs-emoji-sources"), "cached Unicode and CLDR source directory")
	output := flag.String("output", defaultOutput(), "generated data directory")
	check := flag.Bool("check", false, "verify sources against manifest and generated output")
	flag.Parse()
	if flag.NArg() != 0 {
		fmt.Fprintln(os.Stderr, "unexpected arguments")
		os.Exit(2)
	}
	summary, err := emojibuild.Generate(emojibuild.Options{Cache: *cache, Output: *output, Check: *check})
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fmt.Printf("%d sequences, %d families; %d bytes\n", summary.Sequences, summary.Families, summary.Bytes)
}

func defaultOutput() string {
	// Installed helpers live in <config>/.local/bin; a development binary may
	// instead be invoked from the config root. --output is always explicit.
	if executable, err := os.Executable(); err == nil {
		for dir := filepath.Dir(executable); ; dir = filepath.Dir(dir) {
			if _, err := os.Stat(filepath.Join(dir, "shell.qml")); err == nil {
				return filepath.Join(dir, "data")
			}
			if filepath.Dir(dir) == dir {
				break
			}
		}
	}
	return "data"
}
