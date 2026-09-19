package main

import (
	"flag"
	"fmt"
	"os"
	"quickshell.local/helpers/internal/search"
)

func main() {
	catalog := flag.String("emoji-catalog", "", "emoji catalog path")
	display := flag.Bool("export-display", false, "export display-only catalog")
	flag.Parse()
	var err error
	if *display {
		var e *search.Emoji
		e, err = search.LoadEmoji(*catalog)
		if err == nil {
			err = e.Display(os.Stdout)
		}
	} else {
		err = search.Serve(os.Stdin, os.Stdout, *catalog)
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, "qs-search:", err)
		os.Exit(1)
	}
}
