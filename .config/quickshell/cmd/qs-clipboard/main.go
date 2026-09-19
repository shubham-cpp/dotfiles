package main

import (
	"fmt"
	"os"
	"quickshell.local/helpers/internal/clipboard"
)

func main() {
	var err error
	if len(os.Args) >= 2 && os.Args[1] == "content" {
		err = clipboard.Content(os.Args[2:], os.Stdout)
	} else if len(os.Args) == 2 && os.Args[1] == "watch" {
		err = clipboard.Watch(os.Getenv("XDG_RUNTIME_DIR"), os.Getenv("CLIPBOARD_STATE"), os.Getppid(), os.Stdin)
	} else {
		err = fmt.Errorf("invalid arguments")
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, "Clipboard operation failed")
		os.Exit(1)
	}
}
