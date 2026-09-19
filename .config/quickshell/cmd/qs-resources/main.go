package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"quickshell.local/helpers/internal/resources"
)

func main() {
	signal.Ignore(syscall.SIGPIPE)
	if err := resources.Run(os.Stdin, os.Stdout); err != nil {
		if errors.Is(err, syscall.EPIPE) {
			return
		}
		_ = json.NewEncoder(os.Stdout).Encode(map[string]string{"event": "error", "message": "Application monitor unavailable"})
		fmt.Fprintln(os.Stderr, "resource monitor:", err)
		os.Exit(1)
	}
}
