package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"quickshell.local/helpers/internal/session"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: qs-lock-wait SHELL_PATH")
		os.Exit(1)
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	if err := session.WaitLocked(ctx, os.Args[1]); err != nil {
		fmt.Fprintln(os.Stderr, "qs-lock-wait:", err)
		os.Exit(1)
	}
}
