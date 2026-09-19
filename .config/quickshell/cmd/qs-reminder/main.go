package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"quickshell.local/helpers/internal/reminders"
)

func run(ctx context.Context, args []string) error {
	if len(args) == 6 && args[0] == "notify" {
		return reminders.Notify(ctx, args[1], args[2], args[3], args[4], args[5])
	}
	if len(args) == 2 && args[0] == "wake" {
		return reminders.Wake(ctx, args[1])
	}
	return errors.New("usage: qs-reminder notify ID TITLE AT normal|critical DESCRIPTION; qs-reminder wake SHELL_PATH")
}

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	if err := run(ctx, os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "reminder delivery:", err)
		os.Exit(1)
	}
}
