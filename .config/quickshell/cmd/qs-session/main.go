package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"github.com/godbus/dbus/v5"
	"quickshell.local/helpers/internal/session"
)

func run() error {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	conn, err := dbus.ConnectSystemBus()
	if err != nil {
		return err
	}
	bus, err := session.NewDBus(ctx, conn)
	if err != nil {
		return err
	}
	defer bus.Close()
	bridge, err := session.New(bus, os.Getenv("XDG_RUNTIME_DIR"), os.Stdout)
	if err != nil {
		return err
	}
	return bridge.Run(ctx, os.Stdin)
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "session bridge:", err)
		os.Exit(1)
	}
}
