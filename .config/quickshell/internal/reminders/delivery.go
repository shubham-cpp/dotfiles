// Package reminders sends native notifications without retaining action waiters.
package reminders

import (
	"context"
	"errors"
	"os/exec"
	"time"

	"github.com/godbus/dbus/v5"
)

const notificationInterface = "org.freedesktop.Notifications"

func NotificationArgs(id, title, at, urgency, description string) ([]any, error) {
	if urgency != "normal" && urgency != "critical" {
		return nil, errors.New("invalid reminder urgency")
	}
	level := byte(1)
	if urgency == "critical" {
		level = 2
	}
	body := "Open the calendar to check overdue reminders."
	actions := []string{}
	hints := map[string]dbus.Variant{"urgency": dbus.MakeVariant(level)}
	if id != "" {
		when, err := time.Parse(time.RFC3339Nano, at)
		if err != nil {
			return nil, errors.New("invalid reminder timestamp")
		}
		body = when.In(time.FixedZone("IST", 330*60)).Format("15:04 IST  02 Jan")
		if description != "" {
			body = description + "\n" + body
		}
		actions = []string{"snooze", "Snooze 10m", "done", "Done"}
		hints["x-quickshell-reminder-id"] = dbus.MakeVariant(id)
		hints["x-canonical-private-synchronous"] = dbus.MakeVariant("reminder/" + id)
	}
	return []any{"reminders", uint32(0), "", title, body, actions, hints, int32(-1)}, nil
}

func Notify(ctx context.Context, id, title, at, urgency, description string) error {
	args, err := NotificationArgs(id, title, at, urgency, description)
	if err != nil {
		return err
	}
	conn, err := dbus.ConnectSessionBus()
	if err != nil {
		return err
	}
	defer conn.Close()
	return send(ctx, conn, args)
}

func send(ctx context.Context, conn *dbus.Conn, args []any) error {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	return conn.Object(notificationInterface, "/org/freedesktop/Notifications").CallWithContext(ctx, notificationInterface+".Notify", 0, args...).Err
}

func Wake(ctx context.Context, shell string) error {
	return wake(ctx, shell, func(ctx context.Context, shell string) error {
		return exec.CommandContext(ctx, "qs", "-p", shell, "ipc", "call", "reminders", "fire").Run()
	}, func(ctx context.Context) error {
		return Notify(ctx, "", "Reminder", "", "critical", "")
	})
}

func wake(ctx context.Context, shell string, fire func(context.Context, string) error, offline func(context.Context) error) error {
	if shell == "" {
		return errors.New("missing shell path")
	}
	requestCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	err := fire(requestCtx, shell)
	cancel()
	if err == nil {
		return nil
	}
	if ctx.Err() != nil {
		return ctx.Err()
	}
	return offline(ctx)
}
