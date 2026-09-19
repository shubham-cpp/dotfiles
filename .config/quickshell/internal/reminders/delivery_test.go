package reminders

import (
	"bufio"
	"context"
	"errors"
	"os/exec"
	"reflect"
	"testing"
	"time"

	"github.com/godbus/dbus/v5"
)

func TestNotificationIdentityActionsAndIST(t *testing.T) {
	args, err := NotificationArgs("r_1", "Title ' & <", "2026-09-12T18:30:00Z", "normal", "Bring the paperwork")
	if err != nil {
		t.Fatal(err)
	}
	if args[3] != "Title ' & <" || args[4] != "Bring the paperwork\n00:00 IST  13 Sep" {
		t.Fatal(args)
	}
	if !reflect.DeepEqual(args[5], []string{"snooze", "Snooze 10m", "done", "Done"}) {
		t.Fatal(args[5])
	}
	hints := args[6].(map[string]dbus.Variant)
	if hints["urgency"].Value() != byte(1) || hints["x-quickshell-reminder-id"].Value() != "r_1" || hints["x-canonical-private-synchronous"].Value() != "reminder/r_1" {
		t.Fatal(hints)
	}
	if sig := dbus.SignatureOf(args...).String(); sig != "susssasa{sv}i" {
		t.Fatal(sig)
	}
}

func TestOfflineAlertHasNoActions(t *testing.T) {
	args, err := NotificationArgs("", "Reminder", "", "critical", "")
	if err != nil {
		t.Fatal(err)
	}
	if len(args[5].([]string)) != 0 {
		t.Fatal(args[5])
	}
	hints := args[6].(map[string]dbus.Variant)
	if len(hints) != 1 || hints["urgency"].Value() != byte(2) {
		t.Fatal(hints)
	}
	if _, err := NotificationArgs("r", "title", "invalid", "normal", ""); err == nil {
		t.Fatal("invalid date accepted")
	}
	if _, err := NotificationArgs("", "title", "", "invalid", ""); err == nil {
		t.Fatal("invalid urgency accepted")
	}
}

func TestWakeFallbackAndTimeout(t *testing.T) {
	for _, fail := range []bool{false, true} {
		fallback := 0
		err := wake(context.Background(), "/config with spaces", func(ctx context.Context, shell string) error {
			if shell != "/config with spaces" {
				t.Fatal(shell)
			}
			deadline, ok := ctx.Deadline()
			if !ok || time.Until(deadline) > 5*time.Second {
				t.Fatal("unbounded IPC")
			}
			if fail {
				return errors.New("shell absent")
			}
			return nil
		}, func(context.Context) error { fallback++; return nil })
		if err != nil || (fallback == 1) != fail {
			t.Fatalf("%v %d", err, fallback)
		}
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	err := wake(ctx, "/config", func(context.Context, string) error { return context.Canceled }, func(context.Context) error { t.Fatal("fallback on cancellation"); return nil })
	if !errors.Is(err, context.Canceled) {
		t.Fatal(err)
	}
}

type notificationRecorder struct{ calls chan []any }

func (n *notificationRecorder) Notify(app string, replaces uint32, icon, title, body string, actions []string, hints map[string]dbus.Variant, expires int32) (uint32, *dbus.Error) {
	n.calls <- []any{app, replaces, icon, title, body, actions, hints, expires}
	return 17, nil
}

func TestPrivateBusNotificationDelivery(t *testing.T) {
	cmd := exec.Command("dbus-daemon", "--session", "--nofork", "--print-address=1")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err = cmd.Start(); err != nil {
		t.Fatal("private dbus-daemon required:", err)
	}
	defer func() { _ = cmd.Process.Kill(); _ = cmd.Wait() }()
	scanner := bufio.NewScanner(stdout)
	if !scanner.Scan() {
		t.Fatal("bus address missing")
	}
	address := scanner.Text()
	server, err := dbus.Connect(address)
	if err != nil {
		t.Fatal(err)
	}
	defer server.Close()
	recorder := &notificationRecorder{calls: make(chan []any, 1)}
	if err := server.Export(recorder, "/org/freedesktop/Notifications", notificationInterface); err != nil {
		t.Fatal(err)
	}
	if _, err := server.RequestName(notificationInterface, dbus.NameFlagDoNotQueue); err != nil {
		t.Fatal(err)
	}
	client, err := dbus.Connect(address)
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	args, err := NotificationArgs("r_2", "Sent through bus", "2026-09-12T18:30:00Z", "normal", "")
	if err != nil {
		t.Fatal(err)
	}
	if err := send(context.Background(), client, args); err != nil {
		t.Fatal(err)
	}
	select {
	case got := <-recorder.calls:
		if !reflect.DeepEqual(got, args) {
			t.Fatalf("got %#v want %#v", got, args)
		}
	case <-time.After(time.Second):
		t.Fatal("notification missing")
	}
}
