package session

import (
	"context"
	"errors"
	"io"
	"os"
	"syscall"
	"time"

	"github.com/godbus/dbus/v5"
)

const (
	destination      = "org.freedesktop.login1"
	managerPath      = dbus.ObjectPath("/org/freedesktop/login1")
	managerInterface = destination + ".Manager"
	sessionInterface = destination + ".Session"
	callTimeout      = 1500 * time.Millisecond
)

type DBus struct {
	conn    *dbus.Conn
	session dbus.ObjectPath
	id      string
	owner   string
	events  chan string
	signals chan *dbus.Signal
	cancel  context.CancelFunc
}

// NewDBus takes ownership of a dedicated authenticated bus connection.
func NewDBus(ctx context.Context, conn *dbus.Conn) (*DBus, error) {
	ctx, cancel := context.WithCancel(ctx)
	d := &DBus{conn: conn, events: make(chan string, 16), signals: make(chan *dbus.Signal, 16), cancel: cancel}
	if err := d.initialize(ctx); err != nil {
		d.Close()
		return nil, err
	}
	go d.forward(ctx)
	return d, nil
}

func (d *DBus) initialize(ctx context.Context) error {
	if !d.conn.SupportsUnixFDs() {
		return errors.New("session bus transport cannot receive Unix descriptors")
	}
	ownerContext, cancel := context.WithTimeout(ctx, callTimeout)
	err := d.conn.BusObject().CallWithContext(ownerContext, "org.freedesktop.DBus.GetNameOwner", 0, destination).Store(&d.owner)
	cancel()
	if err != nil {
		return err
	}
	if err := d.call(ctx, managerPath, managerInterface+".GetSession", "auto").Store(&d.session); err != nil {
		return err
	}
	var user struct {
		UID  uint32
		Path dbus.ObjectPath
	}
	var kind string
	if err := d.property(ctx, d.session, sessionInterface, "User", &user); err != nil {
		return err
	}
	if err := d.property(ctx, d.session, sessionInterface, "Type", &kind); err != nil {
		return err
	}
	if user.UID != uint32(os.Getuid()) || kind != "wayland" {
		return errors.New("no owned Wayland session")
	}
	if err := d.property(ctx, d.session, sessionInterface, "Id", &d.id); err != nil {
		return err
	}
	if d.id == "" || len(d.id) > maxFrame {
		return errors.New("invalid session identifier")
	}
	d.conn.Signal(d.signals)
	matches := [][]dbus.MatchOption{
		{dbus.WithMatchSender(destination), dbus.WithMatchInterface(managerInterface), dbus.WithMatchMember("PrepareForSleep"), dbus.WithMatchObjectPath(managerPath)},
		{dbus.WithMatchSender(destination), dbus.WithMatchInterface(sessionInterface), dbus.WithMatchMember("Lock"), dbus.WithMatchObjectPath(d.session)},
		{dbus.WithMatchSender("org.freedesktop.DBus"), dbus.WithMatchInterface("org.freedesktop.DBus"), dbus.WithMatchMember("NameOwnerChanged"), dbus.WithMatchObjectPath("/org/freedesktop/DBus"), dbus.WithMatchArg(0, destination)},
	}
	for _, match := range matches {
		matchContext, cancel := context.WithTimeout(ctx, callTimeout)
		err := d.conn.AddMatchSignalContext(matchContext, match...)
		cancel()
		if err != nil {
			return err
		}
	}
	// Close the setup race between discovering the owner and subscribing to
	// owner changes. All later changes are covered by the installed match.
	ownerContext, cancel = context.WithTimeout(ctx, callTimeout)
	var owner string
	err = d.conn.BusObject().CallWithContext(ownerContext, "org.freedesktop.DBus.GetNameOwner", 0, destination).Store(&owner)
	cancel()
	if err != nil {
		return err
	}
	if owner != d.owner {
		return errors.New("session owner changed during initialization")
	}
	return nil
}

func (d *DBus) call(ctx context.Context, path dbus.ObjectPath, method string, args ...any) *dbus.Call {
	ctx, cancel := context.WithTimeout(ctx, callTimeout)
	defer cancel()
	// Address the validated unique owner. An owner replacement requires a new
	// bridge and validation, never continued calls into an unvalidated session.
	return d.conn.Object(d.owner, path).CallWithContext(ctx, method, 0, args...)
}

func (d *DBus) property(ctx context.Context, path dbus.ObjectPath, iface, name string, result any) error {
	var value dbus.Variant
	if err := d.call(ctx, path, "org.freedesktop.DBus.Properties.Get", iface, name).Store(&value); err != nil {
		return err
	}
	return value.Store(result)
}

func (d *DBus) SessionID() string     { return d.id }
func (d *DBus) Events() <-chan string { return d.events }

func (d *DBus) LockedHint(ctx context.Context) (bool, error) {
	var value bool
	err := d.property(ctx, d.session, sessionInterface, "LockedHint", &value)
	return value, err
}

func (d *DBus) PreparingForSleep(ctx context.Context) (bool, error) {
	var value bool
	err := d.property(ctx, managerPath, managerInterface, "PreparingForSleep", &value)
	return value, err
}

func (d *DBus) SetLockedHint(ctx context.Context, secure bool) error {
	return d.call(ctx, d.session, sessionInterface+".SetLockedHint", secure).Err
}

func (d *DBus) Inhibit(ctx context.Context) (io.Closer, error) {
	var descriptor dbus.UnixFD
	if err := d.call(ctx, managerPath, managerInterface+".Inhibit", "sleep", "quickshell", "Secure the session before sleep", "delay").Store(&descriptor); err != nil {
		return nil, err
	}
	if descriptor < 0 {
		return nil, errors.New("invalid sleep-delay descriptor")
	}
	syscall.CloseOnExec(int(descriptor))
	return os.NewFile(uintptr(descriptor), "logind-sleep-delay"), nil
}

func (d *DBus) signalEvent(signal *dbus.Signal) string {
	if signal == nil {
		return ""
	}
	if signal.Name == "org.freedesktop.DBus.NameOwnerChanged" && signal.Sender == "org.freedesktop.DBus" && len(signal.Body) == 3 && signal.Body[0] == destination {
		if signal.Body[2] != d.owner {
			return "lost"
		}
	}
	if signal.Sender != d.owner {
		return ""
	}
	if signal.Name == managerInterface+".PrepareForSleep" && signal.Path == managerPath && len(signal.Body) == 1 {
		if sleeping, ok := signal.Body[0].(bool); ok {
			if sleeping {
				return "sleep"
			}
			return "resume"
		}
	}
	if signal.Name == sessionInterface+".Lock" && signal.Path == d.session {
		return "lock"
	}
	return ""
}

func (d *DBus) forward(ctx context.Context) {
	defer close(d.events)
	for {
		select {
		case <-ctx.Done():
			return
		case <-d.conn.Context().Done():
			return
		case signal, ok := <-d.signals:
			if !ok {
				return
			}
			if event := d.signalEvent(signal); event != "" {
				select {
				case d.events <- event:
				case <-ctx.Done():
					return
				}
				if event == "lost" {
					return
				}
			}
		}
	}
}

func (d *DBus) Close() error {
	d.cancel()
	d.conn.RemoveSignal(d.signals)
	return d.conn.Close()
}
