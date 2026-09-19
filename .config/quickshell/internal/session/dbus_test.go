package session

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"io"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"testing"
	"time"

	"github.com/godbus/dbus/v5"
	"golang.org/x/sys/unix"
)

func privateBus(t *testing.T) string {
	t.Helper()
	cmd := exec.Command("dbus-daemon", "--session", "--nofork", "--print-address=1")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err = cmd.Start(); err != nil {
		t.Fatal("private dbus-daemon required:", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
	scanner := bufio.NewScanner(stdout)
	if !scanner.Scan() {
		t.Fatal("private bus address missing")
	}
	return scanner.Text()
}

type pipePair struct{ read, write *os.File }
type privateLogind struct {
	mu     sync.Mutex
	pipes  []pipePair
	hints  chan bool
	uid    uint32
	kind   string
	setErr bool
}

func (f *privateLogind) GetSession(id string) (dbus.ObjectPath, *dbus.Error) {
	if id != "auto" {
		return "", dbus.MakeFailedError(errors.New("wrong session"))
	}
	return "/org/freedesktop/login1/session/test", nil
}
func (f *privateLogind) Inhibit(what, who, why, mode string) (dbus.UnixFD, *dbus.Error) {
	if what != "sleep" || who != "quickshell" || mode != "delay" {
		return 0, dbus.MakeFailedError(errors.New("wrong inhibitor"))
	}
	r, w, err := os.Pipe()
	if err != nil {
		return 0, dbus.MakeFailedError(err)
	}
	f.mu.Lock()
	f.pipes = append(f.pipes, pipePair{r, w})
	descriptor := dbus.UnixFD(r.Fd())
	f.mu.Unlock()
	return descriptor, nil
}
func (f *privateLogind) SetLockedHint(value bool) *dbus.Error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.hints <- value
	if f.setErr {
		return dbus.MakeFailedError(errors.New("hint rejected"))
	}
	return nil
}
func (f *privateLogind) Get(iface, name string) (dbus.Variant, *dbus.Error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	var value any
	switch name {
	case "User":
		value = struct {
			UID  uint32
			Path dbus.ObjectPath
		}{f.uid, "/org/freedesktop/login1/user/test"}
	case "Type":
		value = f.kind
	case "Id":
		value = "test"
	case "LockedHint", "PreparingForSleep":
		value = false
	default:
		return dbus.Variant{}, dbus.MakeFailedError(errors.New("unknown property"))
	}
	return dbus.MakeVariant(value), nil
}
func (f *privateLogind) close() {
	f.mu.Lock()
	defer f.mu.Unlock()
	for _, pipe := range f.pipes {
		_ = pipe.read.Close()
		_ = pipe.write.Close()
	}
}

func busFixture(t *testing.T) (*privateLogind, *dbus.Conn, *dbus.Conn) {
	t.Helper()
	address := privateBus(t)
	server, err := dbus.Connect(address)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = server.Close() })
	client, err := dbus.Connect(address)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = client.Close() })
	fake := &privateLogind{hints: make(chan bool, 8), uid: uint32(os.Getuid()), kind: "wayland"}
	t.Cleanup(fake.close)
	for _, export := range []struct {
		path  dbus.ObjectPath
		iface string
	}{
		{managerPath, managerInterface}, {managerPath, "org.freedesktop.DBus.Properties"},
		{"/org/freedesktop/login1/session/test", sessionInterface}, {"/org/freedesktop/login1/session/test", "org.freedesktop.DBus.Properties"},
	} {
		if err := server.Export(fake, export.path, export.iface); err != nil {
			t.Fatal(err)
		}
	}
	if _, err := server.RequestName(destination, dbus.NameFlagDoNotQueue); err != nil {
		t.Fatal(err)
	}
	return fake, server, client
}

func TestPrivateBusDescriptorTransferAndOwnership(t *testing.T) {
	fake, _, client := busFixture(t)
	bus, err := NewDBus(context.Background(), client)
	if err != nil {
		t.Fatal(err)
	}
	defer bus.Close()
	delay, err := bus.Inhibit(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	file := delay.(*os.File)
	fd := int(file.Fd())
	fake.mu.Lock()
	pair := fake.pipes[0]
	fake.mu.Unlock()
	if _, err := pair.write.Write([]byte("descriptor received")); err != nil {
		t.Fatal(err)
	}
	data := make([]byte, 19)
	if _, err := io.ReadFull(file, data); err != nil || string(data) != "descriptor received" {
		t.Fatalf("%q %v", data, err)
	}
	flags, err := unix.FcntlInt(uintptr(fd), unix.F_GETFD, 0)
	if err != nil || flags&unix.FD_CLOEXEC == 0 {
		t.Fatalf("descriptor flags %d %v", flags, err)
	}
	if err := delay.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := unix.FcntlInt(uintptr(fd), unix.F_GETFD, 0); !errors.Is(err, syscall.EBADF) {
		t.Fatalf("descriptor still open: %v", err)
	}
}

func TestPrivateBusSessionValidation(t *testing.T) {
	for _, wrong := range []string{"uid", "type"} {
		t.Run(wrong, func(t *testing.T) {
			fake, _, client := busFixture(t)
			fake.mu.Lock()
			if wrong == "uid" {
				fake.uid++
			} else {
				fake.kind = "tty"
			}
			fake.mu.Unlock()
			if bus, err := NewDBus(context.Background(), client); err == nil {
				bus.Close()
				t.Fatal("invalid session accepted")
			}
		})
	}
}

func TestPrivateBusFailedHintRetainsDescriptor(t *testing.T) {
	fake, _, client := busFixture(t)
	bus, err := NewDBus(context.Background(), client)
	if err != nil {
		t.Fatal(err)
	}
	defer bus.Close()
	bridge, err := New(bus, t.TempDir(), io.Discard)
	if err != nil {
		t.Fatal(err)
	}
	if err := bridge.acquire(context.Background()); err != nil {
		t.Fatal(err)
	}
	defer bridge.release()
	bridge.sleeping = true
	descriptor := bridge.delay.(*os.File).Fd()
	fake.mu.Lock()
	fake.setErr = true
	fake.mu.Unlock()
	if err := bridge.accept(context.Background(), ack(bridge.token, true, true)); err == nil {
		t.Fatal("hint failure ignored")
	}
	if _, err := unix.FcntlInt(descriptor, unix.F_GETFD, 0); err != nil {
		t.Fatal("failed hint released delay:", err)
	}
}

func TestPrivateBusSleepAcknowledgementAndOwnerLoss(t *testing.T) {
	fake, server, client := busFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	bus, err := NewDBus(ctx, client)
	if err != nil {
		t.Fatal(err)
	}
	defer bus.Close()
	inputR, inputW := io.Pipe()
	defer inputW.Close()
	outputR, outputW := io.Pipe()
	defer outputR.Close()
	defer outputW.Close()
	bridge, err := New(bus, t.TempDir(), outputW)
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() { done <- bridge.Run(ctx, inputR) }()
	messages := make(chan map[string]any, 8)
	go func() {
		scanner := bufio.NewScanner(outputR)
		for scanner.Scan() {
			var message map[string]any
			if json.Unmarshal(scanner.Bytes(), &message) == nil {
				messages <- message
			}
		}
	}()
	next := func(event string) map[string]any {
		t.Helper()
		select {
		case m := <-messages:
			if m["event"] != event {
				t.Fatalf("want %s got %v", event, m)
			}
			return m
		case <-time.After(2 * time.Second):
			t.Fatalf("missing %s", event)
			return nil
		}
	}
	ready := next("ready")
	if err := server.Emit(managerPath, managerInterface+".PrepareForSleep", true); err != nil {
		t.Fatal(err)
	}
	sleep := next("sleep")
	stale := append(ack(ready["token"].(string), true, true), '\n')
	fresh := append(ack(sleep["token"].(string), true, true), '\n')
	if _, err := inputW.Write(append(stale, fresh[:10]...)); err != nil {
		t.Fatal(err)
	}
	if _, err := inputW.Write(fresh[10:]); err != nil {
		t.Fatal(err)
	}
	select {
	case secure := <-fake.hints:
		if !secure {
			t.Fatal("wrong hint")
		}
	case <-time.After(time.Second):
		t.Fatal("missing hint")
	}
	// A second signal ensures the first acknowledgement was fully consumed.
	if err := server.Emit(managerPath, managerInterface+".PrepareForSleep", false); err != nil {
		t.Fatal(err)
	}
	next("resume")
	if _, err := server.ReleaseName(destination); err != nil {
		t.Fatal(err)
	}
	next("lost")
	select {
	case err := <-done:
		if err == nil {
			t.Fatal("owner loss returned success")
		}
	case <-time.After(time.Second):
		t.Fatal("owner loss did not stop bridge")
	}
	select {
	case extra := <-fake.hints:
		t.Fatalf("stale ack reached bus: %v", extra)
	default:
	}
}
