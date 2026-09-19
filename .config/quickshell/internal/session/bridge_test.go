package session

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

type fakeLogind struct {
	events     chan string
	order      []string
	hintErr    error
	inhibitErr error
	sleepErr   error
	sleeping   bool
	locked     bool
	closed     int
}

type fakeDelay struct{ owner *fakeLogind }

func (d *fakeDelay) Close() error {
	d.owner.closed++
	d.owner.order = append(d.owner.order, "release")
	return nil
}
func (f *fakeLogind) SessionID() string                        { return "test" }
func (f *fakeLogind) Events() <-chan string                    { return f.events }
func (f *fakeLogind) LockedHint(context.Context) (bool, error) { return f.locked, nil }
func (f *fakeLogind) PreparingForSleep(context.Context) (bool, error) {
	return f.sleeping, f.sleepErr
}
func (f *fakeLogind) Inhibit(context.Context) (io.Closer, error) {
	if f.inhibitErr != nil {
		return nil, f.inhibitErr
	}
	f.order = append(f.order, "acquire")
	return &fakeDelay{f}, nil
}
func (f *fakeLogind) SetLockedHint(_ context.Context, value bool) error {
	if value {
		f.order = append(f.order, "hint true")
	} else {
		f.order = append(f.order, "hint false")
	}
	return f.hintErr
}

func bridgeFixture(t *testing.T) (*Bridge, *fakeLogind, *bytes.Buffer) {
	t.Helper()
	bus := &fakeLogind{events: make(chan string, 8)}
	var output bytes.Buffer
	b, err := New(bus, t.TempDir(), &output)
	if err != nil {
		t.Fatal(err)
	}
	return b, bus, &output
}

func ack(token string, secure, requested bool) []byte {
	data, _ := json.Marshal(map[string]any{"token": token, "secure": secure, "requested": requested})
	return data
}

func TestFreshSecureHintPrecedesRelease(t *testing.T) {
	b, bus, _ := bridgeFixture(t)
	ctx := context.Background()
	if err := b.acquire(ctx); err != nil {
		t.Fatal(err)
	}
	old := b.token
	if err := b.event(ctx, "sleep"); err != nil {
		t.Fatal(err)
	}
	if old == b.token {
		t.Fatal("token reused")
	}
	if err := b.accept(ctx, ack(old, true, true)); err != nil {
		t.Fatal(err)
	}
	if len(bus.order) != 1 {
		t.Fatalf("stale ack changed state: %v", bus.order)
	}
	if err := b.accept(ctx, ack(b.token, false, true)); err != nil {
		t.Fatal(err)
	}
	if bus.closed != 0 {
		t.Fatal("insecure ack released delay")
	}
	if err := b.accept(ctx, ack(b.token, true, true)); err != nil {
		t.Fatal(err)
	}
	if got := strings.Join(bus.order, ","); got != "acquire,hint false,hint true,release" {
		t.Fatal(got)
	}
	data, err := os.ReadFile(b.marker)
	if err != nil || string(data) != "test" {
		t.Fatalf("intent = %q, %v", data, err)
	}
	if err := b.accept(ctx, ack(b.token, true, true)); err != nil {
		t.Fatal(err)
	}
	if bus.closed != 1 {
		t.Fatal("delay released twice")
	}
	old = b.token
	if err := b.event(ctx, "resume"); err != nil {
		t.Fatal(err)
	}
	if old == b.token || b.delay == nil {
		t.Fatal("resume did not reacquire with new token")
	}
	if err := b.accept(ctx, ack(b.token, true, true)); err != nil {
		t.Fatal(err)
	}
	if bus.closed != 1 {
		t.Fatal("resume ack released sleep delay")
	}
	b.release()
}

func TestHintFailureAndUnrequestedStateRetainDelay(t *testing.T) {
	for _, fail := range []bool{false, true} {
		b, bus, _ := bridgeFixture(t)
		ctx := context.Background()
		if err := b.acquire(ctx); err != nil {
			t.Fatal(err)
		}
		defer b.release()
		b.sleeping = true
		if fail {
			bus.hintErr = errors.New("hint failed")
		}
		err := b.accept(ctx, ack(b.token, true, fail))
		if (err != nil) != fail || bus.closed != 0 {
			t.Fatalf("err %v, closed %d", err, bus.closed)
		}
	}
}

func TestRunEOFAndOversizeReleaseDelay(t *testing.T) {
	for _, input := range []string{"", strings.Repeat("x", maxFrame+2)} {
		b, bus, output := bridgeFixture(t)
		err := b.Run(context.Background(), io.NopCloser(strings.NewReader(input)))
		if (err != nil) != (len(input) > 0) {
			t.Fatalf("err %v", err)
		}
		if bus.closed != 1 || !strings.Contains(output.String(), `"event":"ready"`) {
			t.Fatalf("closed %d, output %s", bus.closed, output)
		}
	}
}

func TestIntentRecoveryAndDirectorySafety(t *testing.T) {
	b, _, _ := bridgeFixture(t)
	if err := os.WriteFile(b.marker, []byte("test"), 0600); err != nil {
		t.Fatal(err)
	}
	if recover, err := b.recovery(context.Background()); err != nil || !recover {
		t.Fatalf("recover %v, %v", recover, err)
	}
	if err := os.WriteFile(b.marker, []byte("previous-session"), 0600); err != nil {
		t.Fatal(err)
	}
	if recover, err := b.recovery(context.Background()); err != nil || recover {
		t.Fatalf("recover %v, %v", recover, err)
	}
	runtime := t.TempDir()
	if err := os.Symlink(t.TempDir(), filepath.Join(runtime, "qs-lock")); err != nil {
		t.Fatal(err)
	}
	if _, err := New(&fakeLogind{}, runtime, io.Discard); err == nil {
		t.Fatal("accepted symlink directory")
	}
}

func TestReadFramesFragmentedAndCoalesced(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	reader, writer := io.Pipe()
	defer reader.Close()
	frames := make(chan inputFrame, 4)
	go readFrames(ctx, reader, frames)
	go func() {
		defer writer.Close()
		_, _ = writer.Write([]byte(`{"token":`))
		_, _ = writer.Write([]byte("\"a\"}\n{\"token\":\"b\"}\n"))
	}()
	for _, expected := range []string{`{"token":"a"}`, `{"token":"b"}`} {
		select {
		case frame := <-frames:
			if string(frame.line) != expected || frame.err != nil {
				t.Fatalf("frame %+v", frame)
			}
		case <-time.After(time.Second):
			t.Fatal("buffered line not drained")
		}
	}
}

func TestMalformedAcknowledgementsDoNotChangeHint(t *testing.T) {
	b, bus, _ := bridgeFixture(t)
	for _, line := range []string{`{`, `[]`, `{"token":[]}`, `{"token":{}}`, `null`} {
		_ = b.accept(context.Background(), []byte(line))
	}
	if len(bus.order) != 0 {
		t.Fatal(bus.order)
	}
}

func TestUnterminatedAcknowledgementIsNotAccepted(t *testing.T) {
	b, bus, _ := bridgeFixture(t)
	b.sleeping = true
	input := io.NopCloser(bytes.NewReader(ack(b.token, true, true)))
	if err := b.Run(context.Background(), input); err != nil {
		t.Fatal(err)
	}
	if got := strings.Join(bus.order, ","); got != "acquire,release" {
		t.Fatalf("incomplete input changed hint: %s", got)
	}
}

func TestInitialSleepAndResumeFailure(t *testing.T) {
	b, bus, output := bridgeFixture(t)
	bus.sleeping = true
	if err := b.Run(context.Background(), io.NopCloser(strings.NewReader(""))); err != nil {
		t.Fatal(err)
	}
	decoder := json.NewDecoder(output)
	var ready, sleep map[string]any
	if err := decoder.Decode(&ready); err != nil {
		t.Fatal(err)
	}
	if err := decoder.Decode(&sleep); err != nil {
		t.Fatal(err)
	}
	if ready["event"] != "ready" || sleep["event"] != "sleep" || ready["token"] == sleep["token"] {
		t.Fatalf("ready %v, sleep %v", ready, sleep)
	}
	if ready["preparingForSleep"] != true {
		t.Fatalf("ready omitted initial sleep state: %v", ready)
	}
	bus.inhibitErr = errors.New("cannot reacquire")
	if err := b.event(context.Background(), "resume"); err == nil {
		t.Fatal("resume failure accepted")
	}
	if !strings.Contains(output.String(), `"event":"lost"`) {
		t.Fatal(output.String())
	}
}

func TestReadyIncludesAwakeStateAfterRecovery(t *testing.T) {
	b, bus, output := bridgeFixture(t)
	bus.locked = true
	if err := b.Run(context.Background(), io.NopCloser(strings.NewReader(""))); err != nil {
		t.Fatal(err)
	}
	decoder := json.NewDecoder(output)
	var ready map[string]any
	if err := decoder.Decode(&ready); err != nil {
		t.Fatal(err)
	}
	if ready["event"] != "ready" || ready["recover"] != true || ready["preparingForSleep"] != false {
		t.Fatalf("ready did not synchronize recovered awake state: %v", ready)
	}
	if err := decoder.Decode(&ready); !errors.Is(err, io.EOF) {
		t.Fatalf("unexpected startup event: %v, %v", ready, err)
	}
}

func TestSleepQueryFailureDoesNotPublishReady(t *testing.T) {
	b, bus, output := bridgeFixture(t)
	bus.sleepErr = errors.New("sleep state unavailable")
	err := b.Run(context.Background(), io.NopCloser(strings.NewReader("")))
	if !errors.Is(err, bus.sleepErr) {
		t.Fatalf("sleep query failure = %v", err)
	}
	if output.Len() != 0 || bus.closed != 1 {
		t.Fatalf("failed startup output %q, delay closed %d times", output.String(), bus.closed)
	}
}

func TestWaitRequiresSecureAndExplicitPath(t *testing.T) {
	var actions []string
	err := waitLocked(context.Background(), "/config with spaces", func(_ context.Context, shell, action string) (string, error) {
		if shell != "/config with spaces" {
			t.Fatal(shell)
		}
		actions = append(actions, action)
		return " secure\n", nil
	})
	if err != nil || strings.Join(actions, ",") != "activate,status" {
		t.Fatalf("%v %v", actions, err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Millisecond)
	defer cancel()
	err = waitLocked(ctx, "/config", func(_ context.Context, _, _ string) (string, error) { return "LockedHint=true", nil })
	if err == nil {
		t.Fatal("hint was treated as secure")
	}
	if err := WaitLocked(context.Background(), ""); err == nil {
		t.Fatal("missing path accepted")
	}
}

func TestWaitActivationFailureDoesNotQueryStatus(t *testing.T) {
	count := 0
	err := waitLocked(context.Background(), "/config", func(_ context.Context, _, action string) (string, error) {
		count++
		return "", errors.New("not running")
	})
	if err == nil || count != 1 {
		t.Fatalf("%v %d", err, count)
	}
}
