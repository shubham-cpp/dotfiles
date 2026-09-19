// Package session owns the logind sleep-delay descriptor and lock acknowledgements.
package session

import (
	"bufio"
	"bytes"
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"syscall"
)

const maxFrame = 4096

// Logind has a real D-Bus adapter and a deterministic test adapter. Descriptors
// returned by Inhibit belong to Bridge, which closes them exactly once.
type Logind interface {
	SessionID() string
	LockedHint(context.Context) (bool, error)
	PreparingForSleep(context.Context) (bool, error)
	Inhibit(context.Context) (io.Closer, error)
	SetLockedHint(context.Context, bool) error
	Events() <-chan string
}

type Bridge struct {
	logind   Logind
	marker   string
	output   *json.Encoder
	token    string
	sleeping bool
	delay    io.Closer
}

func New(logind Logind, runtimeDir string, output io.Writer) (*Bridge, error) {
	if !filepath.IsAbs(runtimeDir) {
		return nil, errors.New("XDG_RUNTIME_DIR must be an absolute path")
	}
	directory := filepath.Join(runtimeDir, "qs-lock")
	if err := os.Mkdir(directory, 0700); err != nil && !errors.Is(err, os.ErrExist) {
		return nil, err
	}
	info, err := os.Lstat(directory)
	if err != nil {
		return nil, err
	}
	stat, ok := info.Sys().(*syscall.Stat_t)
	if !ok || !info.IsDir() || stat.Uid != uint32(os.Getuid()) {
		return nil, errors.New("invalid session intent directory")
	}
	if err := os.Chmod(directory, 0700); err != nil {
		return nil, err
	}
	return &Bridge{logind: logind, marker: filepath.Join(directory, "requested"), output: json.NewEncoder(output), token: newToken()}, nil
}

func newToken() string {
	var token [16]byte
	_, _ = rand.Read(token[:]) // crypto/rand.Read cannot return an error in this toolchain.
	return hex.EncodeToString(token[:])
}

func (b *Bridge) emit(event string, recover bool) error {
	message := map[string]any{"event": event, "token": b.token}
	if event == "ready" {
		message["recover"] = recover
		message["preparingForSleep"] = b.sleeping
	}
	return b.output.Encode(message)
}

func (b *Bridge) acquire(ctx context.Context) error {
	if b.delay != nil {
		return nil
	}
	fd, err := b.logind.Inhibit(ctx)
	if err != nil {
		return err
	}
	b.delay = fd
	return nil
}

func (b *Bridge) release() {
	if b.delay != nil {
		_ = b.delay.Close()
		b.delay = nil
	}
}

func (b *Bridge) recovery(ctx context.Context) (bool, error) {
	file, err := os.OpenFile(b.marker, os.O_RDONLY|syscall.O_NOFOLLOW|syscall.O_NONBLOCK, 0)
	if err == nil {
		defer file.Close()
		info, err := file.Stat()
		if err != nil || !info.Mode().IsRegular() || info.Size() > maxFrame {
			return false, errors.New("invalid session intent file")
		}
		data, err := io.ReadAll(io.LimitReader(file, maxFrame+1))
		if err != nil {
			return false, err
		}
		if string(data) == b.logind.SessionID() {
			return true, nil
		}
	} else if !errors.Is(err, os.ErrNotExist) {
		return false, err
	}
	return b.logind.LockedHint(ctx)
}

func (b *Bridge) writeIntent(requested bool) error {
	pending := filepath.Join(filepath.Dir(b.marker), "requested.pending")
	file, err := os.OpenFile(pending, os.O_WRONLY|os.O_CREATE|os.O_TRUNC|syscall.O_NOFOLLOW|syscall.O_NONBLOCK, 0600)
	if err != nil {
		return err
	}
	defer file.Close()
	info, err := file.Stat()
	if err != nil || !info.Mode().IsRegular() {
		return errors.New("invalid pending session intent")
	}
	if err := file.Chmod(0600); err != nil {
		return err
	}
	if requested {
		if _, err := io.WriteString(file, b.logind.SessionID()); err != nil {
			return err
		}
	}
	if err := file.Close(); err != nil {
		return err
	}
	return os.Rename(pending, b.marker)
}

func (b *Bridge) accept(ctx context.Context, line []byte) error {
	var message map[string]any
	if err := json.Unmarshal(line, &message); err != nil {
		return errors.New("invalid session acknowledgement")
	}
	if message["token"] != b.token {
		return nil
	}
	secure := message["secure"] == true
	requested := message["requested"] == true
	if err := b.writeIntent(requested); err != nil {
		return err
	}
	// Complete the hint before releasing the descriptor. A failed call cannot
	// acknowledge sleep; QML reacts to the error by securing the session again.
	if err := b.logind.SetLockedHint(ctx, secure); err != nil {
		return err
	}
	if secure && requested && b.sleeping {
		b.release()
	}
	return nil
}

func (b *Bridge) event(ctx context.Context, event string) error {
	switch event {
	case "sleep", "resume":
		b.token = newToken()
		b.sleeping = event == "sleep"
		if !b.sleeping {
			if err := b.acquire(ctx); err != nil {
				_ = b.emit("lost", false)
				return err
			}
		}
	case "lost":
		_ = b.emit("lost", false)
		return errors.New("session integration lost")
	case "lock":
	default:
		return fmt.Errorf("invalid session event %q", event)
	}
	return b.emit(event, false)
}

type inputFrame struct {
	line []byte
	err  error
}

// readFrames drains coalesced messages and bounds incomplete or complete lines.
func readFrames(ctx context.Context, input io.Reader, frames chan<- inputFrame) {
	scanner := bufio.NewScanner(input)
	scanner.Buffer(make([]byte, maxFrame+1), maxFrame+1)
	scanner.Split(func(data []byte, atEOF bool) (int, []byte, error) {
		if end := bytes.IndexByte(data, '\n'); end >= 0 {
			return end + 1, data[:end], nil
		}
		if atEOF {
			return len(data), nil, nil // An unterminated acknowledgement is incomplete.
		}
		return 0, nil, nil
	})
	for scanner.Scan() {
		frame := inputFrame{line: append([]byte(nil), scanner.Bytes()...)}
		select {
		case frames <- frame:
		case <-ctx.Done():
			return
		}
	}
	err := scanner.Err()
	if err == nil {
		err = io.EOF
	}
	select {
	case frames <- inputFrame{err: err}:
	case <-ctx.Done():
	}
}

func (b *Bridge) initialize(ctx context.Context) error {
	if err := b.acquire(ctx); err != nil {
		return err
	}
	recover, err := b.recovery(ctx)
	if err != nil {
		return err
	}
	b.sleeping, err = b.logind.PreparingForSleep(ctx)
	if err != nil {
		return err
	}
	if err := b.emit("ready", recover); err != nil {
		return err
	}
	if b.sleeping {
		return b.event(ctx, "sleep")
	}
	return nil
}

// Run takes ownership of input. Closing input interrupts its sole reader on exit.
func (b *Bridge) Run(ctx context.Context, input io.ReadCloser) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	defer input.Close()
	defer b.release()
	if err := b.initialize(ctx); err != nil {
		return err
	}
	frames := make(chan inputFrame, 1)
	go readFrames(ctx, input, frames)
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case event, ok := <-b.logind.Events():
			if !ok {
				event = "lost"
			}
			if err := b.event(ctx, event); err != nil {
				return err
			}
		case frame := <-frames:
			if frame.err != nil {
				if errors.Is(frame.err, io.EOF) {
					return nil
				}
				return frame.err
			}
			if err := b.accept(ctx, frame.line); err != nil {
				_ = b.emit("error", false)
				return err
			}
		}
	}
}
