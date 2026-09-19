package clipboard

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"golang.org/x/sys/unix"
	"golang.org/x/text/cases"
)

var ignoredApps = map[string]bool{
	"org.keepassxc.keepassxc": true, "keepassxc": true, "bitwarden": true,
	"com.bitwarden.desktop": true, "1password": true, "com.1password.1password": true,
}

type boundedOutput struct {
	bytes.Buffer
	limit int
}

func (b *boundedOutput) Write(p []byte) (int, error) {
	if len(p) > b.limit-b.Len() {
		return 0, errors.New("command output exceeds limit")
	}
	return b.Buffer.Write(p)
}

func focusedApp() (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "mmsg", "get", "focusing-client")
	cmd.WaitDelay = time.Second
	output := &boundedOutput{limit: 65536}
	cmd.Stdout = output
	if err := cmd.Run(); err != nil {
		return "", err
	}
	var client struct {
		AppID *string `json:"appid"`
	}
	if err := json.Unmarshal(output.Bytes(), &client); err != nil {
		return "", err
	}
	if client.AppID == nil {
		return "", errors.New("missing focused application")
	}
	return cases.Fold().String(*client.AppID), nil
}

func cliphist(action string, input []byte, output io.Writer) error {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	args := []string{action}
	if action == "list" {
		args = []string{"-preview-width", "0", action}
	}
	cmd := exec.CommandContext(ctx, "cliphist", args...)
	cmd.WaitDelay = time.Second
	cmd.Stdin = bytes.NewReader(input)
	cmd.Stdout = output
	return cmd.Run()
}

// Retain only the first ID, even when the database's listing is large.
type firstIDWriter struct {
	id   []byte
	done bool
}

func (w *firstIDWriter) Write(p []byte) (int, error) {
	for _, value := range p {
		if w.done {
			break
		}
		if value == '\t' {
			w.done = true
			break
		}
		if value < '0' || value > '9' || len(w.id) >= 32 {
			return 0, errors.New("invalid history listing")
		}
		w.id = append(w.id, value)
	}
	return len(p), nil
}

// Verify a stored offer without allocating a second full clipboard payload.
type compareWriter struct {
	expected []byte
	offset   int
	equal    bool
}

func (w *compareWriter) Write(p []byte) (int, error) {
	if len(p) > len(w.expected)-w.offset {
		w.equal = false
		return 0, errors.New("decoded offer exceeds expected size")
	}
	if !bytes.Equal(w.expected[w.offset:w.offset+len(p)], p) {
		w.equal = false
	}
	w.offset += len(p)
	return len(p), nil
}

func handleOffer(state string, content []byte, previous *string, app string) (*string, error) {
	if state == "nil" || state == "clear" || (state == "data" && len(content) == 0) {
		if previous != nil && historyID.MatchString(*previous) {
			return nil, cliphist("delete", []byte(*previous+"\n"), io.Discard)
		}
		return nil, nil
	}
	if state != "data" || ignoredApps[app] || int64(len(content)) > MaxBytes {
		return nil, nil
	}
	if len(bytes.Trim(content, " \t\n\r\v\f")) == 0 {
		return previous, nil
	}
	if err := cliphist("store", content, io.Discard); err != nil {
		return nil, err
	}
	first := &firstIDWriter{}
	if err := cliphist("list", nil, first); err != nil {
		return nil, err
	}
	if !first.done || len(first.id) == 0 {
		return nil, nil
	}
	comparison := &compareWriter{expected: content, equal: true}
	if err := cliphist("decode", first.id, comparison); err != nil {
		// A policy-rejected store can leave an older, larger item at the head.
		if !comparison.equal {
			return nil, nil
		}
		return nil, err
	}
	if !comparison.equal || comparison.offset != len(content) {
		return nil, nil
	}
	id := string(first.id)
	return &id, nil
}

type watchRecord struct {
	Watcher int     `json:"watcher"`
	ID      *string `json:"id"`
}

// Watch is one direct wl-paste callback, not a second clipboard database or daemon.
// The caller supplies its actual parent PID, preserving first-callback suppression.
func Watch(runtime, state string, owner int, input io.Reader) error {
	if runtime == "" || !filepath.IsAbs(runtime) {
		return errors.New("missing runtime directory")
	}
	dir, err := privateDirectory(runtime)
	if err != nil {
		return err
	}
	defer dir.Close()
	name := "quickshell-clipboard-watch.json"
	fd, err := unix.Openat(int(dir.Fd()), name, unix.O_RDWR|unix.O_CREAT|unix.O_NOFOLLOW|unix.O_NONBLOCK|unix.O_CLOEXEC, 0600)
	if err != nil {
		return err
	}
	record := os.NewFile(uintptr(fd), name)
	defer record.Close()
	var info unix.Stat_t
	if err := unix.Fstat(fd, &info); err != nil {
		return err
	}
	if info.Mode&unix.S_IFMT != unix.S_IFREG || info.Uid != uint32(os.Getuid()) || info.Nlink != 1 {
		return errors.New("invalid watcher state file")
	}
	if err := record.Chmod(0600); err != nil {
		return err
	}
	if err := unix.Flock(fd, unix.LOCK_EX); err != nil {
		return err
	}
	defer unix.Flock(fd, unix.LOCK_UN)
	raw, err := io.ReadAll(io.LimitReader(record, 4096))
	if err != nil {
		return err
	}
	var saved watchRecord
	if err := json.Unmarshal(raw, &saved); err != nil {
		saved = watchRecord{}
	}
	write := func(id *string) error {
		data, err := json.Marshal(watchRecord{Watcher: owner, ID: id})
		if err != nil {
			return err
		}
		if _, err = record.Seek(0, io.SeekStart); err != nil {
			return err
		}
		if _, err = record.Write(data); err != nil {
			return err
		}
		return record.Truncate(int64(len(data)))
	}
	if saved.Watcher != owner {
		return write(nil)
	}
	// Sensitive offers never reach stdin reads or compositor inspection.
	var content []byte
	if state == "data" {
		content, err = io.ReadAll(io.LimitReader(input, MaxBytes+1))
		if err != nil {
			_ = write(nil)
			return err
		}
	}
	app := ""
	if state == "data" && len(content) != 0 {
		app, err = focusedApp()
		if err != nil {
			state = "unavailable"
		}
	}
	next, operationErr := handleOffer(state, content, saved.ID, app)
	if operationErr != nil {
		next = nil
	}
	if err := write(next); err != nil {
		return err
	}
	return operationErr
}
