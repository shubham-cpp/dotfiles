package clipboard

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/sys/unix"
)

func fixture(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	if err := os.Mkdir(filepath.Join(root, "bin"), 0700); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", filepath.Join(root, "bin")+":"+os.Getenv("PATH"))
	t.Setenv("FIXTURE_ROOT", root)
	return root
}

func stub(t *testing.T, root, name, script string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(root, "bin", name), []byte("#!/bin/sh\n"+script+"\n"), 0700); err != nil {
		t.Fatal(err)
	}
}

func put(t *testing.T, path string, data []byte) {
	t.Helper()
	if err := os.WriteFile(path, data, 0600); err != nil {
		t.Fatal(err)
	}
}

func contentFixture(t *testing.T) (string, func(string, string, string, string) (string, error)) {
	root := fixture(t)
	put(t, filepath.Join(root, "decoded"), []byte("synthetic-content\x00\n"))
	stub(t, root, "cliphist", `cat "$FIXTURE_ROOT/decoded"
exit "${DECODE_STATUS:-0}"`)
	stub(t, root, "wl-copy", `cat > "$FIXTURE_ROOT/copied"`)
	return root, func(action, source, id, destination string) (string, error) {
		var output bytes.Buffer
		err := Content([]string{action, source, id, filepath.Join(root, "pins"), destination}, &output)
		return output.String(), err
	}
}

func TestContentRequiresSuccessfulBoundedDecodeBeforeCopy(t *testing.T) {
	for _, scenario := range []string{"success", "failed", "oversize", "empty"} {
		t.Run(scenario, func(t *testing.T) {
			root, invoke := contentFixture(t)
			if scenario == "failed" {
				t.Setenv("DECODE_STATUS", "1")
			}
			if scenario == "oversize" {
				put(t, filepath.Join(root, "decoded"), bytes.Repeat([]byte("x"), int(MaxBytes)+1))
			}
			if scenario == "empty" {
				put(t, filepath.Join(root, "decoded"), nil)
			}
			_, err := invoke("copy", "clip", "22", "")
			copied, readErr := os.ReadFile(filepath.Join(root, "copied"))
			if scenario == "success" {
				if err != nil || readErr != nil || string(copied) != "synthetic-content\x00\n" {
					t.Fatalf("copy: %v, %v, %q", err, readErr, copied)
				}
			} else if err == nil || !os.IsNotExist(readErr) {
				t.Fatalf("failed content reached copy: %v, %v", err, readErr)
			}
		})
	}
}

func TestPinsRejectSymlinkFIFOTraversalAndOversize(t *testing.T) {
	root, invoke := contentFixture(t)
	pins := filepath.Join(root, "pins")
	if err := os.Mkdir(pins, 0700); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(filepath.Join(root, "decoded"), filepath.Join(pins, "p_1")); err != nil {
		t.Fatal(err)
	}
	if err := unix.Mkfifo(filepath.Join(pins, "p_2"), 0600); err != nil {
		t.Fatal(err)
	}
	put(t, filepath.Join(pins, "p_3"), bytes.Repeat([]byte("x"), int(MaxBytes)+1))
	for _, id := range []string{"p_1", "p_2", "p_3", "../decoded"} {
		if _, err := invoke("copy", "pin", id, ""); err == nil {
			t.Fatalf("accepted %q", id)
		}
	}
	if _, err := os.Stat(filepath.Join(root, "copied")); !os.IsNotExist(err) {
		t.Fatal("invalid source invoked wl-copy")
	}
}

func TestPinExclusiveAndImageSlotsBounded(t *testing.T) {
	root, invoke := contentFixture(t)
	if _, err := invoke("pin", "clip", "22", "p_22"); err != nil {
		t.Fatal(err)
	}
	if _, err := invoke("pin", "clip", "22", "p_22"); err == nil {
		t.Fatal("overwrote existing pin")
	}
	if _, err := invoke("copy", "pin", "p_22", ""); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 20; i++ {
		path := filepath.Join(root, "cache", fmt.Sprint(i%CacheSlots))
		if output, err := invoke("image", "clip", "22", path); err != nil || strings.TrimSpace(output) != path {
			t.Fatalf("image: %q %v", output, err)
		}
	}
	entries, err := os.ReadDir(filepath.Join(root, "cache"))
	if err != nil || len(entries) != CacheSlots {
		t.Fatalf("cache size %d: %v", len(entries), err)
	}
	if _, err := invoke("image", "clip", "22", filepath.Join(root, "cache", "16")); err == nil {
		t.Fatal("accepted unbounded slot")
	}
	info, err := os.Stat(filepath.Join(root, "pins", "p_22"))
	if err != nil || info.Mode().Perm() != 0600 {
		t.Fatalf("pin permissions: %v", err)
	}
}

func TestTextOutputBoundedAndCacheDestinationSafe(t *testing.T) {
	root, invoke := contentFixture(t)
	put(t, filepath.Join(root, "decoded"), bytes.Repeat([]byte("x"), 9000))
	if output, err := invoke("text", "clip", "22", ""); err != nil || len(output) != 8000 {
		t.Fatalf("text length %d: %v", len(output), err)
	}
	cache := filepath.Join(root, "cache")
	if err := os.Mkdir(cache, 0700); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(filepath.Join(root, "decoded"), filepath.Join(cache, "0")); err != nil {
		t.Fatal(err)
	}
	if err := unix.Mkfifo(filepath.Join(cache, "1"), 0600); err != nil {
		t.Fatal(err)
	}
	for _, slot := range []string{"0", "1"} {
		if _, err := invoke("image", "clip", "22", filepath.Join(cache, slot)); err == nil {
			t.Fatal("accepted unsafe output")
		}
	}
}

type unreadable struct{ t *testing.T }

func (r unreadable) Read([]byte) (int, error) {
	r.t.Fatal("must not read this clipboard offer")
	return 0, io.EOF
}

func readState(t *testing.T, root string) watchRecord {
	t.Helper()
	raw, err := os.ReadFile(filepath.Join(root, "quickshell-clipboard-watch.json"))
	if err != nil {
		t.Fatal(err)
	}
	var record watchRecord
	if err := json.Unmarshal(raw, &record); err != nil {
		t.Fatal(err)
	}
	return record
}

func TestWatchFirstAndSensitiveCallbacksDoNotReadOrFocus(t *testing.T) {
	root := fixture(t)
	stub(t, root, "mmsg", `touch "$FIXTURE_ROOT/focused"; exit 1`)
	stub(t, root, "cliphist", `touch "$FIXTURE_ROOT/stored"; exit 1`)
	for _, state := range []string{"data", "nil", "sensitive"} {
		put(t, filepath.Join(root, "quickshell-clipboard-watch.json"), []byte(`{"watcher":99,"id":"14"}`))
		if err := Watch(root, state, 100, unreadable{t}); err != nil {
			t.Fatal(err)
		}
		if record := readState(t, root); record.Watcher != 100 || record.ID != nil {
			t.Fatalf("first callback state: %+v", record)
		}
	}
	if err := Watch(root, "sensitive", 100, unreadable{t}); err != nil {
		t.Fatal(err)
	}
	for _, file := range []string{"focused", "stored"} {
		if _, err := os.Stat(filepath.Join(root, file)); !os.IsNotExist(err) {
			t.Fatalf("sensitive callback reached %s", file)
		}
	}
}

func TestWatchUsesIsolatedRealCliphistAndClearsOnlyVerifiedOffer(t *testing.T) {
	if _, err := os.Stat("/usr/bin/cliphist"); err != nil {
		t.Skip("cliphist unavailable")
	}
	root := fixture(t)
	stub(t, root, "mmsg", `printf '%s' '{"appid":"editor"}'`)
	stub(t, root, "cliphist", `exec /usr/bin/cliphist -db-path "$FIXTURE_ROOT/history-db" -config-path /dev/null "$@"`)
	event := func(state, content string) {
		t.Helper()
		if err := Watch(root, state, 100, strings.NewReader(content)); err != nil {
			t.Fatal(err)
		}
	}
	listing := func() string {
		t.Helper()
		output, err := exec.Command(filepath.Join(root, "bin", "cliphist"), "list").Output()
		if err != nil {
			t.Fatal(err)
		}
		return string(output)
	}
	event("nil", "")
	event("data", "synthetic first")
	if !strings.Contains(listing(), "synthetic first") {
		t.Fatal("missing stored offer")
	}
	raw, _ := os.ReadFile(filepath.Join(root, "quickshell-clipboard-watch.json"))
	if bytes.Contains(raw, []byte("synthetic")) {
		t.Fatal("content leaked to state")
	}
	event("sensitive", "synthetic password")
	event("nil", "")
	if got := listing(); !strings.Contains(got, "synthetic first") || strings.Contains(got, "password") {
		t.Fatalf("privacy: %q", got)
	}
	event("data", "synthetic second")
	event("nil", "")
	event("nil", "")
	if got := listing(); !strings.Contains(got, "synthetic first") || strings.Contains(got, "second") {
		t.Fatalf("eviction: %q", got)
	}
}

func TestWatchFailsClosedAndDoesNotAssociateRejectedStore(t *testing.T) {
	for _, app := range []string{`{"appid":"org.keepassxc.KeePassXC"}`, `null`, `{"other":"editor"}`} {
		t.Run(app, func(t *testing.T) {
			root := fixture(t)
			t.Setenv("FOCUS_JSON", app)
			stub(t, root, "mmsg", `printf '%s' "$FOCUS_JSON"`)
			stub(t, root, "cliphist", `touch "$FIXTURE_ROOT/stored"`)
			if err := Watch(root, "nil", 100, unreadable{t}); err != nil {
				t.Fatal(err)
			}
			if err := Watch(root, "data", 100, strings.NewReader("synthetic secret")); err != nil {
				t.Fatal(err)
			}
			if _, err := os.Stat(filepath.Join(root, "stored")); !os.IsNotExist(err) {
				t.Fatal("stored unsafe offer")
			}
		})
	}
	root := fixture(t)
	stub(t, root, "cliphist", `case "$1" in
store) cat >/dev/null;;
-preview-width) printf '22\tolder\n';;
decode) printf 'older';;
esac`)
	if id, err := handleOffer("data", []byte("new"), nil, "editor"); err != nil || id != nil {
		t.Fatalf("associated older item: %v %v", id, err)
	}
}

func TestDirectCallbackCLIPreservesParentIdentity(t *testing.T) {
	root := fixture(t)
	binary := filepath.Join(root, "qs-clipboard")
	build := exec.Command("go", "build", "-o", binary, "./cmd/qs-clipboard")
	build.Dir = filepath.Join("..", "..")
	if output, err := build.CombinedOutput(); err != nil {
		t.Fatalf("build callback: %v %s", err, output)
	}
	stub(t, root, "mmsg", `printf '%s' '{"appid":"editor"}'`)
	stub(t, root, "cliphist", `case "$1" in
store) cat > "$FIXTURE_ROOT/stored";;
-preview-width) printf '22\tfixture\n';;
decode) cat "$FIXTURE_ROOT/stored";;
esac`)
	t.Setenv("XDG_RUNTIME_DIR", root)
	t.Setenv("CLIPBOARD_STATE", "data")
	for i := 0; i < 2; i++ {
		cmd := exec.Command(binary, "watch")
		cmd.Stdin = strings.NewReader("synthetic callback")
		if output, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("callback: %v %s", err, output)
		}
		record := readState(t, root)
		if record.Watcher != os.Getpid() {
			t.Fatalf("parent identity: %d, want %d", record.Watcher, os.Getpid())
		}
		if i == 0 && record.ID != nil {
			t.Fatal("stored initial callback")
		}
		if i == 1 && (record.ID == nil || *record.ID != "22") {
			t.Fatalf("subsequent callback not stored: %+v", record)
		}
	}
}
