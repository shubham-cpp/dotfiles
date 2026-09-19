package notificationimages

import (
	"encoding/json"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"golang.org/x/sys/unix"
)

func fixture(t *testing.T) (string, string) {
	t.Helper()
	root := t.TempDir()
	for _, name := range []string{"bin", "trash"} {
		if err := os.Mkdir(filepath.Join(root, name), 0700); err != nil {
			t.Fatal(err)
		}
	}
	// The real desktop Trash is never used in these tests.
	script := `#!/bin/sh
test "$1" = trash || exit 9
test -z "$TRASH_FAIL" || exit 8
mv -- "$2" "$FIXTURE_TRASH/$(basename "$2").$$"
`
	if err := os.WriteFile(filepath.Join(root, "bin", "gio"), []byte(script), 0700); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", filepath.Join(root, "bin")+":"+os.Getenv("PATH"))
	t.Setenv("FIXTURE_TRASH", filepath.Join(root, "trash"))
	return root, filepath.Join(root, "cache")
}

func put(t *testing.T, path string, data []byte) {
	t.Helper()
	if err := os.WriteFile(path, data, 0600); err != nil {
		t.Fatal(err)
	}
}

func request(t *testing.T, directory, key, source string, keep []string) (Result, error) {
	t.Helper()
	raw, err := json.Marshal(map[string]any{"key": key, "source": source, "keep": keep})
	if err != nil {
		t.Fatal(err)
	}
	return Update(directory, raw)
}

func uri(path string) string { return (&url.URL{Scheme: "file", Path: path}).String() }

func TestCopyPublishesPrivateCompleteFileAndClearPreservesSender(t *testing.T) {
	root, directory := fixture(t)
	source := filepath.Join(root, "source with spaces.png")
	put(t, source, []byte("synthetic image\x00"))
	result, err := request(t, directory, "img-test-1", uri(source), []string{"img-test-1"})
	if err != nil || result.Path == "" || len(result.Kept) != 1 || result.Kept[0] != "img-test-1" {
		t.Fatalf("copy: %+v %v", result, err)
	}
	data, err := os.ReadFile(result.Path)
	if err != nil || string(data) != "synthetic image\x00" {
		t.Fatalf("copy data: %q %v", data, err)
	}
	info, err := os.Stat(result.Path)
	if err != nil || info.Mode().Perm() != 0600 {
		t.Fatalf("private image: %v", err)
	}
	put(t, filepath.Join(directory, "legacy-name"), []byte("legacy"))
	result, err = request(t, directory, "", "", []string{})
	if err != nil || result.Path != "" || len(result.Kept) != 0 {
		t.Fatalf("clear: %+v %v", result, err)
	}
	entries, _ := os.ReadDir(directory)
	if len(entries) != 0 {
		t.Fatalf("cache retains %d entries", len(entries))
	}
	if _, err := os.Stat(source); err != nil {
		t.Fatal("removed sender image")
	}
	trash, _ := os.ReadDir(filepath.Join(root, "trash"))
	if len(trash) != 2 {
		t.Fatalf("expected fixture Trash eviction, got %d", len(trash))
	}
}

func TestInvalidSourcesRejectSymlinkFIFOEmptyOversizeAndRemote(t *testing.T) {
	root, directory := fixture(t)
	source := filepath.Join(root, "source")
	put(t, source, []byte("image"))
	link, fifo, empty, large := filepath.Join(root, "link"), filepath.Join(root, "fifo"), filepath.Join(root, "empty"), filepath.Join(root, "large")
	if err := os.Symlink(source, link); err != nil {
		t.Fatal(err)
	}
	if err := unix.Mkfifo(fifo, 0600); err != nil {
		t.Fatal(err)
	}
	put(t, empty, nil)
	put(t, large, nil)
	if err := os.Truncate(large, MaxImageBytes+1); err != nil {
		t.Fatal(err)
	}
	for _, candidate := range []string{uri(link), uri(fifo), uri(empty), uri(large), uri(filepath.Join(root, "missing")), "https://example.invalid/image", "file://remote/tmp/image", "file:relative", uri(source) + "?q=1", uri(source) + "#fragment", "file:///tmp/%ff"} {
		result, err := request(t, directory, "img-test-1", candidate, []string{"img-test-1"})
		if err != nil || result.Path != "" {
			t.Fatalf("invalid source %q: %+v %v", candidate, result, err)
		}
	}
	result, err := request(t, directory, "../outside", uri(source), []string{"../outside"})
	if err != nil || result.Path != "" {
		t.Fatalf("invalid key: %+v %v", result, err)
	}
	entries, _ := os.ReadDir(directory)
	if len(entries) != 0 {
		t.Fatal("invalid source published file")
	}
}

func TestCountBudgetAndSymlinkEviction(t *testing.T) {
	root, directory := fixture(t)
	if err := os.Mkdir(directory, 0700); err != nil {
		t.Fatal(err)
	}
	keep := make([]string, 0, MaxFiles+3)
	for i := 0; i < MaxFiles+3; i++ {
		key := fmt.Sprintf("img-test-%d", i)
		keep = append(keep, key)
		put(t, filepath.Join(directory, key), []byte("x"))
	}
	outside := filepath.Join(root, "outside")
	put(t, outside, []byte("sender"))
	if err := os.Symlink(outside, filepath.Join(directory, "legacy-link")); err != nil {
		t.Fatal(err)
	}
	result, err := request(t, directory, "", "", keep)
	if err != nil || len(result.Kept) != MaxFiles {
		t.Fatalf("count budget: %d %v", len(result.Kept), err)
	}
	entries, _ := os.ReadDir(directory)
	if len(entries) != MaxFiles {
		t.Fatalf("active cache count %d", len(entries))
	}
	if data, err := os.ReadFile(outside); err != nil || string(data) != "sender" {
		t.Fatal("followed symlink during eviction")
	}
}

func TestByteBudgetReservesImageAndEvictsOldest(t *testing.T) {
	root, directory := fixture(t)
	source := filepath.Join(root, "source")
	put(t, source, nil)
	if err := os.Truncate(source, MaxImageBytes); err != nil {
		t.Fatal(err)
	}
	var keep []string
	for i := 0; i < 5; i++ {
		key := fmt.Sprintf("img-test-%d", i)
		keep = append([]string{key}, keep...)
		result, err := request(t, directory, key, uri(source), keep)
		if err != nil || result.Path == "" {
			t.Fatalf("copy: %+v %v", result, err)
		}
		// Explicit age order avoids depending on filesystem timestamp precision.
		stamp := time.Unix(int64(100+i), 0)
		if err := os.Chtimes(result.Path, stamp, stamp); err != nil {
			t.Fatal(err)
		}
		entries, _ := os.ReadDir(directory)
		var total int64
		for _, entry := range entries {
			info, err := entry.Info()
			if err != nil {
				t.Fatal(err)
			}
			total += info.Size()
		}
		if total > MaxBytes {
			t.Fatalf("cache byte budget exceeded: %d", total)
		}
	}
	if _, err := os.Stat(filepath.Join(directory, "img-test-0")); !os.IsNotExist(err) {
		t.Fatal("oldest image not evicted")
	}
	if _, err := os.Stat(source); err != nil {
		t.Fatal("source removed")
	}
}

func TestFailedPublicationDoesNotExposePartialPath(t *testing.T) {
	root, directory := fixture(t)
	source := filepath.Join(root, "source")
	put(t, source, []byte("image"))
	// A directory occupying the destination causes atomic rename to fail.
	if err := os.MkdirAll(filepath.Join(directory, "img-test-1"), 0700); err != nil {
		t.Fatal(err)
	}
	result, err := request(t, directory, "img-test-1", uri(source), []string{"img-test-1"})
	if err != nil || result.Path != "" || len(result.Kept) != 0 {
		t.Fatalf("failed publication: %+v %v", result, err)
	}
	entries, _ := os.ReadDir(directory)
	for _, entry := range entries {
		if strings.HasPrefix(entry.Name(), ".pending-") {
			t.Fatal("pending file remains")
		}
	}
	trash, _ := os.ReadDir(filepath.Join(root, "trash"))
	if len(trash) != 1 {
		t.Fatal("failed partial image was not moved to fixture Trash")
	}
}

func TestRejectsInvalidRequestAndCacheDirectory(t *testing.T) {
	root, directory := fixture(t)
	for _, raw := range []string{`null`, `[]`, `{}`, `{"keep":null}`, `{"keep":[],"source":null}`, `{"keep":[],"key":1}`} {
		if _, err := Update(directory, []byte(raw)); err == nil {
			t.Fatalf("accepted %s", raw)
		}
	}
	if err := os.Symlink(filepath.Join(root, "trash"), directory); err != nil {
		t.Fatal(err)
	}
	if _, err := request(t, directory, "", "", []string{}); err == nil {
		t.Fatal("accepted symlink cache directory")
	}
}

func TestTrashFailureIsReported(t *testing.T) {
	_, directory := fixture(t)
	if err := os.Mkdir(directory, 0700); err != nil {
		t.Fatal(err)
	}
	put(t, filepath.Join(directory, "legacy"), []byte("x"))
	t.Setenv("TRASH_FAIL", "1")
	if _, err := request(t, directory, "", "", []string{}); err == nil {
		t.Fatal("ignored failed eviction")
	}
}
