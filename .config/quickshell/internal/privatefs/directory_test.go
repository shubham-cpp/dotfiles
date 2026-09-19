package privatefs

import (
	"os"
	"path/filepath"
	"testing"

	"golang.org/x/sys/unix"
)

func TestOpenDirectoryCreatesAndRestrictsAccess(t *testing.T) {
	path := filepath.Join(t.TempDir(), "private")
	for attempt := 0; attempt < 2; attempt++ {
		dir, err := OpenDirectory(path)
		if err != nil {
			t.Fatal(err)
		}
		info, err := dir.Stat()
		if err != nil {
			t.Fatal(err)
		}
		if info.Mode().Perm() != 0700 {
			t.Fatalf("directory mode = %o, want 700", info.Mode().Perm())
		}
		flags, err := unix.FcntlInt(dir.Fd(), unix.F_GETFD, 0)
		if err != nil {
			t.Fatal(err)
		}
		if flags&unix.FD_CLOEXEC == 0 {
			t.Fatal("directory descriptor would survive exec")
		}
		if err := dir.Close(); err != nil {
			t.Fatal(err)
		}
		if err := os.Chmod(path, 0777); err != nil {
			t.Fatal(err)
		}
	}
}

func TestOpenDirectoryRejectsSymlink(t *testing.T) {
	root := t.TempDir()
	link := filepath.Join(root, "link")
	if err := os.Symlink(root, link); err != nil {
		t.Fatal(err)
	}
	if dir, err := OpenDirectory(link); err == nil {
		_ = dir.Close()
		t.Fatal("accepted a symlink as a private directory")
	}
}
