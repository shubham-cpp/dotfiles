// Package privatefs opens owner-only directories for private shell data.
package privatefs

import (
	"errors"
	"os"

	"golang.org/x/sys/unix"
)

var ErrUnowned = errors.New("unowned directory")

// OpenDirectory creates the directory when missing, rejects a final symlink,
// and restricts access only after checking ownership on the open descriptor.
func OpenDirectory(path string) (*os.File, error) {
	if err := os.MkdirAll(path, 0700); err != nil {
		return nil, err
	}
	fd, err := unix.Open(path, unix.O_RDONLY|unix.O_DIRECTORY|unix.O_NOFOLLOW|unix.O_CLOEXEC, 0)
	if err != nil {
		return nil, err
	}
	dir := os.NewFile(uintptr(fd), path)
	var info unix.Stat_t
	if err = unix.Fstat(fd, &info); err == nil && info.Uid != uint32(os.Getuid()) {
		err = ErrUnowned
	}
	if err == nil {
		err = unix.Fchmod(fd, 0700)
	}
	if err != nil {
		_ = dir.Close()
		return nil, err
	}
	return dir, nil
}
