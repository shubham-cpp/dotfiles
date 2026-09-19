// Package clipboard implements bounded cliphist content jobs and wl-paste callbacks.
package clipboard

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"time"

	"golang.org/x/sys/unix"
	"quickshell.local/helpers/internal/privatefs"
)

const MaxBytes int64 = 5_000_000
const CacheSlots = 16

var pinID = regexp.MustCompile(`^p_[0-9]+(?:_[0-9]+)?$`)
var historyID = regexp.MustCompile(`^[0-9]+$`)

func privateDirectory(path string) (*os.File, error) {
	dir, err := privatefs.OpenDirectory(path)
	if errors.Is(err, privatefs.ErrUnowned) {
		return nil, errors.New("unowned clipboard directory")
	}
	return dir, err
}

func copyBounded(target io.Writer, source io.Reader, limit int64) (int64, error) {
	// Read the extra byte without publishing it to the target.
	n, err := io.Copy(target, io.LimitReader(source, limit))
	if err != nil {
		return n, err
	}
	var extra [1]byte
	count, err := io.ReadFull(source, extra[:])
	if count != 0 {
		return n, errors.New("clipboard item too large")
	}
	if err != nil && !errors.Is(err, io.EOF) {
		return n, err
	}
	return n, nil
}

func readPin(directory, id string, target io.Writer) (int64, error) {
	if !pinID.MatchString(id) {
		return 0, errors.New("invalid pin identifier")
	}
	dir, err := privateDirectory(directory)
	if err != nil {
		return 0, err
	}
	defer dir.Close()
	fd, err := unix.Openat(int(dir.Fd()), id, unix.O_RDONLY|unix.O_NOFOLLOW|unix.O_NONBLOCK|unix.O_CLOEXEC, 0)
	if err != nil {
		return 0, err
	}
	file := os.NewFile(uintptr(fd), id)
	defer file.Close()
	var info unix.Stat_t
	if err := unix.Fstat(fd, &info); err != nil {
		return 0, err
	}
	if info.Mode&unix.S_IFMT != unix.S_IFREG || info.Size > MaxBytes || info.Uid != uint32(os.Getuid()) {
		return 0, errors.New("invalid pinned content")
	}
	return copyBounded(target, file, MaxBytes)
}

func decode(id string, target io.Writer) (int64, error) {
	if !historyID.MatchString(id) {
		return 0, errors.New("invalid history identifier")
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "cliphist", "decode", id)
	cmd.WaitDelay = time.Second
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return 0, err
	}
	if err = cmd.Start(); err != nil {
		return 0, err
	}
	n, readErr := copyBounded(target, stdout, MaxBytes)
	if readErr != nil {
		_ = cmd.Process.Kill()
	}
	waitErr := cmd.Wait()
	if readErr != nil {
		return n, readErr
	}
	return n, waitErr
}

func writeContent(directory, name string, source io.Reader, exclusive bool) error {
	dir, err := privateDirectory(directory)
	if err != nil {
		return err
	}
	defer dir.Close()
	flags := unix.O_WRONLY | unix.O_CREAT | unix.O_NOFOLLOW | unix.O_NONBLOCK | unix.O_CLOEXEC
	if exclusive {
		flags |= unix.O_EXCL
	}
	fd, err := unix.Openat(int(dir.Fd()), name, flags, 0600)
	if err != nil {
		return err
	}
	file := os.NewFile(uintptr(fd), name)
	defer file.Close()
	var info unix.Stat_t
	if err = unix.Fstat(fd, &info); err != nil {
		return err
	}
	if info.Mode&unix.S_IFMT != unix.S_IFREG || info.Uid != uint32(os.Getuid()) || info.Nlink != 1 {
		return errors.New("invalid content destination")
	}
	if err = file.Chmod(0600); err != nil {
		return err
	}
	if err = file.Truncate(0); err != nil {
		return err
	}
	_, err = copyBounded(file, source, MaxBytes)
	if err != nil {
		return err
	}
	return file.Close()
}

// Content accepts the five arguments of the former clipboard-content helper.
// Decoding must finish successfully before any clipboard copy or file publication.
func Content(args []string, output io.Writer) error {
	if len(args) != 5 {
		return errors.New("invalid arguments")
	}
	action, source, id, directory, destination := args[0], args[1], args[2], args[3], args[4]
	if action != "copy" && action != "text" && action != "pin" && action != "image" {
		return errors.New("invalid action")
	}
	// Linux unnamed temporary files disappear on close without placing clipboard
	// contents in a named cache or the desktop Trash.
	fd, err := unix.Open(os.TempDir(), unix.O_TMPFILE|unix.O_RDWR|unix.O_CLOEXEC, 0600)
	if err != nil {
		return err
	}
	content := os.NewFile(uintptr(fd), "clipboard-content")
	defer content.Close()
	var size int64
	switch source {
	case "pin":
		size, err = readPin(directory, id, content)
	case "clip":
		size, err = decode(id, content)
	default:
		return errors.New("invalid source")
	}
	if err != nil {
		return err
	}
	if size == 0 {
		return errors.New("empty clipboard item")
	}
	if _, err = content.Seek(0, io.SeekStart); err != nil {
		return err
	}
	switch action {
	case "copy":
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()
		cmd := exec.CommandContext(ctx, "wl-copy")
		cmd.WaitDelay = time.Second
		cmd.Stdin = content
		return cmd.Run()
	case "text":
		_, err = io.Copy(output, io.LimitReader(content, 8000))
		return err
	case "pin":
		if !pinID.MatchString(destination) {
			return errors.New("invalid pin identifier")
		}
		return writeContent(directory, destination, content, true)
	case "image":
		cache, slot := filepath.Split(destination)
		value, parseErr := strconv.Atoi(slot)
		if cache == "" || !historyID.MatchString(slot) || parseErr != nil || value < 0 || value >= CacheSlots {
			return errors.New("invalid image cache slot")
		}
		if err := writeContent(cache, slot, content, false); err != nil {
			return err
		}
		_, err = fmt.Fprintln(output, filepath.Join(cache, slot))
		return err
	}
	return nil
}
