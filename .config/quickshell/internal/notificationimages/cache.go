// Package notificationimages copies owned local images into a bounded history cache.
package notificationimages

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"
	"unicode/utf8"

	"golang.org/x/sys/unix"
	"quickshell.local/helpers/internal/privatefs"
)

const MaxFiles = 80
const MaxBytes int64 = 32 * 1024 * 1024
const MaxImageBytes int64 = 8 * 1024 * 1024

var keyPattern = regexp.MustCompile(`^img-[a-z0-9]+-[0-9]+$`)

type Result struct {
	Path string   `json:"path"`
	Kept []string `json:"kept"`
}

type cache struct {
	directory *os.File
	path      string
}

func openCache(path string) (*cache, error) {
	dir, err := privatefs.OpenDirectory(path)
	if errors.Is(err, privatefs.ErrUnowned) {
		return nil, errors.New("unowned cache directory")
	}
	if err != nil {
		return nil, err
	}
	return &cache{directory: dir, path: path}, nil
}

func trash(path string) error {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "gio", "trash", path)
	cmd.WaitDelay = time.Second
	return cmd.Run()
}

type cachedFile struct {
	name string
	info unix.Stat_t
}

func (c *cache) files() ([]cachedFile, error) {
	// Open a fresh descriptor so repeated scans don't inherit an exhausted offset.
	fd, err := unix.Openat(int(c.directory.Fd()), ".", unix.O_RDONLY|unix.O_DIRECTORY|unix.O_CLOEXEC, 0)
	if err != nil {
		return nil, err
	}
	dir := os.NewFile(uintptr(fd), c.path)
	defer dir.Close()
	var entries []cachedFile
	for {
		names, readErr := dir.Readdirnames(128)
		for _, name := range names {
			var info unix.Stat_t
			if err := unix.Fstatat(fd, name, &info, unix.AT_SYMLINK_NOFOLLOW); err != nil {
				if errors.Is(err, unix.ENOENT) {
					continue
				}
				return nil, err
			}
			kind := info.Mode & unix.S_IFMT
			if kind == unix.S_IFREG || kind == unix.S_IFLNK {
				entries = append(entries, cachedFile{name, info})
			}
		}
		if errors.Is(readErr, io.EOF) {
			break
		}
		if readErr != nil {
			return nil, readErr
		}
	}
	sort.SliceStable(entries, func(i, j int) bool {
		a, b := entries[i].info.Mtim, entries[j].info.Mtim
		return a.Sec > b.Sec || (a.Sec == b.Sec && a.Nsec > b.Nsec)
	})
	return entries, nil
}

func (c *cache) prune(keep map[string]bool, reserve int64) ([]string, error) {
	entries, err := c.files()
	if err != nil {
		return nil, err
	}
	retained := make([]string, 0, MaxFiles)
	total, limit := reserve, MaxFiles
	if reserve != 0 {
		limit--
	}
	for _, entry := range entries {
		if !keep[entry.name] || entry.info.Mode&unix.S_IFMT != unix.S_IFREG || len(retained) >= limit || entry.info.Size > MaxBytes-total {
			if err := trash(filepath.Join(c.path, entry.name)); err != nil {
				return nil, err
			}
			continue
		}
		retained = append(retained, entry.name)
		total += entry.info.Size
	}
	return retained, nil
}

func sourceFile(source string) (*os.File, error) {
	u, err := url.Parse(source)
	if err != nil {
		return nil, err
	}
	if u.Scheme != "file" || (u.Host != "" && u.Host != "localhost") || u.User != nil || u.RawQuery != "" || u.ForceQuery || u.Fragment != "" || strings.Contains(source, "#") || !strings.HasPrefix(u.Path, "/") || !utf8.ValidString(u.Path) {
		return nil, errors.New("not a local absolute image")
	}
	fd, err := unix.Open(u.Path, unix.O_RDONLY|unix.O_NONBLOCK|unix.O_NOFOLLOW|unix.O_CLOEXEC, 0)
	if err != nil {
		return nil, err
	}
	file := os.NewFile(uintptr(fd), u.Path)
	var info unix.Stat_t
	if err = unix.Fstat(fd, &info); err == nil && (info.Mode&unix.S_IFMT != unix.S_IFREG || info.Uid != uint32(os.Getuid()) || info.Size <= 0 || info.Size > MaxImageBytes) {
		err = errors.New("image is not owned bounded regular content")
	}
	if err != nil {
		file.Close()
		return nil, err
	}
	return file, nil
}

func (c *cache) copyImage(key, source string, keep map[string]bool) (path string, err error) {
	reader, err := sourceFile(source)
	if err != nil {
		return "", err
	}
	defer reader.Close()
	remaining := make(map[string]bool, len(keep))
	for name := range keep {
		if name != key {
			remaining[name] = true
		}
	}
	if _, err := c.prune(remaining, MaxImageBytes); err != nil {
		return "", err
	}
	pending := ".pending-" + key
	fd, err := unix.Openat(int(c.directory.Fd()), pending, unix.O_WRONLY|unix.O_CREAT|unix.O_EXCL|unix.O_NOFOLLOW|unix.O_CLOEXEC, 0600)
	if err != nil {
		return "", err
	}
	writer := os.NewFile(uintptr(fd), pending)
	defer writer.Close()
	published := false
	defer func() {
		if !published {
			if trashErr := trash(filepath.Join(c.path, pending)); trashErr != nil {
				err = trashErr
			}
		}
	}()
	if err := writer.Chmod(0600); err != nil {
		return "", err
	}
	n, err := io.Copy(writer, io.LimitReader(reader, MaxImageBytes))
	if err != nil {
		return "", err
	}
	var extra [1]byte
	count, readErr := reader.Read(extra[:])
	if count != 0 || n == 0 {
		return "", errors.New("image size changed beyond limits")
	}
	if readErr != nil && !errors.Is(readErr, io.EOF) {
		return "", readErr
	}
	if err := writer.Close(); err != nil {
		return "", err
	}
	if err := unix.Renameat(int(c.directory.Fd()), pending, int(c.directory.Fd()), key); err != nil {
		return "", err
	}
	published = true
	return filepath.Join(c.path, key), nil
}

// Update accepts the existing JSON request and returns only completely published paths.
func Update(directory string, raw []byte) (Result, error) {
	result := Result{Kept: []string{}}
	if len(raw) > 1024*1024 {
		return result, errors.New("cache request too large")
	}
	var request map[string]json.RawMessage
	if err := json.Unmarshal(raw, &request); err != nil || request == nil {
		return result, errors.New("invalid cache request")
	}
	var keepValues []json.RawMessage
	if err := json.Unmarshal(request["keep"], &keepValues); err != nil || keepValues == nil {
		return result, errors.New("invalid keep list")
	}
	key, source := "", ""
	for name, target := range map[string]*string{"key": &key, "source": &source} {
		if value, present := request[name]; present {
			if string(value) == "null" {
				return result, errors.New("invalid image request")
			}
			if err := json.Unmarshal(value, target); err != nil {
				return result, err
			}
		}
	}
	keep := make(map[string]bool)
	for i, value := range keepValues {
		if i >= MaxFiles {
			break
		}
		var item string
		if json.Unmarshal(value, &item) == nil && keyPattern.MatchString(item) {
			keep[item] = true
		}
	}
	c, err := openCache(directory)
	if err != nil {
		return result, err
	}
	defer c.directory.Close()
	if key != "" && keep[key] && keyPattern.MatchString(key) {
		// Invalid sender images must not prevent cleanup of unrelated cache entries.
		result.Path, _ = c.copyImage(key, source, keep)
	}
	result.Kept, err = c.prune(keep, 0)
	if err != nil {
		return Result{}, err
	}
	retained := false
	for _, name := range result.Kept {
		if name == key {
			retained = true
			break
		}
	}
	if !retained {
		result.Path = ""
	}
	return result, nil
}
