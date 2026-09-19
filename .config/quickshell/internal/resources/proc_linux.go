package resources

import (
	"encoding/binary"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/sys/unix"
)

// metadata includes executable identity so execve invalidates a live PID's cache.
type metadataKey struct {
	Member
	Exe string
}
type identity struct {
	App            *App
	Fallback, Base string
}

type procReader struct {
	root     string
	boot, hz float64
	pageSize uint64
	catalog  *Catalog
	metadata map[metadataKey]identity
	pidfds   pidfdOps
}

// The internal adapter permits identity-race/error tests without signaling a
// real application. Production always uses Linux pidfds, with no PID fallback.
type pidfdOps struct {
	open  func(int, int) (int, error)
	close func(int) error
	send  func(int, unix.Signal, *unix.Siginfo, int) error
}

func newProcReader(catalog *Catalog) (*procReader, error) {
	boot, err := bootTime("/proc")
	if err != nil {
		return nil, err
	}
	hz, err := clockTicks()
	if err != nil {
		return nil, err
	}
	return &procReader{root: "/proc", boot: boot, hz: hz, pageSize: uint64(os.Getpagesize()), catalog: catalog, metadata: make(map[metadataKey]identity), pidfds: pidfdOps{unix.PidfdOpen, unix.Close, unix.PidfdSendSignal}}, nil
}

func bootTime(root string) (float64, error) {
	data, err := os.ReadFile(filepath.Join(root, "stat"))
	if err != nil {
		return 0, err
	}
	for _, line := range strings.Split(string(data), "\n") {
		if strings.HasPrefix(line, "btime ") {
			return strconv.ParseFloat(strings.TrimSpace(line[6:]), 64)
		}
	}
	return 0, errors.New("boot time absent from proc stat")
}

// Linux exposes the process clock rate directly in AT_CLKTCK; do not assume HZ=100.
func clockTicks() (float64, error) {
	data, err := os.ReadFile("/proc/self/auxv")
	if err != nil {
		return 0, err
	}
	width := strconv.IntSize / 8
	value := func(b []byte) uint64 {
		if width == 4 {
			return uint64(binary.NativeEndian.Uint32(b))
		}
		return binary.NativeEndian.Uint64(b)
	}
	for i := 0; i+2*width <= len(data); i += 2 * width {
		if value(data[i:]) == 17 {
			if hz := value(data[i+width:]); hz > 0 {
				return float64(hz), nil
			}
		}
	}
	return 0, errors.New("AT_CLKTCK absent from auxiliary vector")
}

func (p *procReader) path(pid int, leaf string) string {
	return filepath.Join(p.root, strconv.Itoa(pid), leaf)
}

func (p *procReader) readStat(pid int) (record, byte, error) {
	data, err := os.ReadFile(p.path(pid, "stat"))
	if err != nil {
		return record{}, 0, err
	}
	text := string(data)
	left, right := strings.IndexByte(text, '('), strings.LastIndexByte(text, ')')
	if left < 0 || right <= left {
		return record{}, 0, errors.New("invalid process stat")
	}
	fields := strings.Fields(text[right+1:])
	if len(fields) < 22 || len(fields[0]) != 1 {
		return record{}, 0, errors.New("short process stat")
	}
	parent, err := strconv.Atoi(fields[1])
	if err != nil {
		return record{}, 0, err
	}
	user, err := strconv.ParseUint(fields[11], 10, 64)
	if err != nil {
		return record{}, 0, err
	}
	system, err := strconv.ParseUint(fields[12], 10, 64)
	if err != nil {
		return record{}, 0, err
	}
	start, err := strconv.ParseUint(fields[19], 10, 64)
	if err != nil {
		return record{}, 0, err
	}
	r := record{Member: Member{PID: pid, Started: float64(start)/p.hz + p.boot}, PPID: parent, Name: text[left+1 : right], Ticks: float64(user+system) / p.hz}
	return r, fields[0][0], nil
}

func (p *procReader) readUID(pid int) (int, error) {
	data, err := os.ReadFile(p.path(pid, "status"))
	if err != nil {
		return 0, err
	}
	for _, line := range strings.Split(string(data), "\n") {
		if strings.HasPrefix(line, "Uid:") {
			fields := strings.Fields(line)
			if len(fields) > 1 {
				return strconv.Atoi(fields[1])
			}
		}
	}
	return 0, errors.New("real uid absent from process status")
}

func (p *procReader) argv(pid int) []string {
	data, err := os.ReadFile(p.path(pid, "cmdline"))
	if err != nil || len(data) == 0 {
		return nil
	}
	text := string(data)
	separator := " "
	if strings.HasSuffix(text, "\x00") {
		separator = "\x00"
	}
	text = strings.TrimSuffix(text, separator)
	argv := strings.Split(text, separator)
	// setproctitle-style command lines sometimes use spaces instead of NULs.
	if separator == "\x00" && len(argv) == 1 && strings.Contains(text, " ") {
		argv = strings.Split(text, " ")
	}
	return argv
}

func (p *procReader) processIdentity(r record, argv []string) identity {
	base := filepath.Base(r.Exe)
	if r.Exe == "" {
		base = r.Name
	}
	app := p.flatpakApp(r.PID)
	if app == nil {
		app = p.catalog.match(r.Exe, r.Name, argv)
	}
	fallback := r.Exe
	if fallback == "" {
		fallback = r.Name
	}
	if (strings.HasPrefix(base, "python") || strings.HasPrefix(base, "node")) && len(argv) > 1 && !strings.HasPrefix(argv[1], "-") {
		fallback += ":" + argv[1]
	}
	return identity{app, fallback, base}
}

func (p *procReader) flatpakApp(pid int) *App {
	data, err := os.ReadFile(p.path(pid, "root/.flatpak-info"))
	if err != nil {
		return nil
	}
	values := iniSection(string(data), "Application")
	return p.catalog.ids[values["name"]]
}

func (p *procReader) readProcess(pid int, next map[metadataKey]identity) (record, error) {
	r, state, err := p.readStat(pid)
	if err != nil {
		return record{}, err
	}
	if state == 'Z' {
		return record{}, os.ErrNotExist
	}
	r.UID, err = p.readUID(pid)
	if err != nil {
		return record{}, err
	}
	data, err := os.ReadFile(p.path(pid, "statm"))
	if err != nil {
		return record{}, err
	}
	fields := strings.Fields(string(data))
	if len(fields) < 2 {
		return record{}, errors.New("short process statm")
	}
	pages, err := strconv.ParseUint(fields[1], 10, 64)
	if err != nil {
		return record{}, err
	}
	r.Memory = pages * p.pageSize
	if r.Memory == 0 {
		return record{}, os.ErrNotExist
	}
	r.Exe, err = os.Readlink(p.path(pid, "exe"))
	if err != nil && !errors.Is(err, os.ErrPermission) {
		// Kernel threads and processes which have dropped their executable are
		// valid rows when their stat still exists, just as psutil's exe fallback.
		if !errors.Is(err, os.ErrNotExist) {
			return record{}, err
		}
	}
	r.Exe, _, _ = strings.Cut(r.Exe, "\x00")
	if strings.HasSuffix(r.Exe, " (deleted)") {
		if _, err := os.Stat(r.Exe); errors.Is(err, os.ErrNotExist) {
			r.Exe = strings.TrimSuffix(r.Exe, " (deleted)")
		} else if err != nil {
			return record{}, err
		}
	}
	key := metadataKey{r.Member, r.Exe}
	cached, ok := p.metadata[key]
	var argv []string
	if !ok || len(r.Name) >= 15 {
		argv = p.argv(pid)
	}
	if len(r.Name) >= 15 && len(argv) > 0 {
		if name := filepath.Base(argv[0]); strings.HasPrefix(name, r.Name) {
			r.Name = name
		}
	}
	if !ok {
		cached = p.processIdentity(r, argv)
	}
	// Reading several proc files can straddle PID reuse. Never associate
	// metadata/CPU with a different process identity in the same sample.
	confirm, _, err := p.readStat(pid)
	if err != nil {
		return record{}, err
	}
	if confirm.Started != r.Started {
		return record{}, os.ErrNotExist
	}
	next[key] = cached
	r.App, r.Fallback, r.Base = cached.App, cached.Fallback, cached.Base
	return r, nil
}

func (p *procReader) records() []record {
	entries, err := os.ReadDir(p.root)
	if err != nil {
		return nil
	}
	pids := make([]int, 0, len(entries))
	for _, entry := range entries {
		if pid, err := strconv.Atoi(entry.Name()); err == nil && pid > 0 {
			pids = append(pids, pid)
		}
	}
	sort.Ints(pids)
	records := make([]record, 0, len(pids))
	next := make(map[metadataKey]identity)
	for _, pid := range pids {
		if r, err := p.readProcess(pid, next); err == nil {
			records = append(records, r)
		}
	}
	p.metadata = next
	return records
}

// signalMember pins the kernel task before re-reading its captured identity.
// No numeric-PID fallback is safe when pidfds are unavailable.
func (p *procReader) signalMember(member Member, force bool) (bool, error) {
	fd, err := p.pidfds.open(member.PID, 0)
	if err != nil {
		return false, err
	}
	defer p.pidfds.close(fd)
	r, _, err := p.readStat(member.PID)
	if err != nil {
		return false, err
	}
	uid, err := p.readUID(member.PID)
	if err != nil {
		return false, err
	}
	if r.Started != member.Started || uid != os.Getuid() {
		return false, nil
	}
	sig := unix.SIGTERM
	if force {
		sig = unix.SIGKILL
	}
	if err := p.pidfds.send(fd, sig, nil, 0); err != nil {
		return false, err
	}
	return true, nil
}

func cpuCount() int {
	// /proc/stat counts whole-machine logical CPUs, unlike affinity-aware limits.
	data, err := os.ReadFile("/proc/stat")
	if err == nil {
		count := 0
		for _, line := range strings.Split(string(data), "\n") {
			if len(line) > 3 && strings.HasPrefix(line, "cpu") && line[3] >= '0' && line[3] <= '9' {
				count++
			}
		}
		if count > 0 {
			return count
		}
	}
	return max(1, runtime.NumCPU())
}

func unavailable(err error) error { return fmt.Errorf("application monitor: %w", err) }
