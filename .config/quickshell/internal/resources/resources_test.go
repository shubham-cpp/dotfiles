package resources

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"golang.org/x/sys/unix"
)

func testRecord(pid, parent int, base string) record {
	return record{Member: Member{pid, 10}, PPID: parent, UID: 1000, Name: base, Base: base, Fallback: "/bin/" + base, Memory: 100, CPU: 1}
}

func TestGrouping(t *testing.T) {
	t.Run("children and instances inherit desktop identity", func(t *testing.T) {
		app := &App{Key: "browser", Name: "Browser", Icon: "browser-icon"}
		rows := []record{testRecord(10, 1, "browser"), testRecord(11, 10, "renderer"), testRecord(12, 11, "gpu"), testRecord(20, 1, "browser")}
		rows[0].App, rows[3].App = app, app
		groups := groupProcesses(rows, nil, 1000)
		if len(groups) != 1 || groups[0].Memory != 400 || groups[0].CPU != 4 || len(groups[0].Members) != 4 || groups[0].Icon != "browser-icon" {
			t.Fatalf("unexpected groups: %+v", groups)
		}
	})
	t.Run("terminal boundaries and independent desktop identity", func(t *testing.T) {
		rows := []record{testRecord(10, 1, "kitty"), testRecord(11, 10, "fish"), testRecord(12, 11, "codex"), testRecord(13, 12, "node"), testRecord(14, 12, "nvim")}
		rows[0].App = &App{Key: "kitty", Name: "kitty"}
		rows[4].App = &App{Key: "nvim", Name: "Neovim"}
		groups := groupProcesses(rows, nil, 1000)
		if len(groups) != 4 {
			t.Fatalf("unexpected groups: %+v", groups)
		}
		for _, g := range groups {
			if g.Name == "codex" && len(g.Members) != 2 {
				t.Fatal(g)
			}
		}
	})
	t.Run("protected other user and helper", func(t *testing.T) {
		rows := []record{testRecord(10, 1, "quickshell"), testRecord(11, 10, "python3"), testRecord(20, 1, "other"), testRecord(30, 1, "reader")}
		rows[2].UID = 1001
		groups := groupProcesses(rows, map[int]bool{11: true, 30: true}, 1000)
		if len(groups) != 3 || len(groups[0].Members) != 2 {
			t.Fatal(groups)
		}
		for _, group := range groups {
			if group.CanEnd {
				t.Fatalf("protected group can end: %+v", group)
			}
		}
	})
	t.Run("parent cycle terminates", func(t *testing.T) {
		if groups := groupProcesses([]record{testRecord(10, 11, "browser"), testRecord(11, 10, "browser")}, nil, 1000); len(groups) == 0 {
			t.Fatal(groups)
		}
	})
}

func fixtureMonitor() (*Monitor, *[]record, *time.Time) {
	rows := []record{testRecord(123, 1, "test-app")}
	rows[0].CPU, rows[0].Ticks = 0, 1
	now := time.Unix(100, 0)
	m := &Monitor{readRecords: func() []record { return append([]record(nil), rows...) }, now: func() time.Time { return now }, cpus: 4, uid: 1000, previous: make(map[Member]float64), groups: make([]Group, 0), allowed: make(map[string]map[Member]bool), ending: make(map[string]time.Time)}
	m.signalMember = func(Member, bool) (bool, error) { return true, nil }
	return m, &rows, &now
}

func actionRequest(key string, members []Member, force bool) map[string]json.RawMessage {
	data, _ := json.Marshal(map[string]any{"key": key, "members": members, "force": force})
	var request map[string]json.RawMessage
	_ = json.Unmarshal(data, &request)
	return request
}

func TestSamplingIntervals(t *testing.T) {
	m, rows, now := fixtureMonitor()
	if cpu := m.sample().Apps[0].CPU; cpu != -1 {
		t.Fatal(cpu)
	}
	*now = now.Add(2 * time.Second)
	(*rows)[0].Ticks = 3
	if cpu := m.sample().Apps[0].CPU; cpu != 25 {
		t.Fatal(cpu)
	}
	*now = now.Add(2 * time.Second)
	(*rows)[0].Started, (*rows)[0].Ticks = 20, 100
	if cpu := m.sample().Apps[0].CPU; cpu != 0 {
		t.Fatal("PID reuse inherited CPU", cpu)
	}
	*now = now.Add(2 * time.Second)
	(*rows)[0].Started, (*rows)[0].Ticks = 105, 1
	if cpu := m.sample().Apps[0].CPU; cpu != 12.5 {
		t.Fatal("new process interval", cpu)
	}
	m.resetCPU()
	if cpu := m.sample().Apps[0].CPU; cpu != -1 {
		t.Fatal("resume did not reset CPU", cpu)
	}
}

func TestActionRefreshPreservesCPUInterval(t *testing.T) {
	m, rows, now := fixtureMonitor()
	app := m.sample().Apps[0]
	*now = now.Add(time.Second)
	(*rows)[0].Ticks = 2
	if result := m.end(actionRequest(app.Key, app.Members, false)); !result.OK {
		t.Fatal(result)
	}
	if m.sampleTime.Unix() != 100 || m.previous[(*rows)[0].Member] != 1 || m.message(*now).Apps[0].CPU != -1 {
		t.Fatal("action advanced sampling state")
	}
	*now = now.Add(time.Second)
	(*rows)[0].Ticks = 3
	if cpu := m.sample().Apps[0].CPU; cpu != 25 {
		t.Fatal(cpu)
	}
}

func TestTerminationRevalidationAndForce(t *testing.T) {
	m, rows, now := fixtureMonitor()
	app := m.sample().Apps[0]
	signaled := []Member{}
	m.signalMember = func(member Member, force bool) (bool, error) { signaled = append(signaled, member); return true, nil }
	if result := m.end(actionRequest(app.Key, app.Members, true)); result.OK || result.Message != "Try ending the application first" {
		t.Fatal(result)
	}
	if len(signaled) != 0 {
		t.Fatal(signaled)
	}
	stale := []Member{{123, 9}}
	if m.end(actionRequest(app.Key, stale, false)).OK {
		t.Fatal("stale identity accepted")
	}
	(*rows)[0].Base = "quickshell"
	if m.end(actionRequest(app.Key, app.Members, false)).OK {
		t.Fatal("newly protected process accepted")
	}
	(*rows)[0].Base = "test-app"
	if !m.end(actionRequest(app.Key, app.Members, false)).OK {
		t.Fatal("valid end denied")
	}
	if m.message(*now).Apps[0].State != "ending" {
		t.Fatal("ending state missing")
	}
	*now = now.Add(3999 * time.Millisecond)
	if m.end(actionRequest(app.Key, app.Members, true)).OK {
		t.Fatal("force accepted early")
	}
	*now = now.Add(time.Millisecond)
	if m.message(*now).Apps[0].State != "force" || !m.end(actionRequest(app.Key, app.Members, true)).OK {
		t.Fatal("force not available")
	}
	*rows = nil
	if result := m.end(actionRequest(app.Key, app.Members, false)); result.OK {
		t.Fatal("removed group accepted")
	}
	m.sample()
	if len(m.ending) != 0 {
		t.Fatal("exited group retained")
	}
}

func TestTerminationFailuresAndMalformedMembers(t *testing.T) {
	m, _, _ := fixtureMonitor()
	app := m.sample().Apps[0]
	for _, input := range []string{`{"key":null,"members":[]}`, `{"key":123,"members":[]}`, `{"key":"x","members":null}`, `{"key":"x","members":{}}`} {
		var request map[string]json.RawMessage
		_ = json.Unmarshal([]byte(input), &request)
		if result := m.end(request); result.Message != "Invalid application selection" {
			t.Fatal(input, result)
		}
	}
	request := actionRequest(app.Key, app.Members, false)
	request["members"] = json.RawMessage(`[{"pid":123.0,"started":10},{"pid":true,"started":10},null,{"pid":123,"started":null}]`)
	m.signalMember = func(Member, bool) (bool, error) { t.Fatal("invalid member signaled"); return false, nil }
	if m.end(request).OK {
		t.Fatal("invalid selection succeeded")
	}
	request = actionRequest(app.Key, app.Members, false)
	m.signalMember = func(Member, bool) (bool, error) { return false, unix.ESRCH }
	if result := m.end(request); result.Message != "Application exited or is no longer available to end" {
		t.Fatal(result)
	}
	m.signalMember = func(Member, bool) (bool, error) { return false, unix.EPERM }
	if result := m.end(request); result.OK || result.Message != "Some processes could not be ended" {
		t.Fatal(result)
	}
}

func TestStreamFramingPauseAndDeadline(t *testing.T) {
	m, _, now := fixtureMonitor()
	output := []any{}
	s := &monitorStream{monitor: m, deadline: *now, emit: func(message any) error { output = append(output, message); return nil }}
	if err := s.tick(); err != nil {
		t.Fatal(err)
	}
	deadline := s.deadline
	*now = now.Add(time.Second)
	app := m.groups[0]
	action, _ := json.Marshal(map[string]any{"action": "end", "key": app.Key, "members": app.Members})
	action = append(action, '\n')
	if more, err := s.consume(append([]byte("{\"action\":\"pause\",\"paused\":true}\n"), action[:12]...)); !more || err != nil {
		t.Fatal(more, err)
	}
	if more, err := s.consume(action[12:]); !more || err != nil {
		t.Fatal(more, err)
	}
	if err := s.tick(); err != nil {
		t.Fatal(err)
	}
	if !s.paused || len(output) != 3 || !s.deadline.Equal(deadline) || m.sampleTime.Unix() != 100 {
		t.Fatal("paused action changed sampling")
	}
	if more, err := s.consume([]byte("{\"action\":\"pause\",\"paused\":false}\n")); !more || err != nil {
		t.Fatal(more, err)
	}
	if err := s.tick(); err != nil {
		t.Fatal(err)
	}
	if s.paused || m.groups[0].CPU != -1 || len(output) != 4 {
		t.Fatal("resume did not sample with fresh CPU")
	}
	if more, err := s.consume(nil); more || err != nil {
		t.Fatal(more, err)
	}
	if more, err := s.consume(make([]byte, maxFrame+1)); more || err != nil {
		t.Fatal(more, err)
	}
}

func TestStreamMalformedCoalescedAndUTF8(t *testing.T) {
	m, _, now := fixtureMonitor()
	output := []any{}
	s := &monitorStream{monitor: m, deadline: *now, emit: func(message any) error { output = append(output, message); return nil }}
	text := []byte("[]\nnull\n1\ninvalid\n{\"action\":\"unknown\",\"name\":\"é\\n🙂\"}\n")
	for _, b := range text {
		if more, err := s.consume([]byte{b}); !more || err != nil {
			t.Fatal(more, err)
		}
	}
	if len(output) != 1 || output[0].(Action).Message != "Invalid application selection" {
		t.Fatal(output)
	}
	s.emit = func(any) error { return unix.EPIPE }
	if _, err := s.consume([]byte("invalid\n")); !errors.Is(err, unix.EPIPE) {
		t.Fatal(err)
	}
}

func TestActionDoesNotMoveSamplingDeadline(t *testing.T) {
	m, _, now := fixtureMonitor()
	emitted := 0
	s := &monitorStream{monitor: m, deadline: *now, emit: func(any) error { emitted++; return nil }}
	if err := s.tick(); err != nil {
		t.Fatal(err)
	}
	deadline := s.deadline
	*now = now.Add(time.Second)
	if _, err := s.consume([]byte("{\"action\":\"end\",\"key\":\"absent\",\"members\":[]}\n")); err != nil {
		t.Fatal(err)
	}
	if err := s.tick(); err != nil {
		t.Fatal(err)
	}
	if emitted != 3 || !s.deadline.Equal(deadline) {
		t.Fatal(emitted, s.deadline)
	}
	*now = now.Add(time.Second)
	if err := s.tick(); err != nil {
		t.Fatal(err)
	}
	if emitted != 4 {
		t.Fatal(emitted)
	}
}

func TestRunPipesAndPausedEOF(t *testing.T) {
	input, writer, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer input.Close()
	defer writer.Close()
	reader, output, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer reader.Close()
	defer output.Close()
	finished := make(chan error, 1)
	go func() { finished <- Run(input, output) }()
	type decoded struct {
		value map[string]json.RawMessage
		err   error
	}
	replies := make(chan decoded, 1)
	go func() {
		decoder := json.NewDecoder(reader)
		for {
			var value map[string]json.RawMessage
			err := decoder.Decode(&value)
			if err != nil {
				return
			}
			replies <- decoded{value, err}
		}
	}()
	next := func() map[string]json.RawMessage {
		t.Helper()
		select {
		case reply := <-replies:
			if reply.err != nil {
				t.Fatal(reply.err)
			}
			return reply.value
		case err := <-finished:
			t.Fatalf("monitor exited early: %v", err)
		case <-time.After(5 * time.Second):
			t.Fatal("monitor reply timed out")
		}
		return nil
	}
	first := next()
	var groups []Group
	if string(first["event"]) != `"sample"` || json.Unmarshal(first["apps"], &groups) != nil {
		t.Fatal(first)
	}
	for _, group := range groups {
		if group.CPU != -1 {
			t.Fatal("first sample CPU", group.CPU)
		}
	}
	// The invalid line acknowledges that the preceding pause was consumed.
	if _, err := writer.WriteString("{\"action\":\"pause\",\"paused\":true}\ninvalid\n"); err != nil {
		t.Fatal(err)
	}
	if reply := next(); string(reply["event"]) != `"action"` {
		t.Fatal(reply)
	}
	if err := writer.Close(); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-finished:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(time.Second):
		t.Fatal("paused EOF did not stop monitor")
	}
}

func writeFixture(t *testing.T, path, data string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(data), 0600); err != nil {
		t.Fatal(err)
	}
}

func writeProcess(t *testing.T, root string, pid int, start string, ticks int, uid int, exe string) {
	t.Helper()
	fields := strings.Fields("S 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0")
	fields[11], fields[19] = fmt.Sprint(ticks), start
	base := filepath.Join(root, fmt.Sprint(pid))
	writeFixture(t, filepath.Join(base, "stat"), fmt.Sprintf("%d (test-app) %s", pid, strings.Join(fields, " ")))
	writeFixture(t, filepath.Join(base, "status"), fmt.Sprintf("Name:\ttest-app\nUid:\t%d\t%d\t%d\t%d\n", uid, uid, uid, uid))
	writeFixture(t, filepath.Join(base, "statm"), "100 3 0 0 0 0 0\n")
	writeFixture(t, filepath.Join(base, "cmdline"), exe+"\x00arg\x00")
	link := filepath.Join(base, "exe")
	// Atomic rename replaces only this test-owned fixture link.
	if err := os.Symlink(exe, link+".new"); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(link+".new", link); err != nil {
		t.Fatal(err)
	}
}

func fixtureProc(t *testing.T) *procReader {
	t.Helper()
	return &procReader{root: t.TempDir(), boot: 1000, hz: 100, pageSize: 4096, catalog: newCatalog(nil), metadata: make(map[metadataKey]identity)}
}

func TestProcIdentityCacheAndCounters(t *testing.T) {
	p := fixtureProc(t)
	writeProcess(t, p.root, 123, "100", 150, 1000, "/bin/python3")
	rows := p.records()
	if len(rows) != 1 || rows[0].Started != 1001 || rows[0].Ticks != 1.5 || rows[0].Memory != 12288 || rows[0].Fallback != "/bin/python3:arg" {
		t.Fatalf("%+v", rows)
	}
	writeFixture(t, p.path(123, "cmdline"), "/bin/python3\x00changed\x00")
	if rows = p.records(); rows[0].Fallback != "/bin/python3:arg" {
		t.Fatal("identity was not cached")
	}
	writeProcess(t, p.root, 123, "100", 200, 1000, "/bin/node")
	if rows = p.records(); rows[0].Fallback != "/bin/node:arg" || len(p.metadata) != 1 {
		t.Fatal(rows, p.metadata)
	}
	writeProcess(t, p.root, 123, "200", 200, 1000, "/bin/node")
	if rows = p.records(); rows[0].Started != 1002 || len(p.metadata) != 1 {
		t.Fatal(rows, p.metadata)
	}
	writeFixture(t, p.path(123, "statm"), "100 0 0 0\n")
	if rows = p.records(); len(rows) != 0 || len(p.metadata) != 0 {
		t.Fatal(rows, p.metadata)
	}
}

func TestPIDFDIdentityAndCleanup(t *testing.T) {
	p := fixtureProc(t)
	writeProcess(t, p.root, 123, "100", 0, os.Getuid(), "/bin/test")
	closed, sent := 0, 0
	var received unix.Signal
	p.pidfds = pidfdOps{
		open: func(pid, flags int) (int, error) {
			if pid != 123 || flags != 0 {
				t.Fatal(pid, flags)
			}
			return 7, nil
		},
		close: func(fd int) error {
			if fd != 7 {
				t.Fatal(fd)
			}
			closed++
			return nil
		},
		send: func(fd int, sig unix.Signal, info *unix.Siginfo, flags int) error {
			if fd != 7 || info != nil || flags != 0 {
				t.Fatal(fd, info, flags)
			}
			sent++
			received = sig
			return nil
		},
	}
	if ok, err := p.signalMember(Member{123, 1000}, false); ok || err != nil {
		t.Fatal(ok, err)
	}
	if closed != 1 || sent != 0 {
		t.Fatal(closed, sent)
	}
	if ok, err := p.signalMember(Member{123, 1001}, false); !ok || err != nil || received != unix.SIGTERM {
		t.Fatal(ok, err, received)
	}
	if ok, err := p.signalMember(Member{123, 1001}, true); !ok || err != nil || received != unix.SIGKILL {
		t.Fatal(ok, err, received)
	}
	p.pidfds.send = func(int, unix.Signal, *unix.Siginfo, int) error { return unix.EPERM }
	if ok, err := p.signalMember(Member{123, 1001}, false); ok || !errors.Is(err, unix.EPERM) {
		t.Fatal(ok, err)
	}
	if closed != 4 {
		t.Fatal("descriptor leaked", closed)
	}
	writeProcess(t, p.root, 123, "100", 0, os.Getuid()+1, "/bin/test")
	if ok, err := p.signalMember(Member{123, 1001}, false); ok || err != nil {
		t.Fatal("other uid accepted", ok, err)
	}
	if closed != 5 {
		t.Fatal(closed)
	}
	p.pidfds.open = func(int, int) (int, error) { return -1, unix.ENOSYS }
	if ok, err := p.signalMember(Member{123, 1001}, false); ok || !errors.Is(err, unix.ENOSYS) || closed != 5 {
		t.Fatal("unsafe fallback", ok, err, closed)
	}
}

func TestCatalogMetadataAndPrecedence(t *testing.T) {
	root := t.TempDir()
	user, system := filepath.Join(root, "user"), filepath.Join(root, "system")
	editor := filepath.Join(root, "My Editor")
	writeFixture(t, editor, "#!/bin/sh\n")
	if err := os.Chmod(editor, 0700); err != nil {
		t.Fatal(err)
	}
	writeFixture(t, filepath.Join(user, "hidden.desktop"), "[Desktop Entry]\nType=Application\nHidden=true\nName=Hidden\nExec=/bin/true\n")
	writeFixture(t, filepath.Join(system, "hidden.desktop"), "[Desktop Entry]\nType=Application\nName=Hidden\nExec=/bin/true\n")
	writeFixture(t, filepath.Join(system, "browser.desktop"), "[Desktop Entry]\nType=Application\nName=Browser\nName[fr]=Navigateur\nIcon=browser\\sicon\nNoDisplay=true\nOnlyShowIn=Other;\nStartupWMClass=Web\nExec=env FOO=bar /opt/browser %U\n")
	writeFixture(t, filepath.Join(system, "nested", "editor.desktop"), "[Desktop Entry]\nType=Application\nName=Editor\nExec=\""+editor+"\" %F\n")
	writeFixture(t, filepath.Join(system, "missing.desktop"), "[Desktop Entry]\nType=Application\nName=Missing\nTryExec=/missing/qs-test-not-installed\nExec=/bin/true\n")
	c := loadCatalog([]string{user, system}, []string{"fr"})
	if len(c.ids) != 2 || c.ids["browser"].Name != "Navigateur" || c.ids["browser"].Icon != "browser icon" || c.ids["nested-editor"] == nil {
		t.Fatal(c.ids)
	}
	for _, candidate := range []string{"/opt/browser", "Web", "browser"} {
		if app := c.match(candidate, "", nil); app == nil || app.Key != "browser" {
			t.Fatal(candidate, app)
		}
	}
	if app := c.match(editor, "", nil); app == nil || app.Key != "nested-editor" {
		t.Fatal(app)
	}
	c = newCatalog([]catalogEntry{{App: App{Key: "librewolf", Name: "LibreWolf"}, Aliases: []string{"librewolf"}}, {App: App{Key: "flatpak-app", Name: "Flatpak app"}, Aliases: []string{"flatpak"}}})
	if app := c.match("/tmp/.mount_X/usr/bin/librewolf", "Web Content", nil); app == nil || app.Name != "LibreWolf" {
		t.Fatal(app)
	}
	if app := c.match("/usr/bin/flatpak", "flatpak", nil); app != nil {
		t.Fatal(app)
	}
}

func TestShellWordsAndLocales(t *testing.T) {
	for _, tc := range []struct {
		input string
		want  []string
		ok    bool
	}{
		{`env A=b "/opt/App Name" %U`, []string{"env", "A=b", "/opt/App Name", "%U"}, true},
		{`app 'two words' ""`, []string{"app", "two words", ""}, true},
		{`app "a\$b"`, []string{"app", `a\$b`}, true},
		{`app "unfinished`, nil, false},
	} {
		got, ok := shellWords(tc.input)
		if ok != tc.ok || !reflect.DeepEqual(got, tc.want) {
			t.Fatal(tc.input, got, ok)
		}
	}
	t.Setenv("LC_ALL", "fr_FR.UTF-8@euro")
	t.Setenv("LANGUAGE", "")
	if got := localeNames(); !reflect.DeepEqual(got, []string{"fr_FR@euro", "fr@euro", "fr_FR", "fr", "C"}) {
		t.Fatal(got)
	}
}

func TestProcStatParensAndFlatpak(t *testing.T) {
	p := fixtureProc(t)
	p.catalog = newCatalog([]catalogEntry{{App: App{Key: "org.test.App", Name: "Test App"}}})
	writeProcess(t, p.root, 123, "100", 0, 1000, "/bin/flatpak")
	data, err := os.ReadFile(p.path(123, "stat"))
	if err != nil {
		t.Fatal(err)
	}
	writeFixture(t, p.path(123, "stat"), strings.Replace(string(data), "(test-app)", "(test ) ( app)", 1))
	writeFixture(t, p.path(123, "root/.flatpak-info"), "[Application]\nname=org.test.App\n")
	rows := p.records()
	if len(rows) != 1 || rows[0].Name != "test ) ( app" || rows[0].App == nil || rows[0].App.Key != "org.test.App" {
		t.Fatal(rows)
	}
}

func TestProcessTitleCommandLines(t *testing.T) {
	p := fixtureProc(t)
	for _, input := range []string{"/bin/app arg\x00", "/bin/app arg", "/bin/app\x00arg\x00"} {
		writeFixture(t, p.path(123, "cmdline"), input)
		if got := p.argv(123); !reflect.DeepEqual(got, []string{"/bin/app", "arg"}) {
			t.Fatal(input, got)
		}
	}
}

func TestMetadataOnlyAndUnavailableDesktopExecutables(t *testing.T) {
	if entry, ok := parseDesktop("org.quickshell.desktop", "[Desktop Entry]\nType=Application\nName=Quickshell\nNoDisplay=true\nIcon=org.quickshell\n", nil); !ok || entry.App.Name != "Quickshell" {
		t.Fatal(entry, ok)
	}
	if entry, ok := parseDesktop("missing.desktop", "[Desktop Entry]\nType=Application\nName=Missing\nExec=/no/such/qs-test-executable\n", nil); ok {
		t.Fatal(entry)
	}
}
