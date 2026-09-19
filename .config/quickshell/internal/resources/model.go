// Package resources implements the popup-scoped application monitor.
package resources

import (
	"fmt"
	"strings"
)

type App struct{ Key, Name, Icon string }
type Member struct {
	PID     int     `json:"pid"`
	Started float64 `json:"started"`
}
type Group struct {
	Key     string   `json:"key"`
	Name    string   `json:"name"`
	Icon    string   `json:"icon"`
	Memory  uint64   `json:"memory"`
	CPU     float64  `json:"cpu"`
	Members []Member `json:"members"`
	CanEnd  bool     `json:"canEnd"`
	Count   int      `json:"count"`
	State   string   `json:"state"`
}
type record struct {
	Member
	PPID, UID                 int
	Name, Base, Fallback, Exe string
	App                       *App
	Memory                    uint64
	Ticks, CPU                float64
}

var boundaries = wordSet("systemd mango bash sh fish zsh dash kitty foot alacritty ghostty konsole rofi vicinae xdg-open")
var protectedNames = wordSet("systemd mango quickshell qs stasis dbus-broker dbus-broker-launch gnome-keyring-daemon ksecretd")
var wrappers = wordSet("env flatpak bwrap sh bash python python3 node electron gjs gjs-console")

func wordSet(words string) map[string]bool {
	set := make(map[string]bool)
	for _, word := range strings.Fields(words) {
		set[word] = true
	}
	return set
}

func groupProcesses(records []record, protected map[int]bool, uid int) []Group {
	byPID := make(map[int]*record, len(records))
	for i := range records {
		byPID[records[i].PID] = &records[i]
	}
	resolved := make(map[int]*App, len(records))
	var identity func(*record, map[int]bool) *App
	identity = func(r *record, seen map[int]bool) *App {
		if app := resolved[r.PID]; app != nil {
			return app
		}
		app := r.App
		p := byPID[r.PPID]
		if app == nil && !seen[r.PID] && p != nil && !seen[p.PID] && p.UID == r.UID && !boundaries[p.Base] && !boundaries[r.Base] && !protectedNames[r.Base] {
			seen[r.PID] = true
			app = identity(p, seen)
			delete(seen, r.PID)
		}
		if app == nil {
			app = &App{Key: "exe:" + r.Fallback, Name: r.Name}
		}
		resolved[r.PID] = app
		return app
	}
	groups := make([]Group, 0)
	indices := make(map[string]int)
	for i := range records {
		r := &records[i]
		app := identity(r, make(map[int]bool))
		key := fmt.Sprintf("%d:%s", r.UID, app.Key)
		index, exists := indices[key]
		if !exists {
			index = len(groups)
			indices[key] = index
			groups = append(groups, Group{Key: key, Name: app.Name, Icon: app.Icon, CanEnd: true, Members: make([]Member, 0)})
		}
		g := &groups[index]
		g.Memory += r.Memory
		g.CPU += r.CPU
		g.Members = append(g.Members, r.Member)
		if r.UID != uid || protectedNames[r.Base] || protected[r.PID] {
			g.CanEnd = false
		}
	}
	return groups
}
