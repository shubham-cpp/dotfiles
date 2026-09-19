package resources

import (
	"encoding/json"
	"errors"
	"os"
	"strconv"
	"time"

	"golang.org/x/sys/unix"
)

type Sample struct {
	Event string  `json:"event"`
	Apps  []Group `json:"apps"`
}
type Action struct {
	Event   string `json:"event"`
	OK      bool   `json:"ok"`
	Message string `json:"message"`
}
type Monitor struct {
	readRecords  func() []record
	signalMember func(Member, bool) (bool, error)
	now          func() time.Time
	cpus, uid    int
	protected    map[int]bool
	previous     map[Member]float64
	sampleTime   time.Time
	groups       []Group
	allowed      map[string]map[Member]bool
	ending       map[string]time.Time
}

func newMonitor(p *procReader) *Monitor {
	return &Monitor{readRecords: p.records, signalMember: p.signalMember, now: time.Now, cpus: cpuCount(), uid: os.Getuid(), protected: map[int]bool{os.Getpid(): true, os.Getppid(): true}, previous: make(map[Member]float64), groups: make([]Group, 0), allowed: make(map[string]map[Member]bool), ending: make(map[string]time.Time)}
}

func (m *Monitor) updateAllowed(groups []Group) {
	m.allowed = make(map[string]map[Member]bool)
	for _, group := range groups {
		if !group.CanEnd {
			continue
		}
		members := make(map[Member]bool, len(group.Members))
		for _, member := range group.Members {
			members[member] = true
		}
		m.allowed[group.Key] = members
	}
}

func (m *Monitor) resetCPU() { m.previous = make(map[Member]float64); m.sampleTime = time.Time{} }

func (m *Monitor) sample() Sample {
	now := m.now()
	elapsed := 0.0
	if !m.sampleTime.IsZero() {
		elapsed = now.Sub(m.sampleTime).Seconds()
	}
	capacity := elapsed * float64(max(1, m.cpus))
	intervalStart := float64(now.UnixNano())/1e9 - elapsed
	records := m.readRecords()
	counters := make(map[Member]float64, len(records))
	for i := range records {
		r := &records[i]
		previous, exists := m.previous[r.Member]
		if !exists && elapsed > 0 && r.Started >= intervalStart {
			previous, exists = 0, true
		}
		if capacity > 0 && exists {
			r.CPU = max(0, min(100, 100*(r.Ticks-previous)/capacity))
		}
		counters[r.Member] = r.Ticks
	}
	m.previous, m.sampleTime = counters, now
	m.groups = groupProcesses(records, m.protected, m.uid)
	m.updateAllowed(m.groups)
	live := make(map[string]bool, len(m.groups))
	for i := range m.groups {
		g := &m.groups[i]
		live[g.Key] = true
		// Decimal rounding must use the original float, as Python round(x, 1)
		// does; multiplying by ten first changes values such as 2.55.
		g.CPU, _ = strconv.ParseFloat(strconv.FormatFloat(min(100, g.CPU), 'f', 1, 64), 64)
		if elapsed == 0 {
			g.CPU = -1
		}
		g.Count = len(g.Members)
	}
	for key := range m.ending {
		if !live[key] {
			delete(m.ending, key)
		}
	}
	return m.message(now)
}

func (m *Monitor) message(now time.Time) Sample {
	for i := range m.groups {
		g := &m.groups[i]
		g.State = ""
		if started, exists := m.ending[g.Key]; exists {
			g.State = "ending"
			if now.Sub(started) >= 4*time.Second {
				g.State = "force"
			}
		}
	}
	return Sample{Event: "sample", Apps: m.groups}
}

func invalidSelection() Action {
	return Action{Event: "action", Message: "Invalid application selection"}
}

func (m *Monitor) end(request map[string]json.RawMessage) Action {
	var key string
	var members []json.RawMessage
	if json.Unmarshal(request["key"], &key) != nil || string(request["key"]) == "null" || json.Unmarshal(request["members"], &members) != nil || members == nil || len(members) > 10000 {
		return invalidSelection()
	}
	force := string(request["force"]) == "true"
	// Revalidate current grouping and identity without advancing CPU counters.
	m.updateAllowed(groupProcesses(m.readRecords(), m.protected, m.uid))
	allowed := m.allowed[key]
	if started, exists := m.ending[key]; force && (!exists || m.now().Sub(started) < 4*time.Second) {
		return Action{Event: "action", Message: "Try ending the application first"}
	}
	sent, denied := 0, false
	for _, raw := range members {
		var member Member
		var fields map[string]json.RawMessage
		if json.Unmarshal(raw, &fields) != nil || fields == nil || fields["pid"] == nil || fields["started"] == nil || string(fields["started"]) == "null" || json.Unmarshal(raw, &member) != nil || !allowed[member] {
			continue
		}
		ok, err := m.signalMember(member, force)
		if err != nil {
			if !errors.Is(err, os.ErrNotExist) && !errors.Is(err, unix.ESRCH) {
				denied = true
			}
		} else if ok {
			sent++
		}
	}
	if sent > 0 {
		m.ending[key] = m.now()
	}
	message := ""
	if denied {
		message = "Some processes could not be ended"
	} else if sent == 0 {
		message = "Application exited or is no longer available to end"
	}
	return Action{Event: "action", OK: sent > 0 && !denied, Message: message}
}
