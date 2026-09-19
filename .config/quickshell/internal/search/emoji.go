package search

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type Entry struct {
	ID       string   `json:"id"`
	Text     string   `json:"text"`
	Name     string   `json:"name"`
	Search   string   `json:"search"`
	Aliases  []string `json:"aliases"`
	Tones    []int    `json:"tones"`
	FamilyID string   `json:"familyId"`
	nameKey  string
	prepared prepared
}
type Family struct {
	ID       string   `json:"id"`
	Name     string   `json:"name"`
	Group    string   `json:"group"`
	Slots    int      `json:"slots"`
	Variants []string `json:"variants"`
	tuples   map[string]string
}
type Recent struct {
	ID string  `json:"id"`
	At float64 `json:"at"`
}
type Preferences struct {
	Schema    int               `json:"schema"`
	Tone      int               `json:"tone"`
	Overrides map[string]string `json:"overrides"`
	Recents   []Recent          `json:"recents"`
}
type EmojiData struct {
	Schema      int      `json:"schema"`
	Unicode     string   `json:"unicode"`
	Groups      []string `json:"groups"`
	Entries     []Entry  `json:"entries"`
	Families    []Family `json:"families"`
	DisplayOnly bool     `json:"displayOnly,omitempty"`
}
type Emoji struct {
	data     EmojiData
	entries  map[string]*Entry
	families map[string]*Family
	byText   map[string]string
}

var emojiSeparators = strings.NewReplacer("_", " ", ":", " ", ",", " ", "-", " ")

func normalize(s string) string {
	s = lower(s)
	s = emojiSeparators.Replace(s)
	return strings.Join(strings.FieldsFunc(s, jsSpace), " ")
}
func tuple(tones []int) string {
	out := make([]string, len(tones))
	for i, t := range tones {
		out[i] = strconv.Itoa(t)
	}
	return strings.Join(out, ",")
}
func LoadEmoji(path string) (*Emoji, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var data EmojiData
	dec := json.NewDecoder(io.LimitReader(f, 8*1024*1024))
	if err = dec.Decode(&data); err != nil {
		return nil, err
	}
	if data.Schema != 1 || len(data.Entries) == 0 || len(data.Entries) > 20000 {
		return nil, errors.New("unsupported emoji catalog")
	}
	e := &Emoji{data: data, entries: map[string]*Entry{}, families: map[string]*Family{}, byText: map[string]string{}}
	for i := range e.data.Entries {
		row := &e.data.Entries[i]
		var codes []string
		for _, r := range row.Text {
			codes = append(codes, fmt.Sprintf("%x", r))
		}
		if row.ID == "" || e.entries[row.ID] != nil || strings.Join(codes, "-") != row.ID || len(row.Tones) > 2 {
			return nil, errors.New("invalid emoji entry")
		}
		for _, t := range row.Tones {
			if t < 1 || t > 5 {
				return nil, errors.New("invalid emoji tone")
			}
		}
		e.entries[row.ID] = row
		e.byText[row.Text] = row.ID
	}
	seen := map[string]bool{}
	for i := range e.data.Families {
		f := &e.data.Families[i]
		if e.entries[f.ID] == nil || e.families[f.ID] != nil || f.Slots < 0 || f.Slots > 2 {
			return nil, errors.New("invalid emoji family")
		}
		f.tuples = map[string]string{}
		validGroup := false
		for _, g := range data.Groups {
			if f.Group == g {
				validGroup = true
			}
		}
		if !validGroup {
			return nil, errors.New("invalid emoji group")
		}
		for _, id := range f.Variants {
			entry := e.entries[id]
			if entry == nil || entry.FamilyID != f.ID || seen[id] {
				return nil, errors.New("invalid emoji variant")
			}
			seen[id] = true
			tones := entry.Tones
			if len(tones) == 1 && f.Slots == 2 {
				tones = []int{tones[0], tones[0]}
			}
			key := tuple(tones)
			if f.tuples[key] != "" {
				return nil, errors.New("ambiguous emoji tones")
			}
			f.tuples[key] = id
		}
		if f.tuples[""] != f.ID {
			return nil, errors.New("missing default emoji")
		}
		// Search ranks the family default, then resolves the displayed variant.
		// Variant names and sequences remain available without duplicate indexes.
		entry := e.entries[f.ID]
		entry.nameKey = normalize(entry.Name)
		entry.prepared = prepare(entry.Search)
		e.families[f.ID] = f
	}
	if len(seen) != len(e.entries) {
		return nil, errors.New("unreachable emoji")
	}
	return e, nil
}
func (e *Emoji) Display(w io.Writer) error {
	data := e.data
	data.Entries = append([]Entry(nil), data.Entries...)
	data.DisplayOnly = true
	for i := range data.Entries {
		data.Entries[i].Search = ""
		data.Entries[i].Aliases = []string{}
	}
	return json.NewEncoder(w).Encode(data)
}
func (e *Emoji) resolve(f *Family, p Preferences) string {
	if id := p.Overrides[f.ID]; id != "" {
		if entry := e.entries[id]; entry != nil && entry.FamilyID == f.ID {
			return id
		}
	}
	key := ""
	if p.Tone > 0 && p.Tone <= 5 {
		tones := []int{p.Tone}
		if f.Slots == 2 {
			tones = append(tones, p.Tone)
		}
		key = tuple(tones)
	}
	if id := f.tuples[key]; id != "" {
		return id
	}
	return f.ID
}
func allContains(s string, tokens []string) bool {
	for _, t := range tokens {
		if !strings.Contains(s, t) {
			return false
		}
	}
	return true
}
func rankEmoji(q string, tokens []string, fuzzy []token, entry *Entry, allow bool) int {
	if entry.nameKey == q {
		return 10000
	}
	for _, alias := range entry.Aliases {
		if alias == q {
			return 10000
		}
	}
	if allContains(entry.nameKey, tokens) {
		return 8000 - len(units(entry.nameKey))
	}
	if allContains(entry.Search, tokens) {
		return 6000 - len(units(entry.nameKey))
	}
	if allow {
		if score, ok := match(fuzzy, entry.prepared, true); ok {
			return min(4000, max(1, score))
		}
	}
	return -1
}

var tonePattern = regexp.MustCompile(`\b(medium light|medium dark|light|medium|dark)(?: skin tones?)?\b`)

func (e *Emoji) Search(q Query) []string {
	raw := trim(q.Query)
	if id := e.byText[raw]; id != "" {
		return []string{id}
	}
	query := normalize(raw)
	p := q.Preferences
	out := []string{}
	if query == "" {
		if q.Category == "recent" {
			for _, r := range p.Recents {
				if e.entries[r.ID] != nil {
					out = append(out, r.ID)
				}
			}
			return out
		}
		for i := range e.data.Families {
			f := &e.data.Families[i]
			if q.Category == "all" || f.Group == q.Category {
				out = append(out, e.resolve(f, p))
			}
		}
		return out
	}
	tones := []int{}
	names := map[string]int{"light": 1, "medium light": 2, "medium": 3, "medium dark": 4, "dark": 5}
	base := normalize(tonePattern.ReplaceAllStringFunc(query, func(s string) string {
		m := tonePattern.FindStringSubmatch(s)
		tones = append(tones, names[m[1]])
		return " "
	}))
	type hit struct {
		id            string
		score, recent int
	}
	rows := []hit{}
	recents := map[string]int{}
	for i, r := range p.Recents {
		recents[r.ID] = 48 - i
	}
	tokens, baseTokens := strings.Split(query, " "), strings.Split(base, " ")
	fuzzy, baseFuzzy := tokenize(query), tokenize(base)
	for pass := 0; pass < 2 && len(rows) == 0; pass++ {
		for i := range e.data.Families {
			f := &e.data.Families[i]
			id := e.resolve(f, p)
			score := 0
			if len(tones) > 0 && f.Slots > 0 {
				t := tones
				if len(t) == 1 && f.Slots == 2 {
					t = []int{t[0], t[0]}
				}
				id = f.tuples[tuple(t)]
				if id == "" {
					continue
				}
				score = 6000
				if base != "" {
					score = rankEmoji(base, baseTokens, baseFuzzy, e.entries[f.ID], pass == 1)
				}
			} else {
				score = rankEmoji(query, tokens, fuzzy, e.entries[f.ID], pass == 1)
			}
			if score >= 0 {
				rows = append(rows, hit{id, score, recents[id]})
			}
		}
	}
	sort.SliceStable(rows, func(i, j int) bool {
		if rows[i].score != rows[j].score {
			return rows[i].score > rows[j].score
		}
		return rows[i].recent > rows[j].recent
	})
	for _, r := range rows {
		out = append(out, r.id)
	}
	return out
}
