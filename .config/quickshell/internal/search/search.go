package search

import (
	"cmp"
	"errors"
	"math"
	"slices"
	"strings"
	"unicode"
	"unicode/utf16"
)

type Row struct {
	Key                string   `json:"key"`
	ID                 string   `json:"id"`
	Name               string   `json:"name"`
	GenericName        string   `json:"genericName"`
	Comment            string   `json:"comment"`
	Keywords           []string `json:"keywords"`
	Text               string   `json:"text"`
	Kind               string   `json:"kind"`
	Pinned             bool     `json:"pinned"`
	Count              float64  `json:"count"`
	Last               float64  `json:"last"`
	Tie                int      `json:"tie"`
	name               prepared
	extra              []prepared
	nameLower, idLower string
}
type Catalog struct{ rows []Row }

func NewCatalog(rows []Row) (*Catalog, error) {
	if len(rows) > 10000 {
		return nil, errors.New("too many search records")
	}
	seen := make(map[string]bool, len(rows))
	for i := range rows {
		r := &rows[i]
		if r.Key == "" || seen[r.Key] {
			return nil, errors.New("invalid search key")
		}
		seen[r.Key] = true
		r.name = prepare(r.Name)
		r.nameLower = lower(r.Name)
		r.idLower = lower(r.ID)
		if r.Text != "" {
			r.extra = append(r.extra, prepare(r.Text))
		} else {
			for _, s := range append([]string{r.GenericName, r.Comment}, r.Keywords...) {
				r.extra = append(r.extra, prepare(s))
			}
		}
	}
	return &Catalog{rows}, nil
}

type Query struct {
	Query       string      `json:"query"`
	Filter      string      `json:"filter"`
	Now         float64     `json:"now"`
	Category    string      `json:"category"`
	Preferences Preferences `json:"preferences"`
}
type ranked struct {
	key              string
	tier, score, tie int
	frec             float64
}

// Every query word must occur contiguously. Checking all occurrences prevents
// an earlier mid-word hit from hiding a later word prefix (CachyOS Hello).
func matchNameSubstrings(tokens []token, name prepared) (int, bool) {
	score := max(0, 80-len(name.raw))
	for _, q := range tokens {
		boundary, found := substringBoundary(q, name)
		if !found {
			return 0, false
		}
		score += boundary
	}
	return score, true
}

func substringBoundary(q token, name prepared) (int, bool) {
	text := name.folded
	if q.sensitive {
		text = name.raw
	}
	found := false
	for i := 0; i+len(q.chars) <= len(text); i++ {
		if !slices.Equal(text[i:i+len(q.chars)], q.chars) {
			continue
		}
		found = true
		if wordStart(text, i) {
			return 100, true // Word starts dominate the bounded length tie breaker.
		}
	}
	return 0, found
}

func wordStart(text []uint16, i int) bool {
	if i == 0 {
		return true
	}
	prev := rune(text[i-1])
	if i > 1 && utf16.IsSurrogate(prev) {
		prev = utf16.DecodeRune(rune(text[i-2]), prev)
	}
	return !unicode.IsLetter(prev) && !unicode.IsNumber(prev)
}

func desktopMatch(r Row, query string, tokens []token) (tier, score int, ok bool) {
	switch {
	case query == "":
		return 6, 0, true
	case r.nameLower == query || r.idLower == query:
		return 1, 10000, true
	case strings.HasPrefix(r.nameLower, query) || strings.HasPrefix(r.idLower, query):
		return 2, 8000 + max(0, 80-len(r.name.folded)), true
	}
	if score, ok := matchNameSubstrings(tokens, r.name); ok {
		return 3, score, true
	}
	if score, ok := match(tokens, r.name, true); ok {
		return 4, score, true
	}
	score, ok = matchMetadata(tokens, r.extra)
	return 5, score, ok
}

func matchMetadata(tokens []token, fields []prepared) (int, bool) {
	score, found := 0, false
	for _, field := range fields {
		value, hit := match(tokens, field, true)
		if hit {
			score = max(score, value)
			found = true
		}
	}
	return score, found
}

func (r Row) frecency(now float64) float64 {
	if r.Last <= 0 {
		return 0
	}
	return math.Log(1+max(0, r.Count)) + 4*math.Exp(-max(0, now-r.Last)/604800)
}

func (c *Catalog) Launcher(q Query) []string {
	query := trim(q.Query)
	folded := lower(query)
	tokens := tokenize(query)
	out := make([]ranked, 0, len(c.rows))
	for _, r := range c.rows {
		tier, score, ok := desktopMatch(r, folded, tokens)
		if !ok {
			continue
		}
		if r.Pinned {
			tier = 0
		}
		out = append(out, ranked{r.Key, tier, score, r.Tie, r.frecency(q.Now)})
	}
	slices.SortStableFunc(out, func(a, b ranked) int {
		return cmp.Or(cmp.Compare(a.tier, b.tier), cmp.Compare(b.score, a.score),
			cmp.Compare(b.frec, a.frec), cmp.Compare(a.tie, b.tie))
	})
	keys := make([]string, 0, min(50, len(out)))
	for _, r := range out[:min(50, len(out))] {
		keys = append(keys, r.key)
	}
	return keys
}

func (r Row) matchesClipboardFilter(filter string) bool {
	switch filter {
	case "pinned":
		return r.Pinned
	case "text":
		return r.Kind != "image" && r.Kind != "link"
	case "image", "link":
		return r.Kind == filter
	default:
		return true
	}
}

func (c *Catalog) Clipboard(q Query) []string {
	keys := make([]string, 0, 40)
	tokens := tokenize(q.Query)
	for _, r := range c.rows {
		if !r.matchesClipboardFilter(q.Filter) || len(r.extra) == 0 {
			continue
		}
		if _, ok := match(tokens, r.extra[0], false); ok {
			keys = append(keys, r.Key)
			if len(keys) == 40 {
				break
			}
		}
	}
	return keys
}
