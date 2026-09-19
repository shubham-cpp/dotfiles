// Package search implements the shell's search policies without Qt objects.
package search

import (
	"strings"
	"unicode/utf16"

	"golang.org/x/text/cases"
	"golang.org/x/text/language"
)

// Qt's JS engine uses full, non-contextual lowercasing: expand dotted I,
// but do not turn Greek sigma into final sigma. Actual Qt fixtures guard this.
func lower(s string) string {
	return cases.Lower(language.Und, cases.HandleFinalSigma(false)).String(s)
}
func units(s string) []uint16 { return utf16.Encode([]rune(s)) }
func jsSpace(r rune) bool {
	return r == 0xFEFF || r == 0xA0 || r == 0x1680 || r >= 0x2000 && r <= 0x200A || r == 0x2028 || r == 0x2029 || r == 0x202F || r == 0x205F || r == 0x3000 || r == 32 || r >= 9 && r <= 13
}
func trim(s string) string { return strings.TrimFunc(s, jsSpace) }

type prepared struct{ raw, folded []uint16 }

func prepare(s string) prepared { return prepared{units(s), units(lower(s))} }

type token struct {
	chars     []uint16
	sensitive bool
}

func tokenize(q string) []token {
	parts := strings.FieldsFunc(trim(q), jsSpace)
	out := make([]token, 0, len(parts))
	for _, p := range parts {
		folded := lower(p)
		sensitive := folded != p
		if !sensitive {
			p = folded
		}
		out = append(out, token{units(p), sensitive})
	}
	return out
}
func class(ch uint16) int {
	switch {
	case ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r':
		return 0
	case ch == '/' || ch == ',' || ch == ':' || ch == ';' || ch == '|':
		return 1
	case ch >= '0' && ch <= '9':
		return 2
	case ch >= 'a' && ch <= 'z':
		return 3
	case ch >= 'A' && ch <= 'Z':
		return 4
	default:
		return 5
	}
}
func bonus(prev, next int) int {
	switch {
	case prev == 0:
		return 10
	case prev == 1:
		return 9
	case prev == 5:
		return 8
	case prev == 3 && next == 4 || prev != 2 && next == 2:
		return 7
	case next == 5:
		return 8
	case next == 0:
		return 10
	default:
		return 0
	}
}
func matchToken(q token, text prepared, score bool) (int, bool) {
	if len(q.chars) == 0 {
		return 0, true
	}
	t := text.folded
	if q.sensitive {
		t = text.raw
	}
	qi, total, consec, first, prev := 0, 0, 0, 0, 0
	for i, ch := range t {
		cls := 0
		if i < len(text.raw) {
			cls = class(text.raw[i])
		}
		if ch == q.chars[qi] {
			if score {
				b := bonus(prev, cls)
				if i == 0 {
					b = 10
				}
				if qi == 0 {
					b *= 2
				}
				consec++
				if consec == 1 {
					first = b
				} else if b >= 8 && b > first {
					consec = 1
					first = b
				} else {
					b = max(b, 4, first)
				}
				total += 16 + b
			}
			qi++
			if qi == len(q.chars) {
				return max(0, total), true
			}
		} else if score && qi > 0 {
			if consec > 0 {
				total -= 3
			} else {
				total--
			}
			consec = 0
		}
		prev = cls
	}
	return 0, false
}
func match(tokens []token, t prepared, scored bool) (int, bool) {
	total := 0
	for _, q := range tokens {
		s, ok := matchToken(q, t, scored)
		if !ok {
			return 0, false
		}
		total += s
	}
	return total, true
}

// Score is the compatibility interface used by golden tests and emoji fallback.
func Score(query, text string) (int, bool) {
	tokens := tokenize(query)
	if len(tokens) <= 1 {
		folded := lower(query)
		tokens = []token{{chars: units(query), sensitive: folded != query}}
	}
	return match(tokens, prepare(text), true)
}
