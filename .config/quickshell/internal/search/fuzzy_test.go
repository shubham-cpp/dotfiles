package search

import (
	"encoding/json"
	"os"
	"testing"
)

// Golden outcomes are checked against the actual Qt JS engine by
// tests/run-search-compat-tests.py, including Qt's non-contextual sigma mapping.
func TestFuzzyJavaScriptGolden(t *testing.T) {
	raw, err := os.ReadFile("../../tests/fixtures/search/fuzzy.json")
	if err != nil {
		t.Fatal(err)
	}
	var cases []struct {
		Query, Text string
		Matched     bool
		Score       int
	}
	if err := json.Unmarshal(raw, &cases); err != nil {
		t.Fatal(err)
	}
	for _, c := range cases {
		score, matched := Score(c.Query, c.Text)
		if matched != c.Matched || matched && score != c.Score {
			t.Errorf("query=%q text=%q: got (%d,%t), JS reference (%d,%t)", c.Query, c.Text, score, matched, c.Score, c.Matched)
		}
	}
}
