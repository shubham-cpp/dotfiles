package search

import (
	"slices"
	"strings"
	"testing"
)

func TestLauncherContiguousNameBeforeScatteredMatch(t *testing.T) {
	chunk := operation("chunk", 1)
	chunk.Rows = []Row{
		{Key: "heroic", Name: "Heroic Games Launcher"},
		{Key: "hardware", Name: "Hardware Locality lstopo"},
		{Key: "shelly", Name: "Shelly"},
		{Key: "hello", Name: "CachyOS Hello"},
	}
	query := operation("search", 1)
	query.Query.Query = "hel"
	got, err := runProtocol(t, []Request{operation("begin", 1), chunk, operation("commit", 1), query})
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"hello", "shelly", "heroic", "hardware"}
	if keys := got[len(got)-1].Keys; !slices.Equal(keys, want) {
		t.Fatalf("hel ranking = %v, want %v", keys, want)
	}
}

func TestLauncherNameRanking(t *testing.T) {
	for _, tc := range []struct {
		name, query string
		rows        []Row
		want        []string
	}{
		{
			name: "exact then prefix then word then substring then fuzzy then metadata", query: "hel",
			rows: []Row{
				{Key: "metadata", Name: "Assistant", Comment: "Help browser"},
				{Key: "fuzzy", Name: "Heroic Games Launcher", Count: 100000, Last: 1800000000},
				{Key: "substring", Name: "Shelly"},
				{Key: "word", Name: "CachyOS Hello"},
				{Key: "prefix", Name: "Hello"},
				{Key: "exact", Name: "Hel"},
			},
			want: []string{"exact", "prefix", "word", "substring", "fuzzy", "metadata"},
		},
		{
			name: "later word prefix and long names", query: "hel",
			rows: []Row{
				{Key: "substring", Name: "Shelly"},
				{Key: "later", Name: "Shelly " + strings.Repeat("x", 100) + " Hello"},
			},
			want: []string{"later", "substring"},
		},
		{
			name: "reordered tokens and repeated whitespace", query: " hello\t cachy ",
			rows: []Row{
				{Key: "partial", Name: "Hello"},
				{Key: "both", Name: "CachyOS Hello"},
			},
			want: []string{"both"},
		},
		{
			name: "case sensitive substring", query: "Hel",
			rows: []Row{{Key: "lower", Name: "CachyOS hello"}, {Key: "upper", Name: "CachyOS Hello"}},
			want: []string{"upper"},
		},
		{
			name: "unicode letters and separators", query: "hel",
			rows: []Row{
				{Key: "inside", Name: "éhel"},
				{Key: "non-bmp-letter", Name: "𐐀hel"},
				{Key: "separator", Name: "日本語・Hello"},
				{Key: "space", Name: "日本語\u3000Hello"},
			},
			want: []string{"separator", "space", "inside", "non-bmp-letter"},
		},
		{
			name: "fuzzy abbreviations remain available", query: "hgl",
			rows: []Row{{Key: "heroic", Name: "Heroic Games Launcher"}, {Key: "hello", Name: "CachyOS Hello"}},
			want: []string{"heroic"},
		},
		{
			name: "pin preference and eligibility", query: "hel",
			rows: []Row{
				{Key: "hello", Name: "Hello"},
				{Key: "pin", Name: "Shelly", Pinned: true},
				{Key: "unmatched-pin", Name: "Calculator", Pinned: true},
			},
			want: []string{"pin", "hello"},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			catalog, err := NewCatalog(tc.rows)
			if err != nil {
				t.Fatal(err)
			}
			if got := catalog.Launcher(Query{Query: tc.query, Now: 1800000000}); !slices.Equal(got, tc.want) {
				t.Fatalf("%q ranking = %v, want %v", tc.query, got, tc.want)
			}
		})
	}
}
