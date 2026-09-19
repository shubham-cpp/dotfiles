package search

import "testing"

var loadedEmoji *Emoji

// Each iteration opens, decodes, validates and indexes the full catalog. The
// filesystem page cache may be warm; this is not a cold desktop-open benchmark.
func BenchmarkLoadEmoji(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		catalog, err := LoadEmoji("../../data/emoji.json")
		if err != nil {
			b.Fatal(err)
		}
		loadedEmoji = catalog
	}
}
