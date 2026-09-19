package search

import (
	"fmt"
	"testing"
)

func BenchmarkLauncherPrepared(b *testing.B) {
	for _, count := range []int{100, 500, 2000} {
		b.Run(fmt.Sprint(count), func(b *testing.B) {
			rows := make([]Row, count)
			for i := range rows {
				rows[i] = Row{Key: fmt.Sprintf("app-%d", i), ID: fmt.Sprintf("application-%d.desktop", i), Name: fmt.Sprintf("Application %d", i), GenericName: "Synthetic editor", Keywords: []string{"graphics", "development"}, Count: float64(i % 4), Last: 1800000000 - float64(i*10000), Tie: i, Pinned: i%71 == 0}
			}
			catalog, err := NewCatalog(rows)
			if err != nil {
				b.Fatal(err)
			}
			queries := []string{"", "app", "application 2", "editor", "grp", "missing", "app 9"}
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				catalog.Launcher(Query{Query: queries[i%len(queries)], Now: 1800000000})
			}
		})
	}
}

func BenchmarkEmojiPrepared(b *testing.B) {
	catalog, err := LoadEmoji("../../data/emoji.json")
	if err != nil {
		b.Fatal(err)
	}
	queries := []string{"", "thumbs up", "thumbs up dark", "tmbsup", "grnng", "missingzzzz", "woman technologist", "flag india"}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		catalog.Search(Query{Query: queries[i%len(queries)], Category: "all", Preferences: Preferences{Schema: 1}})
	}
}
