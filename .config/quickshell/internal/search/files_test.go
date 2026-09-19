package search

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestFileRanking(t *testing.T) {
	idx, err := NewFileIndex([]string{
		"/home/u/notes.md",
		"/home/u/src/hello.go",
		"/home/u/docs/hello.txt",
		"/home/u/docs/ahello.md",
		"/home/u/Heroic Games Launcher.txt",
		"/home/u/CachyOS Hello.md",
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name, query string
		want        []string
	}{
		{name: "empty query is recents in QML", query: "", want: nil},
		{name: "basename exact before prefix", query: "hello.txt", want: []string{"/home/u/docs/hello.txt"}},
		{
			name:  "basename prefix then word then substring",
			query: "hello",
			want: []string{
				"/home/u/src/hello.go",
				"/home/u/docs/hello.txt",
				"/home/u/CachyOS Hello.md",
				"/home/u/docs/ahello.md",
			},
		},
		{name: "path tokens when basename misses", query: "src hello", want: []string{"/home/u/src/hello.go"}},
		{name: "fuzzy basename abbreviation", query: "hgl", want: []string{"/home/u/Heroic Games Launcher.txt"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := idx.Search(Query{Query: tc.query}); !slices.Equal(got, tc.want) {
				t.Fatalf("%q ranking = %v, want %v", tc.query, got, tc.want)
			}
		})
	}
}

func TestFileResultCap(t *testing.T) {
	paths := make([]string, 80)
	for i := range paths {
		paths[i] = "/home/u/file-" + strings.Repeat("a", i+1) + ".txt"
	}
	idx, err := NewFileIndex(paths)
	if err != nil {
		t.Fatal(err)
	}
	got := idx.Search(Query{Query: "file"})
	if len(got) != fileResultCap {
		t.Fatalf("result cap = %d, want %d", len(got), fileResultCap)
	}
}

func TestFileIndexRejectsOverCap(t *testing.T) {
	if _, err := NewFileIndex(make([]string, maxFileRecords+1)); err == nil {
		t.Fatal("accepted an over-cap path list")
	}
}

func TestFilesProtocolSearchAndEmptyCatalog(t *testing.T) {
	orig := readFiles
	readFiles = func() (*FileIndex, error) {
		return NewFileIndex([]string{"/home/u/notes.md", "/home/u/src/hello.go", "/home/u/docs/hello.txt"})
	}
	t.Cleanup(func() { readFiles = orig })

	req := func(kind string) Request {
		return Request{V: 1, Type: kind, Profile: "files", Epoch: 1, Revision: 1}
	}
	query := req("search")
	query.Query.Query = "hello"
	got, err := runProtocol(t, []Request{req("begin"), req("commit"), query})
	if err != nil {
		t.Fatal(err)
	}
	last := got[len(got)-1]
	want := []string{"/home/u/src/hello.go", "/home/u/docs/hello.txt"}
	if last.Type != "results" || !slices.Equal(last.Keys, want) {
		t.Fatalf("files search = %+v, want %v", last, want)
	}
}

func TestFilesProtocolRejectsUploadedRows(t *testing.T) {
	chunk := Request{V: 1, Type: "chunk", Profile: "files", Epoch: 1, Revision: 1,
		Rows: []Row{{Key: "/home/u/notes.md", Name: "notes.md"}}}
	begin := Request{V: 1, Type: "begin", Profile: "files", Epoch: 1, Revision: 1}
	commit := Request{V: 1, Type: "commit", Profile: "files", Epoch: 1, Revision: 1}
	got, err := runProtocol(t, []Request{begin, chunk, commit})
	if err != nil {
		t.Fatal(err)
	}
	if got[len(got)-1].Type != "error" || got[len(got)-1].Error != "files catalog is walked, not uploaded" {
		t.Fatalf("uploaded files catalog: %+v", got[len(got)-1])
	}
}

func TestFilesProtocolReleaseDropsIndex(t *testing.T) {
	orig := readFiles
	readFiles = func() (*FileIndex, error) {
		return NewFileIndex([]string{"/home/u/hello.go"})
	}
	t.Cleanup(func() { readFiles = orig })
	req := func(kind string, revision int) Request {
		return Request{V: 1, Type: kind, Profile: "files", Epoch: 1, Revision: revision}
	}
	query := req("search", 1)
	query.Query.Query = "hello"
	got, err := runProtocol(t, []Request{req("begin", 1), req("commit", 1), req("release", 1), query})
	if err != nil {
		t.Fatal(err)
	}
	if got[len(got)-1].Type != "error" {
		t.Fatalf("released files index remained searchable: %+v", got[len(got)-1])
	}
}

func TestWalkFilesHonorsExcludes(t *testing.T) {
	if _, err := exec.LookPath("fd"); err != nil {
		t.Skip("fd not installed")
	}
	root := t.TempDir()
	if err := os.WriteFile(filepath.Join(root, "keep.md"), []byte("ok"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.Mkdir(filepath.Join(root, "lib"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(root, "lib", "skip.txt"), []byte("no"), 0o644); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	idx, err := walkFiles(ctx, "fd", root)
	if err != nil {
		t.Fatal(err)
	}
	got := make([]string, len(idx.rows))
	for i, row := range idx.rows {
		got[i] = row.path
	}
	keep := filepath.Join(root, "keep.md")
	skip := filepath.Join(root, "lib", "skip.txt")
	if !slices.Contains(got, keep) {
		t.Fatalf("missing kept file: %v", got)
	}
	if slices.Contains(got, skip) {
		t.Fatalf("excluded path leaked: %v", got)
	}
}
