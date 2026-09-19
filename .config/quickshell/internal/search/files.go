package search

import (
	"bufio"
	"cmp"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"
)

const (
	maxFileRecords = 200000
	maxFileBytes   = 32 * 1024 * 1024
	fileResultCap  = 50
	fileWalkTime   = 2 * time.Second
	maxFilePath    = 4096
)

var fileExcludes = []string{"game", "lib", "renpy", "Games*", "Backups", "GitClones*"}

// readFiles is replaced in protocol tests so ranking does not walk $HOME.
var readFiles = defaultReadFiles

type FileRow struct {
	path, name, nameLower      string
	namePrepared, pathPrepared prepared
	tie                        int
}

type FileIndex struct{ rows []FileRow }

func NewFileIndex(paths []string) (*FileIndex, error) {
	if len(paths) > maxFileRecords {
		return nil, errors.New("too many files")
	}
	seen := make(map[string]bool, len(paths))
	rows := make([]FileRow, 0, len(paths))
	for _, path := range paths {
		if path == "" || strings.ContainsRune(path, 0) || len(path) > maxFilePath || seen[path] {
			continue
		}
		seen[path] = true
		name := filepath.Base(path)
		rows = append(rows, FileRow{
			path: path, name: name, nameLower: lower(name),
			namePrepared: prepare(name), pathPrepared: prepare(path), tie: len(rows),
		})
	}
	return &FileIndex{rows: rows}, nil
}

func defaultReadFiles() (*FileIndex, error) {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return nil, errors.New("home directory unavailable")
	}
	fd, err := exec.LookPath("fd")
	if err != nil {
		return nil, errors.New("fd is not installed")
	}
	ctx, cancel := context.WithTimeout(context.Background(), fileWalkTime)
	defer cancel()
	return walkFiles(ctx, fd, home)
}

func fileFdArgs(home string) []string {
	args := []string{"-t", "f"}
	for _, pattern := range fileExcludes {
		args = append(args, "-E", pattern)
	}
	return append(args, ".", home)
}

func walkFiles(ctx context.Context, fd, home string) (*FileIndex, error) {
	cmd := exec.CommandContext(ctx, fd, fileFdArgs(home)...)
	cmd.Stderr = io.Discard
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	paths, err := readFdPaths(stdout)
	waitErr := cmd.Wait()
	if err != nil {
		return nil, err
	}
	if waitErr != nil {
		if ctx.Err() != nil {
			return nil, errors.New("file listing timed out")
		}
		return nil, fmt.Errorf("fd: %w", waitErr)
	}
	return NewFileIndex(paths)
}

func readFdPaths(r io.Reader) ([]string, error) {
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 64*1024), maxFilePath+1)
	paths := make([]string, 0, 4096)
	total := 0
	for scanner.Scan() {
		path := scanner.Text()
		total += len(path) + 1
		if len(paths)+1 > maxFileRecords || total > maxFileBytes {
			return nil, errors.New("too many files")
		}
		paths = append(paths, path)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return paths, nil
}

func fileMatch(r FileRow, query string, tokens []token) (tier, score int, ok bool) {
	switch {
	case query == "":
		return 6, 0, true
	case r.nameLower == query:
		return 1, 10000, true
	case strings.HasPrefix(r.nameLower, query):
		return 2, 8000 + max(0, 80-len(r.namePrepared.folded)), true
	}
	if score, ok := matchNameSubstrings(tokens, r.namePrepared); ok {
		return 3, score, true
	}
	if score, ok := match(tokens, r.namePrepared, true); ok {
		return 4, score, true
	}
	score, ok = match(tokens, r.pathPrepared, true)
	return 5, score, ok
}

func (idx *FileIndex) Search(q Query) []string {
	query := trim(q.Query)
	if query == "" {
		return nil
	}
	folded := lower(query)
	tokens := tokenize(query)
	out := make([]ranked, 0, 64)
	for _, r := range idx.rows {
		tier, score, ok := fileMatch(r, folded, tokens)
		if !ok {
			continue
		}
		out = append(out, ranked{r.path, tier, score, r.tie, 0})
	}
	slices.SortStableFunc(out, func(a, b ranked) int {
		return cmp.Or(cmp.Compare(a.tier, b.tier), cmp.Compare(b.score, a.score), cmp.Compare(a.tie, b.tie))
	})
	keys := make([]string, 0, min(fileResultCap, len(out)))
	for _, r := range out[:min(fileResultCap, len(out))] {
		keys = append(keys, r.key)
	}
	return keys
}
