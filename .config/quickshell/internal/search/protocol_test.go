package search

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"testing"
)

func runProtocol(t *testing.T, requests []Request) ([]Response, error) {
	t.Helper()
	var input, output bytes.Buffer
	for _, r := range requests {
		if err := json.NewEncoder(&input).Encode(r); err != nil {
			t.Fatal(err)
		}
	}
	err := Serve(&input, &output, "../../data/emoji.json")
	var results []Response
	decoder := json.NewDecoder(&output)
	for {
		var r Response
		e := decoder.Decode(&r)
		if e == io.EOF {
			break
		}
		if e != nil {
			t.Fatal(e)
		}
		results = append(results, r)
	}
	if len(results) == 0 || results[0].Type != "ready" || results[0].Instance == "" {
		t.Fatal("missing ready handshake")
	}
	for _, r := range results {
		if r.V != 1 || r.Instance != results[0].Instance {
			t.Fatal("unstable protocol envelope")
		}
	}
	return results, err
}

func operation(kind string, revision int) Request {
	return Request{V: 1, Type: kind, Profile: "launcher", Epoch: 1, Revision: revision}
}

func TestProtocolAtomicCommitAndRevisionValidation(t *testing.T) {
	begin := operation("begin", 1)
	chunk := operation("chunk", 1)
	chunk.Rows = []Row{{Key: "old", Name: "Old"}}
	query := operation("search", 1)
	query.Request = 11
	chunk2 := operation("chunk", 2)
	chunk2.Rows = []Row{{Key: "new", Name: "New"}}
	query2 := operation("search", 2)
	query2.Request = 22
	requests := []Request{begin, chunk, query, operation("commit", 1), query, operation("begin", 2), chunk2, query, query2, operation("commit", 2), query, query2, operation("release", 2), query2}
	got, err := runProtocol(t, requests)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != len(requests)+1 {
		t.Fatalf("missing terminal replies: %d", len(got))
	}
	for _, i := range []int{3, 9, 11, 14} {
		if got[i].Type != "error" {
			t.Fatalf("reply %d should reject missing revision: %+v", i, got[i])
		}
	}
	for _, i := range []int{5, 8} {
		if len(got[i].Keys) != 1 || got[i].Keys[0] != "old" {
			t.Fatalf("staged data leaked: %+v", got[i])
		}
	}
	if len(got[12].Keys) != 1 || got[12].Keys[0] != "new" || got[12].Request != 22 {
		t.Fatalf("new revision: %+v", got[12])
	}
}

func TestProtocolFailedReplacementPreservesCurrentDataset(t *testing.T) {
	chunk := operation("chunk", 1)
	chunk.Rows = []Row{{Key: "old", Name: "Old"}}
	duplicate := operation("chunk", 2)
	duplicate.Rows = []Row{{Key: "same", Name: "A"}, {Key: "same", Name: "B"}}
	wrong := operation("chunk", 3)
	wrong.Rows = []Row{{Key: "x"}}
	requests := []Request{operation("begin", 1), chunk, operation("commit", 1), operation("begin", 2), wrong, duplicate, operation("commit", 2), operation("search", 1)}
	got, err := runProtocol(t, requests)
	if err != nil {
		t.Fatal(err)
	}
	if got[5].Type != "error" || got[7].Type != "error" {
		t.Fatal("invalid staged input accepted")
	}
	if len(got[8].Keys) != 1 || got[8].Keys[0] != "old" {
		t.Fatal("failed commit destroyed working dataset")
	}
}

type fragmentedReader struct{ data []byte }

func (r *fragmentedReader) Read(p []byte) (int, error) {
	if len(r.data) == 0 {
		return 0, io.EOF
	}
	n := min(3, len(p), len(r.data))
	copy(p, r.data[:n])
	r.data = r.data[n:]
	return n, nil
}

func TestProtocolFragmentedUnicodeAndFrameBounds(t *testing.T) {
	for _, size := range []int{MaxFrame, MaxFrame + 1} {
		base := `{"v":1,"type":"begin","profile":"launcher","epoch":1,"revision":1,"unused":"😀"}`
		data := []byte(base + strings.Repeat(" ", size-len(base)-1) + "\n")
		var output bytes.Buffer
		err := Serve(&fragmentedReader{data}, &output, "")
		if (size == MaxFrame) != (err == nil) {
			t.Fatalf("frame size %d: %v", size, err)
		}
	}
	for _, data := range []string{`{`, `{"v":2,"type":"begin","profile":"launcher"}`, `{"v":1,"type":"begin","profile":"unknown"}`, strings.Repeat("x", MaxFrame+1)} {
		if err := Serve(strings.NewReader(data), io.Discard, ""); err == nil {
			t.Fatalf("accepted invalid frame prefix %q", data[:min(50, len(data))])
		}
	}
}

func TestProtocolQueryAndRecordBudgets(t *testing.T) {
	query := operation("search", 1)
	query.Query.Query = strings.Repeat("😀", 2049)
	requests := []Request{operation("begin", 1), operation("commit", 1), query}
	got, err := runProtocol(t, requests)
	if err != nil {
		t.Fatal(err)
	}
	if got[3].Type != "error" {
		t.Fatal("accepted query above UTF-16 budget")
	}
	requests = []Request{operation("begin", 1)}
	for offset := 0; offset < 11000; offset += 1000 {
		chunk := operation("chunk", 1)
		for i := offset; i < offset+1000; i++ {
			chunk.Rows = append(chunk.Rows, Row{Key: fmt.Sprint(i)})
		}
		requests = append(requests, chunk)
	}
	requests = append(requests, operation("commit", 1))
	got, err = runProtocol(t, requests)
	if err != nil {
		t.Fatal(err)
	}
	if got[len(got)-2].Type != "error" || got[len(got)-1].Type != "error" {
		t.Fatal("record budget did not invalidate staged data")
	}
}

func TestProtocolEncodedDatasetBudgetAbortsStaging(t *testing.T) {
	chunk := operation("chunk", 1)
	chunk.Rows = []Row{{Key: "old", Name: "Old"}}
	requests := []Request{operation("begin", 1), chunk, operation("commit", 1), operation("begin", 2)}
	text := strings.Repeat("x", 192*1024)
	for i := 0; i < 90; i++ {
		next := operation("chunk", 2)
		next.Rows = []Row{{Key: fmt.Sprint(i), Text: text}}
		requests = append(requests, next)
	}
	requests = append(requests, operation("commit", 2), operation("search", 1))
	got, err := runProtocol(t, requests)
	if err != nil {
		t.Fatal(err)
	}
	budgetError := false
	for _, reply := range got {
		if reply.Type == "error" && reply.Error == "dataset exceeds budget" {
			budgetError = true
		}
	}
	if !budgetError || got[len(got)-2].Type != "error" {
		t.Fatal("oversized dataset did not invalidate staging")
	}
	last := got[len(got)-1]
	if len(last.Keys) != 1 || last.Keys[0] != "old" {
		t.Fatal("aborted staging destroyed current dataset")
	}
}

func TestProtocolDoesNotPublishOversizedResponse(t *testing.T) {
	requests := []Request{operation("begin", 1)}
	for i := 0; i < 50; i++ {
		chunk := operation("chunk", 1)
		chunk.Rows = []Row{{Key: fmt.Sprint(i) + strings.Repeat("x", 6000), Name: "Application"}}
		requests = append(requests, chunk)
	}
	requests = append(requests, operation("commit", 1), operation("search", 1))
	got, err := runProtocol(t, requests)
	if err == nil || !strings.Contains(err.Error(), "response exceeds") {
		t.Fatalf("oversized response: %v", err)
	}
	for _, reply := range got {
		if reply.Type == "results" {
			t.Fatal("published oversized results")
		}
	}
}

func TestProtocolStaleReleasePreservesCurrentAndStagedCatalogs(t *testing.T) {
	for _, profile := range []string{"launcher", "clipboard", "emoji"} {
		for _, stale := range []struct{ epoch, revision int }{{1, 2}, {1, 3}, {2, 1}} {
			t.Run(fmt.Sprintf("%s/epoch-%d/revision-%d", profile, stale.epoch, stale.revision), func(t *testing.T) {
				makeRequest := func(kind string, revision int, key string) Request {
					r := Request{V: 1, Type: kind, Profile: profile, Epoch: 2, Revision: revision}
					if key != "" {
						r.Rows = []Row{{Key: key, Name: key, Text: key}}
					}
					if profile == "emoji" {
						r.Query.Query = "😀"
					}
					return r
				}
				release := makeRequest("release", stale.revision, "")
				release.Epoch = stale.epoch
				requests := []Request{
					makeRequest("begin", 2, ""), makeRequest("chunk", 2, "current"), makeRequest("commit", 2, ""),
					makeRequest("begin", 3, ""), makeRequest("chunk", 3, "replacement"), release,
					makeRequest("search", 2, ""), makeRequest("commit", 3, ""), makeRequest("search", 3, ""),
				}
				got, err := runProtocol(t, requests)
				if err != nil {
					t.Fatal(err)
				}
				oldKey, newKey := "current", "replacement"
				if profile == "emoji" {
					oldKey, newKey = "1f600", "1f600"
				}
				if got[7].Type != "results" || len(got[7].Keys) != 1 || got[7].Keys[0] != oldKey {
					t.Fatalf("stale release destroyed current catalog: %+v", got[7])
				}
				if got[8].Type != "commit" {
					t.Fatalf("stale release destroyed staged catalog: %+v", got[8])
				}
				if got[9].Type != "results" || len(got[9].Keys) != 1 || got[9].Keys[0] != newKey {
					t.Fatalf("replacement unusable after stale release: %+v", got[9])
				}
			})
		}
	}
}

func TestProtocolReleaseRemovesOnlyMatchingRevision(t *testing.T) {
	for _, released := range []int{2, 3} {
		t.Run(fmt.Sprintf("revision-%d", released), func(t *testing.T) {
			currentChunk := operation("chunk", 2)
			currentChunk.Rows = []Row{{Key: "current", Name: "Current"}}
			stagedChunk := operation("chunk", 3)
			stagedChunk.Rows = []Row{{Key: "replacement", Name: "Replacement"}}
			requests := []Request{operation("begin", 2), currentChunk, operation("commit", 2), operation("begin", 3), stagedChunk, operation("release", released), operation("search", 2), operation("commit", 3), operation("search", 3)}
			got, err := runProtocol(t, requests)
			if err != nil {
				t.Fatal(err)
			}
			if released == 2 {
				if got[7].Type != "error" {
					t.Fatal("matching current revision survived release")
				}
				if got[8].Type != "commit" || got[9].Type != "results" || len(got[9].Keys) != 1 || got[9].Keys[0] != "replacement" {
					t.Fatal("release of current revision destroyed newer staged revision")
				}
			} else {
				if got[7].Type != "results" || len(got[7].Keys) != 1 || got[7].Keys[0] != "current" {
					t.Fatal("release of staged revision destroyed current revision")
				}
				if got[8].Type != "error" || got[9].Type != "error" {
					t.Fatal("released staged revision remained usable")
				}
			}
		})
	}
}
