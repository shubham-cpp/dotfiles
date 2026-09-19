package search

import (
	"bufio"
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"unicode/utf16"
)

const MaxFrame = 256 * 1024
const MaxDataset = 16 * 1024 * 1024

type Request struct {
	V        int    `json:"v"`
	Type     string `json:"type"`
	Profile  string `json:"profile"`
	Epoch    int    `json:"epoch"`
	Revision int    `json:"revision"`
	Request  int    `json:"request"`
	Rows     []Row  `json:"rows"`
	Query
}
type Response struct {
	Instance string   `json:"instance"`
	V        int      `json:"v"`
	Type     string   `json:"type"`
	Profile  string   `json:"profile,omitempty"`
	Epoch    int      `json:"epoch,omitempty"`
	Revision int      `json:"revision,omitempty"`
	Request  int      `json:"request,omitempty"`
	Keys     []string `json:"keys,omitempty"`
	Error    string   `json:"error,omitempty"`
}
type dataset struct {
	epoch, revision, bytes int
	rows                   []Row
	catalog                *Catalog
}

type protocolState struct {
	current, staged map[string]*dataset
	emoji           *Emoji
	emojiPath       string
	files           *FileIndex
}

func (d *dataset) matches(req Request) bool {
	return d != nil && d.epoch == req.Epoch && d.revision == req.Revision
}

func (s *protocolState) chunk(req Request, size int) error {
	d := s.staged[req.Profile]
	if !d.matches(req) {
		return errors.New("missing staged revision")
	}
	if d.bytes+size > MaxDataset || len(d.rows)+len(req.Rows) > 10000 {
		delete(s.staged, req.Profile)
		return errors.New("dataset exceeds budget")
	}
	d.bytes += size
	d.rows = append(d.rows, req.Rows...)
	return nil
}

func (s *protocolState) commit(req Request) error {
	d := s.staged[req.Profile]
	if !d.matches(req) {
		return errors.New("missing staged revision")
	}
	defer delete(s.staged, req.Profile)
	if req.Profile == "files" {
		if len(d.rows) != 0 {
			return errors.New("files catalog is walked, not uploaded")
		}
		idx, err := readFiles()
		if err != nil {
			return err
		}
		s.files = idx
		s.current[req.Profile] = d
		return nil
	}
	catalog, err := NewCatalog(d.rows)
	if err != nil {
		return err
	}
	d.catalog = catalog
	d.rows = nil
	s.current[req.Profile] = d
	return nil
}

func (s *protocolState) release(req Request) {
	if s.staged[req.Profile].matches(req) {
		delete(s.staged, req.Profile)
	}
	if s.current[req.Profile].matches(req) {
		delete(s.current, req.Profile)
		if req.Profile == "emoji" {
			s.emoji = nil
		}
		if req.Profile == "files" {
			s.files = nil
		}
	}
}

func (s *protocolState) search(req Request) ([]string, error) {
	d := s.current[req.Profile]
	if !d.matches(req) {
		return nil, errors.New("unknown catalog revision")
	}
	if len(utf16.Encode([]rune(req.Query.Query))) > 4096 {
		return nil, errors.New("query exceeds budget")
	}
	switch req.Profile {
	case "launcher":
		return d.catalog.Launcher(req.Query), nil
	case "clipboard":
		return d.catalog.Clipboard(req.Query), nil
	case "files":
		if s.files == nil {
			return nil, errors.New("file list unavailable")
		}
		return s.files.Search(req.Query), nil
	default: // Profile validation restricts this branch to emoji.
		if s.emoji == nil {
			var err error
			s.emoji, err = LoadEmoji(s.emojiPath)
			if err != nil {
				return nil, errors.New("emoji catalog unavailable")
			}
		}
		return s.emoji.Search(req.Query), nil
	}
}

func (s *protocolState) handle(req Request, size int) Response {
	r := Response{Type: req.Type, Profile: req.Profile, Epoch: req.Epoch, Revision: req.Revision, Request: req.Request}
	var err error
	switch req.Type {
	case "begin":
		s.staged[req.Profile] = &dataset{epoch: req.Epoch, revision: req.Revision}
	case "chunk":
		err = s.chunk(req, size)
	case "commit":
		err = s.commit(req)
	case "release":
		s.release(req)
	case "search":
		r.Type = "results"
		r.Keys, err = s.search(req)
	default:
		err = errors.New("unknown operation")
	}
	if err != nil {
		r.Type, r.Error = "error", err.Error()
	}
	return r
}

func validProfile(profile string) bool {
	switch profile {
	case "launcher", "clipboard", "emoji", "files":
		return true
	default:
		return false
	}
}

func decodeRequest(raw []byte) (Request, error) {
	var req Request
	if len(raw)+1 > MaxFrame {
		return req, errors.New("request exceeds frame budget")
	}
	if err := json.Unmarshal(raw, &req); err != nil {
		return req, errors.New("invalid protocol JSON")
	}
	if req.V != 1 {
		return req, errors.New("unsupported protocol")
	}
	if !validProfile(req.Profile) {
		return req, errors.New("unknown search profile")
	}
	return req, nil
}

func sendResponse(writer *bufio.Writer, instance string, r Response) error {
	r.V = 1
	r.Instance = instance
	data, err := json.Marshal(r)
	if err != nil {
		return err
	}
	if len(data)+1 > MaxFrame {
		return errors.New("response exceeds frame budget")
	}
	if _, err = writer.Write(append(data, '\n')); err != nil {
		return err
	}
	return writer.Flush()
}

// Serve is sequential: one acknowledgement per command bounds the caller's pipe
// backlog. Each profile keeps one current revision and one bounded staged revision.
func Serve(in io.Reader, out io.Writer, emojiPath string) error {
	instance := fmt.Sprintf("%x", rand.Text())
	writer := bufio.NewWriter(out)
	if err := sendResponse(writer, instance, Response{Type: "ready"}); err != nil {
		return err
	}
	state := protocolState{current: map[string]*dataset{}, staged: map[string]*dataset{}, emojiPath: emojiPath}
	scanner := bufio.NewScanner(in)
	scanner.Buffer(make([]byte, 32*1024), MaxFrame+1)
	for scanner.Scan() {
		raw := scanner.Bytes()
		req, err := decodeRequest(raw)
		if err != nil {
			return err
		}
		if err := sendResponse(writer, instance, state.handle(req, len(raw))); err != nil {
			return err
		}
	}
	if err := scanner.Err(); err != nil {
		return fmt.Errorf("search input: %w", err)
	}
	return nil
}
