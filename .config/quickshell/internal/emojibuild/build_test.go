package emojibuild

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func fixtureInputs() map[string][]byte {
	return map[string][]byte{
		"emoji-test.txt":  []byte("# group: People\n# subgroup: hands\n1F44D ; fully-qualified # 👍 E1.0 thumbs up\n1F44D 1F3FD ; fully-qualified # 👍🏽 E1.0 thumbs up: medium skin tone\n# group: Symbols\n# subgroup: hearts\n2764 FE0F ; fully-qualified # ❤️ E1.0 red heart\n2764 ; unqualified # ❤ E1.0 red heart\n"),
		"annotations.xml": []byte(`<ldml><annotations><annotation cp="👍">good | yes | hand</annotation><annotation cp="❤">love | heart</annotation></annotations></ldml>`),
		"derived.xml":     []byte(`<ldml><annotations><annotation cp="👍">yes | approval</annotation><annotation cp="👍🏽" type="tts">thumbs up: medium skin tone</annotation><annotation cp="❤️">red</annotation></annotations></ldml>`),
		"LICENSE.txt":     []byte("Test license\n"),
	}
}

func TestBuildCatalog(t *testing.T) {
	catalog, err := Build(fixtureInputs())
	if err != nil {
		t.Fatal(err)
	}
	if catalog.Schema != 1 || catalog.Unicode != "17.0" || catalog.CLDR != "48" || len(catalog.Entries) != 3 || len(catalog.Families) != 2 {
		t.Fatal(catalog)
	}
	if !reflect.DeepEqual(catalog.Groups, []string{"People", "Symbols"}) {
		t.Fatal(catalog.Groups)
	}
	want := Family{Name: "thumbs up", Group: "People", Subgroup: "hands", Variants: []string{"1f44d", "1f44d-1f3fd"}, ID: "1f44d", Slots: 1}
	if !reflect.DeepEqual(catalog.Families[0], want) {
		t.Fatal(catalog.Families[0])
	}
	if catalog.Entries[0].Search != "thumbs up approval good hand yes +1 thumbsup yes" {
		t.Fatal(catalog.Entries[0].Search)
	}
	if !reflect.DeepEqual(catalog.Entries[1].Tones, []int{3}) || catalog.Entries[1].FamilyID != "1f44d" {
		t.Fatal(catalog.Entries[1])
	}
	if catalog.Entries[2].Search != "red heart heart love red heart love" {
		t.Fatal(catalog.Entries[2].Search)
	}
	data, err := encodeJSON(catalog, false)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(data, []byte(`"tones":[],"search"`)) || !bytes.Contains(data, []byte(`"text":"❤️"`)) || data[len(data)-1] != '\n' {
		t.Fatal(string(data))
	}
}

func TestFamilySemantics(t *testing.T) {
	for _, tc := range []struct{ name, want string }{
		{"kiss: person, person, light skin tone, dark skin tone", "kiss"},
		{"couple with heart: person, person, medium skin tone, light skin tone", "couple with heart"},
		{"person: medium skin tone, beard", "person: beard"},
		{"woman running facing right: dark skin tone", "woman running facing right"},
	} {
		if got := familyName(tc.name); got != tc.want {
			t.Fatal(tc.name, got)
		}
	}
	if got := normalized("  RED_Heart:\tLOVE-yes\u00a0good"); got != "red heart love yes good" {
		t.Fatal(got)
	}
}

func TestBuildRejectsInvalidFamiliesAndInput(t *testing.T) {
	for _, tc := range []struct{ name, text, want string }{
		{"missing default", "1F44D 1F3FD ; fully-qualified # 👍🏽 E1.0 thumbs up: medium skin tone", "Ambiguous"},
		{"two defaults", "1F44D ; fully-qualified # 👍 E1.0 thumbs up\n1F44E ; fully-qualified # 👎 E1.0 thumbs up", "Ambiguous"},
		{"duplicate sequence", "1F44D ; fully-qualified # 👍 E1.0 thumbs up\n1F44D ; fully-qualified # 👍 E1.0 another name", "duplicate"},
		{"invalid codepoint", "110000 ; fully-qualified # x E1.0 invalid", "invalid code point"},
		{"invalid UTF8", string([]byte{255}), "invalid UTF-8"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			inputs := fixtureInputs()
			inputs["emoji-test.txt"] = []byte(tc.text)
			if _, err := Build(inputs); err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatal(err)
			}
		})
	}
	for _, xml := range []string{"", "<ldml>", "<ldml/><ldml/>", "<ldml><annotation>no cp</annotation></ldml>"} {
		inputs := fixtureInputs()
		inputs["annotations.xml"] = []byte(xml)
		if _, err := Build(inputs); err == nil {
			t.Fatal("invalid XML accepted", xml)
		}
	}
}

func TestUnicodeJSONPreservesLiteralEscapes(t *testing.T) {
	value := map[string]string{"text": "<heart> & \u2028 \u2029", "literal": `\u2028\u2029`, "mixed": "\\\u2028"}
	data, err := encodeJSON(value, false)
	if err != nil {
		t.Fatal(err)
	}
	var decoded map[string]string
	if err := json.Unmarshal(data, &decoded); err != nil || !reflect.DeepEqual(value, decoded) {
		t.Fatal(string(data), err, decoded)
	}
	if !bytes.Contains(data, []byte("<heart> & \u2028 \u2029")) {
		t.Fatal(string(data))
	}
}

func writeInputs(t *testing.T, cache string) {
	t.Helper()
	if err := os.MkdirAll(cache, 0700); err != nil {
		t.Fatal(err)
	}
	for name, data := range fixtureInputs() {
		if err := os.WriteFile(filepath.Join(cache, name), data, 0600); err != nil {
			t.Fatal(err)
		}
	}
}

type noNetwork struct{ t *testing.T }

func (n noNetwork) RoundTrip(*http.Request) (*http.Response, error) {
	n.t.Fatal("cached build attempted a network request")
	return nil, fmt.Errorf("unexpected network")
}

func TestGenerateAndCheckOffline(t *testing.T) {
	root := t.TempDir()
	options := Options{Cache: filepath.Join(root, "cache"), Output: filepath.Join(root, "data")}
	writeInputs(t, options.Cache)
	client := &http.Client{Transport: noNetwork{t}}
	summary, err := generate(options, client)
	if err != nil {
		t.Fatal(err)
	}
	if summary.Sequences != 3 || summary.Families != 2 || summary.Bytes == 0 {
		t.Fatal(summary)
	}
	options.Check = true
	if checked, err := generate(options, client); err != nil || checked != summary {
		t.Fatal(checked, err)
	}
	license := filepath.Join(options.Output, "emoji-LICENSE.txt")
	if err := os.WriteFile(license, []byte("changed\n"), 0600); err != nil {
		t.Fatal(err)
	}
	if _, err := generate(options, client); err == nil || !strings.Contains(err.Error(), "Mismatch: "+license) {
		t.Fatal(err)
	}
	if data, err := os.ReadFile(license); err != nil || string(data) != "changed\n" {
		t.Fatal("check modified output", string(data), err)
	}
}

func TestManifestDetectsSourceOnlyChanges(t *testing.T) {
	root := t.TempDir()
	options := Options{Cache: filepath.Join(root, "cache"), Output: filepath.Join(root, "data")}
	writeInputs(t, options.Cache)
	client := &http.Client{Transport: noNetwork{t}}
	if _, err := generate(options, client); err != nil {
		t.Fatal(err)
	}
	source := filepath.Join(options.Cache, "emoji-test.txt")
	data, err := os.ReadFile(source)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(source, append(data, []byte("# changed source comment\n")...), 0600); err != nil {
		t.Fatal(err)
	}
	options.Check = true
	if _, err := generate(options, client); err == nil || !strings.Contains(err.Error(), "emoji-sources.json") {
		t.Fatal(err)
	}
}

func TestBuildFailureDoesNotReplaceOutputs(t *testing.T) {
	root := t.TempDir()
	options := Options{Cache: filepath.Join(root, "cache"), Output: filepath.Join(root, "data")}
	writeInputs(t, options.Cache)
	client := &http.Client{Transport: noNetwork{t}}
	if _, err := generate(options, client); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(options.Output, "emoji.json")
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(options.Cache, "annotations.xml"), []byte("broken"), 0600); err != nil {
		t.Fatal(err)
	}
	if _, err := generate(options, client); err == nil {
		t.Fatal("bad XML accepted")
	}
	after, err := os.ReadFile(path)
	if err != nil || !bytes.Equal(before, after) {
		t.Fatal("failed build replaced output", err)
	}
}

func TestDownloadStatusesAndLimits(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/ok":
			fmt.Fprint(w, "source bytes")
		case "/large":
			_, _ = io.CopyN(w, zeroReader{}, 16*1024*1024+1)
		default:
			http.Error(w, "missing", http.StatusNotFound)
		}
	}))
	defer server.Close()
	if data, err := download(server.Client(), server.URL+"/ok"); err != nil || string(data) != "source bytes" {
		t.Fatal(string(data), err)
	}
	if _, err := download(server.Client(), server.URL+"/missing"); err == nil || !strings.Contains(err.Error(), "404") {
		t.Fatal(err)
	}
	if _, err := download(server.Client(), server.URL+"/large"); err == nil || !strings.Contains(err.Error(), "exceeds") {
		t.Fatal(err)
	}
}

type zeroReader struct{}

func (zeroReader) Read(p []byte) (int, error) { clear(p); return len(p), nil }
