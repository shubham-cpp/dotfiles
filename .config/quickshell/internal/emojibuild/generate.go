package emojibuild

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"time"
	"unicode/utf8"
)

type Source struct {
	File   string `json:"file"`
	URL    string `json:"url"`
	SHA256 string `json:"sha256"`
}
type Manifest struct {
	Unicode string   `json:"unicode"`
	CLDR    string   `json:"cldr"`
	Sources []Source `json:"sources"`
}

var sources = []Source{
	{File: "emoji-test.txt", URL: "https://www.unicode.org/Public/17.0.0/emoji/emoji-test.txt"},
	{File: "annotations.xml", URL: "https://raw.githubusercontent.com/unicode-org/cldr/release-48/common/annotations/en.xml"},
	{File: "derived.xml", URL: "https://raw.githubusercontent.com/unicode-org/cldr/release-48/common/annotationsDerived/en.xml"},
	{File: "LICENSE.txt", URL: "https://www.unicode.org/license.txt"},
}

type Options struct {
	Cache, Output string
	Check         bool
}
type Summary struct{ Sequences, Families, Bytes int }

// Generate uses cached sources when present. Missing sources are fetched only
// at build time. Check compares all three outputs without writing any of them.
func Generate(options Options) (Summary, error) {
	return generate(options, &http.Client{Timeout: 30 * time.Second})
}

func generate(options Options, client *http.Client) (Summary, error) {
	if err := os.MkdirAll(options.Cache, 0755); err != nil {
		return Summary{}, err
	}
	inputs := make(map[string][]byte, len(sources))
	manifest := Manifest{Unicode: "17.0", CLDR: "48", Sources: make([]Source, 0, len(sources))}
	for _, source := range sources {
		path := filepath.Join(options.Cache, source.File)
		data, err := os.ReadFile(path)
		if os.IsNotExist(err) {
			data, err = download(client, source.URL)
			if err == nil {
				err = writeAtomic(path, data)
			}
		}
		if err != nil {
			return Summary{}, fmt.Errorf("%s: %w", source.File, err)
		}
		inputs[source.File] = data
		source.SHA256 = fmt.Sprintf("%x", sha256.Sum256(data))
		manifest.Sources = append(manifest.Sources, source)
	}
	catalog, err := Build(inputs)
	if err != nil {
		return Summary{}, err
	}
	catalogJSON, err := encodeJSON(catalog, false)
	if err != nil {
		return Summary{}, err
	}
	manifestJSON, err := encodeJSON(manifest, true)
	if err != nil {
		return Summary{}, err
	}
	if !utf8.Valid(inputs["LICENSE.txt"]) {
		return Summary{}, fmt.Errorf("LICENSE.txt: invalid UTF-8")
	}
	outputs := []struct {
		name string
		data []byte
	}{{"emoji.json", catalogJSON}, {"emoji-sources.json", manifestJSON}, {"emoji-LICENSE.txt", inputs["LICENSE.txt"]}}
	if !options.Check {
		if err := os.MkdirAll(options.Output, 0755); err != nil {
			return Summary{}, err
		}
	}
	for _, output := range outputs {
		path := filepath.Join(options.Output, output.name)
		if options.Check {
			data, err := os.ReadFile(path)
			if err != nil {
				return Summary{}, err
			}
			if !bytes.Equal(data, output.data) {
				return Summary{}, fmt.Errorf("Mismatch: %s", path)
			}
		} else if err := writeAtomic(path, output.data); err != nil {
			return Summary{}, err
		}
	}
	return Summary{len(catalog.Entries), len(catalog.Families), len(catalogJSON)}, nil
}

func download(client *http.Client, url string) ([]byte, error) {
	response, err := client.Get(url)
	if err != nil {
		return nil, err
	}
	defer response.Body.Close()
	if response.StatusCode < 200 || response.StatusCode >= 300 {
		return nil, fmt.Errorf("HTTP %s", response.Status)
	}
	const maxSource = 16 * 1024 * 1024
	data, err := io.ReadAll(io.LimitReader(response.Body, maxSource+1))
	if err != nil {
		return nil, err
	}
	if len(data) > maxSource {
		return nil, fmt.Errorf("source exceeds %d bytes", maxSource)
	}
	return data, nil
}

func writeAtomic(path string, data []byte) error {
	file, err := os.CreateTemp(filepath.Dir(path), ".emoji-build-*")
	if err != nil {
		return err
	}
	temporary := file.Name()
	// Only the temporary file created by this operation is eligible for cleanup.
	defer os.Remove(temporary)
	if _, err := file.Write(data); err != nil {
		file.Close()
		return err
	}
	if err := file.Chmod(0644); err != nil {
		file.Close()
		return err
	}
	if err := file.Close(); err != nil {
		return err
	}
	return os.Rename(temporary, path)
}
