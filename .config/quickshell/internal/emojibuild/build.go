// Package emojibuild builds the offline Unicode 17.0 / CLDR 48 emoji catalog.
package emojibuild

import (
	"bytes"
	"encoding/json"
	"encoding/xml"
	"fmt"
	"io"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type Entry struct {
	ID       string   `json:"id"`
	Text     string   `json:"text"`
	Name     string   `json:"name"`
	Tones    []int    `json:"tones"`
	Search   string   `json:"search"`
	Aliases  []string `json:"aliases"`
	FamilyID string   `json:"familyId"`
}
type Family struct {
	Name     string   `json:"name"`
	Group    string   `json:"group"`
	Subgroup string   `json:"subgroup"`
	Variants []string `json:"variants"`
	ID       string   `json:"id"`
	Slots    int      `json:"slots"`
}
type Catalog struct {
	Schema   int      `json:"schema"`
	Unicode  string   `json:"unicode"`
	CLDR     string   `json:"cldr"`
	Groups   []string `json:"groups"`
	Families []Family `json:"families"`
	Entries  []Entry  `json:"entries"`
}

var tones = regexp.MustCompile(`(?:medium-light|medium-dark|light|medium|dark) skin tone`)
var colonComma = regexp.MustCompile(`:\s*,\s*`)
var doubleComma = regexp.MustCompile(`,\s*,`)
var emojiLine = regexp.MustCompile(`^([0-9A-F ]+)\s*; fully-qualified\s*# \S+ E[0-9.]+ (.+)$`)
var aliases = map[string][]string{
	"thumbs up": {"+1", "thumbsup", "yes"}, "thumbs down": {"-1", "thumbsdown", "no"},
	"red heart": {"heart", "love"}, "face with tears of joy": {"joy", "lol", "laugh"},
	"rolling on the floor laughing": {"rofl", "lmao"}, "party popper": {"tada"},
	"folded hands": {"pray", "thanks"}, "fire": {"lit"}, "pile of poo": {"poop", "shit"},
}

func whitespace(r rune) bool { return unicode.IsSpace(r) || (r >= 0x1c && r <= 0x1f) }
func collapseSpaces(text string) string {
	return strings.Join(strings.FieldsFunc(text, whitespace), " ")
}
func normalized(text string) string {
	text = strings.ToLower(text)
	text = strings.NewReplacer("_", " ", ":", " ", "-", " ").Replace(text)
	return collapseSpaces(text)
}

func familyName(name string) string {
	name = tones.ReplaceAllString(name, "")
	name = colonComma.ReplaceAllString(name, ": ")
	name = doubleComma.ReplaceAllString(name, ",")
	name = strings.Trim(collapseSpaces(name), " ,:")
	switch name {
	case "kiss: person, person":
		return "kiss"
	case "couple with heart: person, person":
		return "couple with heart"
	default:
		return name
	}
}

func readAnnotations(inputs map[string][]byte) (map[string]map[string]bool, error) {
	annotations := make(map[string]map[string]bool)
	for _, filename := range []string{"annotations.xml", "derived.xml"} {
		data, ok := inputs[filename]
		if !ok {
			return nil, fmt.Errorf("missing source: %s", filename)
		}
		decoder := xml.NewDecoder(bytes.NewReader(data))
		depth, roots := 0, 0
		for {
			token, err := decoder.Token()
			if err == io.EOF {
				if roots != 1 || depth != 0 {
					return nil, fmt.Errorf("%s: expected one complete XML document", filename)
				}
				break
			}
			if err != nil {
				return nil, fmt.Errorf("%s: %w", filename, err)
			}
			if _, ok := token.(xml.EndElement); ok {
				depth--
				continue
			}
			start, ok := token.(xml.StartElement)
			if !ok {
				continue
			}
			if depth == 0 {
				roots++
				if roots > 1 {
					return nil, fmt.Errorf("%s: multiple XML roots", filename)
				}
			}
			if start.Name.Local != "annotation" {
				depth++
				continue
			}
			key, found := "", false
			for _, attr := range start.Attr {
				if attr.Name.Local == "cp" {
					key, found = strings.ReplaceAll(attr.Value, "\ufe0f", ""), true
					break
				}
			}
			if !found {
				return nil, fmt.Errorf("%s: annotation missing cp", filename)
			}
			var text string
			if err := decoder.DecodeElement(&text, &start); err != nil {
				return nil, fmt.Errorf("%s: %w", filename, err)
			}
			if annotations[key] == nil {
				annotations[key] = make(map[string]bool)
			}
			for _, value := range strings.Split(text, " | ") {
				annotations[key][value] = true
			}
		}
	}
	return annotations, nil
}

// Build preserves input order for groups, families and variants, and verifies
// each family has exactly one untoned default and every sequence ID is unique.
func Build(inputs map[string][]byte) (Catalog, error) {
	annotations, err := readAnnotations(inputs)
	if err != nil {
		return Catalog{}, err
	}
	data, ok := inputs["emoji-test.txt"]
	if !ok {
		return Catalog{}, fmt.Errorf("missing source: emoji-test.txt")
	}
	if !utf8.Valid(data) {
		return Catalog{}, fmt.Errorf("emoji-test.txt: invalid UTF-8")
	}
	catalog := Catalog{Schema: 1, Unicode: "17.0", CLDR: "48", Groups: make([]string, 0), Families: make([]Family, 0), Entries: make([]Entry, 0)}
	group, subgroup := "", ""
	groups, families, ids := make(map[string]bool), make(map[string]int), make(map[string]int)
	for _, line := range strings.Split(strings.ReplaceAll(string(data), "\r\n", "\n"), "\n") {
		if strings.HasPrefix(line, "# group: ") {
			group = line[9:]
		}
		if strings.HasPrefix(line, "# subgroup: ") {
			subgroup = line[12:]
		}
		hit := emojiLine.FindStringSubmatch(line)
		if hit == nil {
			continue
		}
		entry, err := buildEntry(hit[1], hit[2], annotations)
		if err != nil {
			return Catalog{}, err
		}
		if _, duplicate := ids[entry.ID]; duplicate {
			return Catalog{}, fmt.Errorf("duplicate sequence: %s", entry.ID)
		}
		ids[entry.ID] = len(catalog.Entries)
		name := familyName(entry.Name)
		index, found := families[name]
		if !found {
			index = len(catalog.Families)
			families[name] = index
			catalog.Families = append(catalog.Families, Family{Name: name, Group: group, Subgroup: subgroup, Variants: make([]string, 0)})
		}
		family := &catalog.Families[index]
		family.Variants = append(family.Variants, entry.ID)
		if !groups[group] {
			groups[group] = true
			catalog.Groups = append(catalog.Groups, group)
		}
		catalog.Entries = append(catalog.Entries, entry)
	}
	for i := range catalog.Families {
		family := &catalog.Families[i]
		defaults := make([]string, 0)
		for _, id := range family.Variants {
			entry := &catalog.Entries[ids[id]]
			if len(entry.Tones) == 0 {
				defaults = append(defaults, id)
			}
			family.Slots = max(family.Slots, len(entry.Tones))
		}
		if len(defaults) != 1 {
			return Catalog{}, fmt.Errorf("Ambiguous family %s: %v", family.Name, defaults)
		}
		family.ID = defaults[0]
		for _, id := range family.Variants {
			catalog.Entries[ids[id]].FamilyID = family.ID
		}
	}
	return catalog, nil
}

func buildEntry(codeText, name string, annotations map[string]map[string]bool) (Entry, error) {
	codes := strings.Fields(codeText)
	ids := make([]string, 0, len(codes))
	var sequence strings.Builder
	entry := Entry{Name: name, Tones: make([]int, 0), Aliases: make([]string, 0)}
	for _, text := range codes {
		code, err := strconv.ParseInt(text, 16, 32)
		if err != nil || !utf8.ValidRune(rune(code)) {
			return Entry{}, fmt.Errorf("invalid code point: %s", text)
		}
		ids = append(ids, strconv.FormatInt(code, 16))
		sequence.WriteRune(rune(code))
		if code >= 0x1f3fb && code <= 0x1f3ff {
			entry.Tones = append(entry.Tones, int(code-0x1f3fa))
		}
	}
	entry.ID, entry.Text = strings.Join(ids, "-"), sequence.String()
	keywords := make([]string, 0)
	for keyword := range annotations[strings.ReplaceAll(entry.Text, "\ufe0f", "")] {
		keywords = append(keywords, keyword)
	}
	sort.Strings(keywords)
	words := append([]string{name}, keywords...)
	for _, alias := range aliases[familyName(name)] {
		entry.Aliases = append(entry.Aliases, normalized(alias))
		words = append(words, alias)
	}
	entry.Search = normalized(strings.Join(words, " "))
	return entry, nil
}

func encodeJSON(value any, indent bool) ([]byte, error) {
	var buffer bytes.Buffer
	encoder := json.NewEncoder(&buffer)
	encoder.SetEscapeHTML(false)
	if indent {
		encoder.SetIndent("", "  ")
	}
	if err := encoder.Encode(value); err != nil {
		return nil, err
	}
	// Python ensure_ascii=False preserves these valid Unicode separators too.
	return unescapeSeparators(buffer.Bytes()), nil
}

func unescapeSeparators(data []byte) []byte {
	output := make([]byte, 0, len(data))
	for i := 0; i < len(data); i++ {
		if data[i] == '\\' && i+1 < len(data) {
			if i+6 <= len(data) && (string(data[i:i+6]) == `\u2028` || string(data[i:i+6]) == `\u2029`) {
				separator := '\u2028'
				if data[i+5] == '9' {
					separator = '\u2029'
				}
				output = utf8.AppendRune(output, separator)
				i += 5
				continue
			}
			// Preserve an escaped backslash as a pair, so a literal "\\u2028"
			// cannot be mistaken for the encoder's Unicode separator escape.
			output = append(output, data[i], data[i+1])
			i++
			continue
		}
		output = append(output, data[i])
	}
	return output
}
