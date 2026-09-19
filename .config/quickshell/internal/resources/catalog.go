package resources

import (
	"errors"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"unicode/utf8"
)

type catalogEntry struct {
	App     App
	Aliases []string
}
type Catalog struct{ ids, aliases map[string]*App }

func newCatalog(entries []catalogEntry) *Catalog {
	c := &Catalog{ids: make(map[string]*App), aliases: make(map[string]*App)}
	sort.SliceStable(entries, func(i, j int) bool {
		return utf8.RuneCountInString(entries[i].App.Key) < utf8.RuneCountInString(entries[j].App.Key)
	})
	for i := range entries {
		e := &entries[i]
		// A separate App allocation lets the parser's alias slices be collected.
		app := e.App
		c.ids[app.Key] = &app
		for _, alias := range e.Aliases {
			alias = strings.ToLower(alias)
			if alias != "" && !wrappers[alias] && c.aliases[alias] == nil {
				c.aliases[alias] = &app
			}
		}
	}
	return c
}

func (c *Catalog) match(exe, name string, argv []string) *App {
	arg := ""
	if len(argv) > 0 {
		arg = argv[0]
	}
	base := ""
	if exe != "" {
		base = filepath.Base(exe)
	}
	for _, candidate := range []string{exe, arg, base, name} {
		if app := c.aliases[strings.ToLower(candidate)]; app != nil {
			return app
		}
	}
	return nil
}

func desktopDirectories() []string {
	home := os.Getenv("XDG_DATA_HOME")
	if home == "" {
		if userHome, err := os.UserHomeDir(); err == nil {
			home = filepath.Join(userHome, ".local/share")
		}
	}
	data := os.Getenv("XDG_DATA_DIRS")
	if data == "" {
		data = "/usr/local/share:/usr/share"
	}
	dirs := make([]string, 0)
	for _, dir := range append([]string{home}, filepath.SplitList(data)...) {
		if filepath.IsAbs(dir) {
			dirs = append(dirs, filepath.Join(dir, "applications"))
		}
	}
	return dirs
}

// Read desktop identities once per popup process. Hidden entries mask lower
// priority files; NoDisplay/OnlyShowIn are intentionally not visibility filters:
// these applications still need names when they are already running.
func desktopCatalog() *Catalog {
	return loadCatalog(desktopDirectories(), localeNames())
}

func loadCatalog(dirs, locales []string) *Catalog {
	seen := make(map[string]bool)
	entries := make([]catalogEntry, 0)
	for _, dir := range dirs {
		_ = filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(d.Name(), ".desktop") {
				return nil
			}
			rel, err := filepath.Rel(dir, path)
			if err != nil {
				return nil
			}
			id := strings.ReplaceAll(rel, string(filepath.Separator), "-")
			if seen[id] {
				return nil
			}
			seen[id] = true
			data, err := os.ReadFile(path)
			if err != nil {
				return nil
			}
			if entry, ok := parseDesktop(id, string(data), locales); ok {
				entries = append(entries, entry)
			}
			return nil
		})
	}
	return newCatalog(entries)
}

func iniSection(text, section string) map[string]string {
	values := make(map[string]string)
	active := false
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, ";") {
			continue
		}
		if strings.HasPrefix(line, "[") && strings.HasSuffix(line, "]") {
			active = line == "["+section+"]"
			continue
		}
		if !active {
			continue
		}
		if key, value, ok := strings.Cut(line, "="); ok {
			values[strings.TrimSpace(key)] = strings.TrimSpace(value)
		}
	}
	return values
}

func desktopUnescape(value string) string {
	var out strings.Builder
	for i := 0; i < len(value); i++ {
		if value[i] != '\\' || i+1 == len(value) {
			out.WriteByte(value[i])
			continue
		}
		i++
		switch value[i] {
		case 's':
			out.WriteByte(' ')
		case 'n':
			out.WriteByte('\n')
		case 't':
			out.WriteByte('\t')
		case 'r':
			out.WriteByte('\r')
		case '\\':
			out.WriteByte('\\')
		default:
			out.WriteByte('\\')
			out.WriteByte(value[i])
		}
	}
	return out.String()
}

func parseDesktop(id, text string, locales []string) (catalogEntry, bool) {
	v := iniSection(text, "Desktop Entry")
	if v["Type"] != "Application" || v["Hidden"] == "true" {
		return catalogEntry{}, false
	}
	name := v["Name"]
	for _, locale := range locales {
		if localized, ok := v["Name["+locale+"]"]; ok {
			name = localized
			break
		}
	}
	if name == "" {
		name = "Unnamed"
	}
	if tryExec := desktopUnescape(v["TryExec"]); tryExec != "" {
		if !desktopExecutable(tryExec, desktopUnescape(v["Path"])) {
			return catalogEntry{}, false
		}
	}
	command := desktopUnescape(v["Exec"])
	app := App{Key: strings.TrimSuffix(id, ".desktop"), Name: desktopUnescape(name), Icon: desktopUnescape(v["Icon"])}
	aliases := []string{app.Key, desktopUnescape(v["StartupWMClass"])}
	argv, ok := shellWords(command)
	// Gio accepts metadata-only files with no Exec, but when Exec is present
	// its first executable must exist (even without a TryExec key).
	if command != "" && (!ok || len(argv) == 0 || !desktopExecutable(argv[0], desktopUnescape(v["Path"]))) {
		return catalogEntry{}, false
	}
	if len(argv) > 0 && filepath.Base(argv[0]) == "env" {
		next := make([]string, 0)
		for _, arg := range argv[1:] {
			if !strings.Contains(arg, "=") && !strings.HasPrefix(arg, "-") {
				next = append(next, arg)
			}
		}
		argv = next
	}
	if len(argv) > 0 && !wrappers[filepath.Base(argv[0])] {
		aliases = append(aliases, argv[0], filepath.Base(argv[0]))
	}
	return catalogEntry{app, aliases}, true
}

func desktopExecutable(program, directory string) bool {
	if strings.ContainsRune(program, filepath.Separator) && !filepath.IsAbs(program) && directory != "" {
		program = filepath.Join(directory, program)
	}
	_, err := exec.LookPath(program)
	return err == nil || errors.Is(err, exec.ErrDot)
}

// shellWords mirrors the Python helper's shlex.split, without executing Exec.
func shellWords(text string) ([]string, bool) {
	args := make([]string, 0)
	var word strings.Builder
	quote := byte(0)
	started := false
	for i := 0; i < len(text); i++ {
		ch := text[i]
		if quote == '\'' {
			if ch == '\'' {
				quote = 0
			} else {
				word.WriteByte(ch)
			}
			continue
		}
		if ch == '\\' {
			if i+1 == len(text) {
				return nil, false
			}
			i++
			if quote == '"' && text[i] != '"' && text[i] != '\\' {
				word.WriteByte('\\')
			}
			word.WriteByte(text[i])
			started = true
			continue
		}
		if quote == '"' {
			if ch == '"' {
				quote = 0
			} else {
				word.WriteByte(ch)
			}
			continue
		}
		if ch == '\'' || ch == '"' {
			quote = ch
			started = true
			continue
		}
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			if started {
				args = append(args, word.String())
				word.Reset()
				started = false
			}
			continue
		}
		word.WriteByte(ch)
		started = true
	}
	if quote != 0 {
		return nil, false
	}
	if started {
		args = append(args, word.String())
	}
	return args, true
}

func localeNames() []string {
	base := os.Getenv("LC_ALL")
	if base == "" {
		base = os.Getenv("LC_MESSAGES")
	}
	if base == "" {
		base = os.Getenv("LANG")
	}
	if base == "" || base == "C" || base == "POSIX" {
		return []string{"C"}
	}
	preferred := os.Getenv("LANGUAGE")
	if preferred == "" {
		preferred = base
	}
	result := make([]string, 0)
	for _, locale := range strings.Split(preferred, ":") {
		// GLib locale lookup also tries territory/modifier variants.
		locale, modifier, _ := strings.Cut(locale, "@")
		locale, _, _ = strings.Cut(locale, ".")
		language, _, hasTerritory := strings.Cut(locale, "_")
		if modifier != "" {
			result = append(result, locale+"@"+modifier)
			if hasTerritory {
				result = append(result, language+"@"+modifier)
			}
		}
		result = append(result, locale)
		if hasTerritory {
			result = append(result, language)
		}
	}
	return append(result, "C")
}
