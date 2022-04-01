import os

from qutebrowser.config.config import ConfigContainer  # noqa: F401

# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401

# config = config  # noqa: F821 pylint: disable=E0602,C0103
# c = c  # noqa: F821 pylint: disable=E0602,C0103
config: ConfigAPI = config  # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig(False)

config.set("content.cookies.accept", "all", "chrome-devtools://*")
config.set("content.cookies.accept", "all", "devtools://*")


def nmap(key, command):
    """Bind key to command in normal mode."""
    config.bind(key, command, mode="normal")


# ui
c.completion.scrollbar.width = 10
c.tabs.position = "top"
c.tabs.show = "multiple"
c.tabs.indicator.width = 0
c.tabs.title.format = "{index}: {audio}{current_title}"
c.tabs.title.alignment = "center"
c.downloads.position = "bottom"
c.tabs.favicons.show = "always"  # 'never'

c.url.searchengines = {
    "DEFAULT": "https://google.com/search?q={}",
    "dd": "https://duckduckgo.com/?q={}",
    "aw": "https://wiki.archlinux.org/?search={}",
    "dict": "https://www.dictionary.com/browse/{}",
    "yt": "https://www.youtube.com/results?search_query={}",
    "ub": "https://www.urbandictionary.com/define.php?term={}",
    "gh": "https://github.com/search?q={}&type=Code",
}
c.input.insert_mode.auto_load = True
c.input.insert_mode.auto_leave = False
c.tabs.background = True
c.auto_save.session = True

if "EDITOR" in os.environ:
    c.editor.command = [os.environ["EDITOR"] + ' "{}"']

# default theme:
theme = {
    "panel": {
        "height": 22,
    },
    "fonts": {
        "tabbar": "monospace",
        "completion": "monospace",
        "completion_size": 13,
        "tab_bold": False,
        "tab_size": 14,
        "status_size": 13,
    },
    "colors": {
        "bg": {
            "normal": "#f2e9e3",
            "weak": "#ddd4d3",
            "strong": "#d1c8cd",
            "focused": "#e0c4bf",
        },
        "fg": {
            "normal": "#544b45",
            "weak": "#473e3d",
            "strong": "#3f363b",
            "focused": "#544b45",
            "match": "#916156",  # completion and hints
        },
    },
}

qute_path = os.path.expanduser("~/.config/qutebrowser/ads")
ads_file = [
    f"file://{qute_path}/{f}"
    for f in os.listdir(qute_path)
    if f.endswith(".txt")
]


c.content.blocking.method = "both"
c.content.blocking.hosts.lists = ads_file
c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246"
c.content.headers.custom = {
    "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
}
c.content.tls.certificate_errors = "ask-block-thirdparty"
c.content.notifications.enabled = False
c.content.canvas_reading = False
c.content.autoplay = False
c.content.mute = True
c.content.pdfjs = True

c.downloads.location.directory = os.path.expanduser("~/Downloads")
c.spellcheck.languages = ["en-US", "hi-IN"]
c.aliases = {"q": "quit", "w": "session-save", "wq": "quit --save"}


c.fonts.default_family = "'FuraCode Nerd Font Retina'"
c.fonts.default_size = "12pt"
c.fonts.prompts = "default_size sans-serif"

c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"
c.colors.webpage.darkmode.algorithm = "lightness-hsl"
c.colors.webpage.bg = "#06182b"


# External user scripts
c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre",
]
d_sync = ";;".join(
    [
        "tab-close",
        "set messages.timeout 1",
        "session-save --force _autosave",
        "set messages.timeout 2000",
    ]
)
nmap("d", d_sync)

keymaps_normal = {
    "M": "hint links spawn mpv {hint-url}",
    "Z": "hint links spawn st -e youtube-dl {hint-url}",
    "t": "set-cmd-text -s :open -t",
    "xb": "config-cycle statusbar.show always never",
    "xt": "config-cycle tabs.show always never",
    "xx": "config-cycle statusbar.show always never;; config-cycle tabs.show always never",
    "J": "tab-prev",
    "K": "tab-next",
    "<F12>": "devtools",
    "<Alt-j>": "tab-move -",
    "<Alt-k>": "tab-move +",
    ",,": "hint links right-click",
    ",c": "hint code userscript code_select.py",
    ",f": "spawn --userscript format_json",
}

for key, cmd in keymaps_normal.items():
    config.bind(key, cmd, mode="normal")
