from os import getenv
from os.path import isfile
from typing import LiteralString

from libqtile.config import EzKey as Key
from libqtile.lazy import lazy

from .lazy_functions import (
    move_win_to_immediate_group,
    smart_window_kill,
    toggle_layout,
    toggle_layout_max,
    toggle_sticky_windows,
    update_brightness,
    update_mic_icon,
    update_volume,
)

mod: LiteralString = "mod4"
# terminal = guess_terminal()
terminal = getenv("TERMINAL", "xterm")
# if qtile.core.name == "wayland":
#     terminal = "foot"
browser = getenv("BROWSER", "firefox")

keys = [
    # Toggles {{{
    Key("M-s", lazy.window.toggle_floating(), desc="Toggle Floating"),
    Key("M-S-s", toggle_sticky_windows(), desc="Toggle Floating"),
    Key("M-f", lazy.window.toggle_fullscreen(), desc="Toggle Fullscreen"),
    Key("M-m", toggle_layout_max(), desc="Toggle Maximize"),
    Key("M-b", lazy.hide_show_bar(), desc="Toggle Bar"),
    Key("M-<Tab>", lazy.screen.toggle_group(), desc="View Last Group"),
    Key("M-<Home>", smart_window_kill(), desc="Smartly kill windows"),
    # }}}
    # Moving Among groups {{{
    Key(
        "M-<grave>",
        lazy.screen.next_group(skip_empty=True),
        desc="Move to next active group",
    ),
    Key(
        "M-<apostrophe>",
        lazy.screen.next_group(skip_empty=True),
        desc="Move to next active group",
    ),
    Key(
        "M-<semicolon>",
        lazy.screen.prev_group(skip_empty=True),
        desc="Move to prev active group",
    ),
    Key(
        "M-<bracketleft>",
        lazy.screen.prev_group(),
        desc="Focus to the prev group",
    ),
    Key(
        "M-S-<bracketleft>",
        move_win_to_immediate_group(prev=True),
        desc="Move window to the prev group",
    ),
    Key(
        "M-<bracketright>",
        lazy.screen.next_group(),
        desc="Focus to the prev group",
    ),
    Key(
        "M-S-<bracketright>",
        move_win_to_immediate_group(),
        desc="Move window to the prev group",
    ),
    # }}}
    # Layouts {{{
    Key(
        "M-<space>",
        lazy.next_layout(),
        desc="Move window focus to other window",
    ),
    Key("M-n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key("M-S-m", toggle_layout("monadwide"), desc="Toggle Monad Wide"),
    Key("M-z", toggle_layout("zoomy"), desc="Toggle Zoomy"),
    # }}}
    # Change focus among windows {{{
    Key(
        "M-h",
        lazy.layout.left(),
        desc="Move focus to left",
    ),
    Key(
        "M-l",
        lazy.layout.right(),
        desc="Move focus to right",
    ),
    Key(
        "M-j",
        # lazy.group.next_window(),
        lazy.layout.next(),
        desc="Move focus down",
    ),
    Key(
        "M-k",
        # lazy.group.prev_window(),
        lazy.layout.previous(),
        desc="Move focus up",
    ),
    Key("M-<Left>", lazy.layout.left(), desc="Move focus to left"),
    Key("M-<Right>", lazy.layout.right(), desc="Move focus to right"),
    # Key("M-<Down>", lazy.layout.down(), desc="Move focus down"),
    # Key("M-<Up>", lazy.layout.up(), desc="Move focus up"),
    Key(
        "M-<Down>",
        # lazy.group.next_window(),
        lazy.layout.next(),
        desc="Move focus down",
    ),
    Key(
        "M-<Up>",
        # lazy.group.prev_window(),
        lazy.layout.previous(),
        desc="Move focus up",
    ),
    Key(
        "A-<Tab>",
        # lazy.group.next_window(),
        lazy.layout.next(),
        desc="Switch window focus to next window in group",
    ),
    # }}}
    # Move window {{{
    Key("M-S-h", lazy.layout.swap_left(), desc="Move window to the left"),
    Key("M-S-l", lazy.layout.swap_right(), desc="Move window to the right"),
    Key("M-S-j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key("M-S-k", lazy.layout.shuffle_up(), desc="Move window up"),
    Key(
        "M-S-<Left>",
        lazy.layout.shuffle_left(),
        desc="Move window to the left",
    ),
    Key(
        "M-S-<Right>",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key("M-S-<Down>", lazy.layout.shuffle_down(), desc="Move window down"),
    Key("M-S-<Up>", lazy.layout.shuffle_up(), desc="Move window up"),
    # }}}
    # Resize Windows {{{
    Key(
        "M-A-h",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        desc="Grow window to the left",
    ),
    Key(
        "M-A-l",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        desc="Grow window to the right",
    ),
    Key(
        "M-A-j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        desc="Grow window down",
    ),
    Key(
        "M-A-k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        desc="Grow window up",
    ),
    Key(
        "M-A-<Left>",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        desc="Grow window to the left",
    ),
    Key(
        "M-A-<Right>",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        desc="Grow window to the right",
    ),
    Key(
        "M-A-<Down>",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        desc="Grow window down",
    ),
    Key(
        "M-A-<Up>",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        desc="Grow window up",
    ),
    Key(
        "M-C-h",
        lazy.layout.grow_left(),
        lazy.layout.grow(),
        desc="Grow window to the left",
    ),
    Key(
        "M-C-l",
        lazy.layout.grow_right(),
        lazy.layout.shrink(),
        desc="Grow window to the right",
    ),
    Key(
        "M-C-j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        desc="Grow window down",
    ),
    Key(
        "M-C-k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        desc="Grow window up",
    ),
    # }}}
    # Applications {{{
    Key(
        "M-S-<Return>",
        lazy.spawn("bash -c 'wezterm || kitty'", shell=True),
        desc="Spawn terminal kitty",
        # lazy.layout.toggle_split(),
        # desc="Toggle between split and unsplit sides of stack",
    ),
    Key("M-<Return>", lazy.spawn(terminal), desc=f"Launch {terminal}"),
    Key("M-<KP_Enter>", lazy.spawn("alacritty"), desc="Launch alacritty"),
    Key("M-<KP_End>", lazy.spawn("xterm"), desc="Launch xterm"),
    Key(
        "M-w",
        lazy.spawn(browser),
        desc=f"Launch {browser}",
    ),
    Key(
        "M-S-w",
        # lazy.spawn("thorium-browser"),
        lazy.spawn("bash -c 'brave || brave-browser'", shell=True),
        # lazy.spawn("flatpak run com.github.Eloston.UngoogledChromium"),
        # lazy.spawn(
        #     "firefox"
        #     if browser != "firefox" and isfile("/usr/bin/firefox")
        #     else "brave"
        #     if isfile("/usr/bin/brave") and browser != "brave"
        #     else "brave-browser"
        #     if isfile("/usr/bin/brave-browser") and browser != "brave-browser"
        #     else "chromium"
        #     if isfile("/usr/bin/chromium")
        #     else "chromium-browser"
        #     if isfile("/usr/bin/chromium-browser")
        #     else "flatpak run com.github.Eloston.UngoogledChromium"
        # ),
        desc=f"Launch {browser}",
    ),
    Key(
        "M-e",
        lazy.spawn("bash -c 'thunar || pcmanfm'", shell=True),
        desc="Launch File Manager",
    ),
    Key("M-S-e", lazy.spawn("kitty -e lfv", shell=True), desc="Launch lf"),
    Key("M-S-q", smart_window_kill(), desc="Kill focused window"),
    Key("M-C-r", lazy.reload_config(), desc="Reload the config"),
    Key("M-C-S-r", lazy.restart(), desc="Reload the config"),
    Key("M-C-x", lazy.shutdown(), desc="Shutdown Qtile"),
    Key(
        "M-S-d",
        lazy.spawn("bash -c 'dmenu_run_history -i || dmenu_run -i'", shell=True),
        desc="Spawn Run Prompt",
    ),
    Key(
        "M-d",
        lazy.spawn("rofi -show run -async-read 10 -config ~/.config/rofi/dmenu.rasi"),
        desc="Spawn Run Prompt(Rofi)",
    ),
    Key(
        "M-r",
        lazy.spawn("rofi -show drun -async-read 10"),
        desc="Spawn Application Menu",
    ),
    Key(
        "<XF86Search>",
        lazy.spawn("rofi -show drun -async-read 10"),
        desc="Spawn Application Menu",
    ),
    Key("M-v", lazy.spawn("virt-manager"), desc="Launch Virt-manager"),
    Key(
        "M-g",
        lazy.spawn(
            "bash -c 'qalculate-gtk || gnome-calculator || galculator'", shell=True
        ),
        desc="Launch Calculator",
    ),
    Key(
        "<XF86Calculator>",
        lazy.spawn(
            "qalculate-gtk"
            if isfile("/usr/bin/qalculate-gtk")
            else "gnome-calculator"
            if isfile("/usr/bin/gnome-calculator")
            else "galculator"
        ),
        desc="Launch Calculator",
    ),
    # }}}
    # Volume {{{
    Key(
        "<XF86AudioLowerVolume>",
        lazy.spawn("audio dec"),
        update_volume(),
        desc="Audio Lower",
    ),
    Key(
        "<XF86AudioRaiseVolume>",
        lazy.spawn("audio inc"),
        update_volume(),
        desc="Audio Raise",
    ),
    Key(
        "<XF86AudioMute>",
        lazy.spawn("audio toggle"),
        update_volume(),
        desc="Audio Toggle Mute",
    ),
    Key(
        "S-<XF86AudioMute>",
        lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),
        update_mic_icon(),
        desc="Audio Toggle Mute",
    ),
    # }}}
    # Brightness {{{
    Key(
        "<XF86AudioNext>",
        lazy.spawn(
            'brightnessctl s 10+\
        && notify-send "Brightness: " -t 2000 -i display-brightness\
        -h int:value:$(brightnessctl g)\
        -h string:x-canonical-private-synchronous:backlight',
            shell=True,
        ),
        update_brightness(),
        desc="Inc Brightness",
    ),
    Key(
        "<XF86AudioPrev>",
        lazy.spawn(
            'brightnessctl s 10-\
        && notify-send "Brightness: " -t 2000 -i display-brightness\
        -h int:value:$(brightnessctl g)\
        -h string:x-canonical-private-synchronous:backlight',
            shell=True,
        ),
        update_brightness(),
        desc="Dec Brightness",
    ),
    Key(
        "<XF86MonBrightnessUp>",
        lazy.spawn(
            'brightnessctl s 10+\
        && notify-send "Brightness: " -t 2000 -i display-brightness\
        -h int:value:$(brightnessctl g)\
        -h string:x-canonical-private-synchronous:backlight',
            shell=True,
        ),
        update_brightness(),
        desc="Inc Brightness",
    ),
    Key(
        "<XF86MonBrightnessDown>",
        lazy.spawn(
            'brightnessctl s 10-\
        && notify-send "Brightness: " -t 2000 -i display-brightness\
        -h int:value:$(brightnessctl g)\
        -h string:x-canonical-private-synchronous:backlight',
            shell=True,
        ),
        update_brightness(),
        desc="Dec Brightness",
    ),
    Key(
        "S-<XF86MonBrightnessUp>",
        lazy.spawn("brightnessctl s 150"),
        update_brightness(),
        desc="Inc Brightness",
    ),
    Key(
        "S-<XF86MonBrightnessDown>",
        lazy.spawn("brightnessctl s 10"),
        update_brightness(),
        desc="Dec Brightness",
    ),
    # }}}
    Key("M-<period>", lazy.next_screen(), desc="Move to next screen"),
    Key("M-<comma>", lazy.prev_screen(), desc="Move to prev screen"),
    # Key("M-S-.", lazy.next_screen(), desc="Move to next screen"),
    # Key("M-S-,", lazy.prev_screen(), desc="Move to prev screen"),
    # Custom Scripts {{{
    Key("C-A-e", lazy.spawn("rofie"), desc="Launch Emoji Selector"),
    Key("C-A-v", lazy.spawn("pavucontrol"), desc="Launch Pavucontrol"),
    Key("C-A-c", lazy.spawn("xcolor -s"), desc="Launch Color Picker"),
    Key("C-A-p", lazy.spawn("get-class-name"), desc="Copy WM_CLASS name"),
    Key("<Print>", lazy.spawn("flameshot gui"), desc="Take screenshot(FULL)"),
    Key("S-<Print>", lazy.spawn("flameshot full"), desc="Take screenshot(FOCUS)"),
    Key("<F1>", lazy.spawn("flameshot gui"), desc="Take screenshot(FOCUS)"),
    Key("M-A-c", lazy.spawn("open-rcs"), desc="Open a config file"),
    Key("M-A-g", lazy.spawn("open-games"), desc="Launch game menu"),
    Key("M-C-s", lazy.spawn("logout_prompt"), desc="Launch logout Prompt"),
    Key("M-C-m", lazy.spawn("load_monitors"), desc="Launch monitor script"),
    Key("M-y", lazy.spawn("clipboard"), desc="Launch clipboard history"),
    # }}}
]
