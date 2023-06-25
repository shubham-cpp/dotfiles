from os import getenv
from os.path import isfile

from libqtile.config import EzKey as Key
from libqtile.lazy import lazy

from .lazy_functions import (
    move_win_to_immediate_group,
    smart_window_kill,
    toggle_layout_max,
    update_brightness,
    update_volume,
)

mod = "mod4"
# terminal = guess_terminal()
terminal = getenv("TERMINAL", "xterm")
# if qtile.core.name == "wayland":
#     terminal = "foot"
browser = getenv("BROWSER", "firefox")

keys = [
    # Toggles {{{
    Key("M-s", lazy.window.toggle_floating(), desc="Toggle Floating"),
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
        "M-<period>",
        lazy.screen.next_group(skip_empty=True),
        desc="Move to next active group",
    ),
    Key(
        "M-<comma>",
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
    # }}}
    # Change focus among windows {{{
    Key("M-h", lazy.layout.left(), desc="Move focus to left"),
    Key("M-l", lazy.layout.right(), desc="Move focus to right"),
    Key("M-j", lazy.layout.down(), desc="Move focus down"),
    Key("M-k", lazy.layout.up(), desc="Move focus up"),
    Key("M-<Left>", lazy.layout.left(), desc="Move focus to left"),
    Key("M-<Right>", lazy.layout.right(), desc="Move focus to right"),
    Key("M-<Down>", lazy.layout.down(), desc="Move focus down"),
    Key("M-<Up>", lazy.layout.up(), desc="Move focus up"),
    Key(
        "A-<Tab>",
        lazy.group.next_window(),
        desc="Switch window focus to next window in group",
    ),
    # }}}
    # Move window {{{
    Key("M-S-h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key("M-S-l", lazy.layout.shuffle_right(), desc="Move window to the right"),
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
    # }}}
    # Applications {{{
    Key(
        "M-S-<Return>",
        lazy.spawn("kitty"),
        desc="Spawn terminal kitty",
        # lazy.layout.toggle_split(),
        # desc="Toggle between split and unsplit sides of stack",
    ),
    Key("M-<Return>", lazy.spawn(terminal), desc=f"Launch {terminal}"),
    Key("M-<KP_Enter>", lazy.spawn("alacritty"), desc="Launch alacritty"),
    Key("M-<KP_End>", lazy.spawn("xterm"), desc="Launch xterm"),
    Key("M-w", lazy.spawn(browser), desc=f"Launch {browser}"),
    Key(
        "M-S-w",
        lazy.spawn(
            "firefox"
            if browser != "firefox"
            else "brave"
            if isfile("/usr/bin/brave")
            else "brave-browser"
            if isfile("/usr/bin/brave-browser")
            else "chromium"
            if isfile("/usr/bin/chromium")
            else "chromium-browser"
            if isfile("/usr/bin/chromium-browser")
            else "flatpak run com.github.Eloston.UngoogledChromium"
        ),
        desc=f"Launch {browser}",
    ),
    Key(
        "M-e",
        lazy.spawn("bash -c 'thunar || pcmanfm'", shell=True),
        desc="Launch File Manager",
    ),
    Key("M-S-e", lazy.spawn("alacritty -e lfv"), desc="Launch lf"),
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
    # }}}
    # Brightness {{{
    Key(
        "<XF86AudioNext>",
        lazy.spawn("brightnessctl s 10+"),
        update_brightness(),
        desc="Inc Brightness",
    ),
    Key(
        "<XF86AudioPrev>",
        lazy.spawn("brightnessctl s 10-"),
        update_brightness(),
        desc="Dec Brightness",
    ),
    Key(
        "<XF86MonBrightnessUp>",
        lazy.spawn("brightnessctl s 10+"),
        update_brightness(),
        desc="Inc Brightness",
    ),
    Key(
        "<XF86MonBrightnessDown>",
        lazy.spawn("brightnessctl s 10-"),
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
    # Custom Scripts {{{
    Key("C-A-e", lazy.spawn("rofie"), desc="Launch Emoji Selector"),
    Key("C-A-v", lazy.spawn("pavucontrol"), desc="Launch Pavucontrol"),
    Key("C-A-c", lazy.spawn("xcolor -s"), desc="Launch Color Picker"),
    Key("C-A-p", lazy.spawn("get-class-name"), desc="Copy WM_CLASS name"),
    Key("<Print>", lazy.spawn("take_ss full"), desc="Take screenshot(FULL)"),
    Key("S-<Print>", lazy.spawn("take_ss focus"), desc="Take screenshot(FOCUS)"),
    Key("M-A-c", lazy.spawn("open-rcs"), desc="Open a config file"),
    Key("M-A-g", lazy.spawn("open-games"), desc="Launch game menu"),
    Key("M-C-s", lazy.spawn("logout_prompt"), desc="Launch logout Prompt"),
    Key("M-y", lazy.spawn("clipboard"), desc="Launch clipboard history"),
    # }}}
]
