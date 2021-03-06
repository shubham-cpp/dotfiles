# vim:fileencoding=utf-8:ft=python:foldmethod=marker:foldlevel=1
# Import {{{
import subprocess
from os import getenv
from os.path import isfile

from libqtile import bar, hook, layout, qtile, widget
from libqtile.command import lazy as lz
from libqtile.config import Click, Drag
from libqtile.config import EzKey as Key
from libqtile.config import Group, Match, Screen
from libqtile.lazy import lazy
from libqtile.log_utils import logger

# from shutil import which


# from time import time


# }}}

mod = "mod4"
terminal = ""

if qtile.core.name == "x11":
    terminal = getenv("TERMINAL", "xterm")
elif qtile.core.name == "wayland":

    terminal = "foot"
browser = getenv("BROWSER", "firefox")


# Custom Lazy Functions {{{
# @hook.subscribe.client_urgent_hint_changed
# def go_to(window):
#     logger.debug("Run urgent")
#     qtile.next_urgent()


@lz.function
def toggle_layout_max(qtile):
    """Toggle Max layout

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    lname = qtile.current_group.layout.name
    # indices = [i for i, _ in enumerate(qtile.current_group.layouts)]
    qtile.current_group.use_layout(index=1 if lname != "max" else 0)
    # logger.warn("Current Layout:: " + lname)


@lz.function
def move_win_to_immediate_group(qtile, prev=False):
    """Move to window to immediate next/prev group

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
        prev (bool): Whether to move to prev group
    """
    gname = ""
    if prev:
        gname = qtile.current_group.get_previous_group().name
    else:
        gname = qtile.current_group.get_next_group().name
    if qtile.current_window:
        qtile.current_window.togroup(gname, switch_group=True)
    else:
        logger.warn("No focused window")


@lz.function
def smart_window_kill(qtile):
    """Kill the window and move to last group if it was the last window

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    win_count = len(qtile.current_group.windows)
    qtile.current_window.kill()
    if win_count <= 1:
        qtile.current_screen.toggle_group()


@lz.function
def update_volume(qtile):
    """Update the volume widget on keypress

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    w = qtile.widgets_map["volume"]
    icon = qtile.widgets_map["volume_icon"]
    w.update(w.poll())
    icon.update(icon.poll())


@lz.function
def update_brightness(qtile):
    """Update the brightnesswidget on keypress

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    w = qtile.widgets_map["brightness"]
    icon = qtile.widgets_map["brightness_icon"]
    # widgets = ",".join(qtile.widgets_map)
    # logger.warn("widget names = " + widgets)
    # logger.warn("values of w  = " + ",".join(dir(w)))
    w.update(w.poll())
    icon.update(icon.poll())


# }}}

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
        "M-S-<Left>", lazy.layout.shuffle_left(), desc="Move window to the left"
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
            else "chromium"
        ),
        desc=f"Launch {browser}",
    ),
    Key("M-e", lazy.spawn("thunar"), desc="Launch File Manager"),
    Key("M-S-e", lazy.spawn("alacritty -e lfv"), desc="Launch lf"),
    Key("M-S-q", smart_window_kill(), desc="Kill focused window"),
    Key("M-C-r", lazy.reload_config(), desc="Reload the config"),
    Key("M-C-S-r", lazy.restart(), desc="Reload the config"),
    Key("M-C-x", lazy.shutdown(), desc="Shutdown Qtile"),
    Key(
        "M-d",
        lazy.spawn("dmenu_run_history -i"),
        desc="Spawn Run Prompt",
    ),
    Key(
        "M-S-d",
        lazy.spawn("rofi -show run -async-read 10"),
        desc="Spawn Run Prompt(Rofi)",
    ),
    Key(
        "M-r",
        lazy.spawn("rofi -show drun -async-read 10"),
        desc="Spawn Application Menu",
    ),
    Key("M-v", lazy.spawn("virt-manager"), desc="Launch Virt-manager"),
    Key("M-g", lazy.spawn("qalculate-gtk"), desc="Launch Calculator"),
    # }}}
    # Volume {{{
    Key(
        "<XF86AudioLowerVolume>",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%"),
        update_volume(),
        desc="Audio Lower",
    ),
    Key(
        "<XF86AudioRaiseVolume>",
        lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%"),
        update_volume(),
        desc="Audio Raise",
    ),
    Key(
        "<XF86AudioMute>",
        # lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle"),
        lazy.spawn("pamixer --toggle-mute"),
        update_volume(),
        desc="Audio Toggle Mute ",
    ),
    # }}}
    # Brightness {{{
    Key(
        "<XF86MonBrightnessUp>",
        lazy.spawn("xbacklight -inc 10"),
        update_brightness(),
        desc="Inc Brightness",
    ),
    Key(
        "<XF86MonBrightnessDown>",
        lazy.spawn("xbacklight -dec 10"),
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
    Key(
        "S-<Print>", lazy.spawn("take_ss focus"), desc="Take screenshot(FOCUS)"
    ),
    Key("M-A-c", lazy.spawn("open-rcs"), desc="Open a config file"),
    Key("M-A-g", lazy.spawn("open-games"), desc="Launch game menu"),
    Key("M-C-s", lazy.spawn("logout_prompt"), desc="Launch logout Prompt"),
    Key("M-y", lazy.spawn("clipboard"), desc="Launch clipboard history"),
    # }}}
]

# Groups {{{
groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            Key(
                f"M-{i.name}",
                lazy.group[i.name].toscreen(),
                desc=f"Switch to group {i.name}",
            ),
            Key(
                f"M-S-{i.name}",
                lazy.window.togroup(i.name, switch_group=True),
                desc=f"Switch to & move focused window to group {i.name}",
            ),
        ]
    )
# }}}

# Layout {{{
layout_theme = {
    "border_width": 3,
    "margin": 4,
    "border_focus": "#bd93f9",
    "border_normal": "#1D2330",
}
layouts = [
    layout.MonadTall(
        change_size=10,
        single_border_width=0,
        single_margin=0,
        new_client_position="top",
        **layout_theme,
    ),
    layout.Max(),
    layout.Floating(),
]
# }}}

# Bar {{{
widget_defaults = dict(
    font="Ubuntu Medium",
    fontsize=16,
    padding=3,
)
extension_defaults = widget_defaults.copy()

colors = [
    "#1E1F29",  # 0 darker purple
    "#bfbfbf",  # 1 white
    "#bd93f9",  # 2 lighter purple
    "#ff92d0",  # 3 pink
    "#8fae81",  # 4 soft green
    "#ff6e67",  # 5 red
    "#3A2F4D",  # 6 grey
    "#50fa7b",  # 7 hard green
    "#282a36",  # 8 middle purple
    "#ffae42",  # 9 orange yellow
    "#fff44f",  # 10 lemon yellow
]


def NerdIcon(icon="", fg=colors[0], bg=colors[1]):
    return widget.TextBox(
        fmt=icon,
        fontsize=20,
        font="FuraCode Nerd Font",
        background=bg,
        foreground=fg,
        margin=5,
        padding_x=10,
    )


def calIcon(level: int, icons: list[str]) -> str:
    if level >= 100:
        return icons[4]
    elif level > 80:
        return icons[3]
    elif level > 50:
        return icons[2]
    elif level > 20:
        return icons[1]
    else:
        return icons[0]


def getIcon(name: str) -> str:

    if name == "battery":
        with open("/sys/class/power_supply/BAT0/capacity", "r") as bat_file:
            bat_level = int(bat_file.read().strip())
            return calIcon(
                bat_level,
                [
                    "???",
                    "???",
                    "???",
                    "???",
                    "???",
                ],
            )
    elif name == "volume":
        vol = (
            subprocess.run(
                ["pamixer", "--get-volume-human"],
                stdout=subprocess.PIPE,
            )
            .stdout.decode("utf-8")
            .strip()[:2]
        )
        vol_level = 0 if vol == "mu" else int(vol)
        logger.info(f"Volume {vol_level}")
        return calIcon(vol_level, ["???", "???", "???", "???", "???"])
    elif name == "brightness":
        return calIcon(
            int(
                float(
                    subprocess.run(["light"], stdout=subprocess.PIPE)
                    .stdout.decode("utf-8")
                    .strip()
                )
            ),
            ["???", "???", "???", "???", "???"],
        )
    else:
        logger.warn("function getIcon expects an argument")
        return "Unknown"


def ArrowSep(icon="???"):
    # return widget.TextBox(
    #     text=icon,
    #     font="FuraCode Nerd Font",
    #     fontsize=34,
    # )
    return widget.Spacer(14)


screens = [
    Screen(
        top=bar.Bar(
            [
                # widget.TextBox(
                #     fmt="???",
                #     fontsize=16,
                #     font="Iosevka Nerd Font",
                #     padding=8,
                #     mouse_callbacks={
                #         "Button1": lambda: qtile.cmd_spawn("rofi -show drun")
                #     },
                # ),
                widget.GroupBox(
                    fontsize=18,
                    margin_y=3,
                    margin_x=2,
                    padding_y=5,
                    padding_x=3,
                    borderwidth=3,
                    rounded=False,
                    highlight_method="line",
                    hide_unused=True,
                    this_current_screen_border=colors[4],
                    active=colors[3],
                    inactive=colors[2],
                ),
                widget.Prompt(),
                widget.TaskList(
                    txt_floating="???? ",
                    txt_minimized="???? ",
                    txt_maximized="???? ",
                    highlight_method="block",
                    margin=0,
                    borderwidth=3,
                    urgent_alert_method="text",
                    urgent_text="#834a61",
                ),
                NerdIcon("???", bg=colors[3]),
                widget.Clock(
                    format="%a %d, %I:%M %p",
                    update_interval=60,
                    foreground=colors[0],
                    background="#eeeeee",
                ),
                ArrowSep(),
                NerdIcon("???", bg=colors[2]),
                widget.CPU(
                    format="{load_percent}%",
                    update_interval=2,
                    foreground=colors[0],
                    background="#eeeeee",
                ),
                ArrowSep(),
                NerdIcon("???", bg=colors[9]),
                widget.Memory(
                    format="{MemUsed:.0f}{mm}",
                    update_interval=2,
                    foreground=colors[0],
                    background="#eeeeee",
                ),
                ArrowSep(),
                widget.GenPollText(
                    name="brightness_icon",
                    func=lambda: getIcon("brightness"),
                    font="FuraCode Nerd Font",
                    fontsize=22,
                    foreground=colors[0],
                    background=colors[10],
                    update_interval=60,
                    padding=5,
                ),
                widget.GenPollText(
                    name="brightness",
                    func=lambda: subprocess.run(
                        ["xbacklight"],
                        stdout=subprocess.PIPE,
                    )
                    .stdout.decode("utf-8")
                    .strip()[:2],
                    padding=5,
                    foreground=colors[0],
                    background="#eeeeee",
                ),
                ArrowSep(),
                widget.GenPollText(
                    name="volume_icon",
                    func=lambda: getIcon("volume"),
                    font="FuraCode Nerd Font",
                    fontsize=22,
                    foreground=colors[0],
                    background=colors[5],
                    update_interval=60,
                    padding=5,
                ),
                widget.GenPollText(
                    name="volume",
                    func=lambda: subprocess.run(
                        ["pamixer", "--get-volume-human"],
                        stdout=subprocess.PIPE,
                    )
                    .stdout.decode("utf-8")
                    .strip(),
                    padding=5,
                    foreground=colors[0],
                    background="#eeeeee",
                    update_interval=10,
                ),
                ArrowSep(),
                widget.GenPollText(
                    name="battery_icon",
                    func=lambda: getIcon("battery"),
                    font="FuraCode Nerd Font",
                    fontsize=18,
                    foreground=colors[6],
                    background="#86EFAC",
                    update_interval=60,
                    padding=5,
                ),
                widget.Battery(
                    battery="BAT0",
                    unknown_char="???",
                    discharge_char="???",
                    empty_char="???",
                    charge_char="???",
                    format="{char} {percent:2.0%}",
                    # format="{percent:2.0%} {char}",
                    low_foreground=colors[5],
                    notify_below=15,
                    # font="Iosevka Nerd Font",
                    fontsize=18,
                    padding=5,
                    foreground=colors[0],
                    background="#eeeeee",
                ),
                ArrowSep(),
                widget.Net(format="??? {up}  ??? {down}"),
                ArrowSep(),
                widget.Wttr(location={"Pune": "Home"}, fontsize=15),
                ArrowSep(),
                widget.CurrentLayoutIcon(scale=0.7),
                widget.Systray(),
            ],
            26,
            background="#1a1826",
            opacity=0.6,
        ),
    ),
]
# }}}

# Drag floating layouts. {{{
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]
# }}}

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wl_input_rules = None
wmname = "LG3D"

# Assign app layout/group {{{
floating_layout = layout.Floating(
    border_focus=colors[2],
    border_normal=colors[6],
    border_width=3,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="Pavucontrol"),
        Match(wm_class="Gnome-calculator"),
        Match(wm_class="Qalculate-gtk"),
        Match(wm_class="Galculator"),
        Match(wm_class="Nitrogen"),
        Match(wm_class="Oblogout"),
        Match(wm_class="NoiseTorch"),
        Match(wm_class="matplotlib"),
        Match(wm_class="Arandr"),
        Match(wm_class="Gnome-disks"),
        Match(wm_class="VirtualBox Manager"),
        Match(wm_class="Virt-manager"),
    ],
)


@hook.subscribe.client_new
def assign_app_group(client):
    wm_class = client.window.get_wm_class()[0]
    if wm_class in [
        "Navigator",
        "firefox",
        "brave-browser",
        "Brave-browser",
        "qutebrowser",
        "LibreWolf",
        "Chromium",
        "Chromium-browser",
        "chromium-browser",
        "vieb",
    ]:
        client.togroup(groups[1].name, switch_group=True)
    elif wm_class in ["Vlc", "vlc", "Mpv", "mpv", "gl"]:
        client.togroup(groups[3].name, switch_group=True)
    elif wm_class in [
        "VirtualBox Manager",
        "VirtualBox Machine",
        "Vmplayer",
        "virtualbox manager",
        "virtualbox machine",
        "vmplayer",
    ]:
        client.togroup(groups[5].name)
    elif wm_class in ["Steam", "Lutris"]:
        client.togroup(groups[2].name, switch_group=True)


# }}}
