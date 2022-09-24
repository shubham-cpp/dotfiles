# vim:foldmethod=marker
# Imports {{{
from os import getenv
from typing import List  # noqa: F401

from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag
from libqtile.config import EzKey as Key
from libqtile.config import Group, Match, Screen
from libqtile.lazy import lazy
from libqtile.widget import backlight

# }}}

mod = "mod4"
terminal = getenv("TERMINAL", "st")
browser = getenv("BROWSER", "firefox")

keys = [
    # Change focus among windows {{{
    Key("M-h", lazy.layout.left(), desc="Move focus to left"),
    Key("M-l", lazy.layout.right(), desc="Move focus to right"),
    Key("M-j", lazy.layout.down(), desc="Move focus down"),
    Key("M-k", lazy.layout.up(), desc="Move focus up"),
    Key("M-<Left>", lazy.layout.left(), desc="Move focus to left"),
    Key("M-<Right>", lazy.layout.right(), desc="Move focus to right"),
    Key("M-<Down>", lazy.layout.down(), desc="Move focus down"),
    Key("M-<Up>", lazy.layout.up(), desc="Move focus up"),
    # }}}
    # Move window {{{
    Key("M-S-h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key("M-S-l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key("M-S-j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key("M-S-k", lazy.layout.shuffle_up(), desc="Move window up"),
    Key("M-S-<Left>",
        lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key("M-S-<Right>",
        lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key("M-S-<Down>", lazy.layout.shuffle_down(), desc="Move window down"),
    Key("M-S-<Up>", lazy.layout.shuffle_up(), desc="Move window up"),
    # }}}
    # Resize windows {{{
    Key("M-A-h",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        desc="Grow window to the left"),
    Key("M-A-l",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        desc="Grow window to the right"),
    Key("M-A-j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        desc="Grow window down"),
    Key("M-A-k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        desc="Grow window up"),
    Key("M-A-<Left>",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        desc="Grow window to the left"),
    Key("M-A-<Right>",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        desc="Grow window to the right"),
    Key("M-A-<Down>",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        desc="Grow window down"),
    Key("M-A-<Up>",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        desc="Grow window up"),
    # }}}
    # Moving between workspaces(groups) {{{
    Key("M-<grave>",
        lazy.screen.next_group(skip_empty=True),
        desc="Move to next non-empty group"),
    Key("M-<period>",
        lazy.screen.next_group(skip_empty=True),
        desc="Move to next non-empty group"),
    Key("M-<comma>",
        lazy.screen.prev_group(skip_empty=True),
        desc="Move to prev non-empty group"),
    Key("M-<bracketleft>",
        lazy.screen.prev_group(),
        desc="Move to the group on the left"),
    Key("M-<bracketright>",
        lazy.screen.next_group(),
        desc="Move to the group on the right"),
    # }}}
    # Qtile specific {{{
    Key("M-<Tab>",
        lazy.screen.toggle_group(),
        desc="Move to the last visited group"),
    # Key(
    #     "A-<Tab>",
    #     lazy.group.next_window(),
    #     lazy.window.bring_to_front(),
    #     desc="Switch window focus to next window in group"
    # ),
    Key("A-<Tab>",
        lazy.group.next_window(),
        desc="Switch window focus to next window in group"),
    Key("M-q", lazy.window.kill(), desc="Kill focused window"),
    Key("M-f", lazy.window.toggle_fullscreen(), desc="Toggle Fullscreen"),
    Key("M-s", lazy.window.toggle_floating(), desc="Toggle Floating"),
    Key("M-m", lazy.window.toggle_maximize(), desc="Toggle Maximize"),
    Key("M-C-r", lazy.restart(), desc="Restart Qtile"),
    Key("M-<Escape>", lazy.restart(), desc="Restart Qtile"),
    Key("M-C-x", lazy.shutdown(), desc="Shutdown Qtile"),
    Key("M-<space>", lazy.next_layout(), desc="Change to next Layout"),
    # }}}
    # Launch Programs {{{
    # Terminal {{{
    Key("M-<Return>", lazy.spawn(terminal), desc="Launch terminal"),
    Key("M-S-<Return>", lazy.spawn("kitty"), desc="Launch kitty"),
    Key("M-<KP_End>", lazy.spawn("xterm"), desc="Launch terminal"),
    Key("M-<KP_Enter>", lazy.spawn("alacritty"), desc="Launch terminal"),
    # }}}
    # Gui programs {{{
    Key("M-w", lazy.spawn(browser), desc="Launch Brave Browser"),
    Key("M-S-w", lazy.spawn("firefox"), desc="Launch Firefox Browser"),
    Key("M-e", lazy.spawn("thunar"), desc="Launch Brave Browser"),
    Key("M-S-e", lazy.spawn("xterm -e vifmrun"),
        desc="Launch Firefox Browser"),
    Key("M-g", lazy.spawn("qalculate-gtk"), desc="Launch Calculator"),
    Key("M-v", lazy.spawn("virtualbox"), desc="Launch Calculator"),
    Key("C-A-v", lazy.spawn("pavucontrol"), desc="Launch Pavucontrol"),
    # }}}
    # Launcher {{{
    Key("M-r", lazy.spawn("rofi -show drun"), desc="Launch Rofi drun"),
    Key("M-d", lazy.spawn("dmenu_run_history -i"), desc="Spawn dmenu"),
    # }}}
    # Custom Scripts {{{
    Key("M-A-c",
        lazy.spawn("open-rcs"),
        desc="Script to open configs using dmenu"),
    Key("M-A-g",
        lazy.spawn("open-games"),
        desc="Script to open games using dmenu"),
    Key("C-A-p",
        lazy.spawn("get-class-name"),
        desc="Copy to wm_class name to clipboard"),
    Key("C-A-c", lazy.spawn("xcolor -s"), desc="Color picker"),
    Key("C-A-e", lazy.spawn("rofie"), desc="Emoji picker"),
    Key("M-y", lazy.spawn("clipboard"), desc="clipboard history selector"),
    Key("M-p",
        lazy.spawn("superp"),
        desc="Select & Launch files form Home directory"),
    Key("M-C-s",
        lazy.spawn("prompt 'Wanna Shutdown?' 'poweroff'"),
        desc="Select & Launch files form Home directory"),

    # }}}
    # Screenshot {{{
    Key("<Print>",
        lazy.spawn("take_ss full"),
        desc="Take screenshot of entire desktop"),
    Key("S-<Print>",
        lazy.spawn("take_ss focus"),
        desc="Take screen of focused window"),
    # }}}
    # Sound {{{
    # "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    # pactl set-sink-mute @DEFAULT_SINK@ toggle
    Key("<XF86AudioMute>", lazy.spawn("pamixer --toggle-mute")),
    Key("<XF86AudioRaiseVolume>", lazy.spawn("pamixer -i 10 --allow-boost")),
    Key("<XF86AudioLowerVolume>", lazy.spawn("pamixer -d 10 --allow-boost")),
    # }}}
    # Brightness {{{
    Key(
        "<XF86MonBrightnessUp>", lazy.widget['backlight'].change_backlight(
            backlight.ChangeDirection.UP)),
    Key(
        "<XF86MonBrightnessDown>", lazy.widget['backlight'].change_backlight(
            backlight.ChangeDirection.DOWN)),
    # }}}
    # }}}
]

# Groups(Workspaces) {{{
groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        Key("M-" + i.name,
            lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),
        Key("M-S-" + i.name,
            lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
    ])
# }}}

# layouts {{{

layout_theme = {
    "border_width": 2,
    "margin": 4,
    "border_focus": "#bd93f9",
    "border_normal": "#1D2330"
}
layouts = [
    layout.MonadTall(change_size=10,
                     single_border_width=0,
                     single_margin=0,
                     new_client_position='top',
                     **layout_theme),
    layout.Max(),
    layout.Floating(**layout_theme),
    # Try more layouts by unleashing below layouts.
    # layout.Columns(),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]
# }}}

# Bar {{{

# Colors {{{
colors = [
    "#282828",  # 0
    "#CC241D",  # 1
    "#98971A",  # 2
    "#D79921",  # 3
    "#458588",  # 4
    "#B16286",  # 5
    "#689D6A",  # 6
    "#A89984",  # 7
    "#928374",  # 8
    "#edc242",  # 9
    "#B8BB26",  # 10
    "#FABD2F",  # 11
    "#83A598",  # 12
    "#D3869B",  # 13
    "#8EC07C",  # 14
    "#EBDBB2",  # 15
]
font_sizes = {"fontsize": 22, "padding": 0}
sep_char = ''
# sep_char = ''
widget_defaults = dict(font='Ubuntu',
                       fontsize=12,
                       padding=3,
                       foreground=colors[0],
                       background="#0f1f1f")
extension_defaults = widget_defaults.copy()

# }}}

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(foreground=colors[15], hide_unused=True),
                # widget.Prompt(),
                widget.TaskList(foreground=colors[15]),
                widget.TextBox(sep_char, foreground=colors[9], **font_sizes),
                widget.TextBox("", fontsize=22, background=colors[9]),
                widget.Clock(
                    format='%a %d %b at %I:%M %p',
                    background=colors[9],
                    # background="#f04f52",
                    update_interval=60),
                widget.TextBox(sep_char,
                               background=colors[9],
                               foreground=colors[13],
                               **font_sizes),
                widget.TextBox(
                    "墳",
                    fontsize=22,
                    background=colors[13],
                ),
                widget.Volume(update_interval=1,
                              background=colors[13],
                              volume_app="pavucontrol"),
                widget.TextBox(sep_char,
                               background=colors[13],
                               foreground=colors[11],
                               **font_sizes),
                widget.TextBox("ﯦ", background=colors[11], fontsize=26),
                widget.Backlight(backlight_name='radeon_bl0',
                                 update_interval=1.2,
                                 background=colors[11],
                                 change_command='brightnessctl s {0}'),
                widget.TextBox(sep_char,
                               background=colors[11],
                               foreground=colors[10],
                               **font_sizes),
                widget.Battery(format="{char} {percent:2.0%}",
                               fontsize=15,
                               full_char="",
                               charge_char="",
                               discharge_char="",
                               empty_char="",
                               background=colors[10],
                               notify_below=10),
                widget.TextBox(sep_char,
                               background=colors[10],
                               foreground=colors[14],
                               **font_sizes),
                widget.TextBox(
                    "",
                    fontsize=22,
                    background=colors[14],
                    # foreground="#7aa2f7"
                ),
                widget.Memory(
                    format='{MemUsed:.0f}MB',
                    background=colors[14],
                    update_interval=3,
                    mouse_callbacks={
                        'Button1':
                        lambda: qtile.cmd_spawn(terminal + ' -e bpytop')
                    },
                ),
                widget.TextBox(sep_char,
                               background=colors[14],
                               foreground=colors[15],
                               **font_sizes),
                widget.TextBox("", background=colors[15], fontsize=22),
                widget.CPU(
                    format='{load_percent}%',
                    update_interval=3,
                    background=colors[15],
                    mouse_callbacks={
                        'Button1':
                        lambda: qtile.cmd_spawn(terminal + ' -e htop')
                    },
                ),
                widget.TextBox(sep_char,
                               foreground=colors[12],
                               background=colors[15],
                               **font_sizes),
                widget.TextBox("", fontsize=16, background=colors[12]),
                widget.Wlan(disconnected_message="❌",
                            interface="wlp2s0",
                            background=colors[12],
                            format="{essid} {percent:2.0%}",
                            update_interval=3),
                widget.TextBox(sep_char,
                               background=colors[12],
                               foreground=colors[4],
                               **font_sizes),
                widget.Net(update_interval=3,
                           background=colors[4],
                           format="{down} ⬇⬆ {up}"),
                widget.Sep(),
                widget.CurrentLayoutIcon(scale=0.6),
                widget.Systray(),
            ],
            22,
        ), ),
]
# }}}

# Drag floating layouts {{{
mouse = [
    Drag([mod],
         "Button1",
         lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod],
         "Button3",
         lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]
# }}}

# Qtile defaults {{{
dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wmname = "LG3D"
# }}}

# Floating Rules{{{
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
    Match(wm_class='Pavucontrol'),
    Match(wm_class='Gnome-calculator'),
    Match(wm_class='Qalculate-gtk'),
    Match(wm_class='Galculator'),
    Match(wm_class='Nitrogen'),
    Match(wm_class='Oblogout'),
    Match(wm_class='NoiseTorch'),
    Match(wm_class='matplotlib'),
    Match(wm_class='Arandr'),
    Match(wm_class='Gnome-disks'),
    Match(wm_class='VirtualBox Manager'),
    Match(wm_class='Virt-manager'),
])
# }}}

# Assign Windows to specific workspace {{{


@hook.subscribe.client_new
def assign_app_group(client):
    d = {}
    d[groups[1].name] = [
        "Navigator",  "firefox",  "brave-browser", "Brave-browser",
        "qutebrowser", "LibreWolf", "Chromium", "vieb"
    ]
    d[groups[2].name] = ["Steam", "Lutris"]
    d[groups[3].name] = ["Vlc", "vlc", "Mpv", "mpv"]
    d[groups[5].name] = [
        "VirtualBox Manager",
        "VirtualBox Machine",
        "Vmplayer",
        "virtualbox manager",
        "virtualbox machine",
        "vmplayer",
    ]

    wm_class = client.window.get_wm_class()[0]

    for i in range(len(d)):
        if wm_class in list(d.values())[i]:
            group = list(d.keys())[i]
            client.togroup(group)
            client.group.cmd_toscreen(toggle=False)
# }}}
