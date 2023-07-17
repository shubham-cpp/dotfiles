from typing import List, Union

from libqtile import hook, layout
from libqtile.config import Click, Drag, Match, Rule
from libqtile.layout.base import Layout
from libqtile.layout.floating import Floating
from libqtile.layout.max import Max
from libqtile.layout.spiral import Spiral
from libqtile.layout.xmonad import MonadTall, MonadWide
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from modules.bar import extension_defaults, layout_theme, screens, widget_defaults
from modules.colors import backgroundColor, colors, foregroundColor
from modules.groups import groups
from modules.keys import browser, keys, mod, terminal
from modules.lazy_functions import sticky_windows
from libqtile.log_utils import logger

layouts: List[Layout] = [
    MonadTall(
        change_size=10,
        single_border_width=0,
        single_margin=0,
        new_client_position="top",
        **layout_theme,
    ),
    MonadWide(change_size=10, single_border_width=0, single_margin=0, **layout_theme),
    Max(**layout_theme),
    Floating(),
    Spiral(**layout_theme),
]


# Drag floating layouts.
mouse: List[Union[Drag, Click]] = [
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

dgroups_key_binder = None
dgroups_app_rules = [
    # Rule(Match(wm_class="firefox"), group="2"),
    # Rule(Match(wm_class="Brave-browser"), group=groups[1].name),
]  # type: list
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
floating_layout = Floating(
    border_normal=layout_theme["border_normal"],
    border_focus=layout_theme["border_focus"],
    border_width=layout_theme["border_width"],
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *Floating.default_float_rules,
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
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None
wmname = "LG3D"


@hook.subscribe.client_new
def assign_app_group(client):
    wm_class: str = client.window.get_wm_class()[0]
    if wm_class in [
        "Navigator",
        "firefox",
        "brave-browser",
        "Brave-browser",
        "qutebrowser",
        "LibreWolf",
        "Chromium",
        "chromium",
        "Chromium-browser",
        "chromium-browser",
        "brave-browser",
        "Brave-browser",
        "vieb",
    ]:
        client.togroup("2", switch_group=True)
    elif wm_class in [
        "Vlc",
        "vlc",
        "Mpv",
        "mpv",
        "gl",
        "Celluloid",
        "io.github.celluloid_player.Celluloid",
        "Io.github.celluloid_player.Celluloid",
    ]:
        client.togroup("4", switch_group=True)
    elif wm_class in [
        "VirtualBox Manager",
        "VirtualBox Machine",
        "Vmplayer",
        "virtualbox manager",
        "virtualbox machine",
        "vmplayer",
    ]:
        client.togroup("5", switch_group=True)
    elif wm_class in ["Steam", "Lutris", "lutris", "steam"]:
        client.togroup("3", switch_group=True)


@hook.subscribe.setgroup
def move_sticky_windows():
    for window in sticky_windows:
        window.togroup()
    return


@hook.subscribe.client_killed
def remove_sticky_windows(window):
    if window in sticky_windows:
        sticky_windows.remove(window)
