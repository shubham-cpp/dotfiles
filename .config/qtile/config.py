import os
import subprocess
from typing import TYPE_CHECKING, List, Union

from extras.floating_window_snapping import move_snap_window
from libqtile import hook, layout, qtile
from libqtile.backend.base.window import WindowType
from libqtile.backend.wayland import InputConfig
from libqtile.config import Click, Drag, DropDown, EzClick, EzDrag, EzKey, Match, Rule, ScratchPad
from libqtile.layout.base import Layout
from libqtile.layout.floating import Floating
from libqtile.layout.max import Max
from libqtile.layout import Plasma, Bsp
from libqtile.layout.xmonad import MonadTall, MonadWide
from libqtile.layout.zoomy import Zoomy
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.utils import guess_terminal
from modules.bar import extension_defaults, layout_theme, screens, widget_defaults
from modules.colors import backgroundColor, colors, foregroundColor
from modules.groups import groups
from modules.keys import keys, mod, terminal
from modules.lazy_functions import sticky_windows

layouts: List[Layout] = [
    MonadTall(
        change_size=10,
        single_border_width=0,
        single_margin=0,
        new_client_position="top",
        **layout_theme,
    ),
    MonadWide(change_size=10, single_border_width=0, single_margin=0, **layout_theme),
    Max(),
    Floating(),
    Zoomy(),
]

next_maximum = {
    "x": 0.02,
    "y": 0.02,
    "width": 0.95,
    "height": 0.95,
    "opacity": 1.00,
    "on_focus_lost_hide": False,
}

groups.append(
    ScratchPad(
        "SPD",
        [
            DropDown(
                "VolumnControl",
                "pavucontrol",
                **next_maximum,
            ),
            DropDown(
                "Calculator",
                "qalculate-gtk",
                **next_maximum,
            ),
        ],
    )
)
keys.extend(
    [
        EzKey("M-p", lazy.group["SPD"].dropdown_toggle("VolumnControl")),
        EzKey("<XF86Calculator>", lazy.group["SPD"].dropdown_toggle("Calculator")),
    ]
)
# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        EzKey(
            f"C-A-<f{vt}>",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )

# Drag floating layouts.
mouse: List[Union[Drag, Click]] = [
    EzDrag(
        "M-<Button1>",
        move_snap_window(snap_dist=20),
        # lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    EzDrag(
        "M-<Button3>",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    EzClick( "M-<Button1>", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = [
    # Rule(Match(wm_class=[
    #     "Navigator",
    #     "firefox",
    #     "brave-browser",
    #     "Brave-browser",
    #     "qutebrowser",
    #     "LibreWolf",
    #     "Chromium",
    #     "chromium",
    #     "Chromium-browser",
    #     "chromium-browser",
    #     "brave-browser",
    #     "Brave-browser",
    #     "Thorium-browser",
    #     "thorium-browser",
    #     "vieb",
    # ]), group="2"),
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
        Match(wm_class="pavucontrol"),
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
        Match(title="Bitwarden"),
    ],
)
auto_fullscreen = True
floats_kept_above = True
# focus_on_window_activation = "focus"
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = {
    "type:touchpad": InputConfig(
        scroll_method="two_finger",
        natural_scroll=True,
        pointer_accel=0.45,
        tap=True,
        tap_button_map="lrm",
    ),
    "type:keyboard": InputConfig(kb_repeat_delay=300, kb_repeat_rate=30),
}
wmname = "LG3D"


@hook.subscribe.client_new
def assign_app_group(client: WindowType) -> None:
    # wm_class: str = client.window.get_wm_class()[0]
    wm_class: List[str] | None = client.get_wm_class()

    if wm_class is None or client.togroup is None:
        return
    if client.floating and client.bring_to_front:
        client.command("cmd_bring_to_front")
        return
    if (wm_class[0] or wm_class[1]) in [
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
        "Thorium-browser",
        "thorium-browser",
        "vieb",
        "io.github.ungoogled_software.ungoogled_chromium",
        "Io.github.ungoogled_software.ungoogled_chromium",
    ]:
        client.togroup("2", switch_group=True)
    elif (wm_class[0] or wm_class[1]) in [
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
    elif (wm_class[0] or wm_class[1]) in [
        "VirtualBox Manager",
        "VirtualBox Machine",
        "Vmplayer",
        "virtualbox manager",
        "virtualbox machine",
        "vmplayer",
    ]:
        client.togroup("5", switch_group=True)
    elif (wm_class[0] or wm_class[1]) in ["Steam", "Lutris", "lutris", "steam"]:
        client.togroup("3", switch_group=True)


@hook.subscribe.setgroup
def move_sticky_windows():
    for window in sticky_windows:
        window.togroup()
    return


@hook.subscribe.client_mouse_enter
def floating_to_top(client: WindowType):
    if client.floating and client.bring_to_front:
        client.bring_to_front()
        # client.keep_above(True)


@hook.subscribe.client_killed
def remove_sticky_windows(window: WindowType):
    if window in sticky_windows:
        sticky_windows.remove(window)


# Below is an example how to make Firefox Picture-in-Picture windows automatically sticky.
@hook.subscribe.client_managed
def auto_sticky_windows(window):
    info = window.info()
    if (
        info["wm_class"] == ["Toolkit", "firefox"]
        and info["name"] == "Picture-in-Picture"
    ):
        sticky_windows.append(window)

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser(
        "~/.config/qtile/extras/autostart.sh"
    )  # path to my script, under my user directory
    subprocess.call([home])
