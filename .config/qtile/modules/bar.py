import subprocess
from typing import Union

from libqtile import bar, widget
from libqtile.config import Screen

from .colors import backgroundColor, colors, foregroundColor

layout_theme: dict[str, Union[str, int, list[str]]] = {
    "margin": 5,
    "border_width": 3,
    "border_focus": colors[3],
    "border_normal": colors[1],
}

widget_defaults = dict(
    font="FiraCode Nerd Font",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    font="JetBrainsMono Nerd Font",
                    fontsize=15,
                    margin_y=2,
                    margin_x=4,
                    padding_y=6,
                    padding_x=6,
                    borderwidth=2,
                    disable_drag=True,
                    active=colors[4],
                    inactive=foregroundColor,
                    hide_unused=True,
                    rounded=False,
                    highlight_method="line",
                    highlight_color=[backgroundColor, backgroundColor],
                    this_current_screen_border=colors[5],
                    this_screen_border=colors[7],
                    other_screen_border=colors[6],
                    other_current_screen_border=colors[6],
                    urgent_alert_method="line",
                    urgent_border=colors[9],
                    urgent_text=colors[1],
                    foreground=foregroundColor,
                    background=backgroundColor,
                    use_mouse_wheel=False,
                ),
                widget.TaskList(
                    highlight_method="block",
                    margin=0,
                    padding=4,
                    borderwidth=3,
                    fontsize=14,
                    urgent_alert_method="border",
                    urgent_border=colors[1],
                    rounded=False,
                    txt_floating="🗗 ",
                    txt_maximized="🗖 ",
                    txt_minimized="🗕 ",
                    font="FontAwesome",
                ),
                widget.Sep(
                    linewidth=1,
                    padding=10,
                    foreground=colors[5],
                    background=backgroundColor,
                ),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Wttr(
                    location={"Pune": "Home"},
                    font="JetBrainsMono Nerd Font",
                    fontsize=14,
                    padding=5,
                ),
                widget.Net(
                    format="\uf1eb {down} \uf175\uf176 {up}",
                    padding=10,
                ),
                widget.TextBox(
                    text=" ",
                    fontsize=16,
                    font="JetBrainsMono Nerd Font",
                    foreground=colors[7],
                ),
                widget.CPU(
                    font="JetBrainsMono Nerd Font",
                    update_interval=1.0,
                    format="{load_percent}%",
                    foreground=foregroundColor,
                    padding=5,
                ),
                widget.Sep(linewidth=0, padding=10),
                widget.TextBox(
                    text="",
                    fontsize=16,
                    font="JetBrainsMono Nerd Font",
                    foreground=colors[3],
                ),
                widget.Memory(
                    font="JetBrainsMonoNerdFont",
                    foreground=foregroundColor,
                    format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}",
                    measure_mem="G",
                    padding=5,
                ),
                widget.Sep(linewidth=0, padding=10),
                widget.GenPollText(
                    name="volume_icon",
                    func=lambda: subprocess.run(
                        ["sb-volume"],
                        stdout=subprocess.PIPE,
                    )
                    .stdout.decode("utf-8")
                    .split()[0],
                    font="JetBrainsMonoNerdFont",
                    fontsize=16,
                    foreground=colors[9],
                    update_interval=60,
                    padding=3,
                ),
                widget.GenPollText(
                    name="volume",
                    func=lambda: subprocess.run(
                        ["sb-volume"],
                        stdout=subprocess.PIPE,
                    )
                    .stdout.decode("utf-8")
                    .split()[1],
                    padding=4,
                    foreground=foregroundColor,
                    update_interval=10,
                ),
                widget.Sep(linewidth=0, padding=10),
                widget.TextBox(
                    text=" ",
                    fontsize=14,
                    font="JetBrainsMono Nerd Font",
                    foreground=colors[10],
                ),
                widget.Clock(format="%a %d, %I:%M %p"),
                widget.Sep(
                    linewidth=1,
                    padding=10,
                    foreground=colors[5],
                    background=backgroundColor,
                ),
                # widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                widget.Systray(),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                widget.Sep(
                    linewidth=1,
                    padding=10,
                    foreground=colors[5],
                    background=backgroundColor,
                ),
                widget.CurrentLayoutIcon(
                    scale=0.7, foreground=colors[6], background=backgroundColor
                ),
                widget.Sep(
                    linewidth=1,
                    padding=10,
                    foreground=colors[5],
                    background=backgroundColor,
                ),
                widget.QuickExit(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
            background=backgroundColor,
            opacity=0.9,
        ),
    ),
]
