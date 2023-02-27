import subprocess

from libqtile import bar  # , widget
from libqtile.log_utils import logger
from qtile_extras import widget
from qtile_extras.widget.decorations import RectDecoration

CATPPUCCIN = {
    "rosewater": "#F5E0DC",
    "flamingo": "#F2CDCD",
    "pink": "#F5C2E7",
    "mauve": "#CBA6F7",
    "red": "#F38BA8",
    "maroon": "#EBA0AC",
    "peach": "#FAB387",
    "yellow": "#F9E2AF",
    "green": "#A6E3A1",
    "teal": "#94E2D5",
    "sky": "#89DCEB",
    "sapphire": "#74C7EC",
    "blue": "#89B4FA",
    "darkblue": "#545AA7",
    "lavender": "#B4BEFE",
    "text": "#CDD6F4",
    "subtext1": "#BAC2DE",
    "subtext0": "#A6ADC8",
    "overlay2": "#9399b2",
    "overlay1": "#7F849C",
    "surface2": "#585B70",
    "surface1": "#45475A",
    "surface0": "#313244",
    "base": "#1E1E2E",
    "mantle": "#181825",
    "crust": "#11111B",
    "transparent": "#00000000",
}
# colors = [
#     "#1E1F29",  # 0 darker purple
#     "#bfbfbf",  # 1 white
#     "#bd93f9",  # 2 lighter purple
#     "#ff92d0",  # 3 pink
#     "#8fae81",  # 4 soft green
#     "#ff6e67",  # 5 red
#     "#3A2F4D",  # 6 grey
#     "#50fa7b",  # 7 hard green
#     "#282a36",  # 8 middle purple
#     "#ffae42",  # 9 orange yellow
#     "#fff44f",  # 10 lemon yellow
# ]
colors = [
    ["#2e3440", "#2e3440"],  # 0 background
    ["#d8dee9", "#d8dee9"],  # 1 foreground
    ["#3b4252", "#3b4252"],  # 2 background lighter
    ["#bf616a", "#bf616a"],  # 3 red
    ["#a3be8c", "#a3be8c"],  # 4 green
    ["#ebcb8b", "#ebcb8b"],  # 5 yellow
    ["#81a1c1", "#81a1c1"],  # 6 blue
    ["#b48ead", "#b48ead"],  # 7 magenta
    ["#88c0d0", "#88c0d0"],  # 8 cyan
    ["#e5e9f0", "#e5e9f0"],  # 9 white
    ["#4c566a", "#4c566a"],  # 10 grey
    ["#d08770", "#d08770"],  # 11 orange
    ["#8fbcbb", "#8fbcbb"],  # 12 super cyan
    ["#5e81ac", "#5e81ac"],  # 13 super blue
    ["#242831", "#242831"],  # 14 super dark background
]

group_box_settings = {
    "padding": 4,
    "borderwidth": 4,
    "active": colors[9],
    "inactive": colors[10],
    "disable_drag": True,
    "rounded": True,
    "highlight_color": colors[2],
    "block_highlight_text_color": colors[6],
    "highlight_method": "block",
    "this_current_screen_border": colors[14],
    "this_screen_border": colors[7],
    "other_current_screen_border": colors[14],
    "other_screen_border": colors[14],
    "foreground": colors[1],
    # "background": colors[0],
    "urgent_border": colors[3],
    "hide_unused": True,
}


def NerdIcon(icon="", fg=colors[0], bg=colors[1]):
    return widget.TextBox(
        fmt=icon,
        fontsize=18,
        font="Hack Nerd Font",
        background=bg,
        foreground=fg,
        # margin=2,
        padding=7,
    )


def calIcon(level: int, icons: list[str]) -> str:
    if level >= 100:
        return icons[4]
    elif level > 80:
        return icons[3]
    elif level > 50:
        return icons[2]
    elif level > 5:
        return icons[1]
    else:
        return icons[0]


def getIcon(name: str) -> str:
    if name == "battery":
        with open("/sys/class/power_supply/BAT1/capacity", "r") as bat_file:
            bat_level = int(bat_file.read().strip())
            return calIcon(
                bat_level,
                [
                    "",
                    "",
                    "",
                    "",
                    "",
                ],
            )
    elif name == "volume":
        vol = (
            # subprocess.run(
            #     ["pamixer", "--get-volume-human"],
            #     stdout=subprocess.PIPE,
            # )
            # .stdout.decode("utf-8")
            # .strip()[:2]
            subprocess.run(
                ["pactl", "get-sink-volume", "@DEFAULT_SINK@"], stdout=subprocess.PIPE
            )
            .stdout.decode("utf-8")
            .split("/")[1]
            .strip()
        )
        is_mute = (
            subprocess.run(
                ["pactl", "get-sink-mute", "@DEFAULT_SINK@"], stdout=subprocess.PIPE
            )
            .stdout.decode("utf-8")
            .split(":")[1]
            .strip()
        )
        # vol_level = 0 if vol == "mu" else int(vol[: len(vol) - 1])
        vol_level = 0 if is_mute == "yes" else int(vol[: len(vol) - 1])
        logger.info(f"Volume {vol_level}")
        return calIcon(vol_level, ["婢", "奔", "墳", "", ""])
    elif name == "brightness":
        return calIcon(
            int(
                float(
                    subprocess.run(["brightnessctl", "g"],
                                   stdout=subprocess.PIPE)
                    .stdout.decode("utf-8")
                    .strip()
                )
            ),
            ["", "", "", "", ""],
        )
    else:
        logger.warn("function getIcon expects an argument")
        return "Unknown"


def ArrowSep():
    # icon=""
    # return widget.TextBox(
    #     text=icon,
    #     font="FuraCode Nerd Font",
    #     fontsize=34,
    # )
    return widget.Spacer(14)


top = bar.Bar(
    [
        widget.GroupBox(
            fontsize=14,
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
            txt_floating="  ",
            txt_minimized=" ",
            txt_maximized=" ",
            highlight_method="block",
            margin=0,
            padding=4,
            borderwidth=3,
            fontsize=14,
            urgent_alert_method="text",
            urgent_text="#834a61",
            font="FontAwesome",
        ),
        NerdIcon("", bg=colors[3]),
        widget.Clock(
            format="%a %d, %I:%M %p",
            update_interval=60,
            foreground=colors[0],
            background="#eeeeee",
        ),
        ArrowSep(),
        NerdIcon("省", bg=colors[2]),
        widget.CPU(
            format="{load_percent}%",
            update_interval=2,
            foreground=colors[0],
            background="#eeeeee",
        ),
        ArrowSep(),
        NerdIcon("", bg=colors[9]),
        widget.Memory(
            format="{MemUsed:.0f}{mm}",
            update_interval=2,
            foreground=colors[0],
            background="#eeeeee",
        ),
        # ArrowSep(),
        # widget.GenPollText(
        #     name="brightness_icon",
        #     func=lambda: getIcon("brightness"),
        #     font="FuraCode Nerd Font",
        #     fontsize=18,
        #     foreground=colors[0],
        #     background=colors[10],
        #     update_interval=60,
        #     padding=3,
        # ),
        # widget.GenPollText(
        #     name="brightness",
        #     func=lambda: subprocess.run(
        #         ["brightnessctl", "g"],
        #         stdout=subprocess.PIPE,
        #     )
        #     .stdout.decode("utf-8")
        #     .strip(),  # [:2],
        #     padding=3,
        #     foreground=colors[0],
        #     background="#eeeeee",
        # ),
        ArrowSep(),
        widget.GenPollText(
            name="volume_icon",
            func=lambda: getIcon("volume"),
            font="FuraCode Nerd Font",
            fontsize=18,
            foreground=colors[0],
            background=colors[5],
            update_interval=60,
            padding=3,
        ),
        widget.GenPollText(
            name="volume",
            func=lambda: subprocess.run(
                ["pactl", "get-sink-volume", "@DEFAULT_SINK@"],
                stdout=subprocess.PIPE,
            )
            .stdout.decode("utf-8")
            .split("/")[1]
            .strip(),
            padding=3,
            foreground=colors[0],
            background="#eeeeee",
            update_interval=10,
        ),
        # ArrowSep(),
        # widget.GenPollText(
        #     name="battery_icon",
        #     func=lambda: getIcon("battery"),
        #     font="FuraCode Nerd Font",
        #     fontsize=18,
        #     foreground=colors[6],
        #     background="#86EFAC",
        #     update_interval=60,
        #     padding=3,
        # ),
        # widget.Battery(
        #     battery="BAT1",
        #     unknown_char="",
        #     discharge_char="",
        #     empty_char="",
        #     charge_char="",
        #     format="{char} {percent:2.0%}",
        #     # format="{percent:2.0%} {char}",
        #     low_foreground=colors[5],
        #     notify_below=15,
        #     # font="Iosevka Nerd Font",
        #     fontsize=14,
        #     padding=3,
        #     foreground=colors[0],
        #     background="#eeeeee",
        # ),
        ArrowSep(),
        widget.CapsNumLockIndicator(update_interval=1.5, fontsize=14),
        ArrowSep(),
        widget.Net(format=" {up}   {down}"),
        ArrowSep(),
        widget.Wttr(location={"Pune": "Home"}, fontsize=15),
        ArrowSep(),
        widget.CurrentLayoutIcon(scale=0.7),
        widget.Systray(),
    ],
    24,
    background="#1a1826",
    opacity=0.6,
)
group_box_settings2 = {
    "hide_unused": True,
    "disable_drag": True,
    "toggle": False,
    "highlight_method": "block",
    "borderwidth": 4,
    "this_current_screen_border": CATPPUCCIN["surface0"],  # block fill color
    "block_highlight_text_color": CATPPUCCIN["text"],  # block text color
    "active": CATPPUCCIN["crust"],  # text color
    "urgent_alert_method": "block",
    "urgent_border": CATPPUCCIN["lavender"],
    "urgent_text": CATPPUCCIN["crust"],
    "spacing": 4,
    "margin_x": 6,
    "margin_y": 3,  # push labels down
    "padding_x": 2,
    "padding_y": 2,
    "decorations": [
        RectDecoration(
            colour=CATPPUCCIN["lavender"],
            radius=7,
            filled=True,
            # group=True
        )
    ],
}

top_another = bar.Bar(
    [
        widget.GroupBox(**group_box_settings2),
        widget.Prompt(),
        widget.Spacer(length=10),
        widget.TaskList(
            txt_floating="  ",
            txt_minimized=" ",
            txt_maximized=" ",
            highlight_method="block",
            icon_size=24,
            margin=0,
            padding_x=2,
            borderwidth=0,
            fontsize=16,
            urgent_alert_method="text",
            urgent_text="#834a61",
            font="FontAwesome",
        ),
        widget.Clock(
            format=" %a %d, %I:%M %p",
            update_interval=60,
            margin=0,
            padding=14,
            foreground=colors[1],
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        widget.Spacer(18),
        widget.GenPollText(
            name="volume_icon",
            func=lambda: getIcon("volume"),
            font="FuraCode Nerd Font",
            fontsize=18,
            foreground=colors[1],
            update_interval=60,
            padding=3,
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        widget.GenPollText(
            name="volume",
            func=lambda: subprocess.run(
                ["pactl", "get-sink-volume", "@DEFAULT_SINK@"],
                stdout=subprocess.PIPE,
            )
            .stdout.decode("utf-8")
            .split("/")[1]
            .strip(),
            padding=4,
            foreground=colors[1],
            update_interval=10,
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        # widget.TextBox(fmt="    ", foreground=colors[1], margin=0, padding=0),
        widget.Spacer(18),
        widget.Memory(
            format=" {MemUsed:.0f}{mm}",
            update_interval=2,
            margin=0,
            padding=14,
            foreground=colors[1],
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        # widget.TextBox(fmt="    ", foreground=colors[1], margin=0, padding=0),
        widget.Spacer(14),
        widget.CPU(
            format=" {load_percent}%",
            margin=0,
            padding=10,
            update_interval=2,
            foreground=colors[1],
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        widget.CapsNumLockIndicator(update_interval=1.5, fontsize=14),
        widget.Spacer(14),
        widget.Net(
            format="\uf1eb {down} \uf175\uf176 {up}",
            padding=10,
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        widget.Spacer(14),
        widget.Wttr(
            location={"Pune": "Home"},
            fontsize=13,
            padding=10,
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"], radius=4, filled=True, padding_y=2
                )
            ],
        ),
        widget.Spacer(length=8),
        widget.CurrentLayoutIcon(
            scale=0.7,
            use_mask=True,
            foreground=[CATPPUCCIN["text"]],
            padding=8,
            decorations=[
                RectDecoration(
                    colour=CATPPUCCIN["surface0"],
                    radius=4,
                    filled=True,
                    group=True,
                )
            ],
        ),
        widget.Systray(),
    ],
    24,
    # border_width=[0, 0, 3, 0],
    # border_color="#3b4252",
    background="#1a1826",
    opacity=0.8,
    # margin=[6, 6, 6, 6],
)
