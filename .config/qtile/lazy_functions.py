from libqtile.command import lazy as lz
from libqtile.log_utils import logger
from libqtile import hook

# Custom Lazy Functions {{{
# @hook.subscribe.client_urgent_hint_changed
# def go_to(window):
#     logger.debug("Run urgent")
#     qtile.next_urgent()
sticky_windows: list = []

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
    # widgets = ",".join(qtile.widgets_map)
    # logger.warn("widget names = " + widgets)
    # logger.warn("values of w  = " + ",".join(dir(w)))
    w.poll()
    icon.poll()
    w.force_update()
    icon.force_update()


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
    w.poll()
    icon.poll()
    w.force_update()
    icon.force_update()

@lz.function
def toggle_sticky_windows(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
    if window in sticky_windows:
        sticky_windows.remove(window)
    else:
        sticky_windows.append(window)
    return window
@hook.subscribe.setgroup
def move_sticky_windows():
    for window in sticky_windows:
        window.togroup()
    return

@hook.subscribe.client_killed
def remove_sticky_windows(window):
    if window in sticky_windows:
        sticky_windows.remove(window)

# }}}
