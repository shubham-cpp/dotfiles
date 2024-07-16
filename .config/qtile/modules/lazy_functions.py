from libqtile.lazy import lazy as lz
from libqtile.log_utils import logger
from libqtile.core.manager import Qtile
# Custom Lazy Functions {{{
# @hook.subscribe.client_urgent_hint_changed
# def go_to(window):
#     logger.debug("Run urgent")
#     qtile.next_urgent()

sticky_windows: list = []


@lz.function
def toggle_layout_max(qtile: Qtile):
    """Toggle Max layout

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    lname = qtile.current_group.layout.name
    # indices = [i for i, _ in enumerate(qtile.current_group.layouts)]
    qtile.current_group.use_layout(index=2 if lname != "max" else 0)
    # logger.warn("Current Layout:: " + lname)


@lz.function
def move_win_to_immediate_group(qtile: Qtile, prev=False):
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
def smart_window_kill(qtile: Qtile):
    """Kill the window and move to last group if it was the last window

    Args:
        qtile (libqtile.core.manager.Qtile): By default passed by lz.function
    """
    win_count = len(qtile.current_group.windows)
    if qtile.current_window:
        qtile.current_window.kill()
    if win_count <= 1:
        qtile.current_screen.toggle_group()


@lz.function
def update_volume(qtile):
    """Update the volume widget on keypress

    Args:
        qtile (libqtile.core.manager.Qtile): By default passed by lz.function
    """
    w = qtile.widgets_map["volume"]
    icon = qtile.widgets_map["volume_icon"]
    if w:
        w.poll()
        w.force_update()
    if icon:
        icon.poll()
        icon.force_update()


@lz.function
def update_mic_icon(qtile):
    """Update the volume_mic widget on keypress

    Args:
        qtile (libqtile.core.manager.Qtile): By default passed by lz.function
    """
    icon = qtile.widgets_map["volume_mic_icon"]
    icon.poll()
    icon.force_update()


@lz.function
def update_brightness(qtile):
    """Update the brightnesswidget on keypress

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    w = qtile.widgets_map["brightness"]
    w.force_update()


@lz.function
def toggle_sticky_windows(qtile: Qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
    if window in sticky_windows:
        sticky_windows.remove(window)
    else:
        sticky_windows.append(window)
    return window


@lz.function
def toggle_monad_tall_wide(qtile: Qtile):
    """Toggle Monadtall and MonadWide layout

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    lname = qtile.current_group.layout.name
    logger.warn("Current Layout:: " + lname)
    change_to = "monadtall" if lname != "monadtall" else "monadwide"
    qtile.current_group.setlayout(change_to)


last_layout = "monadtall"  # Default layout


@lz.function
def toggle_layout(qtile: Qtile, switch_to: str):
    """Toggle between current layout and last layout

    Args:
        qtile (libqtile.core.manager.Qtile): By default passed by lz.function
    """
    current_layout_name = qtile.current_group.layout.name
    if current_layout_name != switch_to:
        global last_layout
        last_layout = current_layout_name
        qtile.current_group.setlayout(switch_to)
    else:
        qtile.current_group.setlayout(last_layout)


@lz.function
def focus_next_class(qtile: Qtile):
    """Focus to next window of the same class

    Args:
        qtile (libqtile.qtile): By default passed by lz.function
    """
    windows = qtile.current_group.focus_history
    current_window = qtile.current_window
    for w in windows:
        if w == current_window:
            continue
        if w.window.get_wm_class() == current_window.window.get_wm_class() and w.is_visible:
            w.group.focus(w, True)
            w.keep_above(True)
            break

# }}}
