#!/usr/bin/env python3
"""Drive the real calendar surface and verify that it receives keyboard input."""
import json
import subprocess
import time
from pathlib import Path


ROOT = str(Path(__file__).resolve().parents[1])
MARKER = "QSCALENDARFOCUS"
SHOT = "/tmp/qs-calendar-input-repro.png"
CLOSED_SHOT = "/tmp/qs-calendar-input-closed.png"


def run(*args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["rtk", *args],
        check=check,
        capture_output=True,
        text=True,
    )


def cursor() -> tuple[float, float]:
    position = json.loads(run("mmsg", "get", "cursorpos").stdout)
    return position["x"], position["y"]


def move_to(target_x: float, target_y: float) -> None:
    for _ in range(16):
        current_x, current_y = cursor()
        delta_x = target_x - current_x
        delta_y = target_y - current_y
        if abs(delta_x) <= 2 and abs(delta_y) <= 2:
            return
        step_x = round(max(-120, min(120, delta_x * 0.7)))
        step_y = round(max(-120, min(120, delta_y * 0.7)))
        run("ydotool", "mousemove", "-x", str(step_x), "-y", str(step_y))
        time.sleep(0.03)
    raise RuntimeError(f"could not position cursor at {target_x},{target_y}")


def click_at(x: float, y: float) -> None:
    move_to(x, y)
    run("ydotool", "click", "0xC0")
    time.sleep(0.25)


monitors = json.loads(run("mmsg", "get", "all-monitors").stdout)["monitors"]
monitor = next(item for item in monitors if item["active"])
WIDTH = 520
popup_left = monitor["x"] + round((monitor["width"] - WIDTH) / 2)
popup_top = monitor["y"] + 50

run("qs", "-n", "-p", ROOT, "ipc", "call", "calendar", "close")
time.sleep(0.2)
click_at(monitor["x"] + monitor["width"] / 2, monitor["y"] + 20)
click_at(popup_left + WIDTH - 50, popup_top + 444)
run("ydotool", "type", "--key-delay", "10", MARKER)
time.sleep(0.2)
run("grim", "-g", f"{popup_left},{popup_top} {WIDTH}x600", SHOT)
ocr = run("tesseract", SHOT, "stdout").stdout.replace(" ", "").upper()

if MARKER not in ocr:
    run("qs", "-n", "-p", ROOT, "ipc", "call", "calendar", "close")
    print("FAIL: typed marker did not reach the calendar reminder input")
    raise SystemExit(1)

click_at(monitor["x"] + 100, monitor["y"] + 700)
run("grim", "-g", f"{popup_left},{popup_top} {WIDTH}x600", CLOSED_SHOT)
closed_ocr = run("tesseract", CLOSED_SHOT, "stdout").stdout.replace(" ", "").upper()
run("qs", "-n", "-p", ROOT, "ipc", "call", "calendar", "close")
if MARKER in closed_ocr:
    print("FAIL: calendar stayed open after a click outside the panel")
    raise SystemExit(1)

print("PASS: calendar reminder input captured the typed marker")
