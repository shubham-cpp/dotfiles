#!/usr/bin/python3
"""Send a reminder without retaining an action-waiting process.

Quickshell routes the notification's actions back to the reminder record.
The systemd wake mode falls back to a generic alert if the shell is absent.
"""
import argparse
from datetime import datetime, timedelta, timezone
import subprocess


def notification_args(reminder_id, title, at, urgency):
    from gi.repository import GLib

    body = "Open the calendar to check overdue reminders."
    actions = []
    hints = {"urgency": GLib.Variant("y", 2 if urgency == "critical" else 1)}
    if reminder_id:
        when = datetime.fromisoformat(at).astimezone(timezone(timedelta(minutes=330)))
        body = when.strftime("%H:%M IST  %d %b")
        actions = ["snooze", "Snooze 10m", "done", "Done"]
        hints["x-quickshell-reminder-id"] = GLib.Variant("s", reminder_id)
        hints["x-canonical-private-synchronous"] = GLib.Variant("s", "reminder/" + reminder_id)
    return GLib.Variant("(susssasa{sv}i)", ("reminders", 0, "", title, body, actions, hints, -1))


def notify(reminder_id="", title="Reminder", at="", urgency="critical"):
    from gi.repository import Gio

    bus = Gio.bus_get_sync(Gio.BusType.SESSION, None)
    bus.call_sync("org.freedesktop.Notifications", "/org/freedesktop/Notifications",
                  "org.freedesktop.Notifications", "Notify",
                  notification_args(reminder_id, title, at, urgency), None,
                  Gio.DBusCallFlags.NONE, 5000, None)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    send = commands.add_parser("notify")
    send.add_argument("id")
    send.add_argument("title")
    send.add_argument("at")
    send.add_argument("urgency", choices=("critical", "normal"))
    wake = commands.add_parser("wake")
    wake.add_argument("shell")
    args = parser.parse_args()
    if args.command == "notify":
        notify(args.id, args.title, args.at, args.urgency)
        return
    try:
        result = subprocess.run(["qs", "-p", args.shell, "ipc", "call", "reminders", "fire"],
                                timeout=5, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if result.returncode == 0:
            return
    except (OSError, subprocess.TimeoutExpired):
        pass
    notify()


if __name__ == "__main__":
    main()
