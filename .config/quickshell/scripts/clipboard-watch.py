#!/usr/bin/env python3
"""One wl-paste callback. cliphist remains the only clipboard database."""
import fcntl
import json
import os
from pathlib import Path
import re
import subprocess
import sys

MAX_BYTES = 5_000_000
IGNORED_APPS = {"org.keepassxc.keepassxc", "keepassxc", "bitwarden", "com.bitwarden.desktop", "1password", "com.1password.1password"}


def focused_app():
    result = subprocess.run(["mmsg", "get", "focusing-client"], capture_output=True, check=True, timeout=2)
    client = json.loads(result.stdout)
    if not isinstance(client, dict) or not isinstance(client.get("appid"), str):
        raise ValueError("Missing focused application")
    return client["appid"].casefold()


def run_cliphist(action, payload=None):
    command = ["cliphist", "-preview-width", "0", action] if action == "list" else ["cliphist", action]
    return subprocess.run(command, input=payload, stdout=subprocess.PIPE,
                          stderr=subprocess.DEVNULL, check=True, timeout=10).stdout


def handle(state, content, previous, app):
    if state in ("nil", "clear") or (state == "data" and not content):
        if isinstance(previous, str) and re.fullmatch(r"[0-9]+", previous):
            run_cliphist("delete", (previous + "\n").encode())
        return None
    if state != "data" or app in IGNORED_APPS or len(content) > MAX_BYTES:
        return None
    if not content.strip():
        return previous
    run_cliphist("store", content)
    # Capture only the identifier. Never write previews or clipboard bytes to logs/state.
    listing = run_cliphist("list")
    first = listing.split(b"\t", 1)[0].decode("ascii", errors="ignore")
    if not re.fullmatch(r"[0-9]+", first):
        return None
    # Store can succeed without inserting when cliphist's configured size/length
    # policy rejects an item. Never associate that offer with an older row.
    return first if run_cliphist("decode", first.encode()) == content else None


def main():
    os.umask(0o077)
    runtime = Path(os.environ["XDG_RUNTIME_DIR"])
    state_path = runtime / "quickshell-clipboard-watch.json"
    fd = os.open(state_path, os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "r+") as record:
        fcntl.flock(record, fcntl.LOCK_EX)
        try:
            saved = json.loads(record.read(4096))
        except (ValueError, TypeError):
            saved = {}
        owner = os.getppid()
        if not isinstance(saved, dict) or saved.get("watcher") != owner:
            # data-control sends the current selection when the watcher binds.
            # Its origin predates this watcher, so do not attribute it to the current focus.
            record.seek(0)
            json.dump({"watcher": owner, "id": None}, record)
            record.truncate()
            return
        previous = saved.get("id")
        state = os.environ.get("CLIPBOARD_STATE", "")
        # Sensitive offers are rejected before reading their bytes or asking the compositor.
        content = sys.stdin.buffer.read(MAX_BYTES + 1) if state == "data" else b""
        app = ""
        if state == "data" and content:
            try:
                app = focused_app()
            except (OSError, ValueError, subprocess.SubprocessError):
                state = "unavailable"  # Fail closed if the focus policy cannot be evaluated.
        next_id = None
        try:
            next_id = handle(state, content, previous, app)
        finally:
            record.seek(0)
            json.dump({"watcher": owner, "id": next_id}, record)
            record.truncate()


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.SubprocessError):
        print("Clipboard history update failed", file=sys.stderr)
        sys.exit(1)
