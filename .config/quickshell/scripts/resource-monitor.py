#!/usr/bin/env python3
"""On-demand application samples and identity-checked termination over JSON lines.

One reader lives only while the resources popup is open. No per-tick subprocesses.
CPU is a fraction of whole-machine capacity; memory is summed resident memory.
"""
import configparser
from dataclasses import dataclass
import json
import os
from pathlib import Path
import selectors
import shlex
import signal
import sys
import time

import psutil


BOUNDARIES = {"systemd", "mango", "bash", "sh",
              "fish", "zsh", "dash", "kitty", "foot", "alacritty", "ghostty",
              "konsole", "rofi", "vicinae", "xdg-open"}
PROTECTED = {"systemd", "mango", "quickshell", "qs", "stasis", "dbus-broker",
             "dbus-broker-launch", "gnome-keyring-daemon", "ksecretd"}
WRAPPERS = {"env", "flatpak", "bwrap", "sh", "bash", "python", "python3",
            "node", "electron", "gjs", "gjs-console"}


@dataclass(frozen=True)
class App:
    key: str
    name: str
    icon: str = ""


class Catalog:
    def __init__(self, entries):
        self.ids = {}
        self.aliases = {}
        for app, aliases in sorted(entries, key=lambda item: len(item[0].key)):
            self.ids[app.key] = app
            for alias in aliases:
                if alias and alias.lower() not in WRAPPERS:
                    self.aliases.setdefault(alias.lower(), app)

    def match(self, exe, name, argv):
        for candidate in (exe, argv[0] if argv else "", Path(exe).name, name):
            if candidate.lower() in self.aliases:
                return self.aliases[candidate.lower()]
        return None


def desktop_entry(entry):
    app_id = entry.get_id().removesuffix(".desktop")
    app = App(app_id, entry.get_name(), entry.get_string("Icon") or "")
    aliases = [app_id, entry.get_startup_wm_class() or ""]
    try:
        argv = shlex.split(entry.get_commandline() or "")
    except ValueError:
        argv = []
    if argv and Path(argv[0]).name == "env":
        argv = [arg for arg in argv[1:] if "=" not in arg and not arg.startswith("-")]
    if argv and Path(argv[0]).name not in WRAPPERS:
        aliases.extend((argv[0], Path(argv[0]).name))
    return app, aliases


def desktop_catalog():
    import gi
    gi.require_version("Gio", "2.0")
    gi.require_version("GioUnix", "2.0")
    from gi.repository import Gio, GioUnix

    return Catalog(desktop_entry(entry) for entry in Gio.AppInfo.get_all()
                   if isinstance(entry, GioUnix.DesktopAppInfo))


def flatpak_app(pid, catalog):
    try:
        config = configparser.ConfigParser(interpolation=None)
        config.read(f"/proc/{pid}/root/.flatpak-info")
        app_id = config.get("Application", "name", fallback="")
        return catalog.ids.get(app_id)
    except (OSError, configparser.Error):
        return None


def group_processes(records, protected):
    """Prefer an explicit desktop identity; helpers inherit across non-shell parents."""
    by_pid = {record["pid"]: record for record in records}
    resolved = {}

    def identity(record, seen):
        pid = record["pid"]
        if pid in resolved:
            return resolved[pid]
        app = record["app"]
        parent = by_pid.get(record["ppid"])
        if (not app and pid not in seen and parent and parent["pid"] not in seen
                and parent["uid"] == record["uid"]
                and parent["base"] not in BOUNDARIES
                and record["base"] not in BOUNDARIES | PROTECTED):
            app = identity(parent, seen | {pid})
        if not app:
            app = App("exe:" + record["fallback"], record["name"])
        resolved[pid] = app
        return app

    groups = {}
    for record in records:
        app = identity(record, set())
        key = f'{record["uid"]}:{app.key}'
        group = groups.setdefault(key, {"key": key, "name": app.name, "icon": app.icon,
                                      "memory": 0, "cpu": 0, "members": [], "canEnd": True})
        group["memory"] += record["memory"]
        group["cpu"] += record["cpu"]
        group["members"].append({"pid": record["pid"], "started": record["started"]})
        if record["uid"] != os.getuid() or record["base"] in PROTECTED or record["pid"] in protected:
            group["canEnd"] = False
    return list(groups.values())


class Monitor:
    def __init__(self, catalog):
        self.catalog = catalog
        self.previous = {}
        self.metadata = {}
        self.sample_time = None
        self.groups = []
        self.allowed = {}
        self.ending = {}
        self.protected = {os.getpid(), os.getppid()}

    def process_identity(self, process, exe, name):
        try:
            argv = process.cmdline()
        except psutil.AccessDenied:
            argv = []
        base = Path(exe).name or name
        app = flatpak_app(process.pid, self.catalog) or self.catalog.match(exe, name, argv)
        fallback = exe or name
        if base.startswith(("python", "node")) and len(argv) > 1 and not argv[1].startswith("-"):
            fallback += ":" + argv[1]
        return app, fallback, base

    def read_process(self, pid, metadata):
        # Fresh Process instances avoid cached creation times after PID reuse.
        process = psutil.Process(pid)
        with process.oneshot():
            uid = process.uids().real
            started = process.create_time()
            name = process.name()
            memory = process.memory_info().rss
            if not memory or process.status() == psutil.STATUS_ZOMBIE:
                return None
            cpu = process.cpu_times()
            parent = process.ppid()
            try:
                exe = process.exe()
            except psutil.AccessDenied:
                exe = ""
        cache_key = (pid, started, exe)
        identity = self.metadata.get(cache_key)
        if identity is None:
            identity = self.process_identity(process, exe, name)
        metadata[cache_key] = identity
        app, fallback, base = identity
        return {"pid": pid, "ppid": parent, "started": started, "uid": uid,
                "name": name, "base": base, "fallback": fallback, "app": app,
                "memory": memory, "ticks": cpu.user + cpu.system, "cpu": 0}

    def read_records(self):
        records, metadata = [], {}
        for pid in psutil.pids():
            try:
                record = self.read_process(pid, metadata)
                if record is not None:
                    records.append(record)
            except (psutil.Error, OSError):
                continue
        self.metadata = metadata
        return records

    def update_allowed(self, groups):
        self.allowed = {group["key"]: {(member["pid"], member["started"]) for member in group["members"]}
                        for group in groups if group["canEnd"]}

    def refresh_membership(self):
        # Action validation must not advance the CPU sampling interval.
        self.update_allowed(group_processes(self.read_records(), self.protected))

    def reset_cpu(self):
        self.previous = {}
        self.sample_time = None

    def sample(self):
        now = time.monotonic()
        elapsed = now - self.sample_time if self.sample_time is not None else 0
        capacity = elapsed * (psutil.cpu_count() or 1)
        interval_start = time.time() - elapsed
        records = self.read_records()
        counters = {}
        for record in records:
            token = (record["pid"], record["started"])
            previous = self.previous.get(token)
            # A process born during the interval has no earlier CPU time.
            if previous is None and elapsed and record["started"] >= interval_start:
                previous = 0
            if capacity and previous is not None:
                record["cpu"] = max(0, min(100, 100 * (record["ticks"] - previous) / capacity))
            counters[token] = record["ticks"]
        self.previous, self.sample_time = counters, now
        self.groups = group_processes(records, self.protected)
        self.update_allowed(self.groups)
        live = {group["key"] for group in self.groups}
        self.ending = {key: value for key, value in self.ending.items() if key in live}
        for group in self.groups:
            group["cpu"] = round(min(100, group["cpu"]), 1) if elapsed else -1
            group["count"] = len(group["members"])
        return self.message(now)

    def message(self, now=None):
        if now is None:
            now = time.monotonic()
        for group in self.groups:
            started = self.ending.get(group["key"])
            group["state"] = "" if started is None else "force" if now - started >= 4 else "ending"
        return {"event": "sample", "apps": self.groups}

    def signal_member(self, token, force):
        # The fd keeps the signal tied to this process even if the PID is reused.
        fd = os.pidfd_open(token[0])
        try:
            process = psutil.Process(token[0])
            if process.create_time() != token[1] or process.uids().real != os.getuid():
                return False
            signal.pidfd_send_signal(fd, signal.SIGKILL if force else signal.SIGTERM)
            return True
        finally:
            os.close(fd)

    def end(self, request):
        key = request.get("key")
        members = request.get("members")
        force = request.get("force") is True
        if not isinstance(key, str) or not isinstance(members, list) or len(members) > 10000:
            return {"event": "action", "ok": False, "message": "Invalid application selection"}
        self.refresh_membership()
        allowed = self.allowed.get(key, set())
        if force and (key not in self.ending or time.monotonic() - self.ending[key] < 4):
            return {"event": "action", "ok": False, "message": "Try ending the application first"}
        sent, denied = 0, False
        for token in requested_members(members, allowed):
            try:
                sent += self.signal_member(token, force)
            except (ProcessLookupError, psutil.NoSuchProcess):
                pass
            except (OSError, psutil.Error):
                denied = True
        if sent:
            self.ending[key] = time.monotonic()
        message = "Some processes could not be ended" if denied else "" if sent else "Application exited or is no longer available to end"
        return {"event": "action", "ok": bool(sent) and not denied, "message": message}


def requested_members(members, allowed):
    for member in members:
        if not isinstance(member, dict):
            continue
        token = (member.get("pid"), member.get("started"))
        if type(token[0]) is int and isinstance(token[1], (int, float)) and token in allowed:
            yield token


def emit(message):
    print(json.dumps(message, separators=(",", ":")), flush=True)


class MonitorStream:
    def __init__(self, monitor):
        self.monitor = monitor
        self.buffer = b""
        self.paused = False
        self.deadline = time.monotonic()

    def request(self, line):
        try:
            request = json.loads(line)
            if not isinstance(request, dict):
                return
            if request.get("action") == "pause":
                self.paused = request.get("paused") is True
                if not self.paused:
                    self.monitor.reset_cpu()
                    self.deadline = time.monotonic()
            elif request.get("action") == "end":
                emit(self.monitor.end(request))
                emit(self.monitor.message())
        except (ValueError, TypeError):
            emit({"event": "action", "ok": False, "message": "Invalid application selection"})

    def consume(self, chunk):
        if not chunk:
            return False
        self.buffer += chunk
        if len(self.buffer) > 1024 * 1024:
            return False
        while b"\n" in self.buffer:
            line, self.buffer = self.buffer.split(b"\n", 1)
            self.request(line)
        return True

    def tick(self):
        if not self.paused and time.monotonic() >= self.deadline:
            emit(self.monitor.sample())
            self.deadline = time.monotonic() + 2


def run():
    stream = MonitorStream(Monitor(desktop_catalog()))
    with selectors.DefaultSelector() as selector:
        selector.register(sys.stdin, selectors.EVENT_READ)
        while True:
            timeout = None if stream.paused else max(0, stream.deadline - time.monotonic())
            if selector.select(timeout) and not stream.consume(os.read(sys.stdin.fileno(), 65536)):
                return
            stream.tick()


if __name__ == "__main__":
    try:
        run()
    except BrokenPipeError:
        pass
    except Exception as error:
        emit({"event": "error", "message": "Application monitor unavailable"})
        print(f"resource monitor: {error}", file=sys.stderr)
        sys.exit(1)
