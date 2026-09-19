#!/usr/bin/python3
"""Child of Quickshell: logind events, lock hint, and a sleep-delay FD.

No idle scheduling or authentication. JSON messages on inherited pipes only.
Every acknowledgement belongs to a fresh token, especially across sleep.
"""
import json
import os
from pathlib import Path
import sys
import uuid

from gi.repository import Gio, GLib

DEST = "org.freedesktop.login1"
MANAGER = "/org/freedesktop/login1"
MANAGER_IFACE = DEST + ".Manager"
SESSION_IFACE = DEST + ".Session"


class SleepGate:
    """Reject stale confirmations and release a delay only for secure sleep."""
    def __init__(self, release):
        self.release = release
        self.token = uuid.uuid4().hex
        self.sleeping = False

    def begin(self, sleeping):
        self.token = uuid.uuid4().hex
        self.sleeping = sleeping
        return self.token

    def accept(self, token, secure):
        if token != self.token:
            return False
        if secure and self.sleeping:
            self.release()
        return True


class Bridge:
    def __init__(self):
        self.fd = None
        self.input_buffer = b""
        self.loop = GLib.MainLoop()
        self.bus = Gio.bus_get_sync(Gio.BusType.SYSTEM, None)
        self.gate = SleepGate(self.release)
        self.session = self.call(MANAGER, MANAGER_IFACE, "GetSession", "(s)", ("auto",))[0]
        user = self.property(self.session, SESSION_IFACE, "User")
        if user[0] != os.getuid() or self.property(self.session, SESSION_IFACE, "Type") != "wayland":
            raise RuntimeError("No owned Wayland session")
        self.session_id = self.property(self.session, SESSION_IFACE, "Id")
        # Runtime-only recovery intent. Never store a password or an auth result.
        directory = Path(GLib.get_user_runtime_dir()) / "qs-lock"
        directory.mkdir(mode=0o700, exist_ok=True)
        self.marker = directory / "requested"
        self.bus.signal_subscribe(DEST, MANAGER_IFACE, "PrepareForSleep", MANAGER,
                                  None, Gio.DBusSignalFlags.NONE, self.sleep_event)
        self.bus.signal_subscribe(DEST, SESSION_IFACE, "Lock", self.session,
                                  None, Gio.DBusSignalFlags.NONE, self.lock_event)
        self.bus.signal_subscribe("org.freedesktop.DBus", "org.freedesktop.DBus",
                                  "NameOwnerChanged", "/org/freedesktop/DBus", DEST,
                                  Gio.DBusSignalFlags.NONE, self.owner_event)
        self.acquire()
        GLib.io_add_watch(sys.stdin, GLib.IO_IN | GLib.IO_HUP | GLib.IO_ERR, self.input_event)
        recover = ((self.marker.exists() and self.marker.read_text() == self.session_id)
                   or self.property(self.session, SESSION_IFACE, "LockedHint"))
        self.emit("ready", recover=recover)
        if self.property(MANAGER, MANAGER_IFACE, "PreparingForSleep"):
            self.sleep_event(None, None, None, None, None, GLib.Variant("(b)", (True,)))

    def call(self, path, interface, method, signature=None, args=()):
        value = GLib.Variant(signature, args) if signature else None
        return self.bus.call_sync(DEST, path, interface, method, value, None,
                                  Gio.DBusCallFlags.NONE, 1500, None).unpack()

    def property(self, path, interface, name):
        return self.call(path, "org.freedesktop.DBus.Properties", "Get", "(ss)", (interface, name))[0]

    def emit(self, event, **values):
        print(json.dumps({"event": event, "token": self.gate.token, **values}), flush=True)

    def acquire(self):
        if self.fd is not None:
            return
        result, descriptors = self.bus.call_with_unix_fd_list_sync(
            DEST, MANAGER, MANAGER_IFACE, "Inhibit",
            GLib.Variant("(ssss)", ("sleep", "quickshell", "Secure the session before sleep", "delay")),
            GLib.VariantType.new("(h)"), Gio.DBusCallFlags.NONE, 1500, None, None)
        self.fd = descriptors.get(result.unpack()[0])

    def release(self):
        if self.fd is not None:
            os.close(self.fd)
            self.fd = None

    def sleep_event(self, _bus, _sender, _path, _interface, _signal, parameters):
        sleeping = parameters.unpack()[0]
        self.gate.begin(sleeping)
        try:
            if not sleeping:
                self.acquire()
        except (OSError, GLib.Error):
            self.emit("lost")
            self.loop.quit()
            return
        self.emit("sleep" if sleeping else "resume")

    def lock_event(self, *_args):
        self.emit("lock")

    def owner_event(self, _bus, _sender, _path, _interface, _signal, parameters):
        if parameters.unpack()[2] == "":
            self.emit("lost")
            self.loop.quit()

    def input_event(self, _source, condition):
        if condition & GLib.IO_ERR:
            self.loop.quit()
            return False
        # Read the FD directly: TextIO can prefetch the next JSON line and leave
        # it stuck in Python's buffer, invisible to the GLib readiness watch.
        chunk = os.read(sys.stdin.fileno(), 4096)
        if not chunk:
            self.loop.quit()
            return False
        self.input_buffer += chunk
        while b"\n" in self.input_buffer:
            line, self.input_buffer = self.input_buffer.split(b"\n", 1)
            self.accept_state(line)
        if len(self.input_buffer) > 4096:
            self.loop.quit()
            return False
        return True

    def accept_state(self, line):
        try:
            message = json.loads(line)
            if not isinstance(message, dict):
                return
            token = message.get("token")
            if token != self.gate.token:
                return
            secure = message.get("secure") is True
            requested = message.get("requested") is True
            pending = self.marker.with_suffix(".pending")
            pending.write_text(self.session_id if requested else "")
            pending.replace(self.marker)
            # Complete hint write before releasing the delay; failures retain it.
            self.call(self.session, SESSION_IFACE, "SetLockedHint", "(b)", (secure,))
            self.gate.accept(token, secure and requested)
        except (ValueError, OSError, GLib.Error) as error:
            print("session bridge: " + str(error), file=sys.stderr, flush=True)
            self.emit("error")

    def run(self):
        try:
            self.loop.run()
        finally:
            self.release()


if __name__ == "__main__":
    try:
        Bridge().run()
    except (OSError, RuntimeError, GLib.Error) as error:
        print("session bridge: " + str(error), file=sys.stderr, flush=True)
        sys.exit(1)
