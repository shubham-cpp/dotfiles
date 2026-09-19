"""Grouping and termination checks. Signals are sent only to test-owned children."""
import importlib.util
import json
from contextlib import nullcontext
import os
from pathlib import Path
import signal
import subprocess
import sys
import time
import unittest
from types import SimpleNamespace
from unittest.mock import Mock, patch

spec = importlib.util.spec_from_file_location("resource_monitor", Path(__file__).parents[1] / "scripts/resource-monitor.py")
monitor = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = monitor
spec.loader.exec_module(monitor)


def record(pid, parent=1, base="browser", app=None, memory=100, cpu=1, started=10):
    return {"pid": pid, "ppid": parent, "started": started, "uid": os.getuid(),
            "name": base, "base": base, "fallback": "/bin/" + base,
            "app": app, "memory": memory, "cpu": cpu}


def fake_process(values):
    return SimpleNamespace(
        pid=123, oneshot=nullcontext, uids=lambda: SimpleNamespace(real=os.getuid()),
        create_time=lambda: values["started"], name=lambda: "test-app",
        memory_info=lambda: SimpleNamespace(rss=123), status=lambda: "running",
        cpu_times=lambda: SimpleNamespace(user=values["cpu"], system=0),
        ppid=lambda: 1, exe=lambda: values.get("exe", "/bin/test-app"),
        cmdline=Mock(return_value=["/bin/test-app"]))


class Grouping(unittest.TestCase):
    def test_children_and_instances_share_application(self):
        browser = monitor.App("browser", "Browser", "browser-icon")
        rows = [record(10, app=browser), record(11, 10, "renderer"), record(12, 11, "gpu"),
                record(20, app=browser)]
        groups = monitor.group_processes(rows, set())
        self.assertEqual(len(groups), 1)
        self.assertEqual(groups[0]["memory"], 400)
        self.assertEqual(groups[0]["cpu"], 4)
        self.assertEqual(len(groups[0]["members"]), 4)

    def test_terminal_and_independent_app_are_boundaries(self):
        rows = [record(10, base="kitty", app=monitor.App("kitty", "kitty")),
                record(11, 10, "fish"), record(12, 11, "codex"), record(13, 12, "node"),
                record(14, 12, "nvim", app=monitor.App("nvim", "Neovim"))]
        groups = {g["name"]: g for g in monitor.group_processes(rows, set())}
        self.assertEqual(len(groups), 4)
        self.assertEqual(len(groups["codex"]["members"]), 2)
        self.assertEqual(len(groups["Neovim"]["members"]), 1)

    def test_session_and_other_users_cannot_be_ended(self):
        other = record(20)
        other["uid"] = os.getuid() + 1
        groups = monitor.group_processes([record(10, base="quickshell"), other, record(30, base="reader")], {30})
        self.assertTrue(all(not group["canEnd"] for group in groups))

    def test_parent_cycle_does_not_recurse_forever(self):
        self.assertTrue(monitor.group_processes([record(10, 11), record(11, 10)], set()))

    def test_shell_helpers_belong_to_the_protected_shell(self):
        groups = monitor.group_processes([record(10, base="quickshell"), record(11, 10, "python3")], {11})
        self.assertEqual(len(groups), 1)
        self.assertEqual(groups[0]["name"], "quickshell")
        self.assertFalse(groups[0]["canEnd"])

    def test_catalog_matches_appimage_and_does_not_match_generic_runtime(self):
        catalog = monitor.Catalog([(monitor.App("librewolf", "LibreWolf"), ["librewolf"]),
                                   (monitor.App("flatpak-app", "Flatpak app"), ["flatpak"])])
        self.assertEqual(catalog.match("/tmp/.mount_X/usr/bin/librewolf", "Web Content", []).name, "LibreWolf")
        self.assertIsNone(catalog.match("/usr/bin/flatpak", "flatpak", []))


class Termination(unittest.TestCase):
    def setUp(self):
        self.child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
        self.started = monitor.psutil.Process(self.child.pid).create_time()
        self.reader = monitor.Monitor(monitor.Catalog([]))
        self.key = "test"
        self.reader.allowed = {self.key: {(self.child.pid, self.started)}}
        self.sample = patch.object(self.reader, "refresh_membership")
        self.sample.start()

    def tearDown(self):
        self.sample.stop()
        if self.child.poll() is None:
            self.child.terminate()
        self.child.wait(timeout=3)

    def request(self, started=None, force=False):
        return {"key": self.key, "members": [{"pid": self.child.pid, "started": self.started if started is None else started}], "force": force}

    def test_normal_end_terminates_only_the_captured_child(self):
        result = self.reader.end(self.request())
        self.assertTrue(result["ok"])
        self.assertEqual(self.child.wait(timeout=3), -signal.SIGTERM)

    def test_reused_pid_creation_time_is_rejected(self):
        wrong = self.started - 1
        self.reader.allowed = {self.key: {(self.child.pid, wrong)}}
        self.assertFalse(self.reader.end(self.request(started=wrong))["ok"])
        self.assertIsNone(self.child.poll())

    def test_removed_or_protected_group_is_rejected(self):
        self.reader.allowed = {}
        self.assertFalse(self.reader.end(self.request())["ok"])
        self.assertIsNone(self.child.poll())

    def test_force_requires_an_unsuccessful_normal_end(self):
        self.assertFalse(self.reader.end(self.request(force=True))["ok"])
        self.assertIsNone(self.child.poll())
        self.reader.ending[self.key] = time.monotonic() - 5
        self.assertTrue(self.reader.end(self.request(force=True))["ok"])
        self.assertEqual(self.child.wait(timeout=3), -signal.SIGKILL)


class Sampling(unittest.TestCase):
    def test_cpu_uses_interval_and_whole_machine_capacity_and_resets_on_pid_reuse(self):
        values = {"cpu": 1, "started": 10}
        process = fake_process(values)
        reader = monitor.Monitor(monitor.Catalog([]))
        with patch.object(monitor.psutil, "pids", return_value=[123]), \
             patch.object(monitor.psutil, "Process", return_value=process), \
             patch.object(monitor.psutil, "cpu_count", return_value=4), \
             patch.object(monitor, "flatpak_app", return_value=None), \
             patch.object(monitor.time, "monotonic", side_effect=[100, 102, 104]):
            self.assertEqual(reader.sample()["apps"][0]["cpu"], -1)
            values["cpu"] = 3
            self.assertEqual(reader.sample()["apps"][0]["cpu"], 25)
            values.update(cpu=100, started=20)
            self.assertEqual(reader.sample()["apps"][0]["cpu"], 0)

    def test_action_refresh_keeps_the_original_cpu_interval(self):
        values = {"cpu": 1, "started": 10, "now": 100}
        reader = monitor.Monitor(monitor.Catalog([]))
        with patch.object(monitor.psutil, "pids", return_value=[123]), \
             patch.object(monitor.psutil, "Process", return_value=fake_process(values)), \
             patch.object(monitor.psutil, "cpu_count", return_value=4), \
             patch.object(monitor, "flatpak_app", return_value=None), \
             patch.object(monitor.time, "monotonic", side_effect=lambda: values["now"]), \
             patch.object(reader, "signal_member", return_value=True):
            app = reader.sample()["apps"][0]
            values.update(cpu=2, now=101)
            request = {"key": app["key"], "members": app["members"]}
            self.assertTrue(reader.end(request)["ok"])
            self.assertEqual(reader.sample_time, 100)
            self.assertEqual(reader.previous, {(123, 10): 1})
            self.assertEqual(reader.message()["apps"][0]["cpu"], -1)
            values.update(cpu=3, now=102)
            self.assertEqual(reader.sample()["apps"][0]["cpu"], 25)

    def test_identity_cache_invalidates_on_exec_pid_reuse_and_exit(self):
        values = {"cpu": 1, "started": 10}
        process = fake_process(values)
        reader = monitor.Monitor(monitor.Catalog([]))
        with patch.object(monitor.psutil, "pids", return_value=[123]) as pids, \
             patch.object(monitor.psutil, "Process", return_value=process) as create, \
             patch.object(monitor, "flatpak_app", return_value=None):
            reader.read_records()
            reader.read_records()
            self.assertEqual(process.cmdline.call_count, 1)
            values["exe"] = "/bin/replacement"
            reader.read_records()
            values["started"] = 20
            reader.read_records()
            self.assertEqual(process.cmdline.call_count, 3)
            self.assertEqual(create.call_count, 4)
            self.assertEqual(list(reader.metadata), [(123, 20, "/bin/replacement")])
            pids.return_value = []
            reader.read_records()
            self.assertEqual(reader.metadata, {})


class SignalSafety(unittest.TestCase):
    def test_pidfd_is_closed_when_identity_changed_or_signaling_fails(self):
        reader = monitor.Monitor(monitor.Catalog([]))
        process = fake_process({"cpu": 1, "started": 20})
        with patch.object(monitor.os, "pidfd_open", return_value=7), \
             patch.object(monitor.os, "close") as close, \
             patch.object(monitor.psutil, "Process", return_value=process), \
             patch.object(monitor.signal, "pidfd_send_signal", side_effect=PermissionError) as send:
            self.assertFalse(reader.signal_member((123, 10), False))
            send.assert_not_called()
            close.assert_called_once_with(7)
            close.reset_mock()
            with self.assertRaises(PermissionError):
                reader.signal_member((123, 20), False)
            close.assert_called_once_with(7)


class Stream(unittest.TestCase):
    def test_fragmented_paused_action_uses_captured_members_without_sampling(self):
        reader = Mock()
        stream = monitor.MonitorStream(reader)
        captured = [{"pid": 123, "started": 10}]
        action = json.dumps({"action": "end", "key": "app", "members": captured}).encode() + b"\n"
        with patch.object(monitor, "emit") as output:
            self.assertTrue(stream.consume(b'{"action":"pause","paused":true}\n' + action[:12]))
            self.assertTrue(stream.paused)
            self.assertTrue(stream.consume(action[12:]))
            stream.tick()
            reader.end.assert_called_once_with({"action": "end", "key": "app", "members": captured})
            reader.sample.assert_not_called()
            reader.reset_cpu.assert_not_called()
            self.assertEqual(output.call_count, 2)
            self.assertTrue(stream.consume(b'{"action":"pause","paused":false}\n'))
            reader.reset_cpu.assert_called_once()

    def test_actions_do_not_reset_the_next_sampling_deadline(self):
        reader = Mock()
        clock = {"now": 100}
        with patch.object(monitor.time, "monotonic", side_effect=lambda: clock["now"]), \
             patch.object(monitor, "emit"):
            stream = monitor.MonitorStream(reader)
            stream.tick()
            self.assertEqual(stream.deadline, 102)
            clock["now"] = 101
            stream.consume(b'{"action":"end","key":"app","members":[]}\n')
            stream.tick()
            self.assertEqual(reader.sample.call_count, 1)
            self.assertEqual(stream.deadline, 102)
            clock["now"] = 102
            stream.tick()
            self.assertEqual(reader.sample.call_count, 2)

    def test_eof_and_oversized_frames_stop_the_reader(self):
        self.assertFalse(monitor.MonitorStream(Mock()).consume(b""))
        self.assertFalse(monitor.MonitorStream(Mock()).consume(b"x" * (1024 * 1024 + 1)))


if __name__ == "__main__":
    unittest.main()
