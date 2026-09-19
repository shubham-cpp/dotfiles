#!/usr/bin/env python3
"""Clipboard process regressions with synthetic input and stub executables."""
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]


def load(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / "scripts" / (name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


WATCH = load("clipboard-watch")
CONTENT = load("clipboard-content")


class WatchTests(unittest.TestCase):
    def test_new_watcher_ignores_initial_callback_before_reading_or_focusing(self):
        directory = Path(tempfile.mkdtemp(prefix="qs-clipboard-start-test-", dir=Path.home() / ".cache"))
        record = directory / "quickshell-clipboard-watch.json"
        try:
            for initial_state in ("data", "nil", "sensitive"):
                for saved in ({}, {"watcher": 99, "id": "14"}):
                    with self.subTest(state=initial_state, saved=saved):
                        record.write_text(json.dumps(saved))
                        with patch.dict(WATCH.os.environ, {"XDG_RUNTIME_DIR": str(directory), "CLIPBOARD_STATE": initial_state}), \
                                patch.object(WATCH.os, "getppid", return_value=100), \
                                patch.object(WATCH.sys, "stdin") as stdin, \
                                patch.object(WATCH, "focused_app", return_value="editor") as focus, \
                                patch.object(WATCH, "handle", return_value="55") as handle:
                            stdin.buffer.read.side_effect = AssertionError("Initial offer must not be read")
                            WATCH.main()
                            focus.assert_not_called()
                            handle.assert_not_called()
                            self.assertEqual(json.loads(record.read_text()), {"watcher": 100, "id": None})
                            WATCH.os.environ["CLIPBOARD_STATE"] = "data"
                            stdin.buffer.read.side_effect = None
                            stdin.buffer.read.return_value = b"synthetic new copy"
                            WATCH.main()
                            focus.assert_called_once()
                            handle.assert_called_once_with("data", b"synthetic new copy", None, "editor")
                            self.assertEqual(json.loads(record.read_text()), {"watcher": 100, "id": "55"})
        finally:
            subprocess.run(["gio", "trash", str(directory)], check=True, stdout=subprocess.DEVNULL)

    def test_clear_evicts_only_known_last_id(self):
        for state in ("nil", "clear", "data"):
            with self.subTest(state=state), patch.object(WATCH, "run_cliphist") as run:
                self.assertIsNone(WATCH.handle(state, b"", "14", ""))
                run.assert_called_once_with("delete", b"14\n")
                run.reset_mock()
                self.assertIsNone(WATCH.handle(state, b"", None, ""))
                run.assert_not_called()

    def test_sensitive_unknown_and_ignored_apps_do_not_store(self):
        for state, app in [("sensitive", ""), ("unavailable", ""), ("data", "keepassxc"), ("data", "com.bitwarden.desktop")]:
            with self.subTest(state=state, app=app), patch.object(WATCH, "run_cliphist") as run:
                self.assertIsNone(WATCH.handle(state, b"test secret", "14", app))
                run.assert_not_called()

    def test_store_records_only_id(self):
        with patch.object(WATCH, "run_cliphist", side_effect=[b"", b"22\ttest content\n21\tolder\n", b"test content"]) as run:
            self.assertEqual(WATCH.handle("data", b"test content", None, "editor"), "22")
            self.assertEqual(run.call_args_list[0].args, ("store", b"test content"))

    def test_skipped_store_does_not_assign_an_older_item_to_the_new_offer(self):
        with patch.object(WATCH, "run_cliphist", side_effect=[b"", b"22\tolder\n", b"older"]):
            self.assertIsNone(WATCH.handle("data", b"new", None, "editor"))

    def test_focus_uses_mango_appid_and_rejects_unknown_shape(self):
        result = subprocess.CompletedProcess([], 0, b'{"appid":"org.keepassxc.KeePassXC"}')
        with patch.object(WATCH.subprocess, "run", return_value=result):
            self.assertEqual(WATCH.focused_app(), "org.keepassxc.keepassxc")
            result.stdout = b'null'
            with self.assertRaises(ValueError):
                WATCH.focused_app()


class ContentTests(unittest.TestCase):
    def setUp(self):
        self.directory = Path(tempfile.mkdtemp(prefix="qs-clipboard-test-", dir=Path.home() / ".cache"))
        self.bin = self.directory / "bin"
        self.bin.mkdir()
        self.pin_dir = self.directory / "pins"
        self.copied = self.directory / "copied"
        self.stub("cliphist", "import os,sys\nsys.stdout.buffer.write(b'test-content')\nsys.exit(int(os.environ.get('DECODE_STATUS','0')))\n")
        self.stub("wl-copy", "import os,pathlib,sys\npathlib.Path(os.environ['COPY_DEST']).write_bytes(sys.stdin.buffer.read())\n")
        self.env = {**os.environ, "PATH": str(self.bin) + ":" + os.environ["PATH"], "COPY_DEST": str(self.copied)}

    def tearDown(self):
        subprocess.run(["gio", "trash", str(self.directory)], check=True, stdout=subprocess.DEVNULL)

    def stub(self, name, body):
        file = self.bin / name
        file.write_text("#!/usr/bin/python3\n" + body)
        file.chmod(0o700)

    def invoke(self, action, source="clip", identifier="22", destination="", **env):
        return subprocess.run(["/usr/bin/python3", "-I", str(ROOT / "scripts/clipboard-content.py"), action,
                               source, identifier, str(self.pin_dir), destination],
                              env={**self.env, **env}, capture_output=True, timeout=10)

    def test_failed_decode_never_invokes_copy(self):
        result = self.invoke("copy", DECODE_STATUS="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(self.copied.exists())
        self.assertNotIn(b"test-content", result.stderr)

    def test_successful_copy_passes_exact_bytes_after_decode(self):
        self.assertEqual(self.invoke("copy").returncode, 0)
        self.assertEqual(self.copied.read_bytes(), b"test-content")

    def test_pin_symlink_and_traversal_cannot_be_copied(self):
        self.pin_dir.mkdir()
        (self.pin_dir / "p_22").symlink_to(self.directory / "outside")
        (self.directory / "outside").write_bytes(b"untrusted")
        for identifier in ("p_22", "../outside"):
            self.assertNotEqual(self.invoke("copy", "pin", identifier).returncode, 0)
        self.assertFalse(self.copied.exists())

    def test_pin_is_exclusive_and_images_reuse_bounded_slots(self):
        self.assertEqual(self.invoke("pin", destination="p_22").returncode, 0)
        self.assertNotEqual(self.invoke("pin", destination="p_22").returncode, 0)
        cache = self.directory / "cache"
        for i in range(20):
            self.assertEqual(self.invoke("image", destination=str(cache / str(i % 16))).returncode, 0)
        self.assertEqual(len(list(cache.iterdir())), 16)
        self.assertNotEqual(self.invoke("image", destination=str(cache / "16")).returncode, 0)

    def test_watch_store_clear_and_sensitive_offer_with_isolated_real_cliphist(self):
        self.stub("mmsg", 'print(\'{"appid":"editor"}\')\n')
        database = str(self.directory / "history-db")
        self.stub("cliphist", "import os,sys\nos.execv('/usr/bin/cliphist', ['/usr/bin/cliphist', '-db-path', "
                  + repr(database) + ", '-config-path', '/dev/null'] + sys.argv[1:])\n")
        def event(state, content=b""):
            result = subprocess.run(["/usr/bin/python3", "-I", str(ROOT / "scripts/clipboard-watch.py")],
                                    input=content, capture_output=True, timeout=10,
                                    env={**self.env, "XDG_RUNTIME_DIR": str(self.directory), "CLIPBOARD_STATE": state})
            self.assertEqual(result.returncode, 0, result.stderr.decode())
        def listing():
            return subprocess.run([str(self.bin / "cliphist"), "list"], env=self.env,
                                  capture_output=True, check=True).stdout
        event("nil")
        event("data", b"synthetic first")
        self.assertIn(b"synthetic first", listing())
        state_file = self.directory / "quickshell-clipboard-watch.json"
        self.assertNotIn("synthetic", state_file.read_text())
        event("sensitive", b"synthetic password")
        event("nil")
        self.assertIn(b"synthetic first", listing())
        self.assertNotIn(b"password", listing())
        event("data", b"synthetic second")
        event("nil")
        self.assertNotIn(b"synthetic second", listing())
        self.assertIn(b"synthetic first", listing())
        event("nil")
        self.assertIn(b"synthetic first", listing())

    def test_large_decode_cannot_reach_copy(self):
        self.stub("cliphist", "import sys\nsys.stdout.buffer.write(b'x' * 5000001)\n")
        self.assertNotEqual(self.invoke("copy").returncode, 0)
        self.assertFalse(self.copied.exists())


if __name__ == "__main__":
    unittest.main()
