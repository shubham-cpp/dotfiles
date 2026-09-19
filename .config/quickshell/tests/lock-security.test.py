import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

BASE = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location("bridge", BASE / "scripts/session-bridge.py")
bridge = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bridge)


class SecurityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = Path(tempfile.mkdtemp(prefix="qs-pam-tests-"))
        cls.binary = cls.build / "auth-fixture"
        subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror", str(BASE / "scripts/lock-auth.c"),
                        str(BASE / "tests/pam-fixture.c"), "-o", str(cls.binary)], check=True)

    def run_auth(self, mode, text="fixture\n"):
        return subprocess.run([str(self.binary)], input=text, text=True, capture_output=True,
                              env={**os.environ, "QS_TEST_CASE": mode, "USER": "wrong-user"}, timeout=3)

    def test_account_policy_must_pass(self):
        success = self.run_auth("success")
        self.assertEqual((success.returncode, success.stdout.strip()), (0, "QS_AUTH_SUCCESS"))
        account = self.run_auth("account-fail")
        self.assertNotEqual(account.returncode, 0)
        self.assertNotIn("QS_AUTH_SUCCESS", account.stdout)

    def test_failure_paths_never_report_success(self):
        for mode in ["auth-fail", "start-fail", "multi-prompt"]:
            with self.subTest(mode=mode):
                result = self.run_auth(mode)
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("QS_AUTH_SUCCESS", result.stdout)

    def test_empty_unterminated_nul_and_oversize_password(self):
        for text in ["\n", "", "fixture", "bad\0secret\n", "x" * 4096 + "\n"]:
            with self.subTest(length=len(text)):
                result = self.run_auth("success", text)
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("QS_AUTH_SUCCESS", result.stdout)

    def test_sleep_gate_requires_fresh_secure_ack(self):
        released = []
        gate = bridge.SleepGate(lambda: released.append(True))
        old = gate.token
        current = gate.begin(True)
        self.assertFalse(gate.accept(old, True))
        self.assertTrue(gate.accept(current, False))
        self.assertEqual(released, [])
        self.assertTrue(gate.accept(current, True))
        self.assertEqual(released, [True])
        gate.begin(False)
        self.assertFalse(gate.accept(current, True))
        self.assertTrue(gate.accept(gate.token, True))
        self.assertEqual(released, [True])

    def bridge_fixture(self):
        instance = bridge.Bridge.__new__(bridge.Bridge)
        instance.input_buffer = b""
        instance.session = "/session/test"
        instance.session_id = "test"
        instance.marker = self.build / "requested"
        events = []
        instance.gate = bridge.SleepGate(lambda: events.append("released"))
        instance.gate.begin(True)
        instance.call = lambda *args: events.append(("hint", args[-1][0]))
        instance.emit = lambda *args: events.append(args)
        instance.loop = type("Loop", (), {"quit": lambda _: events.append("quit")})()
        return instance, events

    def test_hint_completes_before_delay_release_and_intent_survives(self):
        instance, events = self.bridge_fixture()
        instance.accept_state(json.dumps(dict(token="stale", secure=True, requested=True)))
        self.assertEqual(events, [])
        instance.accept_state(json.dumps(dict(token=instance.gate.token, secure=True, requested=True)))
        self.assertEqual(events, [("hint", True), "released"])
        self.assertEqual(instance.marker.read_text(), "test")

    def test_failed_hint_keeps_delay_held(self):
        instance, events = self.bridge_fixture()
        def fail(*_):
            raise OSError("test hint failure")
        instance.call = fail
        instance.accept_state(json.dumps(dict(token=instance.gate.token, secure=True, requested=True)))
        self.assertNotIn("released", events)

    def test_coalesced_and_fragmented_pipe_messages_are_drained(self):
        instance, events = self.bridge_fixture()
        def line(secure):
            return json.dumps(dict(token=instance.gate.token, secure=secure, requested=True)).encode() + b"\n"
        payload = line(False) + line(True)
        with patch.object(bridge.os, "read", side_effect=[payload[:10], payload[10:]]):
            self.assertTrue(instance.input_event(None, bridge.GLib.IO_IN))
            self.assertEqual(events, [])
            self.assertTrue(instance.input_event(None, bridge.GLib.IO_IN))
        self.assertEqual(events, [("hint", False), ("hint", True), "released"])
        self.assertEqual(instance.input_buffer, b"")


if __name__ == "__main__":
    unittest.main()
