import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location("reminder_delivery", Path(__file__).parents[1] / "scripts/reminder-delivery.py")
delivery = importlib.util.module_from_spec(spec)
spec.loader.exec_module(delivery)


class Notification(unittest.TestCase):
    def test_actions_and_identity_are_on_the_native_notification(self):
        args = delivery.notification_args("r_1", "Title ' & <", "2026-09-12T18:30:00Z", "normal").unpack()
        self.assertEqual(args[3], "Title ' & <")
        self.assertEqual(args[4], "00:00 IST  13 Sep")
        self.assertEqual(args[5], ["snooze", "Snooze 10m", "done", "Done"])
        self.assertEqual(args[6]["x-quickshell-reminder-id"], "r_1")
        self.assertEqual(args[6]["urgency"], 1)

    def test_offline_alert_has_no_unusable_actions(self):
        args = delivery.notification_args("", "Reminder", "", "critical").unpack()
        self.assertEqual(args[5], [])
        self.assertNotIn("x-quickshell-reminder-id", args[6])


if __name__ == "__main__":
    unittest.main()
