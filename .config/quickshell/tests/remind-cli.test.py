import json
import runpy
import subprocess
from datetime import datetime
from pathlib import Path
import unittest
from zoneinfo import ZoneInfo


SCRIPT = Path.home() / ".local/bin/myscripts/remind"
REMIND = runpy.run_path(SCRIPT)
IST = ZoneInfo("Asia/Kolkata")


class ReminderCommandParsing(unittest.TestCase):
    def test_gui_line_uses_the_same_arguments_as_the_cli(self):
        now = datetime(2026, 9, 14, 10, 0, tzinfo=IST)
        parsed = REMIND["parse_command_line"](
            'remind --urgent "tomorrow 3pm" "Call Mum" "Ask about the appointment"',
            now,
        )
        self.assertEqual(
            parsed,
            {
                "title": "Call Mum",
                "description": "Ask about the appointment",
                "at": "2026-09-15T15:00:00+05:30",
                "urgent": True,
            },
        )

    def test_parse_subcommand_is_json_and_has_no_scheduling_side_effect(self):
        result = subprocess.run(
            [SCRIPT, "parse", '10m "Take a break"'],
            check=True,
            capture_output=True,
            text=True,
        )
        parsed = json.loads(result.stdout)
        self.assertEqual(parsed["title"], "Take a break")
        self.assertFalse(parsed["urgent"])
        self.assertGreater(datetime.fromisoformat(parsed["at"]), datetime.now(IST))

    def test_unclosed_quotes_and_empty_titles_are_rejected(self):
        with self.assertRaisesRegex(REMIND["RemindError"], "No closing quotation"):
            REMIND["parse_command_line"]('10m "unfinished')
        with self.assertRaisesRegex(REMIND["RemindError"], "Title cannot be empty"):
            REMIND["parse_command_line"]('10m ""')


if __name__ == "__main__":
    unittest.main()
