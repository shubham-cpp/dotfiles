import QtQuick
import QtTest
import Quickshell
import qs.Services

TestCase {
    name: "Scheduling"

    function cleanup() {
        Reminders.items = [];
        Reminders.retryAfter = 0;
        Reminders.commandJob = null;
        Reminders.error = "";
        Reminders.schedule();
        Quickshell.commands = [];
    }

    function test_native_reminder_action_connection() {
        Reminders.items = [{ id: "r_test", title: "test", fired: true, at: "2026-01-01T00:00:00Z" }];
        Notifications.reminderAction("r_test", "snooze");
        compare(Reminders.items[0].fired, false);
        verify(Math.abs(Date.parse(Reminders.items[0].at) - Date.now() - 600000) < 1000);
        Notifications.reminderAction("r_test", "done");
        compare(Reminders.items.length, 1);
    }

    function test_resume_connection_fires_overdue_reminder() {
        Logind.preparingForSleep = true;
        Reminders.items = [{ id: "r_test", title: "test", fired: false, urgent: false, at: "2026-01-01T00:00:00Z" }];
        Quickshell.commands = [];
        Logind.preparingForSleep = false;
        compare(Reminders.items[0].fired, false);
        tryVerify(() => Reminders.items[0].fired);
        verify(Quickshell.commands.some(command => command[1] === "notify"));
    }

    function test_qt_date_parsing_and_football_noop_binding() {
        compare(Reminders.parseLocal("2026-09-12T18:30:00Z"), "2026-09-13T00:00:00+05:30");
        compare(Reminders.parseLocal("2026-02-30 12:00"), "");
        const before = Football.gen;
        Football.setChip(Football.chip);
        compare(Football.gen, before);
    }

    function test_failed_notification_start_preserves_due_item_and_allows_retry() {
        Quickshell.failNextNotification = true;
        Reminders.items = [{ id: "r_fail", title: "test", fired: false, urgent: false, at: "2026-01-01T00:00:00Z" }];
        Reminders.fire();
        tryCompare(Reminders, "delivery", null);
        verify(!Reminders.items[0].fired);
        verify(Reminders.error.length > 0);
        verify(Reminders.retryAfter > Date.now());
        Reminders.retryAfter = 0;
        Reminders.fire();
        tryVerify(() => Reminders.items[0].fired);
        compare(Reminders.error, "");
    }

    function test_remind_command_parser_adds_the_validated_payload() {
        verify(Reminders.addCommand('--urgent "tomorrow 3pm" "Call Mum" "Ask about the appointment"'));
        verify(Reminders.commandBusy);
        Reminders.testCommandParser.finish(JSON.stringify({
            title: "Call Mum",
            description: "Ask about the appointment",
            at: "2099-09-15T15:00:00+05:30",
            urgent: true
        }), "", 0, false);
        compare(Reminders.commandBusy, false);
        compare(Reminders.items.length, 1);
        compare(Reminders.items[0].title, "Call Mum");
        compare(Reminders.items[0].description, "Ask about the appointment");
        compare(Reminders.items[0].urgent, true);
    }

    function test_remind_command_parser_reports_script_errors() {
        verify(Reminders.addCommand('10m "unfinished'));
        Reminders.testCommandParser.finish("", "Error: Could not read command: No closing quotation\n", 1, true);
        compare(Reminders.items.length, 0);
        compare(Reminders.error, "Could not read command: No closing quotation");
    }
}
