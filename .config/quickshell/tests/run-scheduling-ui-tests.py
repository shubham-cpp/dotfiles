#!/usr/bin/env python3
"""Exercise production scheduling singleton bindings with external IO replaced."""
from pathlib import Path
from qml_test_support import QmlTestEnvironment

root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-scheduling-ui-")
try:
    stage.module("Quickshell", [
        ("Singleton", "import QtQuick\nItem {}", False),
        ("Quickshell", '''pragma Singleton
import QtQuick
QtObject {
    property string shellDir: "/test"
    property string cacheDir: "/test/cache"
    property string dataDir: "/test/data"
    property var commands: []
    property bool failNextNotification: false
    function shellPath(path) { return "/test/" + path; }
    function env(name) { return ""; }
    function execDetached(command) { commands = commands.concat([command]); }
}''', True)])
    stage.module("Quickshell.Io", [
        ("FileView", '''import QtQuick
Item {
    property string path
    property bool blockLoading
    property bool printErrors
    function text() { return ""; }
    function setText(value) {}
}''', False),
        ("Process", '''import QtQuick
import Quickshell
Item {
    property bool running: false
    property var command: []
    property QtObject stdout: null
    property QtObject stderr: null
    signal exited(int exitCode, int exitStatus)
    onRunningChanged: if (running) {
        Quickshell.execDetached(command);
        if (command[1] === "parse") return;
        const fail = command[1] === "notify" && Quickshell.failNextNotification;
        if (fail) Quickshell.failNextNotification = false;
        Qt.callLater(() => {
            if (!fail) exited(0, 0);
            running = false;
        });
    }
    function finish(output, message, code, outputFirst) {
        if (outputFirst) {
            stdout.text = output; stdout.streamFinished();
            stderr.text = message; stderr.streamFinished();
        }
        exited(code, 0);
        running = false;
        if (!outputFirst) {
            stdout.text = output; stdout.streamFinished();
            stderr.text = message; stderr.streamFinished();
        }
    }
}''', False),
        ("StdioCollector", 'import QtQuick\nQtObject { property string text: ""; signal streamFinished() }', False),
        ("IpcHandler", 'import QtQuick\nItem { property string target }', False)])
    reminders = (root / "Services/Reminders.qml").read_text().replace(
        "    id: root", "    id: root\n    property alias testCommandParser: commandParser", 1)
    stage.module("qs.Services", [
        ("Reminders", reminders, True),
        ("Football", (root / "Services/Football.qml").read_text(), True),
        ("Notifications", 'pragma Singleton\nimport QtQuick\nQtObject { signal reminderAction(string reminderId, string action) }', True),
        ("Logind", 'pragma Singleton\nimport QtQuick\nQtObject { property bool preparingForSleep: false }', True),
        ("Agenda", 'pragma Singleton\nimport QtQuick\nQtObject { property string tab: ""; property bool open: false }', True)])
    result = stage.run(root / "tests/qml/tst_Scheduling.qml")
finally:
    stage.close()
raise SystemExit(result.returncode)
