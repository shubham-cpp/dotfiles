#!/usr/bin/env python3
"""Check launcher bindings and image jobs with synthetic process signals."""
from pathlib import Path

from qml_test_support import QmlTestEnvironment


ROOT = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-service-lifecycle-")
try:
    stage.module("Quickshell", [
        ("Singleton", "import QtQuick\nItem {}", False),
        ("Quickshell", '''pragma Singleton
import QtQuick
QtObject {
    property string cacheDir: "/test/cache"
    property string stateDir: "/test/state"
    property string dataDir: "/test/data"
    function shellPath(path) { return path; }
}''', True),
        ("DesktopEntries", '''pragma Singleton
import QtQuick
QtObject { property QtObject applications: QtObject { property var values: [] } }''', True)])
    stage.module("Quickshell.Io", [
        ("Process", (ROOT / "tests/qml/fixtures/Process.qml").read_text(), False),
        ("StdioCollector", 'import QtQuick\nQtObject { property string text: ""; signal streamFinished() }', False),
        ("FileView", '''import QtQuick
QtObject {
    property string path: ""
    property bool blockLoading: false
    property bool printErrors: false
    property string content: ""
    function text() { return content; }
    function setText(value) { content = value; }
}''', False),
        ("IpcHandler", 'import QtQuick\nQtObject { property string target: "" }', False)])
    stage.module("Quickshell.Services.Notifications", [("NotificationServer", '''import QtQuick
QtObject {
    property bool actionsSupported: false
    property bool actionIconsSupported: false
    property bool imageSupported: false
    property bool bodySupported: false
    property bool bodyMarkupSupported: false
    property bool persistenceSupported: false
    property bool inlineReplySupported: false
    property bool keepOnReload: false
    property var trackedNotifications: ({values: []})
    signal notification(var notification)
}''', False)])
    stage.module("qs.Common", [("Tokens", 'pragma Singleton\nimport QtQuick\nQtObject { property int toastCap: 4 }', True)])
    (stage.path / "qs/Common/NotificationPolicy.js").write_text((ROOT / "Common/NotificationPolicy.js").read_text())
    notifications = (ROOT / "Services/Notifications.qml").read_text().replace(
        "    id: root", "    id: root\n    property alias testWorker: imageWorker", 1)
    launcher = (ROOT / "Services/LauncherStats.qml").read_text().replace(
        "    id: root", "    id: root\n    property alias testStatsFile: statsFile\n    property alias testPinsFile: pinsFile", 1)
    stage.module("qs.Services", [
        ("Notifications", notifications, True),
        ("LauncherStats", launcher, True),
        ("Search", '''pragma Singleton
import QtQuick
QtObject {
    property int catalogs: 0
    property int queries: 0
    signal pending(string profile)
    signal resultsReady(string profile, var keys)
    signal failed(string profile, string message)
    function setCatalog(profile, rows) { catalogs++; }
    function search(profile, request) { queries++; }
    function release(profile) {}
}''', True)])
    result = stage.run(ROOT / "tests/qml/tst_ServiceLifecycle.qml")
finally:
    stage.close()
raise SystemExit(result.returncode)
