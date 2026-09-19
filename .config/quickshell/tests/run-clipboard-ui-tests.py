#!/usr/bin/env python3
"""Run production clipboard bindings with synthetic process completions."""
from pathlib import Path
from qml_test_support import QmlTestEnvironment

ROOT = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-clipboard-qml-")
temporary = stage.path
module = stage.module


try:
    module("Quickshell", [("Quickshell", '''pragma Singleton
import QtQuick
QtObject {
    property string dataDir: "/synthetic/data"
    property string cacheDir: "/synthetic/cache"
    function shellPath(path) { return path; }
    function execDetached(command) {}
}''', True)])
    module("Quickshell.Io", [
        ("Process", (ROOT / "tests/qml/fixtures/Process.qml").read_text(), False),
        ("StdioCollector", '''import QtQuick
QtObject { property string text: ""; signal streamFinished() }''', False),
        ("FileView", '''import QtQuick
QtObject {
    property string path: ""
    property bool blockLoading: false
    property bool printErrors: false
    property string content: "[]"
    function text() { return content; }
    function setText(value) { content = value; }
}''', False),
        ("IpcHandler", '''import QtQuick
QtObject { property string target: "" }''', False)])
    source = (ROOT / "Services/Clipboard.qml").read_text().replace("Singleton {", "Item {", 1)
    source = source.replace("    id: root", '''    id: root
    property alias testLister: lister
    property alias testDecoder: decoder
    property alias testCopier: copier
    property alias testPinner: pinner
    property alias testPinsFile: pinsFile''', 1)
    module("qs.Services", [("Clipboard", source, True), ("Search", '''pragma Singleton
import QtQuick
import "../Common/Fuzzy.js" as Fuzzy
QtObject {
    property var rows: []
    signal pending(string profile)
    signal resultsReady(string profile, var keys)
    signal failed(string profile, string message)
    function setCatalog(profile, records) { rows = records; pending(profile); }
    function release(profile) { rows = []; }
    function search(profile, request) {
        pending(profile);
        const matches = rows.map((row, index) => ({row: row, index: index})).filter(item => {
            const row = item.row, f = request.filter;
            if (f === "pinned" && !row.pinned) return false;
            if (f === "text" && ["image", "link"].includes(row.kind)) return false;
            if (["image", "link"].includes(f) && row.kind !== f) return false;
            return !!Fuzzy.scoreMultiTokenAND(request.query, row.text);
        });
        resultsReady(profile, matches.slice(0, 40).map(item => String(item.index)));
    }
}''', True)])
    common = temporary / "qs/Common"
    common.mkdir(parents=True)
    (common / "ClipboardFormat.js").write_text((ROOT / "Common/ClipboardFormat.js").read_text())
    # Only this synthetic Qt adapter uses the frozen JavaScript oracle.
    (common / "Fuzzy.js").write_text((ROOT / "tests/reference/Fuzzy.js").read_text())
    result = stage.run(ROOT / "tests/qml/tst_ClipboardLifecycle.qml")
finally:
    stage.close()

raise SystemExit(result.returncode)
