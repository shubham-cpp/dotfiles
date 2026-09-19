#!/usr/bin/env python3
"""Exercise task and rate delegate lifetimes without a second shell."""
from pathlib import Path

from qml_test_support import QmlTestEnvironment

root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-bar-lifetime-")
try:
    tokens = (root / "Common/Tokens.qml").read_text().replace("import Quickshell\n", "").replace("Singleton {", "QtObject {", 1)
    stage.module("qs.Common", [("Tokens", tokens, True),
        ("Icons", 'pragma Singleton\nimport QtQuick\nQtObject { property var glyphs: ({}); function fromAppId(id) { return ""; } }', True),
        ("Glyph", (root / "Common/Glyph.qml").read_text(), False)])
    stage.module("Quickshell.Widgets", [("IconImage", Path("/usr/lib/qt6/qml/Quickshell/Widgets/IconImage.qml").read_text(), False)])
    stage.module("Quickshell.Wayland", [("ToplevelManager", 'pragma Singleton\nimport QtQuick\nQtObject { property var toplevels: [] }', True)])
    stage.module("qs.Services", [
        ("Network", (root / "tests/qml/fixtures/BarNetwork.qml").read_text(), True),
        ("Agenda", '''pragma Singleton
import QtQuick
QtObject {
    property bool open: false
    property var anchorWindow: null
    function toggle(window) { anchorWindow = window; open = !open; return open; }
}''', True),
        ("Clock", 'pragma Singleton\nimport QtQuick\nQtObject { property string text: "1:00 PM  13 Sep" }', True)])
    stage.module("qs.Modules.bar", [(name, (root / f"Modules/bar/{name}.qml").read_text().replace("import Quickshell\n", ""), False)
                                  for name in ("TaskList", "NetworkStatus", "BarModule", "BarLabel", "ClockWidget")])
    result = stage.run(root / "tests/qml/tst_BarLifetime.qml")
finally:
    stage.close()
raise SystemExit(result.returncode)
