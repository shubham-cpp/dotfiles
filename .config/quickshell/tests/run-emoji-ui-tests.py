#!/usr/bin/env python3
"""Exercise production emoji controls in Qt without another Quickshell."""
import json
from pathlib import Path
from qml_test_support import QmlTestEnvironment
import shutil

root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-emoji-ui-")
temporary = stage.path
module = stage.module


try:
    tokens = (root/"Common/Tokens.qml").read_text().replace("import Quickshell\n", "").replace("Singleton {", "QtObject {", 1)
    fixture = 'import QtQuick\nimport "EmojiCatalog.js" as Catalog\nQtObject { readonly property var catalog: Catalog.index(JSON.parse(' + json.dumps((root/".local/emoji-display.json").read_text()) + ')) }\n'
    module("qs.Common", [("Tokens", tokens, True), ("CatalogFixture", fixture, False)])
    shutil.copy2(root/"Common/EmojiCatalog.js", temporary/"qs/Common/EmojiCatalog.js")
    module("qs.Modules.emoji", [(name, (root/f"Modules/emoji/{name}.qml").read_text(), False)
                                for name in ("EmojiButton", "ToneSelect", "VariantChooser")])
    result = stage.run(root / "tests/qml/tst_EmojiVariants.qml")
finally:
    stage.close()

raise SystemExit(result.returncode)
