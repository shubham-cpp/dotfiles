#!/usr/bin/env python3
"""Exercise production resource rows and rankings without a second shell."""
from pathlib import Path
from qml_test_support import QmlTestEnvironment

root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-resource-ui-")
temporary = stage.path
module = stage.module


try:
    tokens = (root / "Common/Tokens.qml").read_text().replace("import Quickshell\n", "").replace("Singleton {", "QtObject {", 1)
    icons = (root / "Common/Icons.qml").read_text().replace("import Quickshell", "import QtQuick").replace("Singleton {", "QtObject {", 1)
    module("qs.Common", [("Tokens", tokens, True), ("Icons", icons, True)] +
           [(name, (root / f"Common/{name}.qml").read_text(), False) for name in ("Glyph", "IconButton")])
    (temporary / "qs/Common/ResourceList.js").write_text((root / "Common/ResourceList.js").read_text())
    # IconImage is a pure QtQuick component; the QS plugin is linked into qs only.
    module("Quickshell.Widgets", [("IconImage", Path("/usr/lib/qt6/qml/Quickshell/Widgets/IconImage.qml").read_text(), False)])
    module("qs.Services", [("Resources", (root / "tests/qml/fixtures/Resources.qml").read_text(), True)])
    module("qs.Modules.bar", [(name, (root / f"Modules/bar/{name}.qml").read_text(), False)
                              for name in ("ResourceRow", "ResourceColumn", "NetworkButton")])
    result = stage.run(root / "tests/qml/tst_Resources.qml")
finally:
    stage.close()

raise SystemExit(result.returncode)
