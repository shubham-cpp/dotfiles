"""Native Qt UI checks, with no Quickshell process or live session lock."""
from pathlib import Path
from qml_test_support import QmlTestEnvironment

root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-lock-qml-")
try:
    sources = []
    for name in ("Tokens", "Icons", "Glyph", "BatteryIcon"):
        source = (root / f"Common/{name}.qml").read_text()
        source = source.replace("import Quickshell", "import QtQuick").replace("Singleton {", "QtObject {")
        sources.append((name, source, name not in ("Glyph", "BatteryIcon")))
    stage.module("qs.Common", sources)
    result = stage.run(root / "tests/qml/tst_LockContent.qml")
finally:
    stage.close()
raise SystemExit(result.returncode)
