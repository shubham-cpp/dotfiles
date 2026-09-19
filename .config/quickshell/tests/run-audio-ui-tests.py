#!/usr/bin/env python3
"""Exercise audio controls with real QObjects, without another shell process."""
from pathlib import Path
from qml_test_support import QmlTestEnvironment

root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-audio-ui-")
temporary = stage.path
module = stage.module

try:
    tokens = (root / "Common/Tokens.qml").read_text().replace("import Quickshell\n", "").replace("Singleton {", "QtObject {", 1)
    icons = (root / "Common/Icons.qml").read_text().replace("import Quickshell", "import QtQuick").replace("Singleton {", "QtObject {", 1)
    module("qs.Common", [("Tokens", tokens, True), ("Icons", icons, True)] +
           [(name, (root / f"Common/{name}.qml").read_text(), False) for name in ("Glyph", "IconButton")])
    module("qs.Services", [("Audio", (root / "tests/qml/fixtures/Audio.qml").read_text(), True)])
    module("qs.Modules.bar", [(name, (root / f"Modules/bar/{name}.qml").read_text(), False)
                              for name in ("AudioVolumeControl", "AudioDeviceSelector")])
    result = stage.run(root / "tests/qml/tst_AudioControls.qml")
finally:
    stage.close()

raise SystemExit(result.returncode)
