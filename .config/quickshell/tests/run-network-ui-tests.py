#!/usr/bin/env python3
"""Run actual Wi-Fi controls against a fake service without starting a shell."""
from pathlib import Path
from qml_test_support import QmlTestEnvironment

root = Path(__file__).resolve().parent.parent
stage = QmlTestEnvironment("qs-network-ui-")
temporary = stage.path
module = stage.module
status = 0


try:
    # Use production tokens without importing the Quickshell runtime singleton.
    tokens = (root / "Common/Tokens.qml").read_text()
    tokens = tokens.replace("import Quickshell\n", "").replace("Singleton {", "QtObject {", 1)
    module("qs.Common", [("Tokens", tokens, True)])
    module("qs.Services", [("Network", (root / "tests/qml/fixtures/Network.qml").read_text(), True)])
    module("qs.Modules.bar", [(name, (root / f"Modules/bar/{name}.qml").read_text(), False)
                               for name in ("NetworkPassword", "NetworkButton")])
    for test in ("tst_NetworkList.qml", "tst_NetworkPassword.qml"):
        result = stage.run(root / "tests/qml" / test)
        status = status or result.returncode

finally:
    stage.close()

raise SystemExit(status)
