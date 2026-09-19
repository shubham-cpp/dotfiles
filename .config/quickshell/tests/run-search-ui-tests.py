#!/usr/bin/env python3
"""Exercise the production Search state machine with asynchronous fake pipes.

The fake only tests transport state and lifetime, not Go matching or Qt's actual
pipe implementation. --real-transport instead imports the real Quickshell.Io
plugin and runs qs-search. That mode requires a loadable Io plugin, unavailable
when Quickshell links its plugins only into the qs executable.
"""
import argparse
import json
from pathlib import Path

from qml_test_support import QmlTestEnvironment


root = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--real-transport", action="store_true")
args = parser.parse_args()
binary = root / ".local/bin/qs-search"
if args.real_transport and not binary.is_file():
    raise SystemExit("Build .local/bin/qs-search before running search UI tests")

stage = QmlTestEnvironment("qs-search-ui-")
try:
    source = (root / "Services/Search.qml").read_text()
    source = source.replace("Singleton {", "Item {", 1)
    source = source.replace("    id: root\n", """    id: root
    property alias testWorker: worker
    property alias testWatchdog: watchdog
    property alias testRetry: retry
""", 1)
    shell = """pragma Singleton
import QtQuick
QtObject {
    function shellPath(relative) { return ROOT + "/" + relative; }
}
""".replace("ROOT", json.dumps(str(root)))
    stage.module("Quickshell", [("Quickshell", shell, True)])
    if not args.real_transport:
        # Deterministic peer for lifecycle tests. Matching here deliberately
        # handles only these fixtures; Go's own suite verifies ranking rules.
        process = r'''import QtQuick
Item {
    id: root
    property bool running: false
    property bool stdinEnabled: false
    property var command: []
    property QtObject stdout: null
    property QtObject stderr: null
    property int processId: 0
    property int serial: 0
    property string instance: ""
    property var catalogs: ({})
    property var writes: []
    signal exited(int exitCode, int exitStatus)

    onRunningChanged: {
        if (running) {
            serial++;
            const generation = serial;
            processId = serial;
            instance = "fake-" + serial;
            catalogs = ({});
            Qt.callLater(() => {
                if (root.running && root.serial === generation)
                    root.stdout.read(JSON.stringify({v: 1, type: "ready", instance: root.instance}));
            });
        } else {
            const generation = serial;
            Qt.callLater(() => {
                if (!root.running && root.serial === generation) {
                    root.processId = 0;
                    root.exited(0, 0);
                }
            });
        }
    }

    function write(line) {
        const message = JSON.parse(line);
        writes = writes.concat([message]);
        const generation = serial;
        Qt.callLater(() => {
            if (!root.running || root.serial !== generation) return;
            const response = {v: 1, type: message.type, instance: root.instance,
                request: message.request, profile: message.profile,
                epoch: message.epoch, revision: message.revision};
            if (message.type === "begin") root.catalogs[message.profile] = [];
            else if (message.type === "chunk") {
                root.catalogs[message.profile] = root.catalogs[message.profile].concat(message.rows);
            } else if (message.type === "release") delete root.catalogs[message.profile];
            else if (message.type === "search") {
                const query = (message.query || "").toLowerCase();
                response.type = "results";
                response.keys = root.catalogs[message.profile]
                    .filter(row => String(row.text || row.name).toLowerCase().indexOf(query) !== -1)
                    .map(row => row.key);
            }
            root.stdout.read(JSON.stringify(response));
        });
    }

    function signal(number) { if (number === 9 || number === 15) running = false; }
}
'''
        stage.module("Quickshell.Io", [
            ("Process", process, False),
            ("SplitParser", "import QtQuick\nQtObject { signal read(string data) }\n", False),
            ("IpcHandler", "import QtQuick\nQtObject { property string target: \"\" }\n", False),
        ])
    print("Search UI transport: " + ("real Quickshell.Io" if args.real_transport else
          "FAKE asynchronous Process/SplitParser; state-machine coverage only"), flush=True)
    stage.module("qs.Services", [("Search", source, True)])
    result = stage.run(root / "tests/qml/tst_Search.qml")
finally:
    stage.close()

raise SystemExit(result.returncode)
