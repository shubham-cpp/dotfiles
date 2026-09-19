pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    readonly property bool ready: true
    property bool present: false
    property bool running: false
    property bool starting: false
    property int pid: 0
    property int argvPid: 0
    property var command: ["wlsunset", "-l", "18.5204", "-L", "73.8567", "-T", "5800", "-t", "2700"]
    readonly property bool enabled: running || starting

    function refresh() {
        if (!probe.running)
            probe.running = true;
        if (!which.running)
            which.running = true;
    }

    function handlePid(text) {
        root.starting = false;
        const n = parseInt(String(text).trim(), 10);
        if (!(n > 0)) {
            root.pid = 0;
            root.running = false;
            return;
        }
        root.pid = n;
        root.running = true;
        root.present = true;
        if (root.argvPid !== n && !argvReader.running) {
            argvReader.command = ["sh", "-c", "tr '\\0' '\\n' < /proc/" + n + "/cmdline"];
            argvReader.running = true;
        }
    }

    function handleArgv(text) {
        const parts = String(text).split("\n").map(s => s.trim()).filter(s => s.length);
        if (parts.length && parts[0].indexOf("wlsunset") !== -1) {
            root.command = parts;
            root.argvPid = root.pid;
        }
    }

    function handleWhich(text, code) {
        if (String(text).trim().length)
            root.present = true;
        else if (code !== 0 && !root.running)
            root.present = false;
    }

    function start() {
        if (root.running || root.starting || !root.present)
            return false;
        root.starting = true;
        Quickshell.execDetached(root.command);
        afterStart.restart();
        return true;
    }

    function stop() {
        if (killer.running)
            return false;
        killer.command = ["pkill", "-TERM", "-x", "wlsunset"];
        killer.running = true;
        root.running = false;
        root.pid = 0;
        root.starting = false;
        return true;
    }

    function setEnabled(on) {
        if (on)
            start();
        else
            stop();
    }

    function toggle() {
        setEnabled(!root.enabled);
        return root.enabled;
    }

    IpcHandler {
        target: "nightlight"

        function toggle(): bool {
            return root.toggle();
        }

        function set(on: bool): void {
            root.setEnabled(on);
        }

        function status(): string {
            return JSON.stringify({
                present: root.present,
                running: root.running,
                starting: root.starting,
                pid: root.pid,
                command: root.command
            });
        }
    }

    Process {
        id: which
        command: ["sh", "-c", "command -v wlsunset"]
        stdout: StdioCollector {
            onStreamFinished: root.handleWhich(this.text, 0)
        }
        onExited: (exitCode, exitStatus) => {
            if (exitStatus !== 0 || exitCode !== 0)
                root.handleWhich("", exitCode);
        }
    }

    Process {
        id: probe
        command: ["pidof", "-s", "wlsunset"]
        stdout: StdioCollector {
            onStreamFinished: root.handlePid(this.text)
        }
        onExited: (exitCode, exitStatus) => {
            if (exitStatus !== 0 || exitCode !== 0)
                root.handlePid("");
        }
    }

    Process {
        id: argvReader
        stdout: StdioCollector {
            onStreamFinished: root.handleArgv(this.text)
        }
    }

    Process {
        id: killer
        onExited: root.refresh()
        onRunningChanged: {
            if (!running && !root.running)
                Qt.callLater(root.refresh);
        }
    }

    Timer {
        id: afterStart
        interval: 400
        repeat: false
        onTriggered: root.refresh()
    }

    Component.onCompleted: root.refresh()
}
