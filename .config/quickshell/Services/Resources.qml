pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    readonly property bool ready: true
    property bool open: false
    property bool paused: false
    property bool available: false
    property var apps: []
    property string errorText: ""
    property var anchorWindow: null
    property var anchorItem: null

    function toggle(win, item) {
        if (open) {
            close();
            return false;
        }
        if (Lock.locked)
            return false;
        Brightness.close();
        anchorWindow = win || null;
        anchorItem = item || null;
        errorText = "";
        apps = [];
        available = false;
        paused = false;
        open = true;
        return true;
    }

    function close() {
        open = false;
        anchorWindow = null;
        anchorItem = null;
        apps = [];
        available = false;
        paused = false;
    }

    function togglePause() {
        if (!available || !reader.running)
            return;
        paused = !paused;
        reader.write(JSON.stringify({
            action: "pause",
            paused: paused
        }) + "\n");
    }

    function endApplication(key, members, force) {
        if (!open || !available || !reader.running)
            return;
        if (paused)
            togglePause();
        errorText = "";
        reader.write(JSON.stringify({
            action: "end",
            key: key,
            members: JSON.parse(members),
            force: force
        }) + "\n");
    }

    function handleLine(line) {
        if (!open)
            return;
        let message;
        try {
            message = JSON.parse(line);
        } catch (_) {
            return;
        }
        if (message.event === "sample" && Array.isArray(message.apps)) {
            apps = message.apps;
            available = true;
        } else if (message.event === "error") {
            available = false;
            apps = [];
            errorText = message.message;
        } else if (message.event === "action") {
            errorText = message.message || "";
        }
    }

    // One stream while open; closing the popup stops all application sampling.
    Process {
        id: reader
        running: root.open
        stdinEnabled: true
        command: [Quickshell.shellPath(".local/bin/qs-resources")]
        stdout: SplitParser {
            onRead: data => root.handleLine(data)
        }
        onExited: {
            if (root.open) {
                root.available = false;
                root.apps = [];
                root.errorText = "Application monitor stopped. Close and reopen to retry.";
            }
        }
    }

    Connections {
        target: Lock
        function onLockedChanged() {
            if (Lock.locked)
                root.close();
        }
    }

    IpcHandler {
        target: "resources"
        function toggle(): bool {
            return root.toggle(null, null);
        }
        function close(): void {
            root.close();
        }
        function status(): string {
            return JSON.stringify({
                open: root.open,
                available: root.available,
                paused: root.paused,
                applications: root.apps.length,
                error: root.errorText
            });
        }
    }
}
