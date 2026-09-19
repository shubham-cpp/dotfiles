pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

// Phase A: caffeine only. Do not add IdleMonitor lock.
// Wayland half is IdleInhibitor on the bar. Logind half is this child process;
// QML cannot hold the inhibit fd.
Singleton {
    id: root

    property bool enabled: false
    readonly property bool ready: true

    function toggle(): bool {
        enabled = !enabled;
        return enabled;
    }

    function set(on: bool): void {
        enabled = on;
    }

    IpcHandler {
        target: "caffeine"

        function toggle(): bool {
            return root.toggle();
        }

        function set(on: bool): void {
            root.set(on);
        }
    }

    Process {
        id: inhibitor
        running: root.enabled
        command: ["systemd-inhibit", "--what=idle", "--who=quickshell", "--why=caffeine", "--mode=block", "sleep", "infinity"]
        // Queued off/on requests restart after runningChanged; check afterward.
        // FailedToStart emits this signal without exited.
        onRunningChanged: {
            if (!running && root.enabled)
                Qt.callLater(() => { if (!inhibitor.running) root.enabled = false; });
        }
    }
}
