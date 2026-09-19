pragma Singleton

import Quickshell
import Quickshell.Io
import Quickshell.WindowManager
import QtQuick

// Mangowc tags: occupancy from `mmsg watch` (not in ext-workspace).
// Activate prefers WindowManager; mmsg dispatch is the fallback.
Singleton {
    id: root

    property var tagsByMonitor: ({})
    property int gen: 0

    function tagsFor(screenName) {
        const _ = gen;
        return tagsByMonitor[screenName] || [];
    }

    function activate(index) {
        const sets = WindowManager.windowsets;
        for (let i = 0; i < sets.length; i++) {
            const ws = sets[i];
            if (Number(ws.name) === index || ws.name === String(index)) {
                if (ws.canActivate)
                    ws.activate();
                return;
            }
        }
        Quickshell.execDetached(["mmsg", "dispatch", "view," + String(index)]);
    }

    function step(direction) {
        const cmd = direction < 0 ? "viewtoleft_have_client" : "viewtoright_have_client";
        Quickshell.execDetached(["mmsg", "dispatch", cmd]);
    }

    function applyJson(text) {
        let obj;
        try {
            obj = JSON.parse(text);
        } catch (e) {
            return;
        }

        const next = {};
        const rows = obj.all_tags || (obj.monitor ? [obj] : []);
        for (let i = 0; i < rows.length; i++) {
            const mon = rows[i];
            const tags = mon.tags || [];
            const mapped = [];
            for (let j = 0; j < tags.length; j++) {
                const t = tags[j];
                mapped.push({
                    index: t.index,
                    name: String(t.index),
                    active: !!t.is_active,
                    urgent: !!t.is_urgent,
                    occupied: (t.client_count || 0) > 0
                });
            }
            next[mon.monitor] = mapped;
        }
        root.tagsByMonitor = next;
        root.gen++;
    }

    Process {
        id: watcher
        running: true
        command: ["mmsg", "watch", "all-tags"]
        stdout: SplitParser {
            onRead: data => root.applyJson(data)
        }
        onRunningChanged: {
            if (!running)
                retry.restart();
            else
                retry.stop();
        }
    }

    Timer {
        id: retry
        interval: 1000
        onTriggered: if (!watcher.running) watcher.running = true;
    }
}
