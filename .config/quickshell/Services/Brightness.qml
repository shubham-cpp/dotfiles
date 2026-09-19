pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    property string device: ""
    property bool present: false
    property int value: 0
    property int maxValue: 1
    property bool writing: false
    property int pendingValue: -1
    property bool rescanPending: false
    property bool open: false
    property var anchorWindow: null
    property var anchorItem: null
    property var anchorControl: null
    readonly property real percent: maxValue > 0 ? value / maxValue : 0
    readonly property string text: present ? ("brt " + Math.round(percent * 100)) : ""

    function adjust(deltaPercent) {
        if (!present || !isFinite(deltaPercent))
            return;
        setPercent(Math.round(percent * 100) + deltaPercent);
    }

    function setPercent(p) {
        if (!present || !isFinite(p))
            return;
        const pct = Math.max(1, Math.min(100, Math.round(p)));
        const next = Math.max(1, Math.round(maxValue * pct / 100));
        if (next === value)
            return;
        root.value = next;
        root.pendingValue = next;
        root.flushWrite();
    }

    function toggle(win, item, control) {
        if (open) {
            close();
            return false;
        }
        if (Lock.locked || !present)
            return false;
        Audio.close();
        Network.close();
        Power.close();
        Resources.close();
        anchorWindow = win || null;
        anchorItem = item || null;
        anchorControl = control || null;
        open = true;
        NightLight.refresh();
        return true;
    }

    function close() {
        open = false;
        anchorWindow = null;
        anchorItem = null;
        anchorControl = null;
    }

    function flushWrite() {
        if (writing || pendingValue < 0 || !present)
            return;
        const next = pendingValue;
        root.pendingValue = -1;
        root.writing = true;
        writer.command = ["brightnessctl", "--quiet", "--device=" + device, "set", String(next)];
        writer.running = true;
    }

    function finishWrite(exitCode) {
        root.writing = false;
        if (exitCode !== 0) {
            console.warn("Brightness write failed:", exitCode);
            root.pendingValue = -1;
        }
        if (pendingValue >= 0)
            root.flushWrite();
        else
            root.refresh();
    }

    function refresh() {
        // A previous write's event must not replace a newer requested value.
        if (!present || writing || pendingValue >= 0)
            return;
        valFile.reload();
        const v = parseInt(valFile.text(), 10);
        if (!isNaN(v))
            root.value = v;
    }

    function discover() {
        if (discovery.running)
            root.rescanPending = true;
        else
            discovery.running = true;
    }

    function selectDevice(output) {
        const names = output.trim().split(/\s+/).filter(name => name.length > 0);
        const next = names.indexOf(device) >= 0 ? device : (names[0] || "");
        if (next === device) {
            root.refresh();
            return;
        }
        root.pendingValue = -1;
        root.present = false;
        root.device = next;
        root.value = 0;
        root.maxValue = 1;
        if (!next.length)
            return;
        maxFile.reload();
        valFile.reload();
        const maximum = parseInt(maxFile.text(), 10);
        const current = parseInt(valFile.text(), 10);
        if (maximum > 0 && !isNaN(current)) {
            root.maxValue = maximum;
            root.value = current;
            root.present = true;
        }
    }

    function handleEvent(line) {
        // udevadm prints this after subscribing, closing the startup snapshot race.
        if (line.trim() === "KERNEL - the kernel uevent") {
            root.discover();
            return;
        }
        const event = line.match(/^KERNEL\[[^\]]+\]\s+(\w+)\s+(\S+)\s+\(backlight\)/);
        if (!event)
            return;
        if (event[1] === "add" || event[1] === "remove")
            root.discover();
        else if (event[1] === "change" && event[2].endsWith("/" + device))
            root.refresh();
    }

    Connections {
        target: Lock
        function onLockedChanged() {
            if (Lock.locked)
                root.close();
        }
    }

    IpcHandler {
        target: "brightness"

        function toggle(): bool {
            return root.toggle(null, null, null);
        }

        function close(): void {
            root.close();
        }

        function setPercent(p: int): void {
            root.setPercent(p);
        }

        function adjust(delta: int): void {
            root.adjust(delta);
        }

        function status(): string {
            return JSON.stringify({
                open: root.open,
                device: root.device,
                value: root.value,
                maxValue: root.maxValue,
                writing: root.writing,
                monitoring: monitor.running
            });
        }
    }

    Process {
        id: writer
        onExited: (exitCode, exitStatus) => root.finishWrite(exitStatus === 0 ? exitCode : -1)
        // FailedToStart emits runningChanged without exited.
        onRunningChanged: {
            if (!running && root.writing)
                root.finishWrite(-1);
        }
    }

    Process {
        id: discovery
        command: ["ls", "/sys/class/backlight"]
        stdout: StdioCollector {
            onStreamFinished: root.selectDevice(this.text)
        }
        onExited: {
            if (root.rescanPending) {
                root.rescanPending = false;
                root.discover();
            }
        }
    }

    FileView {
        id: maxFile
        path: root.device.length ? "/sys/class/backlight/" + root.device + "/max_brightness" : ""
        blockAllReads: true
        printErrors: false
    }

    FileView {
        id: valFile
        path: root.device.length ? "/sys/class/backlight/" + root.device + "/brightness" : ""
        blockAllReads: true
        printErrors: false
    }

    Process {
        id: monitor
        running: true
        command: ["udevadm", "monitor", "--kernel", "--subsystem-match=backlight"]
        environment: ({
                LC_ALL: "C"
            })
        stdout: SplitParser {
            onRead: data => root.handleEvent(data)
        }
        onExited: console.warn("Brightness event monitor exited; reload the shell to reconnect")
    }
}
