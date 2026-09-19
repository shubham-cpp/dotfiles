pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    readonly property bool ready: true
    property real memoryUsedKiB: -1
    property real memoryTotalKiB: -1
    property real cpuPercent: -1
    property real temperature: NaN
    property string temperaturePath: ""
    property var previousCpu: null
    readonly property string memoryText: formatMemory(memoryUsedKiB)
    readonly property string cpuText: cpuPercent >= 0 ? Math.round(cpuPercent) + "%" : "--%"
    readonly property string temperatureText: isFinite(temperature) ? Math.round(temperature) + "°C" : "--°C"

    function formatMemory(kib) {
        if (!isFinite(kib) || kib < 0)
            return "--";
        return kib < 1024 * 1024 ? Math.floor(kib / 1024) + " MB" : (kib / (1024 * 1024)).toFixed(1) + " GB";
    }

    function updateMemory(text) {
        const total = text.match(/^MemTotal:\s+(\d+)\s+kB/m);
        const available = text.match(/^MemAvailable:\s+(\d+)\s+kB/m);
        root.memoryTotalKiB = total ? Number(total[1]) : -1;
        root.memoryUsedKiB = total && available ? Math.max(0, Number(total[1]) - Number(available[1])) : -1;
    }

    function updateCpu(text) {
        const match = text.match(/^cpu\s+([^\n]+)/);
        const values = match ? match[1].trim().split(/\s+/).map(Number) : [];
        if (values.length < 8 || values.some(value => !isFinite(value) || value < 0)) {
            root.previousCpu = null;
            root.cpuPercent = -1;
            return;
        }
        // Guest time is already included in user/nice, so only sum the first eight fields.
        const total = values.slice(0, 8).reduce((sum, value) => sum + value, 0);
        const idle = values[3] + values[4];
        const previous = root.previousCpu;
        root.previousCpu = { total: total, idle: idle };
        if (!previous || total <= previous.total || idle < previous.idle) {
            root.cpuPercent = -1;
            return;
        }
        root.cpuPercent = Math.max(0, Math.min(100, 100 * (1 - (idle - previous.idle) / (total - previous.total))));
    }

    function updateTemperature(text) {
        const value = text.trim();
        root.temperature = /^-?\d+$/.test(value) ? Number(value) / 1000 : NaN;
    }

    FileView {
        id: memoryFile
        path: "/proc/meminfo"
        printErrors: false
        onLoaded: root.updateMemory(text())
        onLoadFailed: {
            root.memoryUsedKiB = -1;
            root.memoryTotalKiB = -1;
        }
    }

    FileView {
        id: cpuFile
        path: "/proc/stat"
        printErrors: false
        onLoaded: root.updateCpu(text())
        onLoadFailed: {
            root.previousCpu = null;
            root.cpuPercent = -1;
        }
    }

    FileView {
        id: temperatureFile
        path: root.temperaturePath
        printErrors: false
        onLoaded: root.updateTemperature(text())
        onLoadFailed: root.temperature = NaN
    }

    // Discover the CPU sensor once. Sampling below uses asynchronous file reads.
    Process {
        running: true
        command: ["sh", "-c", [
            'for dir in /sys/class/hwmon/hwmon*; do',
            '    read -r name < "$dir/name" || continue',
            '    case "$name" in coretemp|k10temp|zenpower) ;; *) continue ;; esac',
            '    for label in "$dir"/temp*_label; do',
            '        [ -r "$label" ] || continue',
            '        read -r name < "$label"',
            '        case "$name" in "Package id "*|Tctl|Tdie)',
            '            input="${label%_label}_input"',
            '            [ -r "$input" ] && { printf "%s\\n" "$input"; exit; } ;;',
            '        esac',
            '    done',
            '    for input in "$dir"/temp*_input; do',
            '        [ -r "$input" ] && { printf "%s\\n" "$input"; exit; }',
            '    done',
            'done'
        ].join("\n")]
        stdout: StdioCollector {
            onStreamFinished: root.temperaturePath = this.text.trim()
        }
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        onTriggered: {
            memoryFile.reload();
            cpuFile.reload();
            if (root.temperaturePath.length)
                temperatureFile.reload();
        }
    }
}
