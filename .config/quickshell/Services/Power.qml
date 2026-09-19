pragma Singleton

import Quickshell
import Quickshell.Io
import Quickshell.Services.UPower
import QtQuick
import qs.Common

Singleton {
    id: root

    property bool open: false
    property var anchorWindow: null

    readonly property var battery: UPower.displayDevice
    readonly property var cell: {
        const list = UPower.devices.values;
        for (let i = 0; i < list.length; i++) {
            const d = list[i];
            if (d && d.isLaptopBattery)
                return d;
        }
        return null;
    }
    readonly property bool present: battery && battery.ready && battery.isLaptopBattery
    readonly property real percent: present ? battery.percentage : 0
    readonly property int percentInt: Math.round(percent * 100)
    readonly property int state: present ? battery.state : UPowerDeviceState.Unknown
    readonly property bool charging: state === UPowerDeviceState.Charging || state === UPowerDeviceState.FullyCharged || state === UPowerDeviceState.PendingCharge
    readonly property bool plugged: charging
    readonly property bool activeCharge: state === UPowerDeviceState.Charging
    readonly property bool discharging: state === UPowerDeviceState.Discharging || state === UPowerDeviceState.PendingDischarge || state === UPowerDeviceState.Empty
    readonly property int profile: PowerProfiles.profile
    readonly property bool hasPerformance: PowerProfiles.hasPerformanceProfile
    readonly property bool ready: present
    readonly property bool healthSupported: !!(cell && cell.healthSupported)
    readonly property int healthInt: healthSupported ? Math.round(cell.healthPercentage) : 0
    readonly property string severity: {
        if (!present || !discharging)
            return "ok";
        if (percentInt < 15)
            return "danger";
        if (percentInt < 30)
            return "warning";
        return "ok";
    }
    readonly property color tone: {
        if (!present)
            return Tokens.barText;
        if (severity === "danger")
            return Tokens.danger;
        if (severity === "warning")
            return Tokens.warning;
        if (plugged)
            return Tokens.success;
        return Tokens.barText;
    }
    readonly property string glyph: {
        const n = percentInt;
        let bucket = Math.min(100, Math.floor((n + 5) / 10) * 10);
        if (state === UPowerDeviceState.FullyCharged)
            return "batteryCharging100";
        if (state === UPowerDeviceState.Charging) {
            if (bucket < 10)
                bucket = 10;
            return "batteryCharging" + bucket;
        }
        if (discharging && n < 5)
            return "batteryAlert";
        if (bucket <= 0)
            return "battery0";
        return "battery" + bucket;
    }
    readonly property string stateLabel: {
        switch (state) {
        case UPowerDeviceState.Charging:
            return "Charging";
        case UPowerDeviceState.FullyCharged:
            return "Fully charged";
        case UPowerDeviceState.Discharging:
            return "Discharging";
        case UPowerDeviceState.PendingCharge:
            return "Waiting to charge";
        case UPowerDeviceState.PendingDischarge:
            return "On battery";
        case UPowerDeviceState.Empty:
            return "Empty";
        default:
            return present ? "Unknown" : "";
        }
    }
    readonly property real etaSeconds: {
        if (!present)
            return 0;
        if (state === UPowerDeviceState.Charging)
            return Number(battery.timeToFull) || 0;
        if (state === UPowerDeviceState.Discharging)
            return Number(battery.timeToEmpty) || 0;
        return 0;
    }
    readonly property string etaLabel: {
        if (state === UPowerDeviceState.Charging)
            return formatEta(etaSeconds, true);
        if (state === UPowerDeviceState.Discharging)
            return formatEta(etaSeconds, false);
        return "";
    }
    readonly property var profiles: {
        const items = [
            {
                profile: PowerProfile.PowerSaver,
                name: "Saver",
                icon: "powerSaver"
            },
            {
                profile: PowerProfile.Balanced,
                name: "Balanced",
                icon: "powerBalanced"
            }
        ];
        if (hasPerformance)
            items.push({
                profile: PowerProfile.Performance,
                name: "Performance",
                icon: "powerPerformance"
            });
        return items;
    }

    readonly property string text: {
        if (!present)
            return "";
        const mark = charging && state !== UPowerDeviceState.FullyCharged ? "+" : "";
        return percentInt + mark + "%";
    }

    function formatEta(seconds, untilFull) {
        if (!seconds || seconds <= 0 || !isFinite(seconds))
            return "";
        const totalMin = Math.round(seconds / 60);
        const suffix = untilFull ? " until full" : " remaining";
        if (totalMin < 1)
            return "Less than a minute" + suffix;
        const h = Math.floor(totalMin / 60);
        const m = totalMin % 60;
        let span;
        if (h <= 0)
            span = m + " min";
        else if (m === 0)
            span = h + " h";
        else
            span = h + " h " + m + " min";
        return "About " + span + suffix;
    }

    function setProfile(p) {
        if (typeof p === "string") {
            const n = p;
            if (n === "power-saver" || n === "PowerSaver")
                p = PowerProfile.PowerSaver;
            else if (n === "balanced" || n === "Balanced")
                p = PowerProfile.Balanced;
            else if (n === "performance" || n === "Performance")
                p = PowerProfile.Performance;
            else
                return;
        }
        if (p === PowerProfile.Performance && !hasPerformance)
            return;
        PowerProfiles.profile = p;
    }

    function toggle(win) {
        if (open) {
            close();
            return false;
        }
        Brightness.close();
        if (win)
            anchorWindow = win;
        open = true;
        return true;
    }

    function close() {
        open = false;
    }

    IpcHandler {
        target: "power"

        function toggle(): bool {
            return root.toggle(null);
        }

        function close(): void {
            root.close();
        }

        function setProfile(name: string): void {
            root.setProfile(name);
        }
    }
}
