import Quickshell
import Quickshell.Services.UPower
import QtQuick
import qs.Common
import qs.Services

LockContent {
    id: root

    dateTime: clock.date
    wallpaperSource: Qt.resolvedUrl("../../Assets/lock-mountains.jpg")
    userName: Quickshell.env("USER")
    lockNotifications: Notifications.lockNotifications
    secure: Lock.secure
    busy: Lock.unlockInProgress
    sleeping: Logind.preparingForSleep
    errorText: Lock.errorText !== "" ? Lock.errorText : Logind.errorText
    passwordText: Lock.currentText
    onPasswordTextChanged: {
        if (Lock.currentText !== passwordText)
            Lock.currentText = passwordText;
    }
    onSubmitted: Lock.tryUnlock()

    readonly property bool validBattery: Power.present && Power.battery.isPresent && isFinite(Power.percent)
    readonly property int percent: Math.round(Math.max(0, Math.min(1, Power.percent)) * 100)
    readonly property bool draining: Power.state === UPowerDeviceState.Discharging || Power.state === UPowerDeviceState.Empty
    readonly property bool critical: draining && percent <= 5
    readonly property bool low: draining && percent <= 15

    batteryVisible: validBattery
    batteryFraction: Math.max(0, Math.min(1, Power.percent))
    batteryColor: critical ? Tokens.danger : low ? Tokens.warning : Power.activeCharge ? Tokens.success : Tokens.text
    batteryCharging: Power.plugged
    batteryText: {
        const value = percent + "%";
        if (Power.state === UPowerDeviceState.Empty)
            return value + " · Battery empty";
        if (critical)
            return value + " · Very low battery";
        if (low)
            return value + " · Low battery";
        if (Power.state === UPowerDeviceState.Discharging)
            return value;
        if (Power.state === UPowerDeviceState.Unknown)
            return value + " · Status unavailable";
        return value + " · " + Power.stateLabel;
    }
    batteryDetail: critical ? "Connect your charger now" : low ? "Connect your charger" : ""

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }

    Connections {
        target: Lock
        function onFocusGenChanged() {
            root.focusPassword();
        }
        function onSecureChanged() {
            if (Lock.secure)
                root.focusPassword();
        }
    }

    Component.onCompleted: focusPassword()
}
