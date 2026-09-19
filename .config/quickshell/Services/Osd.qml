pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick
import qs.Common

Singleton {
    id: root

    property bool ready: true
    property bool open: false
    property bool armed: false
    property string kind: "volume"

    readonly property real level: kind === "brightness" ? Brightness.percent : (Audio.muted ? 0 : Audio.volume)
    readonly property string label: {
        if (kind === "brightness")
            return Brightness.text;
        return Audio.text;
    }

    function show(k) {
        if (!armed || (k === "volume" && Audio.open) || (k === "brightness" && Brightness.open))
            return;
        kind = k;
        open = true;
        hide.restart();
    }

    function showVolume() {
        show("volume");
    }

    function showBrightness() {
        if (Brightness.present)
            show("brightness");
    }

    IpcHandler {
        target: "osd"

        function volume(): void {
            root.showVolume();
        }

        function brightness(): void {
            root.showBrightness();
        }
    }

    Timer {
        interval: 1200
        running: true
        repeat: false
        onTriggered: root.armed = true
    }

    Timer {
        id: hide
        interval: Tokens.osdHideMs
        repeat: false
        onTriggered: root.open = false
    }

    Connections {
        target: Audio
        function onOpenChanged() {
            if (Audio.open && root.kind === "volume") {
                root.open = false;
                hide.stop();
            }
        }
        function onVolumeChanged() {
            root.showVolume();
        }
        function onMutedChanged() {
            root.showVolume();
        }
    }

    Connections {
        target: Brightness
        function onOpenChanged() {
            if (Brightness.open && root.kind === "brightness") {
                root.open = false;
                hide.stop();
            }
        }
        function onValueChanged() {
            root.showBrightness();
        }
    }
}
