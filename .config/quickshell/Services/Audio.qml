pragma Singleton

import Quickshell
import Quickshell.Io
import Quickshell.Services.Pipewire
import QtQuick

Singleton {
    id: root

    property bool open: false
    property var anchorWindow: null
    property var anchorItem: null
    property var anchorControl: null
    property int streamCount: 0
    property int groupCount: 0

    readonly property bool ready: Pipewire.ready
    readonly property var sink: Pipewire.defaultAudioSink
    readonly property var source: Pipewire.defaultAudioSource
    readonly property var audio: sink && sink.ready ? sink.audio : null
    readonly property real volume: audio ? audio.volume : 0
    readonly property bool muted: audio ? audio.muted : false
    readonly property string text: {
        if (!audio)
            return "vol —";
        if (muted)
            return "vol mute";
        return "vol " + Math.round(volume * 100);
    }

    PwObjectTracker {
        objects: root.sink ? [root.sink] : []
    }

    function toggleMute() {
        toggleNodeMute(sink);
    }

    function adjust(delta) {
        if (audio)
            setVolume(sink, volume + delta);
    }

    function usable(node) {
        return ready && !!node && Pipewire.nodes.values.indexOf(node) !== -1 && node.ready && !!node.audio;
    }

    function setVolume(node, value) {
        if (!usable(node) || !Number.isFinite(value))
            return false;
        node.audio.volume = Math.max(0, Math.min(1, value));
        node.audio.muted = false;
        return true;
    }

    function toggleNodeMute(node) {
        if (!usable(node))
            return false;
        node.audio.muted = !node.audio.muted;
        return true;
    }

    function selectDevice(node, input) {
        if (!usable(node) || node.isStream || node.isSink === input)
            return false;
        if (input)
            Pipewire.preferredDefaultAudioSource = node;
        else
            Pipewire.preferredDefaultAudioSink = node;
        return true;
    }

    function toggle(win, item, control) {
        if (open) {
            close();
            return false;
        }
        if (Lock.locked)
            return false;
        Brightness.close();
        anchorWindow = win || null;
        anchorItem = item || null;
        anchorControl = control || null;
        open = true;
        return true;
    }

    function close() {
        open = false;
        anchorWindow = null;
        anchorItem = null;
        anchorControl = null;
        streamCount = 0;
        groupCount = 0;
    }

    Connections {
        target: Lock
        function onLockedChanged() {
            if (Lock.locked)
                root.close();
        }
    }

    IpcHandler {
        target: "audio"
        function toggle(): bool {
            return root.toggle(null, null);
        }
        function close(): void {
            root.close();
        }
        function status(): string {
            return JSON.stringify({
                open: root.open,
                ready: root.ready,
                output: root.sink ? root.sink.name : "",
                input: root.source ? root.source.name : "",
                streams: root.streamCount,
                groups: root.groupCount
            });
        }
    }
}
