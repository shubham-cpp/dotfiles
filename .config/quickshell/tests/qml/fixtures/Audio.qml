pragma Singleton
import QtQuick
QtObject {
    property bool ready: true
    property var nodes: []
    property int writes: 0
    property int selections: 0
    property var selected: null
    property bool open: false
    function usable(node) { return ready && !!node && nodes.indexOf(node) !== -1 && node.ready && !!node.audio; }
    function setVolume(node, value) {
        if (!usable(node) || !Number.isFinite(value)) return false;
        writes++;
        node.audio.volume = Math.max(0, Math.min(1, value));
        node.audio.muted = false;
        return true;
    }
    function toggleNodeMute(node) {
        if (!usable(node)) return false;
        writes++;
        node.audio.muted = !node.audio.muted;
        return true;
    }
    function selectDevice(node, input) { selections++; selected = node; return usable(node); }
}
