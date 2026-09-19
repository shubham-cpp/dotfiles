pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Services.Pipewire
import QtQuick
import qs.Services
import "../../Common/AudioList.js" as AudioList

Scope {
    id: root

    property bool freeze: false
    property var inputs: []
    property var outputs: []
    property alias rows: entries
    property int contentHeight: 0
    signal removingFocusedTarget(var node)
    signal reconciled()

    ListModel { id: entries }

    PwObjectTracker {
        objects: Pipewire.nodes.values.filter(node => !!node.audio)
    }

    function refresh() {
        const nodes = Audio.ready ? Pipewire.nodes.values : [];
        const nextInputs = AudioList.devices(nodes, true);
        const nextOutputs = AudioList.devices(nodes, false);
        if (!AudioList.sameDevices(inputs, nextInputs)) inputs = nextInputs;
        if (!AudioList.sameDevices(outputs, nextOutputs)) outputs = nextOutputs;
        // v0.3.1's node tracker follows the wrong direction for playback
        // streams and can stop early at monitor links. Use explicit endpoints.
        const links = Pipewire.linkGroups.values.filter(link => link.target && !AudioList.monitor(link.target));
        const desired = AudioList.rows(nodes, links, Audio.sink);
        for (let i = 0; i < entries.count; i++) {
            const node = entries.get(i).node;
            if (!desired.some(row => row.node === node))
                removingFocusedTarget(node);
        }
        AudioList.sync(entries, desired, freeze);
        let height = 0;
        let previous = "";
        let groups = 0;
        for (let i = 0; i < entries.count; i++) {
            const row = entries.get(i);
            if (row.group !== previous) {
                height += 28;
                groups++;
                previous = row.group;
            }
            height += row.subtitle ? 60 : 44;
        }
        contentHeight = height;
        Audio.streamCount = entries.count;
        Audio.groupCount = groups;
        reconciled();
    }

    Instantiator {
        id: observers
        model: Pipewire.nodes
        delegate: Scope {
            id: observer
            required property var modelData
            Connections {
                target: observer.modelData
                function onReadyChanged() { Qt.callLater(root.refresh); }
                function onPropertiesChanged() { Qt.callLater(root.refresh); }
            }
        }
        onObjectAdded: Qt.callLater(root.refresh)
        onObjectRemoved: Qt.callLater(root.refresh)
    }

    Connections {
        target: Pipewire.linkGroups
        function onValuesChanged() { Qt.callLater(root.refresh); }
    }
    Connections {
        target: Audio
        function onSinkChanged() { Qt.callLater(root.refresh); }
        function onReadyChanged() { Qt.callLater(root.refresh); }
    }
    onFreezeChanged: Qt.callLater(root.refresh)
    Component.onCompleted: Qt.callLater(root.refresh)
    Component.onDestruction: {
        Audio.streamCount = 0;
        Audio.groupCount = 0;
    }
}
