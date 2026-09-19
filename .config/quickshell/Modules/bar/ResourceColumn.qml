pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls as Controls
import qs.Common
import qs.Services
import "../../Common/ResourceList.js" as ResourceList

Item {
    id: root

    required property string metric
    required property string title
    required property string summary
    property string highlightedKey: ""
    property int interactions: 0
    readonly property bool frozen: hover.hovered || interactions > 0 || list.moving || Resources.paused
    readonly property real maximum: Math.max(1, ...Resources.apps.map(app => app[metric]))
    signal hoveredApp(string key)

    function refresh() { ResourceList.sync(entries, Resources.apps, metric, frozen); }
    function updateInteractions() {
        let count = 0;
        for (let i = 0; i < list.count; i++) {
            const item = list.itemAtIndex(i) as ResourceRow;
            if (item && item.interacting) count++;
        }
        interactions = count;
    }
    onFrozenChanged: Qt.callLater(root.refresh)
    Component.onCompleted: refresh()
    Connections {
        target: Resources
        function onAppsChanged() { root.refresh(); }
    }
    ListModel { id: entries }

    Text {
        id: heading
        x: 8
        text: root.title
        color: Tokens.text
        font.family: Tokens.fontFamily
        font.pixelSize: Tokens.fontMd
        font.weight: Font.Medium
    }
    Text {
        anchors.right: parent.right
        anchors.rightMargin: 8
        text: root.summary
        color: Tokens.subtext
        font.family: Tokens.barFontFamily
        font.pixelSize: Tokens.fontSm
    }
    MouseArea {
        anchors.left: parent.left
        anchors.right: parent.right
        height: 24
        hoverEnabled: true
        acceptedButtons: Qt.NoButton
        Controls.ToolTip.visible: containsMouse
        Controls.ToolTip.delay: 700
        Controls.ToolTip.text: root.metric === "memory"
            ? "Applications show resident memory. Shared pages may be counted more than once."
            : "Application CPU is a percentage of total machine capacity."
    }
    Rectangle {
        y: 29
        width: parent.width
        height: 1
        color: Tokens.separator
    }
    ListView {
        id: list
        anchors.top: parent.top
        anchors.topMargin: 36
        anchors.bottom: parent.bottom
        width: parent.width
        clip: true
        model: entries
        boundsBehavior: Flickable.StopAtBounds
        cacheBuffer: 480
        Controls.ScrollBar.vertical: Controls.ScrollBar { policy: Controls.ScrollBar.AsNeeded }
        HoverHandler { id: hover }

        delegate: ResourceRow {
            width: list.width
            metric: root.metric
            maximum: root.maximum
            highlighted: appKey === root.highlightedKey
            onHoveredApp: key => root.hoveredApp(key)
            onInteractingChanged: root.updateInteractions()
            Component.onDestruction: Qt.callLater(root.updateInteractions)
            onEndRequested: (key, identities, force) => Resources.endApplication(key, identities, force)
        }
    }
    Text {
        anchors.centerIn: list
        visible: entries.count === 0
        text: Resources.errorText ? "Unavailable" : Resources.available ? "No applications" : "Measuring…"
        color: Tokens.subtext
        font.family: Tokens.fontFamily
        font.pixelSize: Tokens.fontMd
    }
}
