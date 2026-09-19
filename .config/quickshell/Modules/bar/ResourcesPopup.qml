pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import qs.Common
import "../../Common/PopupGeometry.js" as PopupGeometry
import qs.Services

PanelWindow {
    id: win

    required property var anchorWindow
    required property var anchorItem
    property real panelX: 8
    property string highlightedKey: ""
    readonly property int panelWidth: Math.max(1, Math.min(Tokens.resourcesWidth, width - 16))

    visible: Resources.open
    screen: anchorWindow ? anchorWindow.screen : Quickshell.screens[0]
    color: "transparent"
    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-resources"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    function updatePosition() {
        if (!anchorWindow) return;
        const center = anchorItem ? anchorItem.mapToItem(anchorWindow.contentItem, anchorItem.width / 2, 0).x : anchorWindow.width / 2;
        panelX = PopupGeometry.popupX(center, panelWidth, width);
    }
    onWidthChanged: Qt.callLater(win.updatePosition)
    onPanelWidthChanged: Qt.callLater(win.updatePosition)
    onAnchorItemChanged: Qt.callLater(win.updatePosition)
    onAnchorWindowChanged: {
        if (!anchorWindow) Resources.close();
        else Qt.callLater(win.updatePosition);
    }
    Connections {
        target: win.anchorWindow
        function onScreenChanged() { if (!win.anchorWindow.screen) Resources.close(); }
    }
    Component.onCompleted: updatePosition()

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: Resources.close()
        onWheel: event => { event.accepted = true; }
    }
    Rectangle {
        id: panel
        x: win.panelX
        y: (win.anchorWindow ? win.anchorWindow.height : Tokens.barHeight) + Tokens.overlayGap
        width: win.panelWidth
        height: Math.max(1, Math.min(504 + (error.visible ? 36 : 0), win.height - y - 8))
        radius: Tokens.radius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border
        focus: true
        Keys.onEscapePressed: Resources.close()
        MouseArea { anchors.fill: parent }

        Text {
            x: 20
            y: 21
            text: "System resources"
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: Tokens.fontLg
            font.weight: Font.Medium
        }
        Row {
            anchors.right: parent.right
            anchors.rightMargin: 12
            y: 12
            spacing: 4
            NetworkButton {
                text: Resources.paused ? "Resume" : "Pause"
                enabled: Resources.available
                onClicked: Resources.togglePause()
            }
            IconButton { iconName: "close"; text: "Close"; onClicked: Resources.close() }
        }
        Row {
            id: columns
            x: 12
            y: 56
            width: parent.width - 24
            height: Math.max(1, parent.height - y - 12 - (error.visible ? 36 : 0))
            spacing: 12

            ResourceColumn {
                width: (columns.width - 25) / 2
                height: columns.height
                metric: "memory"
                title: "Memory"
                summary: SystemStats.memoryText + " / " + SystemStats.formatMemory(SystemStats.memoryTotalKiB)
                highlightedKey: win.highlightedKey
                onHoveredApp: key => win.highlightedKey = key
            }
            Rectangle { width: 1; height: parent.height; color: Tokens.separator }
            ResourceColumn {
                width: (columns.width - 25) / 2
                height: columns.height
                metric: "cpu"
                title: "CPU"
                summary: SystemStats.cpuText
                highlightedKey: win.highlightedKey
                onHoveredApp: key => win.highlightedKey = key
            }
        }
        Text {
            id: error
            x: 20
            anchors.bottom: parent.bottom
            anchors.bottomMargin: 12
            width: parent.width - 40
            visible: Resources.errorText.length > 0
            text: Resources.errorText
            textFormat: Text.PlainText
            elide: Text.ElideRight
            color: Tokens.danger
            font.family: Tokens.fontFamily
            font.pixelSize: Tokens.fontSm
        }
    }
}
