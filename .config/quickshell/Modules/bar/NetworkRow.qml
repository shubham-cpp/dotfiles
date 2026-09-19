pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

Item {
    id: root
    required property var net
    readonly property bool chosen: net && Network.selected === net
    implicitHeight: 52
    ItemDelegate {
        id: row
        anchors.fill: parent
        focusPolicy: Qt.NoFocus
        hoverEnabled: true
        onClicked: Network.activate(root.net)
        ToolTip.visible: hovered && nameLabel.truncated
        ToolTip.delay: 700
        ToolTip.text: root.net ? root.net.name : ""
        background: Rectangle {
            radius: Tokens.radiusSm
            color: root.chosen ? Tokens.selection : (row.hovered ? Tokens.hover : "transparent")
            border.width: root.chosen ? 1 : 0
            border.color: Tokens.border
        }
        Glyph {
            id: signalIcon
            anchors.left: parent.left
            anchors.leftMargin: 10
            anchors.verticalCenter: parent.verticalCenter
            name: root.net && root.net.connected ? "check" : Network.wifiGlyph(root.net)
            color: root.net && root.net.connected ? Tokens.success : Tokens.subtext
        }
        Glyph {
            visible: root.net && !Network.isOpen(root.net) && !root.net.connected
            anchors.left: signalIcon.right
            anchors.leftMargin: 2
            anchors.verticalCenter: parent.verticalCenter
            name: "lock"
            font.pixelSize: 10
            color: Tokens.overlay
        }
        Column {
            anchors.left: parent.left
            anchors.leftMargin: 42
            anchors.right: parent.right
            anchors.rightMargin: 10
            anchors.verticalCenter: parent.verticalCenter
            spacing: 2
            Text {
                id: nameLabel
                width: parent.width
                text: root.net ? root.net.name : ""
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: 13
                elide: Text.ElideRight
                textFormat: Text.PlainText
                renderType: Text.NativeRendering
            }
            Text {
                width: parent.width
                text: {
                    if (!root.net)
                        return "";
                    const status = Network.securityLabel(root.net);
                    if (root.net.connected && Network.iface === root.net.device.name && !Network.pendingNet)
                        return status + " · " + Network.fmt(Network.downBps) + "/s ↓  " + Network.fmt(Network.upBps) + "/s ↑";
                    return status;
                }
                color: root.net && root.net.connected ? Tokens.success : Tokens.overlay
                font.family: Tokens.fontFamily
                font.pixelSize: 11
                elide: Text.ElideRight
                textFormat: Text.PlainText
                renderType: Text.NativeRendering
            }
        }
    }
}
