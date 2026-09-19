pragma ComponentBehavior: Bound

import Quickshell.Widgets
import QtQuick
import QtQuick.Controls as Controls
import qs.Common
import "../../Common/ResourceList.js" as ResourceList

Rectangle {
    id: root

    required property string appKey
    required property string appName
    required property string iconName
    required property real memory
    required property real cpu
    required property int processCount
    required property string members
    required property bool canEnd
    required property string actionState
    required property bool gone
    property string metric: "memory"
    property real maximum: 1
    property bool highlighted: false
    property bool confirming: false
    property string capturedMembers: ""
    property bool capturedForce: false
    readonly property bool interacting: confirming || endButton.activeFocus || confirmButton.activeFocus || cancelButton.activeFocus
    readonly property string iconSource: Icons.src(iconName)
    signal hoveredApp(string key)
    signal endRequested(string key, string identities, bool force)

    height: 40
    radius: Tokens.radiusSm
    color: hover.hovered ? Tokens.hover : highlighted ? Tokens.selection : "transparent"
    opacity: gone ? 0.45 : 1
    onGoneChanged: { if (gone) confirming = false; }
    onCanEndChanged: { if (!canEnd) confirming = false; }

    HoverHandler {
        id: hover
        onHoveredChanged: root.hoveredApp(hovered ? root.appKey : "")
    }
    Controls.ToolTip {
        id: detailTip
        visible: hover.hovered && !root.confirming
        delay: 700
        text: root.appName + " · " + root.processCount + (root.processCount === 1 ? " process" : " processes")
            + (root.gone ? " · Exited" : !root.canEnd ? " · Managed by the session or another user" : "")
        contentItem: Text {
            text: detailTip.text
            textFormat: Text.PlainText
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: Tokens.fontSm
        }
        background: Rectangle { color: Tokens.bgAlt; radius: Tokens.radiusSm; border.color: Tokens.border }
    }

    IconImage {
        id: appIcon
        x: 8
        anchors.verticalCenter: parent.verticalCenter
        implicitSize: 22
        source: root.iconSource
        asynchronous: true
        visible: root.iconSource.length > 0 && status !== Image.Error
    }
    Text {
        x: 8
        width: 22
        anchors.verticalCenter: parent.verticalCenter
        visible: !appIcon.visible
        text: root.appName.charAt(0).toUpperCase()
        color: Tokens.subtext
        font.family: Tokens.fontFamily
        font.pixelSize: Tokens.fontLg
        horizontalAlignment: Text.AlignHCenter
    }

    Text {
        id: label
        x: 40
        y: root.confirming ? 10 : 4
        width: Math.max(0, (root.confirming ? confirmation.x : value.x) - x - 8)
        text: root.confirming ? (root.capturedForce ? "Force kill " : "End ") + root.appName + "?" : root.appName
        textFormat: Text.PlainText
        elide: Text.ElideRight
        color: Tokens.text
        font.family: Tokens.fontFamily
        font.pixelSize: Tokens.fontMd
        renderType: Text.NativeRendering
    }
    Text {
        id: value
        visible: !root.confirming
        anchors.right: endButton.left
        anchors.rightMargin: 8
        y: 5
        text: root.gone ? "Exited" : root.actionState === "ending" ? "Ending…"
            : root.metric === "memory" ? ResourceList.memoryText(root.memory)
            : root.cpu < 0 ? "--%" : root.cpu.toFixed(1) + "%"
        color: Tokens.subtext
        font.family: Tokens.barFontFamily
        font.pixelSize: Tokens.fontSm
        renderType: Text.NativeRendering
    }
    Rectangle {
        visible: !root.confirming && !root.gone
        x: 40
        y: 29
        width: Math.max(0, endButton.x - x - 8)
        height: 3
        radius: 1
        color: Tokens.surface
        Rectangle {
            width: parent.width * Math.max(0, Math.min(1, (root.metric === "memory" ? root.memory : root.cpu) / Math.max(1, root.maximum)))
            height: parent.height
            radius: 1
            color: hover.hovered || root.highlighted ? Tokens.accent : Tokens.overlay
        }
    }

    IconButton {
        id: endButton
        objectName: "resourceEnd"
        anchors.right: parent.right
        anchors.rightMargin: 4
        anchors.verticalCenter: parent.verticalCenter
        visible: !root.confirming
        enabled: root.canEnd && !root.gone && root.actionState !== "ending"
        iconName: "close"
        text: (root.actionState === "force" ? "Force kill " : "End ") + root.appName
        contentItem: Glyph {
            name: "close"
            color: endButton.hovered || endButton.visualFocus ? Tokens.danger : Tokens.subtext
        }
        onClicked: {
            root.capturedMembers = root.members;
            root.capturedForce = root.actionState === "force";
            root.confirming = true;
            cancelButton.forceActiveFocus();
        }
    }
    Row {
        id: confirmation
        visible: root.confirming
        anchors.right: parent.right
        anchors.rightMargin: 4
        anchors.verticalCenter: parent.verticalCenter
        spacing: 2

        NetworkButton {
            id: confirmButton
            objectName: "resourceConfirm"
            text: root.capturedForce ? "Kill" : "End"
            prominent: true
            enabled: root.canEnd && !root.gone
            onClicked: {
                root.endRequested(root.appKey, root.capturedMembers, root.capturedForce);
                root.confirming = false;
                endButton.forceActiveFocus();
            }
        }
        IconButton {
            id: cancelButton
            objectName: "resourceCancel"
            iconName: "close"
            text: "Cancel"
            onClicked: {
                root.confirming = false;
                endButton.forceActiveFocus();
            }
        }
    }
    Keys.onEscapePressed: event => {
        if (confirming) {
            confirming = false;
            endButton.forceActiveFocus();
            event.accepted = true;
        } else event.accepted = false;
    }
}
