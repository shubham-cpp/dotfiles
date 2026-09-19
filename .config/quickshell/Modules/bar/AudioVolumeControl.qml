pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

Item {
    id: root

    property var node: null
    property string label: "Output"
    property string destination: ""
    readonly property bool usable: Audio.usable(node)
    readonly property real level: usable ? node.audio.volume : 0
    readonly property bool muted: usable && node.audio.muted
    readonly property bool interacting: slider.pressed || muteButton.down
    readonly property bool hasFocus: slider.activeFocus || muteButton.activeFocus
    property bool navigationHandled: false
    property var dragTarget: null
    property var muteTarget: null
    property bool cancelled: false
    signal focusEntered()
    signal advance(bool backwards)
    signal targetLost()
    implicitHeight: 36
    implicitWidth: 240

    function focusMute() { muteButton.forceActiveFocus(Qt.TabFocusReason); }
    function focusSlider() { slider.forceActiveFocus(Qt.TabFocusReason); }
    function cancelGesture() {
        cancelled = true;
        dragTarget = null;
        muteTarget = null;
    }
    onNodeChanged: {
        if (hasFocus) targetLost();
        cancelGesture();
    }
    onUsableChanged: {
        if (!usable) {
            if (hasFocus) targetLost();
            cancelGesture();
        }
    }

    IconButton {
        id: muteButton
        objectName: "audioMute"
        anchors.left: parent.left
        anchors.verticalCenter: parent.verticalCenter
        enabled: root.usable
        iconName: root.muted ? "volumeOff" : "volume"
        checked: root.muted
        text: (root.muted ? "Unmute " : "Mute ") + root.label + " audio"
        onPressed: root.muteTarget = root.node
        onClicked: {
            if (root.muteTarget && root.muteTarget === root.node)
                Audio.toggleNodeMute(root.muteTarget);
            root.muteTarget = null;
        }
        onActiveFocusChanged: { if (activeFocus) root.focusEntered(); }
        Keys.onTabPressed: event => {
            if (root.navigationHandled) slider.forceActiveFocus(Qt.TabFocusReason);
            event.accepted = root.navigationHandled;
        }
        Keys.onBacktabPressed: event => {
            if (root.navigationHandled) root.advance(true);
            event.accepted = root.navigationHandled;
        }
    }

    Slider {
        id: slider
        objectName: "audioSlider"
        anchors.left: muteButton.right
        anchors.leftMargin: 8
        anchors.right: percent.left
        anchors.rightMargin: 10
        anchors.verticalCenter: parent.verticalCenter
        height: 36
        from: 0
        to: 1
        stepSize: 0.01
        live: true
        enabled: root.usable
        wheelEnabled: false
        focusPolicy: Qt.StrongFocus
        hoverEnabled: true
        padding: 6
        Accessible.name: root.label + " volume"
        Accessible.description: (root.muted ? "Muted. " : "") + Math.round(root.level * 100) + " percent"
            + (root.destination ? ", " + root.destination : "")

        Binding {
            target: slider
            property: "value"
            value: Math.max(0, Math.min(1, root.level))
            when: !slider.pressed
            restoreMode: Binding.RestoreNone
        }
        onPressedChanged: {
            if (pressed) {
                root.dragTarget = root.node;
                root.cancelled = false;
            } else {
                root.dragTarget = null;
            }
        }
        onMoved: {
            if (pressed && !root.cancelled && root.dragTarget === root.node)
                Audio.setVolume(root.dragTarget, value);
        }
        Keys.onPressed: event => {
            let value = root.level;
            if (event.key === Qt.Key_Left || event.key === Qt.Key_Down) value -= 0.01;
            else if (event.key === Qt.Key_Right || event.key === Qt.Key_Up) value += 0.01;
            else if (event.key === Qt.Key_PageDown) value -= 0.05;
            else if (event.key === Qt.Key_PageUp) value += 0.05;
            else if (event.key === Qt.Key_Home) value = 0;
            else if (event.key === Qt.Key_End) value = 1;
            else return;
            Audio.setVolume(root.node, value);
            event.accepted = true;
        }
        Keys.onTabPressed: event => {
            if (root.navigationHandled) root.advance(false);
            event.accepted = root.navigationHandled;
        }
        Keys.onBacktabPressed: event => {
            if (root.navigationHandled) muteButton.forceActiveFocus(Qt.BacktabFocusReason);
            event.accepted = root.navigationHandled;
        }
        onActiveFocusChanged: { if (activeFocus) root.focusEntered(); }
        background: Rectangle {
            x: slider.leftPadding
            y: (slider.height - height) / 2
            width: slider.availableWidth
            height: 4
            radius: 2
            color: Tokens.border
            Rectangle {
                width: slider.visualPosition * parent.width
                height: parent.height
                radius: 2
                color: root.muted ? Tokens.overlay : Tokens.accent
                opacity: slider.enabled ? 1 : 0.4
            }
        }
        handle: Rectangle {
            x: slider.leftPadding + slider.visualPosition * (slider.availableWidth - width)
            y: (slider.height - height) / 2
            width: 12
            height: 12
            radius: 6
            color: root.muted ? Tokens.subtext : Tokens.text
            opacity: slider.enabled ? 1 : 0.4
            border.width: slider.visualFocus ? 2 : 0
            border.color: Tokens.accent
        }
    }

    Text {
        id: percent
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        width: 42
        text: root.usable ? Math.round((slider.pressed && !root.cancelled ? slider.value : root.level) * 100) + "%" : "--"
        horizontalAlignment: Text.AlignRight
        color: root.muted ? Tokens.subtext : Tokens.text
        font.family: Tokens.fontFamily
        font.pixelSize: 12
    }
}
