pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

Column {
    id: root

    property bool input: false
    property var devices: []
    property var current: null
    property var pending: null
    property bool waiting: false
    property string message: ""
    readonly property bool popupVisible: combo.popup.visible
    property bool navigationHandled: false
    signal opening()
    signal advance(bool backwards)
    signal focusEntered()
    spacing: 6

    function closeMenu() { combo.popup.close(); }
    function focusSelector() {
        if (combo.enabled) combo.forceActiveFocus(Qt.TabFocusReason);
        return combo.enabled;
    }
    function settle() {
        if (!waiting)
            return;
        if (!pending || !Audio.ready || !devices.some(d => d.node === pending)) {
            message = "Device unavailable";
        } else if (current !== pending) {
            return;
        }
        waiting = false;
        pending = null;
        timeout.stop();
    }

    onCurrentChanged: { message = ""; settle(); }
    onDevicesChanged: {
        // An open menu must never activate a different object after an index shift.
        if (combo.popup.visible)
            closeMenu();
        settle();
    }
    Connections {
        target: Audio
        function onReadyChanged() { root.settle(); }
    }
    Timer {
        id: timeout
        interval: 3000
        onTriggered: {
            root.waiting = false;
            root.pending = null;
            root.message = "Switch could not be confirmed";
        }
    }

    Text {
        text: root.input ? "Input" : "Output"
        color: Tokens.subtext
        font.family: Tokens.fontFamily
        font.pixelSize: 12
    }
    ComboBox {
        id: combo
        objectName: root.input ? "audioInput" : "audioOutput"
        width: parent.width
        height: 36
        model: root.devices
        textRole: "label"
        currentIndex: root.devices.findIndex(d => d.node === root.current)
        enabled: Audio.ready && !root.waiting && root.devices.length > 1
        focusPolicy: Qt.StrongFocus
        hoverEnabled: true
        Accessible.name: (root.input ? "Input" : "Output") + " device"
        onActiveFocusChanged: { if (activeFocus) root.focusEntered(); }
        Keys.onTabPressed: event => {
            if (root.navigationHandled) root.advance(false);
            event.accepted = root.navigationHandled;
        }
        Keys.onBacktabPressed: event => {
            if (root.navigationHandled) root.advance(true);
            event.accepted = root.navigationHandled;
        }
        displayText: {
            if (root.waiting) return "Switching…";
            const entry = root.devices.find(d => d.node === root.current);
            return entry ? entry.label : (root.input ? "No input device" : "No output device");
        }
        onActivated: index => {
            const entry = root.devices[index];
            if (!entry || entry.node === root.current)
                return;
            root.pending = entry.node;
            root.waiting = true;
            root.message = "";
            timeout.restart();
            if (!Audio.selectDevice(entry.node, root.input)) {
                root.pending = null;
                root.waiting = false;
                root.message = "Device unavailable";
                timeout.stop();
            }
            root.settle();
        }
        ToolTip.visible: hovered
        ToolTip.delay: 600
        ToolTip.text: root.message || displayText + (root.devices.length === 1 ? " · Only available device" : "")
        background: Rectangle {
            radius: Tokens.radiusSm
            color: combo.hovered ? Tokens.hover : Tokens.surface
            border.width: 1
            border.color: combo.visualFocus ? Tokens.accent : Tokens.border
            opacity: Audio.ready ? 1 : 0.5
        }
        contentItem: Item {
            Glyph {
                id: icon
                x: 10
                anchors.verticalCenter: parent.verticalCenter
                name: root.input ? "microphone" : "volume"
                color: Tokens.subtext
            }
            Text {
                anchors.left: icon.right
                anchors.leftMargin: 8
                anchors.right: parent.right
                anchors.rightMargin: 24
                anchors.verticalCenter: parent.verticalCenter
                text: combo.displayText
                textFormat: Text.PlainText
                elide: Text.ElideLeft
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: 12
            }
        }
        indicator: Text {
            x: combo.width - width - 10
            anchors.verticalCenter: parent.verticalCenter
            text: "⌄"
            color: Tokens.subtext
        }
        delegate: ItemDelegate {
            id: option
            required property var modelData
            required property int index
            width: combo.width - 8
            implicitHeight: Math.max(36, optionText.implicitHeight + 16)
            highlighted: combo.highlightedIndex === index
            contentItem: Text {
                id: optionText
                text: (option.modelData.node === root.current ? "✓ " : "") + option.modelData.label
                textFormat: Text.PlainText
                wrapMode: Text.Wrap
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: 12
            }
            background: Rectangle { color: option.highlighted ? Tokens.selection : "transparent"; radius: 4 }
        }
        popup: Popup {
            y: combo.height + 4
            width: combo.width
            padding: 4
            popupType: Popup.Item
            implicitHeight: Math.min(240, deviceList.contentHeight + 8)
            closePolicy: Popup.CloseOnEscape | Popup.CloseOnPressOutsideParent
            onAboutToShow: root.opening()
            onClosed: combo.forceActiveFocus(Qt.PopupFocusReason)
            background: Rectangle {
                color: Tokens.bgAlt
                radius: Tokens.radiusSm
                border.width: 1
                border.color: Tokens.border
            }
            contentItem: ListView {
                id: deviceList
                clip: true
                implicitHeight: contentHeight
                model: combo.popup.visible ? combo.delegateModel : null
                currentIndex: combo.highlightedIndex
                boundsBehavior: Flickable.StopAtBounds
                ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }
            }
        }
    }
    Text {
        visible: root.message.length > 0
        width: parent.width
        text: root.message
        wrapMode: Text.Wrap
        color: Tokens.warning
        font.family: Tokens.fontFamily
        font.pixelSize: 11
    }
}
