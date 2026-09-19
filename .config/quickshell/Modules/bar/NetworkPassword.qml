pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

Column {
    id: root
    required property var net
    spacing: 8

    function submit() {
        // Accepted submission unloads this editor; do not access it afterwards.
        Network.submitPsk(password.text);
    }

    Text {
        width: parent.width
        text: "Password for " + (root.net ? root.net.name : "")
        color: Tokens.subtext
        font.family: Tokens.fontFamily
        font.pixelSize: 12
        wrapMode: Text.Wrap
        textFormat: Text.PlainText
    }
    TextField {
        id: password
        objectName: "networkPasswordInput"
        Accessible.name: "Wi-Fi password"
        width: parent.width
        height: 38
        echoMode: reveal.checked ? TextInput.Normal : TextInput.Password
        placeholderText: "Password"
        color: Tokens.text
        placeholderTextColor: Tokens.overlay
        font.family: Tokens.fontFamily
        font.pixelSize: 13
        selectByMouse: true
        inputMethodHints: Qt.ImhSensitiveData | Qt.ImhNoPredictiveText
        onAccepted: root.submit()
        Keys.onEscapePressed: event => {
            Network.promptNet = null;
            event.accepted = true;
        }
        background: Rectangle {
            radius: Tokens.radiusSm
            color: Tokens.bgAlt
            border.width: 1
            border.color: Network.failNet === root.net && Network.failMessage.length ? Tokens.danger : (password.activeFocus ? Tokens.accent : Tokens.border)
        }
        Component.onCompleted: forceActiveFocus()
    }
    Row {
        spacing: 8
        NetworkButton {
            id: reveal
            checkable: true
            text: checked ? "Hide" : "Show"
        }
        NetworkButton {
            text: "Cancel"
            onClicked: Network.promptNet = null
        }
        NetworkButton {
            text: "Join"
            prominent: true
            enabled: password.text.length > 0 && !Network.busy
            onClicked: root.submit()
        }
    }
    onNetChanged: {
        password.clear();
        reveal.checked = false;
        password.forceActiveFocus();
    }
    Component.onDestruction: password.clear()
}
