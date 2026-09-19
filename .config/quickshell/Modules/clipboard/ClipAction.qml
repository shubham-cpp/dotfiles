import QtQuick
import qs.Common

MouseArea {
    id: root
    property string text: ""
    property string hint: ""
    property bool primary: false
    signal triggered()
    implicitWidth: label.implicitWidth + 20
    implicitHeight: 30
    hoverEnabled: true
    cursorShape: enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
    onClicked: triggered()

    Rectangle {
        anchors.fill: parent
        radius: Tokens.radiusSm
        color: root.primary ? (root.containsMouse ? "#ffffff" : Tokens.text) : (root.containsMouse ? Tokens.hover : "transparent")
        opacity: root.enabled ? 1 : 0.35
    }
    Text {
        id: label
        anchors.centerIn: parent
        text: root.text + (root.hint ? "   " + root.hint : "")
        color: !root.enabled ? Tokens.overlay : (root.primary ? Tokens.bgAlt : Tokens.subtext)
        font.family: Tokens.fontFamily
        font.pixelSize: 12
    }
}
