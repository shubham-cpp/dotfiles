import QtQuick
import QtQuick.Controls
import qs.Common

Button {
    id: root
    property bool prominent: false
    implicitWidth: contentItem.implicitWidth + 20
    implicitHeight: 30
    padding: 6
    focusPolicy: Qt.StrongFocus
    hoverEnabled: true

    background: Rectangle {
        radius: Tokens.radiusSm
        color: root.down || root.prominent ? Tokens.selection : (root.hovered ? Tokens.hover : Tokens.surface)
        border.width: root.visualFocus ? 1 : 0
        border.color: Tokens.accent
        opacity: root.enabled ? 1 : 0.45
    }
    contentItem: Text {
        text: root.text
        color: root.enabled ? (root.prominent ? Tokens.accent : Tokens.text) : Tokens.overlay
        font.family: Tokens.fontFamily
        font.pixelSize: 12
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
        textFormat: Text.PlainText
        renderType: Text.NativeRendering
    }
}
