import QtQuick
import QtQuick.Controls
import qs.Common

Button {
    id: control
    implicitHeight: 34
    implicitWidth: Math.max(34, label.implicitWidth + 20)
    font.family: Tokens.fontFamily
    font.pixelSize: Tokens.fontMd
    hoverEnabled: true
    focusPolicy: Qt.StrongFocus
    contentItem: Text {
        id: label
        text: control.text
        font: control.font
        color: control.enabled ? Tokens.text : Tokens.overlay
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
        elide: Text.ElideRight
    }
    background: Rectangle {
        radius: Tokens.radiusSm
        color: control.down || control.checked ? Tokens.selection : control.hovered ? Tokens.hover : "transparent"
        border.width: control.activeFocus || control.checked ? 1 : 0
        border.color: Tokens.accent
    }
}
