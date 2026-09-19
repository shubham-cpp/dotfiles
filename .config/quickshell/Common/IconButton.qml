pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls as Controls
import qs.Common

Controls.AbstractButton {
    id: button

    required property string iconName

    implicitWidth: 32
    implicitHeight: 32
    hoverEnabled: true
    focusPolicy: Qt.StrongFocus
    opacity: enabled ? 1 : 0.4
    Accessible.name: text

    Controls.ToolTip {
        text: button.text
        visible: button.hovered && button.enabled
        delay: 600
        padding: 8

        contentItem: Text {
            text: button.text
            textFormat: Text.PlainText
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 11
        }

        background: Rectangle {
            color: Tokens.bgAlt
            radius: Tokens.radiusSm
            border.width: 1
            border.color: Tokens.border
        }
    }

    background: Rectangle {
        radius: Tokens.radiusSm
        color: button.checked ? Tokens.selection : (button.down || button.hovered ? Tokens.hover : "transparent")
        border.width: button.visualFocus ? 1 : 0
        border.color: Tokens.accent
    }

    contentItem: Glyph {
        name: button.iconName
        color: button.checked ? Tokens.accent : (button.hovered || button.visualFocus ? Tokens.text : Tokens.subtext)
    }

    HoverHandler {
        cursorShape: button.enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
    }
}
