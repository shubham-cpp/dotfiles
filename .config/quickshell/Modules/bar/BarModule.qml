import QtQuick
import qs.Common

Rectangle {
    id: root

    property string icon: ""
    property Component iconDelegate
    property string text: ""
    property bool hovered: false
    property color foreground: Tokens.barText
    readonly property Item iconItem: iconLoader.item ? iconLoader.item : glyph

    implicitWidth: content.implicitWidth + 24
    implicitHeight: Tokens.barModuleHeight
    radius: 4
    color: hovered ? Tokens.barHover : Tokens.barModule

    Row {
        id: content
        anchors.centerIn: parent
        spacing: 8

        Loader {
            id: iconLoader
            active: root.iconDelegate !== null
            visible: active && item
            sourceComponent: root.iconDelegate
            anchors.verticalCenter: parent.verticalCenter
        }

        Glyph {
            id: glyph
            visible: !iconLoader.active && root.icon.length > 0
            anchors.verticalCenter: parent.verticalCenter
            name: root.icon
            color: root.foreground
        }

        BarLabel {
            visible: root.text.length > 0
            anchors.verticalCenter: parent.verticalCenter
            text: root.text
            color: root.foreground
            font.family: Tokens.barFontFamily
        }
    }
}
