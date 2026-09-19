import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    required property var anchorWindow
    implicitWidth: content.implicitWidth + 28
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    onClicked: Agenda.toggle(root.anchorWindow)

    Rectangle {
        anchors.fill: parent
        radius: 4
        color: root.containsMouse || Agenda.open ? Tokens.barHover : Tokens.barModule
    }

    Row {
        id: content
        anchors.centerIn: parent
        spacing: 8

        Glyph {
            anchors.verticalCenter: parent.verticalCenter
            name: "clock"
            color: Tokens.barText
        }

        BarLabel {
            anchors.verticalCenter: parent.verticalCenter
            text: Clock.text.split("  ")[0]
            color: Tokens.barText
            font.family: Tokens.barFontFamily
        }

        Glyph {
            anchors.verticalCenter: parent.verticalCenter
            name: "calendar"
            color: Tokens.barText
        }

        BarLabel {
            anchors.verticalCenter: parent.verticalCenter
            text: Clock.text.split("  ")[1] || ""
            color: Tokens.barText
            font.family: Tokens.barFontFamily
        }
    }
}
