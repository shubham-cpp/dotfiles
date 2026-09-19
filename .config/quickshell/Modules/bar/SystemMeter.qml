pragma ComponentBehavior: Bound

import Quickshell
import QtQuick
import qs.Common
import qs.Services as Services

Rectangle {
    id: root

    readonly property Item popupAnchor: root
    implicitWidth: content.implicitWidth + 16
    implicitHeight: Tokens.barModuleHeight
    radius: 4
    color: mouse.containsMouse || Services.Resources.open ? Tokens.barHover : Tokens.barModule

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: Services.Resources.toggle(root.QsWindow.window, root.popupAnchor)
    }

    Row {
        id: content
        anchors.centerIn: parent
        spacing: 8

        Repeater {
            model: [
                { icon: "memory", value: Services.SystemStats.memoryText },
                { icon: "cpu", value: Services.SystemStats.cpuText },
                { icon: "temperature", value: Services.SystemStats.temperatureText }
            ]

            delegate: Row {
                id: metric
                required property var modelData
                spacing: 4

                Glyph {
                    anchors.verticalCenter: parent.verticalCenter
                    name: metric.modelData.icon
                    color: Tokens.barText
                }

                BarLabel {
                    anchors.verticalCenter: parent.verticalCenter
                    text: metric.modelData.value
                    color: Tokens.barText
                    font.family: Tokens.barFontFamily
                }
            }
        }
    }
}
