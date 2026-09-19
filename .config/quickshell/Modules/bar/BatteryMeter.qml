import Quickshell
import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    visible: Power.present
    implicitWidth: visible ? block.implicitWidth : 0
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    onClicked: Power.toggle(root.QsWindow.window)

    BarModule {
        id: block
        anchors.fill: parent
        iconDelegate: BatteryIcon {
            iconHeight: 13
            fraction: Power.percent
            charging: Power.plugged
            foreground: Power.tone
        }
        text: Power.percentInt + "%"
        foreground: Power.tone
        hovered: root.containsMouse || Power.open
    }
}
