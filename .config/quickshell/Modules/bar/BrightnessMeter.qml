import Quickshell
import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    readonly property Item popupAnchor: block.iconItem

    visible: Brightness.present
    implicitWidth: visible ? block.implicitWidth : 0
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    onClicked: Brightness.toggle(root.QsWindow.window, root.popupAnchor, root)
    onWheel: event => {
        Brightness.adjust(event.angleDelta.y > 0 ? 5 : -5);
        event.accepted = true;
    }

    BarModule {
        id: block
        anchors.fill: parent
        icon: "brightness"
        text: Math.round(Brightness.percent * 100) + "%"
        hovered: root.containsMouse || Brightness.open
    }
}
