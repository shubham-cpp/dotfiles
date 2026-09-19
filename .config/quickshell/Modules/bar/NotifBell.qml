import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    implicitWidth: block.implicitWidth
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    acceptedButtons: Qt.LeftButton | Qt.RightButton
    onClicked: mouse => {
        if (mouse.button === Qt.RightButton)
            Notifications.toggleDnd();
        else
            Notifications.toggleCenter();
    }

    BarModule {
        id: block
        anchors.fill: parent
        icon: Notifications.dnd ? "bellOff" : "bell"
        text: Notifications.unread > 0 ? String(Notifications.unread) : ""
        foreground: Notifications.dnd ? Tokens.danger : (Notifications.unread > 0 ? Tokens.warning : Tokens.barText)
        hovered: root.containsMouse || Notifications.centerOpen
    }
}
