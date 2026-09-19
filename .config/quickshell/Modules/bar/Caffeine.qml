import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    implicitWidth: block.implicitWidth
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    onClicked: Idle.toggle()

    BarModule {
        id: block
        anchors.fill: parent
        icon: "coffee"
        foreground: Idle.enabled ? Tokens.warning : Tokens.barText
        hovered: root.containsMouse
    }
}
