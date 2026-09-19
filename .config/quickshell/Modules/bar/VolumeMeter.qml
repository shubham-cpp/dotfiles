import Quickshell
import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    readonly property Item popupAnchor: block.iconItem

    implicitWidth: block.implicitWidth
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    acceptedButtons: Qt.LeftButton | Qt.RightButton
    onClicked: mouse => {
        if (mouse.button === Qt.RightButton)
            Audio.toggleMute();
        else
            Audio.toggle(root.QsWindow.window, root.popupAnchor, root);
    }
    onWheel: event => {
        if (event.angleDelta.y !== 0)
            Audio.adjust(event.angleDelta.y > 0 ? 0.05 : -0.05);
        event.accepted = true;
    }

    BarModule {
        id: block
        anchors.fill: parent
        icon: Audio.muted ? "volumeOff" : "volume"
        text: Audio.audio ? Math.round(Audio.volume * 100) + "%" : "--"
        foreground: Audio.muted ? Tokens.danger : Tokens.barText
        hovered: root.containsMouse || Audio.open
    }
}
