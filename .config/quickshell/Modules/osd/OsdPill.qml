import Quickshell
import Quickshell.Wayland
import QtQuick
import qs.Common
import qs.Services
import qs.Modules.bar

PanelWindow {
    id: win

    readonly property int panelWidth: 280

    visible: Osd.open
    screen: Quickshell.screens[0] || null
    exclusionMode: ExclusionMode.Ignore
    color: "transparent"
    focusable: false
    implicitWidth: panelWidth
    implicitHeight: 64
    mask: Region {
        item: pill
        radius: Tokens.radius
    }

    anchors.left: true
    anchors.bottom: true
    margins.bottom: 36
    margins.left: Math.max(0, Math.round(((screen ? screen.width : 1920) - panelWidth) / 2))

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-osd"

    Rectangle {
        id: pill
        anchors.fill: parent
        radius: Tokens.radius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border

        Glyph {
            id: icon
            anchors.left: parent.left
            anchors.leftMargin: 18
            anchors.verticalCenter: parent.verticalCenter
            width: 22
            name: Osd.kind === "brightness" ? "brightness" : (Audio.muted ? "volumeOff" : "volume")
            font.pixelSize: 20
            color: Tokens.subtext
        }

        Column {
            anchors.left: icon.right
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            anchors.leftMargin: 14
            anchors.rightMargin: 18
            spacing: 10

            Item {
                width: parent.width
                height: 18

                BarLabel {
                    anchors.left: parent.left
                    text: Osd.kind === "brightness" ? "Brightness" : (Audio.muted ? "Muted" : "Volume")
                    font.pixelSize: 13
                }

                BarLabel {
                    anchors.right: parent.right
                    text: Math.round(Osd.level * 100) + "%"
                    color: Tokens.subtext
                    font.pixelSize: 13
                }
            }

            Rectangle {
                width: parent.width
                height: 4
                radius: 2
                color: Tokens.surface

                Rectangle {
                    width: parent.width * Math.max(0, Math.min(1, Osd.level))
                    height: parent.height
                    radius: 2
                    color: (Osd.kind === "volume" && Audio.muted) ? Tokens.overlay : Tokens.accent
                }
            }
        }
    }
}
