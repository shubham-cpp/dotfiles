import Quickshell
import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root

    property bool showRates: true
    readonly property Item popupAnchor: wifiBlock.iconItem

    implicitWidth: content.implicitWidth
    implicitHeight: Tokens.barModuleHeight
    hoverEnabled: true
    cursorShape: Qt.PointingHandCursor
    acceptedButtons: Qt.LeftButton | Qt.RightButton
    onClicked: mouse => {
        if (mouse.button === Qt.RightButton)
            Network.setWifiEnabled(!Network.wifiRadio);
        else
            Network.toggle(root.QsWindow.window, root.popupAnchor);
    }

    Row {
        id: content
        spacing: Tokens.barGap
        height: Tokens.barModuleHeight

        BarModule {
            id: wifiBlock
            icon: {
                if (!Network.wifiRadio)
                    return "wifiOff";
                if (Network.wifi)
                    return "wifi";
                return Network.connected ? "ethernet" : "wifi";
            }
            text: {
                if (!Network.wifiRadio)
                    return "wifi off";
                if (!Network.connected)
                    return "offline";
                const name = Network.ssid || Network.iface;
                return name.length > 12 ? name.slice(0, 11) + "…" : name;
            }
            foreground: Network.connected ? Tokens.barText : Tokens.overlay
            hovered: root.containsMouse || Network.open
        }

        Loader {
            active: root.showRates && root.visible && Network.connected
            visible: active
            sourceComponent: Rectangle {
                implicitWidth: rates.implicitWidth + 24
                implicitHeight: Tokens.barModuleHeight
                radius: 4
                color: root.containsMouse || Network.open ? Tokens.barHover : Tokens.barModule
                Component.onCompleted: Network.setRateConsumer(root, true)
                Component.onDestruction: Network.setRateConsumer(root, false)

                Row {
                    id: rates
                    anchors.centerIn: parent
                    spacing: 6

                    Glyph {
                        anchors.verticalCenter: parent.verticalCenter
                        name: "download"
                        color: Tokens.barText
                    }
                    BarLabel {
                        anchors.verticalCenter: parent.verticalCenter
                        text: Network.fmt(Network.downBps) + "/s"
                        font.family: Tokens.barFontFamily
                        color: Tokens.barText
                    }
                    Glyph {
                        anchors.verticalCenter: parent.verticalCenter
                        name: "upload"
                        color: Tokens.barText
                    }
                    BarLabel {
                        anchors.verticalCenter: parent.verticalCenter
                        text: Network.fmt(Network.upBps) + "/s"
                        font.family: Tokens.barFontFamily
                        color: Tokens.barText
                    }
                }
            }
        }
    }
}
