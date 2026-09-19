pragma ComponentBehavior: Bound

import Quickshell
import QtQuick
import qs.Common
import qs.Services

PopupWindow {
    id: win

    required property var anchorWindow

    readonly property var host: Power.anchorWindow || anchorWindow
    readonly property int panelWidth: 320
    readonly property color fillColor: {
        if (Power.severity === "danger")
            return Tokens.danger;
        if (Power.severity === "warning")
            return Tokens.warning;
        if (Power.plugged)
            return Tokens.success;
        return Tokens.accent;
    }

    anchor.window: host
    anchor.rect.x: (host ? host.width : 1920) - implicitWidth - 8
    anchor.rect.y: (host ? host.height : Tokens.barHeight) + Tokens.overlayGap
    grabFocus: true
    onVisibleChanged: {
        if (!visible && Power.open)
            Power.close();
    }

    visible: Power.open
    implicitWidth: panelWidth
    implicitHeight: Math.max(1, panel.implicitHeight)
    color: "transparent"
    mask: Region {
        item: panel
        radius: Tokens.radius
    }

    Rectangle {
        id: panel
        width: panelWidth
        implicitHeight: col.implicitHeight + 32
        height: implicitHeight
        radius: Tokens.radius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border
        focus: true
        Keys.onEscapePressed: Power.close()

        Column {
            id: col
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.margins: 16
            spacing: 14

            Row {
                width: parent.width
                spacing: 12

                BatteryIcon {
                    anchors.verticalCenter: parent.verticalCenter
                    iconHeight: 18
                    fraction: Power.percent
                    charging: Power.plugged
                    foreground: win.fillColor
                }

                Column {
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 2

                    Text {
                        text: Power.percentInt + "%"
                        color: win.fillColor
                        font.family: Tokens.fontFamily
                        font.pixelSize: 22
                        font.weight: Font.Medium
                        renderType: Text.NativeRendering
                    }

                    Text {
                        text: Power.stateLabel
                        color: Tokens.subtext
                        font.family: Tokens.fontFamily
                        font.pixelSize: 13
                        renderType: Text.NativeRendering
                    }
                }
            }

            Item {
                width: parent.width
                height: 16

                Text {
                    anchors.fill: parent
                    text: Power.etaLabel
                    color: Tokens.subtext
                    font.family: Tokens.fontFamily
                    font.pixelSize: 13
                    renderType: Text.NativeRendering
                    elide: Text.ElideRight
                }
            }

            Rectangle {
                width: parent.width
                height: 8
                radius: 4
                color: Tokens.surface

                Rectangle {
                    width: Math.max(0, Math.min(parent.width, parent.width * Power.percent))
                    height: parent.height
                    radius: 4
                    color: win.fillColor
                }
            }

            Item {
                visible: Power.healthSupported
                width: parent.width
                height: visible ? 20 : 0

                Text {
                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Health"
                    color: Tokens.subtext
                    font.family: Tokens.fontFamily
                    font.pixelSize: 13
                    renderType: Text.NativeRendering
                }

                Text {
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    text: Power.healthInt + "%"
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 13
                    font.weight: Font.Medium
                    renderType: Text.NativeRendering
                }
            }

            Rectangle {
                width: parent.width
                height: 1
                color: Tokens.separator
            }

            Rectangle {
                id: segs
                width: parent.width
                height: 40
                radius: Tokens.radiusSm
                color: Tokens.bgAlt
                border.width: 1
                border.color: Tokens.border
                clip: true

                Row {
                    id: segRow
                    anchors.fill: parent
                    anchors.margins: 3
                    spacing: 2

                    Repeater {
                        model: Power.profiles

                        delegate: MouseArea {
                            id: seg
                            required property var modelData

                            width: Math.max(1, (segRow.width - 2 * Math.max(0, Power.profiles.length - 1)) / Math.max(1, Power.profiles.length))
                            height: segRow.height
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: Power.setProfile(modelData.profile)

                            readonly property bool selected: Power.profile === modelData.profile

                            Rectangle {
                                anchors.fill: parent
                                radius: 4
                                color: seg.selected ? Tokens.selection : (seg.containsMouse ? Tokens.hover : "transparent")
                            }

                            Row {
                                anchors.centerIn: parent
                                spacing: 6

                                Glyph {
                                    anchors.verticalCenter: parent.verticalCenter
                                    name: seg.modelData.icon
                                    color: seg.selected ? Tokens.accent : Tokens.subtext
                                }

                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: seg.modelData.name
                                    color: seg.selected ? Tokens.accent : Tokens.subtext
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 12
                                    font.weight: seg.selected ? Font.DemiBold : Font.Normal
                                    renderType: Text.NativeRendering
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
