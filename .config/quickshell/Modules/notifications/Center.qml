pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls as Controls
import qs.Common
import qs.Services
import qs.Modules.bar
import "../../Common/NotificationHistory.js" as History

PanelWindow {
    id: win

    required property var anchorWindow
    readonly property string today: Qt.formatDateTime(Clock.date, "yyyy-MM-dd")
    readonly property var historyRows: History.rows(Notifications.history, new Date(today + "T00:00:00"))
    onHistoryRowsChanged: History.sync(historyModel, historyRows)
    Component.onCompleted: History.sync(historyModel, historyRows)

    function entryForKey(key) {
        return Notifications.history.find(entry => History.key(entry) === key) || {};
    }

    ListModel {
        id: historyModel
    }

    screen: anchorWindow ? anchorWindow.screen : (Quickshell.screens[0] || null)
    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-notification-centre"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
    onVisibleChanged: {
        if (!visible && Notifications.centerOpen)
            Notifications.centerOpen = false;
    }

    visible: Notifications.centerOpen
    color: "transparent"

    MouseArea {
        anchors.fill: parent
        onClicked: Notifications.centerOpen = false
    }

    Rectangle {
        id: panel
        anchors.top: parent.top
        anchors.right: parent.right
        anchors.topMargin: Tokens.barHeight + Tokens.toastMargin
        anchors.rightMargin: Tokens.toastMargin
        width: Math.min(Tokens.notificationCenterWidth, Math.max(1, win.width - 2 * Tokens.toastMargin))
        height: Math.min(Tokens.notificationCenterMaxHeight, Math.max(1, win.height - Tokens.barHeight - 2 * Tokens.toastMargin), Math.max(240, header.height + historyColumn.implicitHeight + 32))
        radius: Tokens.radius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border
        focus: true
        Keys.onEscapePressed: Notifications.centerOpen = false

        MouseArea { anchors.fill: parent }

        Item {
            id: header
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            height: 64

            Row {
                anchors.left: parent.left
                anchors.leftMargin: 16
                anchors.verticalCenter: parent.verticalCenter
                spacing: 8

                BarLabel {
                    text: "Notifications"
                    font.pixelSize: 16
                    font.weight: Font.DemiBold
                }

                BarLabel {
                    anchors.verticalCenter: parent.verticalCenter
                    text: Notifications.history.length
                    color: Tokens.overlay
                    font.pixelSize: 12
                }
            }

            Row {
                anchors.right: parent.right
                anchors.rightMargin: 12
                anchors.verticalCenter: parent.verticalCenter
                spacing: 6

                IconButton {
                    iconName: Notifications.dnd ? "bellOff" : "bell"
                    text: Notifications.dnd ? "Turn off Do not disturb" : "Turn on Do not disturb"
                    checkable: true
                    checked: Notifications.dnd
                    onClicked: Notifications.toggleDnd()
                }

                Controls.Button {
                    id: clearButton
                    text: "Clear all"
                    enabled: Notifications.history.length > 0
                    implicitWidth: 68
                    implicitHeight: 32
                    opacity: enabled ? 1 : 0.4
                    onClicked: Notifications.clearHistory()

                    contentItem: BarLabel {
                        text: clearButton.text
                        color: Tokens.subtext
                        font.pixelSize: 12
                        horizontalAlignment: Text.AlignHCenter
                    }

                    background: Rectangle {
                        radius: Tokens.radiusSm
                        color: clearButton.hovered ? Tokens.hover : "transparent"
                        border.width: clearButton.visualFocus ? 1 : 0
                        border.color: Tokens.accent
                    }
                }

                IconButton {
                    iconName: "close"
                    text: "Close notification centre"
                    onClicked: Notifications.centerOpen = false
                }
            }

            Rectangle {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.leftMargin: 16
                anchors.rightMargin: 16
                height: 1
                color: Tokens.separator
            }
        }

        Flickable {
            id: historyView
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: header.bottom
            anchors.bottom: parent.bottom
            anchors.margins: 16
            contentWidth: width
            contentHeight: historyColumn.implicitHeight
            flickableDirection: Flickable.VerticalFlick
            boundsBehavior: Flickable.StopAtBounds
            clip: true

            Column {
                id: historyColumn
                width: historyView.width
                spacing: 12

                Repeater {
                    model: historyModel

                    delegate: Column {
                        id: sectionRow
                        required property string rowKey
                        required property string section
                        width: historyColumn.width
                        spacing: 8

                        BarLabel {
                            visible: sectionRow.section.length > 0
                            height: 20
                            text: sectionRow.section
                            color: Tokens.overlay
                            font.pixelSize: 12
                        }

                        HistoryCard {
                            width: parent.width
                            entry: win.entryForKey(sectionRow.rowKey)
                        }
                    }
                }
            }

            Controls.ScrollBar.vertical: Controls.ScrollBar {
                id: scrollBar
                width: 4
                policy: Controls.ScrollBar.AsNeeded
                contentItem: Rectangle {
                    implicitWidth: 3
                    radius: 1.5
                    color: Tokens.overlay
                    opacity: scrollBar.active ? 0.8 : 0.35
                }
                background: Item {}
            }
        }

        Column {
            visible: Notifications.history.length === 0
            anchors.centerIn: historyView
            width: historyView.width
            spacing: 10

            Glyph {
                anchors.horizontalCenter: parent.horizontalCenter
                name: "bell"
                font.pixelSize: 24
                color: Tokens.overlay
            }

            BarLabel {
                width: parent.width
                text: "No notifications"
                horizontalAlignment: Text.AlignHCenter
                color: Tokens.subtext
                font.pixelSize: 14
            }
        }
    }
}
