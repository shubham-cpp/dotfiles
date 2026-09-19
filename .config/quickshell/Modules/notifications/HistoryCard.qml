pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Controls as Controls
import qs.Common
import qs.Services
import qs.Modules.bar
import "../../Common/NotificationHistory.js" as History

Rectangle {
    id: root

    required property var entry
    property bool copied: false

    readonly property var liveNotification: {
        Notifications.gen;
        return entry.live ? Notifications.liveById(entry.id) : null;
    }
    readonly property var actions: liveNotification ? liveNotification.actions.filter(action => action.identifier === "default" || String(action.text || "").trim().length > 0) : []
    readonly property string iconSrc: {
        if (entry.appIcon)
            return Quickshell.iconPath(entry.appIcon, true);
        const desktop = DesktopEntries.heuristicLookup(entry.desktopEntry || entry.appName);
        return desktop && desktop.icon ? Quickshell.iconPath(desktop.icon, true) : "";
    }
    readonly property string timestamp: {
        const date = new Date(entry.time || 0);
        return Qt.formatDateTime(date, History.dayGroup(date.getTime(), Clock.date) === "Earlier" ? "d MMM, HH:mm" : "HH:mm");
    }

    implicitHeight: content.implicitHeight + 24
    radius: Tokens.radius
    color: Tokens.bgAlt
    border.width: 1
    border.color: entry.urgency === 2 ? Tokens.danger : Tokens.separator

    function copyText(summaryOnly) {
        Notifications.copy(summaryOnly ? root.entry.summary : (root.entry.body || root.entry.summary));
        root.copied = true;
        copyFeedback.restart();
    }

    function sendReply() {
        if (root.liveNotification && reply.text.trim().length)
            root.liveNotification.sendInlineReply(reply.text);
    }

    Timer {
        id: copyFeedback
        interval: 1400
        onTriggered: root.copied = false
    }

    Column {
        id: content
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 12
        spacing: 10

        Item {
            width: parent.width
            height: 32

            Row {
                id: metadata
                anchors.left: parent.left
                anchors.right: timestampLabel.left
                anchors.rightMargin: 8
                anchors.verticalCenter: parent.verticalCenter
                spacing: 6

                Rectangle {
                    id: unreadDot
                    visible: !root.entry.read
                    anchors.verticalCenter: parent.verticalCenter
                    width: 5
                    height: 5
                    radius: 2.5
                    color: Tokens.accent
                    Accessible.name: "Unread"
                }

                BarLabel {
                    width: metadata.width - (unreadDot.visible ? 11 : 0)
                    text: root.entry.appName || "Notification"
                    textFormat: Text.PlainText
                    font.pixelSize: 11
                    color: Tokens.overlay
                    elide: Text.ElideRight
                }
            }

            BarLabel {
                id: timestampLabel
                anchors.right: utilities.left
                anchors.rightMargin: 8
                anchors.verticalCenter: parent.verticalCenter
                text: root.timestamp
                color: Tokens.overlay
                font.pixelSize: 11
            }

            Row {
                id: utilities
                anchors.right: parent.right
                spacing: 4

                IconButton {
                    iconName: root.copied ? "check" : "copy"
                    text: root.copied ? "Copied" : "Copy text (right-click for title)"
                    onClicked: root.copyText(false)

                    MouseArea {
                        anchors.fill: parent
                        acceptedButtons: Qt.RightButton
                        onClicked: root.copyText(true)
                    }
                }

                IconButton {
                    iconName: "close"
                    text: "Dismiss notification"
                    onClicked: Notifications.dismissHistory(root.entry)
                }
            }
        }

        Row {
            width: parent.width
            spacing: 8

            IconImage {
                visible: root.iconSrc.length > 0
                implicitSize: 24
                source: root.iconSrc
                asynchronous: true
            }

            TextEdit {
                width: parent.width - (root.iconSrc.length > 0 ? 32 : 0)
                topPadding: root.iconSrc.length > 0 ? 3 : 0
                readOnly: true
                selectByMouse: true
                text: root.entry.summary || ""
                textFormat: TextEdit.PlainText
                wrapMode: TextEdit.Wrap
                color: Tokens.text
                selectionColor: Tokens.accent
                selectedTextColor: Tokens.bgAlt
                font.family: Tokens.fontFamily
                font.pixelSize: 14
                font.weight: Font.DemiBold
            }
        }

        TextEdit {
            visible: text.length > 0
            width: parent.width
            readOnly: true
            selectByMouse: true
            text: root.entry.body || ""
            textFormat: TextEdit.PlainText
            wrapMode: TextEdit.Wrap
            color: Tokens.subtext
            selectionColor: Tokens.accent
            selectedTextColor: Tokens.bgAlt
            font.family: Tokens.fontFamily
            font.pixelSize: 13
        }

        Flow {
            visible: root.actions.length > 0
            width: parent.width
            spacing: 6

            Repeater {
                model: root.actions

                delegate: Controls.Button {
                    id: actionButton
                    required property var modelData
                    text: String(modelData.text || "").trim() || "Open"
                    implicitWidth: Math.min(content.width, actionLabel.implicitWidth + 20)
                    implicitHeight: 28
                    onClicked: Notifications.invoke(root.entry.id, modelData.identifier)

                    contentItem: BarLabel {
                        id: actionLabel
                        text: actionButton.text
                        textFormat: Text.PlainText
                        color: Tokens.accent
                        font.pixelSize: 12
                        horizontalAlignment: Text.AlignHCenter
                        elide: Text.ElideRight
                    }

                    background: Rectangle {
                        radius: Tokens.radiusSm
                        color: actionButton.hovered ? Tokens.hover : Tokens.surface
                        border.width: actionButton.visualFocus ? 1 : 0
                        border.color: Tokens.accent
                    }
                }
            }
        }

        Row {
            visible: root.liveNotification !== null && root.liveNotification.hasInlineReply
            width: parent.width
            spacing: 6

            Controls.TextField {
                id: reply
                width: parent.width - sendButton.width - parent.spacing
                height: 32
                placeholderText: root.liveNotification ? (root.liveNotification.inlineReplyPlaceholder || "Reply...") : "Reply..."
                color: Tokens.text
                placeholderTextColor: Tokens.overlay
                selectionColor: Tokens.accent
                selectedTextColor: Tokens.bgAlt
                font.family: Tokens.fontFamily
                font.pixelSize: 12
                onAccepted: root.sendReply()

                background: Rectangle {
                    radius: Tokens.radiusSm
                    color: Tokens.bg
                    border.width: 1
                    border.color: reply.activeFocus ? Tokens.accent : Tokens.border
                }
            }

            IconButton {
                id: sendButton
                iconName: "arrowRight"
                text: "Send reply"
                enabled: reply.text.trim().length > 0
                onClicked: root.sendReply()
            }
        }
    }
}
