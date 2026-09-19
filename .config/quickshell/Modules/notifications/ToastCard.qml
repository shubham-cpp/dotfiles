pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Widgets
import QtQuick
import qs.Common
import qs.Services
import qs.Modules.bar

Rectangle {
    id: root

    required property var notification
    signal replyHoverChanged(Item input, bool hovered)

    readonly property var n: notification
    readonly property string iconSrc: {
        if (n.image && n.image.length)
            return n.image;
        if (n.appIcon && n.appIcon.length)
            return Quickshell.iconPath(n.appIcon, true);
        const entry = DesktopEntries.heuristicLookup(n.desktopEntry || n.appName);
        if (entry && entry.icon)
            return Quickshell.iconPath(entry.icon, true);
        return "";
    }
    readonly property real expireSec: Notifications.expireSec(n)
    readonly property string plainSummary: Notifications.strip(n.summary)
    readonly property string plainBody: Notifications.strip(n.body)
    readonly property var labeledActions: n.actions.filter(action => action.identifier === "default" || String(action.text || "").trim().length > 0)

    property bool hovered: false
    property bool copied: false
    property real expiryProgress: 1

    width: Tokens.toastWidth
    implicitHeight: col.implicitHeight + 32
    radius: Tokens.radius
    color: Tokens.bg
    border.width: 1
    border.color: n.urgency === NotificationUrgency.Critical ? Tokens.danger : Tokens.border

    function copyText(summaryOnly) {
        Notifications.copy(summaryOnly || !root.plainBody.length ? root.plainSummary : root.plainBody);
        root.copied = true;
        copyFeedback.restart();
    }

    function restartExpiry() {
        expiry.stop();
        root.expiryProgress = 1;
        if (root.expireSec > 0)
            expiry.start();
    }

    Component.onCompleted: root.restartExpiry()

    Connections {
        target: Notifications
        function onToastUpdated(notificationId) {
            if (notificationId === root.n.id)
                root.restartExpiry();
        }
    }


    HoverHandler {
        onHoveredChanged: root.hovered = hovered
    }

    Timer {
        id: copyFeedback
        interval: 1400
        onTriggered: root.copied = false
    }

    NumberAnimation {
        id: expiry
        target: root
        property: "expiryProgress"
        from: 1
        to: 0
        duration: Math.max(0, root.expireSec * 1000)
        paused: running && root.hovered
        onFinished: Notifications.expireToast(root.n)
    }

    Column {
        id: col
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 16
        spacing: 10

        Item {
            width: parent.width
            height: 32

            BarLabel {
                anchors.left: parent.left
                anchors.right: utilities.left
                anchors.rightMargin: 8
                anchors.verticalCenter: parent.verticalCenter
                text: root.n.appName || "Notification"
                textFormat: Text.PlainText
                color: Tokens.overlay
                font.pixelSize: 11
                elide: Text.ElideRight
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
                    text: "Dismiss"
                    onClicked: Notifications.dismissLive(root.n)
                }
            }
        }
        Row {
            spacing: 8
            width: parent.width

            IconImage {
                id: appIcon
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
                text: root.plainSummary
                color: Tokens.text
                selectionColor: Tokens.accent
                selectedTextColor: Tokens.bgAlt
                wrapMode: Text.Wrap
                textFormat: TextEdit.PlainText
                font.family: Tokens.fontFamily
                font.pixelSize: 14
                font.weight: Font.DemiBold
                persistentSelection: false
            }
        }

        TextEdit {
            visible: root.plainBody.length > 0
            width: parent.width
            readOnly: true
            selectByMouse: true
            text: root.plainBody
            color: Tokens.subtext
            selectionColor: Tokens.accent
            selectedTextColor: Tokens.bgAlt
            wrapMode: Text.Wrap
            textFormat: TextEdit.PlainText
            font.family: Tokens.fontFamily
            font.pixelSize: 13
        }

        Flow {
            width: parent.width
            spacing: 6
            visible: root.labeledActions.length > 0

            Repeater {
                model: root.labeledActions

                delegate: MouseArea {
                    id: actionButton
                    required property var modelData
                    implicitWidth: Math.min(col.width, actLabel.implicitWidth + 16)
                    implicitHeight: 28
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        const id = root.n.id;
                        const resident = root.n.resident;
                        Notifications.invoke(id, modelData.identifier);
                        if (!resident)
                            Notifications.hideToast(id);
                    }

                    Rectangle {
                        anchors.fill: parent
                        radius: Tokens.radiusSm
                        color: actionButton.containsMouse ? Tokens.hover : Tokens.surface
                    }

                    BarLabel {
                        id: actLabel
                        anchors.centerIn: parent
                        width: parent.width - 16
                        text: String(actionButton.modelData.text || "").trim() || "Open"
                        textFormat: Text.PlainText
                        color: Tokens.accent
                        font.pixelSize: 12
                        elide: Text.ElideRight
                    }
                }
            }
        }

        Row {
            visible: root.n.hasInlineReply
            spacing: 6
            width: parent.width

            Rectangle {
                width: parent.width - 52
                height: 32
                radius: Tokens.radiusSm
                color: Tokens.bgAlt
                border.width: 1
                border.color: reply.activeFocus ? Tokens.accent : Tokens.border

                TextInput {
                    id: reply
                    anchors.fill: parent
                    anchors.margins: 8
                    verticalAlignment: Text.AlignVCenter
                    color: Tokens.text
                    selectionColor: Tokens.accent
                    selectedTextColor: Tokens.bgAlt
                    font.family: Tokens.fontFamily
                    font.pixelSize: 12
                    clip: true
                    Keys.onReturnPressed: {
                        root.n.sendInlineReply(reply.text);
                        Notifications.hideToast(root.n.id);
                    }

                    HoverHandler {
                        cursorShape: Qt.IBeamCursor
                        onHoveredChanged: root.replyHoverChanged(reply, hovered)
                    }
                }

                BarLabel {
                    anchors.fill: reply
                    visible: reply.text.length === 0
                    text: "Reply..."
                    color: Tokens.overlay
                    font.pixelSize: 12
                }
            }

            MouseArea {
                implicitWidth: 46
                implicitHeight: 32
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                    root.n.sendInlineReply(reply.text);
                    Notifications.hideToast(root.n.id);
                }

                BarLabel {
                    anchors.centerIn: parent
                    text: "Send"
                    color: Tokens.accent
                    font.pixelSize: 12
                }
            }
        }

    }

    Rectangle {
        id: drainTrack
        visible: root.expireSec > 0
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.leftMargin: Tokens.radius
        anchors.rightMargin: Tokens.radius
        anchors.bottomMargin: 1
        height: 2
        radius: 1
        color: Tokens.surface

        Rectangle {
            height: parent.height
            radius: 1
            width: drainTrack.width * root.expiryProgress
            color: Tokens.accent
        }
    }
}
