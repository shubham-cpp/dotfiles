pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import Quickshell.Widgets
import QtQuick
import qs.Common
import qs.Services
import qs.Modules.bar

PanelWindow {
    id: win

    readonly property var selectedResult: LauncherStats.results[LauncherStats.selected] || null
    readonly property int panelWidth: Math.min(Tokens.launcherWidth, Math.max(1, (screen ? screen.width : 1920) - 48))
    readonly property int panelHeight: Math.min(Tokens.searchHeight + Tokens.launcherRowHeight * 8 + 28 + Tokens.pad * 2 + Tokens.footerHeight, Math.max(1, (screen ? screen.height : 1080) - 80))

    visible: LauncherStats.open
    screen: Quickshell.screens[0] || null
    exclusionMode: ExclusionMode.Ignore
    color: "transparent"
    implicitWidth: panelWidth
    implicitHeight: panelHeight
    mask: Region {
        item: panel
        radius: Tokens.panelRadius
    }

    anchors.left: true
    anchors.top: true
    margins.left: Math.max(0, Math.round(((screen ? screen.width : 1920) - panelWidth) / 2))
    margins.top: Math.max(0, Math.round(((screen ? screen.height : 1080) - panelHeight) / 2))

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-launcher"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    Rectangle {
        id: panel
        anchors.fill: parent
        radius: Tokens.panelRadius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border

        Item {
            id: inputRow
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            height: Tokens.searchHeight

            Glyph {
                id: searchIcon
                anchors.left: parent.left
                anchors.leftMargin: 22
                anchors.verticalCenter: parent.verticalCenter
                name: "search"
                color: Tokens.overlay
                font.pixelSize: 20
            }

            TextInput {
                id: input
                anchors.left: searchIcon.right
                anchors.right: closeButton.left
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.leftMargin: 14
                anchors.rightMargin: 12
                verticalAlignment: Text.AlignVCenter
                color: Tokens.text
                selectionColor: Tokens.accent
                selectedTextColor: Tokens.bgAlt
                font.family: Tokens.fontFamily
                font.pixelSize: 22
                selectByMouse: true
                clip: true
                focus: LauncherStats.open
                text: LauncherStats.query
                onTextChanged: LauncherStats.query = text
                Keys.onEscapePressed: LauncherStats.close()
                Keys.onReturnPressed: LauncherStats.launchSelected()
                Keys.onEnterPressed: LauncherStats.launchSelected()
                Keys.onDownPressed: LauncherStats.move(1)
                Keys.onUpPressed: LauncherStats.move(-1)
                Keys.onPressed: event => {
                    if (!(event.modifiers & Qt.ControlModifier))
                        return;
                    if (event.key === Qt.Key_J || event.key === Qt.Key_K) {
                        LauncherStats.move(event.key === Qt.Key_J ? 1 : -1);
                        event.accepted = true;
                    } else if (event.key === Qt.Key_P) {
                        if (win.selectedResult)
                            LauncherStats.togglePin(win.selectedResult.id);
                        event.accepted = true;
                    }
                }
            }

            Text {
                anchors.fill: input
                verticalAlignment: Text.AlignVCenter
                visible: input.text.length === 0
                text: "Search applications..."
                color: Tokens.overlay
                font: input.font
                elide: Text.ElideRight
            }

            MouseArea {
                id: closeButton
                anchors.right: parent.right
                anchors.rightMargin: 14
                anchors.verticalCenter: parent.verticalCenter
                width: 32
                height: 32
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: LauncherStats.close()

                Rectangle {
                    anchors.fill: parent
                    radius: Tokens.radiusSm
                    color: closeButton.containsMouse ? Tokens.hover : "transparent"
                }

                Glyph {
                    anchors.centerIn: parent
                    name: "close"
                    color: Tokens.overlay
                }
            }

            Rectangle {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.leftMargin: 1
                anchors.rightMargin: 1
                height: 1
                color: Tokens.separator
            }
        }

        ListView {
            id: list
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: inputRow.bottom
            anchors.bottom: footer.top
            anchors.margins: Tokens.pad
            clip: true
            boundsBehavior: Flickable.StopAtBounds
            spacing: 4
            model: LauncherStats.results
            currentIndex: LauncherStats.selected
            highlightMoveDuration: 0
            onCurrentIndexChanged: positionViewAtIndex(currentIndex, ListView.Contain)

            delegate: MouseArea {
                id: row
                required property var modelData
                required property int index

                width: list.width
                height: Tokens.launcherRowHeight
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: LauncherStats.launch(modelData)
                onEntered: LauncherStats.selected = index

                Rectangle {
                    anchors.fill: parent
                    radius: Tokens.radius
                    color: row.index === LauncherStats.selected ? Tokens.selection : "transparent"
                }

                IconImage {
                    id: appIcon
                    anchors.left: parent.left
                    anchors.leftMargin: 12
                    anchors.verticalCenter: parent.verticalCenter
                    implicitSize: 28
                    source: Icons.src(row.modelData.icon || row.modelData.id)
                    asynchronous: true
                    visible: status !== Image.Error && status !== Image.Null
                }

                Text {
                    anchors.centerIn: appIcon
                    visible: !appIcon.visible
                    text: String(row.modelData.name || "?").charAt(0).toUpperCase()
                    font.family: Tokens.fontFamily
                    font.pixelSize: 22
                    font.weight: Font.DemiBold
                    color: Tokens.subtext
                }

                Column {
                    anchors.left: appIcon.right
                    anchors.right: pinIcon.left
                    anchors.leftMargin: 12
                    anchors.rightMargin: 12
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 2

                    BarLabel {
                        text: row.modelData.name
                        textFormat: Text.PlainText
                        color: Tokens.text
                        font.pixelSize: 16
                        elide: Text.ElideRight
                        width: parent.width
                    }

                    BarLabel {
                        visible: String(row.modelData.comment || "").length > 0 && LauncherStats.query.length > 0
                        text: row.modelData.comment
                        textFormat: Text.PlainText
                        color: Tokens.overlay
                        font.pixelSize: 12
                        elide: Text.ElideRight
                        width: parent.width
                    }
                }

                Glyph {
                    id: pinIcon
                    anchors.right: parent.right
                    anchors.rightMargin: 16
                    anchors.verticalCenter: parent.verticalCenter
                    width: 16
                    name: "pin"
                    opacity: row.modelData.pinned ? 1 : 0
                    color: Tokens.accent
                }
            }
        }

        BarLabel {
            anchors.centerIn: list
            visible: LauncherStats.results.length === 0
            text: LauncherStats.searchError || (LauncherStats.searching ? "Searching…" : "No applications found")
            color: Tokens.overlay
            font.pixelSize: Tokens.fontLg
        }

        Item {
            id: footer
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            height: Tokens.footerHeight

            Rectangle {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.top: parent.top
                anchors.leftMargin: 1
                anchors.rightMargin: 1
                height: 1
                color: Tokens.separator
            }

            BarLabel {
                anchors.left: parent.left
                anchors.leftMargin: 20
                anchors.verticalCenter: parent.verticalCenter
                text: "Applications"
                color: Tokens.overlay
                visible: footer.width > 420
            }

            Row {
                anchors.right: parent.right
                anchors.rightMargin: 12
                anchors.verticalCenter: parent.verticalCenter
                spacing: 8

                MouseArea {
                    id: pinButton
                    width: pinLabel.implicitWidth + 16
                    height: 28
                    enabled: !!win.selectedResult
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: LauncherStats.togglePin(win.selectedResult.id)

                    Rectangle {
                        anchors.fill: parent
                        radius: Tokens.radiusSm
                        color: pinButton.containsMouse ? Tokens.hover : "transparent"
                    }

                    BarLabel {
                        id: pinLabel
                        anchors.centerIn: parent
                        text: win.selectedResult && win.selectedResult.pinned ? "Ctrl+P  Unpin" : "Ctrl+P  Pin"
                        color: pinButton.enabled ? Tokens.subtext : Tokens.overlay
                        font.pixelSize: 12
                    }
                }

                MouseArea {
                    id: openButton
                    width: openLabel.implicitWidth + 16
                    height: 28
                    enabled: !!win.selectedResult
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: LauncherStats.launchSelected()

                    Rectangle {
                        anchors.fill: parent
                        radius: Tokens.radiusSm
                        color: openButton.containsMouse ? Tokens.hover : Tokens.surface
                    }

                    BarLabel {
                        id: openLabel
                        anchors.centerIn: parent
                        text: "Enter  Open"
                        color: openButton.enabled ? Tokens.text : Tokens.overlay
                        font.pixelSize: 12
                    }
                }
            }
        }
    }

    Connections {
        target: LauncherStats
        function onOpenChanged() {
            if (LauncherStats.open) {
                input.text = "";
                input.forceActiveFocus();
                list.positionViewAtBeginning();
            }
        }
    }
}
