pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import qs.Common
import qs.Services
import qs.Modules.bar

PanelWindow {
    id: win

    readonly property var selectedResult: Files.results[Files.selected] || null
    readonly property int panelWidth: Math.min(Tokens.launcherWidth, Math.max(1, (screen ? screen.width : 1920) - 48))
    readonly property int panelHeight: Math.min(Tokens.searchHeight + Tokens.launcherRowHeight * 8 + 28 + Tokens.pad * 2 + Tokens.footerHeight, Math.max(1, (screen ? screen.height : 1080) - 80))

    visible: Files.open
    screen: Quickshell.screens.find(screen => screen.name === Files.screenName) || Quickshell.screens[0] || null
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
    WlrLayershell.namespace: "quickshell-files"
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
                focus: Files.open
                text: Files.query
                onTextChanged: Files.query = text
                Keys.onEscapePressed: Files.close()
                Keys.onReturnPressed: Files.openSelected()
                Keys.onEnterPressed: Files.openSelected()
                Keys.onDownPressed: Files.move(1)
                Keys.onUpPressed: Files.move(-1)
                Keys.onPressed: event => {
                    if (!(event.modifiers & Qt.ControlModifier))
                        return;
                    if (event.key === Qt.Key_J || event.key === Qt.Key_K) {
                        Files.move(event.key === Qt.Key_J ? 1 : -1);
                        event.accepted = true;
                    }
                }
            }

            Text {
                anchors.fill: input
                verticalAlignment: Text.AlignVCenter
                visible: input.text.length === 0
                text: "Search files..."
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
                onClicked: Files.close()

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
            model: Files.results
            currentIndex: Files.selected
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
                onClicked: Files.openRow(row.modelData)
                onEntered: Files.selected = index

                Rectangle {
                    anchors.fill: parent
                    radius: Tokens.radius
                    color: row.index === Files.selected ? Tokens.selection : "transparent"
                }

                Glyph {
                    id: fileIcon
                    anchors.left: parent.left
                    anchors.leftMargin: 16
                    anchors.verticalCenter: parent.verticalCenter
                    name: "file"
                    color: Tokens.subtext
                    font.pixelSize: 18
                }

                Column {
                    anchors.left: fileIcon.right
                    anchors.right: parent.right
                    anchors.leftMargin: 12
                    anchors.rightMargin: 16
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
                        visible: String(row.modelData.dir || "").length > 0
                        text: row.modelData.dir
                        textFormat: Text.PlainText
                        color: Tokens.overlay
                        font.pixelSize: 12
                        elide: Text.ElideMiddle
                        width: parent.width
                    }
                }
            }
        }

        BarLabel {
            anchors.centerIn: list
            visible: Files.results.length === 0
            text: Files.searchError || (Files.searching ? "Searching…" : (Files.query.length ? "No files found" : "Type to search files"))
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
                text: "Files"
                color: Tokens.overlay
                visible: footer.width > 420
            }

            MouseArea {
                id: openButton
                anchors.right: parent.right
                anchors.rightMargin: 12
                anchors.verticalCenter: parent.verticalCenter
                width: openLabel.implicitWidth + 16
                height: 28
                enabled: !!win.selectedResult
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: Files.openSelected()

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

    Connections {
        target: Files
        function onOpenChanged() {
            if (Files.open) {
                input.text = "";
                input.forceActiveFocus();
                list.positionViewAtBeginning();
            }
        }
    }
}
