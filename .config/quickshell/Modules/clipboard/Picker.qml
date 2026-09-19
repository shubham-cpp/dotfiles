pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

PanelWindow {
    id: win

    visible: Clipboard.open
    screen: Quickshell.screens[0] || null
    color: "transparent"
    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-clipboard"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    property bool expanded: false
    readonly property bool split: panel.width >= 760
    readonly property bool hasResults: Clipboard.results.length > 0
    readonly property int listWidth: split ? 400 : panel.width - 2

    MouseArea {
        anchors.fill: parent
        onClicked: Clipboard.close()
    }

    function togglePreview() {
        if (hasResults) {
            expanded = !expanded;
            input.forceActiveFocus();
        }
    }
    function dismiss() {
        if (expanded) {
            expanded = false;
            input.forceActiveFocus();
        } else {
            Clipboard.close();
        }
    }

    function cycleFilter(direction) {
        const filters = ["all", "text", "image", "link", "pinned"];
        Clipboard.filter = filters[(filters.indexOf(Clipboard.filter) + direction + filters.length) % filters.length];
        input.forceActiveFocus();
    }

    function handleKey(event) {
        const ctrl = event.modifiers & Qt.ControlModifier;
        if (event.key === Qt.Key_Escape)
            dismiss();
        else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
            Clipboard.copySelected();
        else if (event.key === Qt.Key_Down || (ctrl && event.key === Qt.Key_J))
            Clipboard.move(1);
        else if (event.key === Qt.Key_Up || (ctrl && event.key === Qt.Key_K))
            Clipboard.move(-1);
        else if (ctrl && event.key === Qt.Key_P)
            Clipboard.togglePinSelected();
        else if (ctrl && event.key === Qt.Key_Delete)
            Clipboard.deleteSelected();
        else if (ctrl && event.key === Qt.Key_F) {
            input.forceActiveFocus();
            input.selectAll();
        } else if (event.key === Qt.Key_Tab || event.key === Qt.Key_Backtab) {
            if (ctrl)
                cycleFilter(event.modifiers & Qt.ShiftModifier ? -1 : 1);
            else
                togglePreview();
        } else if (event.key === Qt.Key_PageDown)
            preview.scrollPage(1);
        else if (event.key === Qt.Key_PageUp)
            preview.scrollPage(-1);
        else
            return;
        event.accepted = true;
    }

    Rectangle {
        id: panel
        anchors.centerIn: parent
        width: Math.min(Tokens.clipboardWidth, win.width - 32)
        height: Math.min(624, win.height - Tokens.barHeight - 32)
        radius: Tokens.panelRadius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border

        MouseArea { anchors.fill: parent }

        Item {
            id: search
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            height: 72
            Glyph {
                x: 24
                anchors.verticalCenter: parent.verticalCenter
                name: "search"
                font.pixelSize: 20
                color: Tokens.overlay
            }
            TextInput {
                id: input
                anchors.fill: parent
                anchors.leftMargin: 58
                anchors.rightMargin: 76
                verticalAlignment: Text.AlignVCenter
                color: Tokens.text
                selectionColor: Tokens.selection
                selectedTextColor: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: 22
                clip: true
                selectByMouse: true
                focus: true
                text: Clipboard.query
                onTextChanged: Clipboard.query = text
                Keys.onPressed: event => win.handleKey(event)
                Text {
                    anchors.fill: parent
                    verticalAlignment: Text.AlignVCenter
                    text: "Search clipboard…"
                    visible: !input.text
                    color: Tokens.overlay
                    font: input.font
                }
                Component.onCompleted: forceActiveFocus()
            }
            ClipAction {
                anchors.right: parent.right
                anchors.rightMargin: 18
                anchors.verticalCenter: parent.verticalCenter
                text: "Esc"
                onTriggered: win.dismiss()
            }
        }
        Item {
            id: filters
            anchors.top: search.bottom
            anchors.left: parent.left
            anchors.right: parent.right
            height: 42
            Row {
                x: 18
                height: parent.height
                spacing: 4
                Repeater {
                    model: [ { key: "all", label: "All" }, { key: "text", label: "Text" }, { key: "image", label: "Images" }, { key: "link", label: "Links" }, { key: "pinned", label: "Pinned" } ]
                    delegate: MouseArea {
                        id: tab
                        required property var modelData
                        width: tabLabel.implicitWidth + 24
                        height: filters.height
                        hoverEnabled: true
                        onClicked: {
                            Clipboard.filter = modelData.key;
                            input.forceActiveFocus();
                        }
                        Text {
                            id: tabLabel
                            anchors.centerIn: parent
                            text: tab.modelData.label
                            font.family: Tokens.fontFamily
                            font.pixelSize: 13
                            color: Clipboard.filter === tab.modelData.key ? Tokens.text : (tab.containsMouse ? Tokens.subtext : Tokens.overlay)
                        }
                        Rectangle {
                            anchors.bottom: parent.bottom
                            anchors.horizontalCenter: parent.horizontalCenter
                            width: tabLabel.width
                            height: 2
                            color: Tokens.accent
                            visible: Clipboard.filter === tab.modelData.key
                        }
                    }
                }
            }
            Text {
                anchors.right: parent.right
                anchors.rightMargin: 24
                anchors.verticalCenter: parent.verticalCenter
                visible: panel.width >= 620
                text: Clipboard.results.length + (Clipboard.results.length === 1 ? " item" : " items")
                color: Tokens.overlay
                font.family: Tokens.fontFamily
                font.pixelSize: 12
            }
        }
        Rectangle {
            anchors.top: filters.bottom
            width: parent.width - 2
            x: 1
            height: 1
            color: Tokens.separator
        }
        Item {
            id: body
            anchors.top: filters.bottom
            anchors.topMargin: 1
            anchors.bottom: actionError.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.leftMargin: 1
            anchors.rightMargin: 1
            clip: true

            ListView {
                id: list
                x: 8
                width: win.listWidth - 16
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.topMargin: 8
                anchors.bottomMargin: 8
                visible: win.hasResults && !win.expanded
                clip: true
                model: Clipboard.results
                currentIndex: Clipboard.selected
                boundsBehavior: Flickable.StopAtBounds
                ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }
                onCurrentIndexChanged: positionViewAtIndex(currentIndex, ListView.Contain)
                delegate: ClipEntry {
                    required property var modelData
                    required property int index
                    width: list.width
                    entry: modelData
                    selected: index === Clipboard.selected
                    onSelectionRequested: { Clipboard.selected = index; input.forceActiveFocus(); }
                    onCopyRequested: Clipboard.copySelected()
                }
            }
            Rectangle {
                x: win.listWidth
                width: 1
                height: parent.height
                color: Tokens.separator
                visible: win.split && !win.expanded && win.hasResults
            }
            ClipPreview {
                id: preview
                x: win.expanded ? 0 : win.listWidth + 1
                width: Math.max(0, parent.width - x)
                height: parent.height
                visible: win.hasResults && (win.split || win.expanded)
                expanded: win.expanded
                keyHandler: event => win.handleKey(event)
                onToggleExpanded: win.togglePreview()
            }
            Column {
                anchors.centerIn: parent
                width: Math.min(parent.width - 48, 360)
                spacing: 12
                visible: !win.hasResults
                Glyph {
                    anchors.horizontalCenter: parent.horizontalCenter
                    name: Clipboard.filter === "pinned" ? "pin" : "clipboard"
                    font.pixelSize: 30
                    color: Tokens.overlay
                }
                Text {
                    width: parent.width
                    horizontalAlignment: Text.AlignHCenter
                    text: Clipboard.query ? "No matches" : (Clipboard.filter === "pinned" ? "No pinned items" : "Nothing here yet")
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 18
                }
                Text {
                    width: parent.width
                    horizontalAlignment: Text.AlignHCenter
                    wrapMode: Text.WordWrap
                    text: Clipboard.query ? "Try a different search or choose All." : (Clipboard.filter === "pinned" ? "Pin an item with Ctrl+P to keep it here." : "Copied content will appear in your history.")
                    color: Tokens.overlay
                    font.family: Tokens.fontFamily
                    font.pixelSize: 14
                }
            }
        }
        Text {
            id: actionError
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.leftMargin: 20
            anchors.rightMargin: 20
            anchors.bottom: footer.top
            height: Clipboard.actionError.length ? 40 : 0
            visible: height > 0
            text: Clipboard.actionError
            textFormat: Text.PlainText
            color: Tokens.danger
            font.family: Tokens.fontFamily
            font.pixelSize: 12
            wrapMode: Text.WordWrap
            verticalAlignment: Text.AlignVCenter
        }
        Item {
            id: footer
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            height: 48
            Rectangle { width: parent.width - 2; x: 1; height: 1; color: Tokens.separator }
            Row {
                x: 12
                anchors.verticalCenter: parent.verticalCenter
                spacing: 2
                ClipAction {
                    text: Clipboard.current && Clipboard.current.source === "pin" ? "Unpin" : "Pin"
                    hint: "Ctrl+P"
                    enabled: win.hasResults
                    onTriggered: Clipboard.togglePinSelected()
                }
                ClipAction {
                    text: "Delete"
                    hint: "Ctrl+Del"
                    enabled: win.hasResults
                    onTriggered: Clipboard.deleteSelected()
                }
                ClipAction {
                    visible: !win.split
                    text: win.expanded ? "Back" : "Preview"
                    hint: "Tab"
                    enabled: win.hasResults
                    onTriggered: win.togglePreview()
                }
            }
            Text {
                anchors.right: parent.right
                anchors.rightMargin: 132
                anchors.verticalCenter: parent.verticalCenter
                visible: win.split
                text: "Ctrl+Tab  filter"
                color: Tokens.overlay
                font.family: Tokens.fontFamily
                font.pixelSize: 12
            }
            ClipAction {
                anchors.right: parent.right
                anchors.rightMargin: 14
                anchors.verticalCenter: parent.verticalCenter
                text: Clipboard.copying ? "Copying…" : "Copy"
                hint: "Enter"
                primary: true
                enabled: win.hasResults && !Clipboard.copying
                onTriggered: Clipboard.copySelected()
            }
        }
    }
}
