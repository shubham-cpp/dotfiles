pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import qs.Common
import qs.Services
import "../../Common/EmojiCatalog.js" as Catalog

PanelWindow {
    id: win
    property string variantId: ""
    property string chooserSeed: ""
    readonly property var selectedFamily: Emoji.current ? Emoji.catalog.families[Emoji.current.familyId] : null
    readonly property var categories: [{ id: "recent", name: "Recent" }, { id: "all", name: "All" }].concat(
        Emoji.catalog ? Emoji.catalog.groups.map(group => ({ id: group, name: group })) : [])
    readonly property int columns: Math.max(1, Math.floor(grid.width / Tokens.emojiCellSize))

    visible: Emoji.open
    screen: Quickshell.screens.find(screen => screen.name === Emoji.screenName) || Quickshell.screens[0] || null
    color: "transparent"
    anchors { left: true; right: true; top: true; bottom: true }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-emoji"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    function variants() {
        if (selectedFamily && selectedFamily.variants.length > 1) {
            chooserSeed = Emoji.current.id;
            variantId = Emoji.current.id;
        }
    }

    function selectAt(index) {
        Emoji.selected = Math.max(0, Math.min(Emoji.results.length - 1, index));
        grid.positionViewAtIndex(Emoji.selected, GridView.Contain);
    }

    function copy() {
        if (Emoji.current)
            Emoji.copyId(Emoji.current.id);
    }

    function handleKey(event, fromGrid) {
        if (variantId)
            return;
        if (event.key === Qt.Key_Escape) {
            Emoji.close();
        } else if (event.key === Qt.Key_F && event.modifiers & Qt.ControlModifier) {
            search.forceActiveFocus();
            search.selectAll();
        } else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
            if (event.modifiers & Qt.ShiftModifier)
                variants();
            else
                copy();
        } else if (event.key === Qt.Key_Down || event.key === Qt.Key_Up) {
            if (fromGrid)
                selectAt(Emoji.selected + (event.key === Qt.Key_Down ? columns : -columns));
            grid.forceActiveFocus();
        } else if (fromGrid && (event.key === Qt.Key_Right || event.key === Qt.Key_Left)) {
            selectAt(Emoji.selected + (event.key === Qt.Key_Right ? 1 : -1));
        } else if (fromGrid && (event.key === Qt.Key_Home || event.key === Qt.Key_End)) {
            selectAt(event.key === Qt.Key_Home ? 0 : Emoji.results.length - 1);
        } else if (fromGrid && (event.key === Qt.Key_PageDown || event.key === Qt.Key_PageUp)) {
            selectAt(Emoji.selected + (event.key === Qt.Key_PageDown ? 1 : -1) * columns * Math.max(1, Math.floor(grid.height / grid.cellHeight)));
        } else {
            return;
        }
        event.accepted = true;
    }

    onVariantIdChanged: Emoji.variantOpen = variantId !== ""
    Component.onCompleted: { Emoji.uiCount++; search.forceActiveFocus(); }
    Component.onDestruction: { Emoji.uiCount--; Emoji.variantOpen = false; }

    MouseArea { anchors.fill: parent; onClicked: Emoji.close() }

    Rectangle {
        id: panel
        width: Math.max(1, Math.min(Tokens.emojiWidth, win.width - 32))
        height: Math.max(1, Math.min(560, win.height - 32))
        anchors.centerIn: parent
        color: Tokens.bg
        radius: Tokens.radius
        border.width: 1
        border.color: Tokens.border
        clip: true
        Keys.onPressed: event => {
            if (event.key === Qt.Key_Escape || (event.key === Qt.Key_F && event.modifiers & Qt.ControlModifier))
                win.handleKey(event, false);
        }
        MouseArea { anchors.fill: parent }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 16
            spacing: 10
            enabled: !win.variantId

            RowLayout {
                Layout.fillWidth: true
                TextField {
                    id: search
                    Layout.fillWidth: true
                    implicitHeight: 42
                    placeholderText: "Search emoji"
                    text: Emoji.query
                    color: Tokens.text
                    placeholderTextColor: Tokens.overlay
                    selectionColor: Tokens.accent
                    selectedTextColor: Tokens.bg
                    font.family: Tokens.fontFamily
                    font.pixelSize: Tokens.fontLg
                    selectByMouse: true
                    Accessible.name: "Search emoji"
                    background: Rectangle { color: Tokens.bgAlt; radius: Tokens.radiusSm; border.width: 1; border.color: search.activeFocus ? Tokens.accent : Tokens.border }
                    onTextChanged: Emoji.query = text
                    Keys.onPressed: event => win.handleKey(event, false)
                }
                EmojiButton { text: "×"; Accessible.name: "Close emoji picker"; onClicked: Emoji.close() }
            }

            RowLayout {
                Layout.fillWidth: true
                Text { text: "Skin tone"; color: Tokens.subtext; font.family: Tokens.fontFamily; font.pixelSize: Tokens.fontMd }
                ToneSelect {
                    id: globalTone
                    model: Catalog.toneNames
                    currentIndex: Emoji.preferences.tone
                    enabled: !Emoji.loading && !!Emoji.catalog
                    Accessible.name: "Default skin tone"
                    onActivated: Emoji.setTone(currentIndex)
                    Layout.preferredWidth: 150
                }
                Item { Layout.fillWidth: true }
                Text {
                    text: Emoji.query.trim() ? Emoji.results.length + (Emoji.results.length === 1 ? " result" : " results") : ""
                    color: Tokens.overlay
                    font.family: Tokens.fontFamily
                    font.pixelSize: Tokens.fontSm
                }
            }

            Flickable {
                id: categoryScroll
                Layout.fillWidth: true
                Layout.preferredHeight: 36
                contentWidth: categoryRow.width
                contentHeight: height
                clip: true
                boundsBehavior: Flickable.StopAtBounds
                Row {
                    id: categoryRow
                    spacing: 4
                    Repeater {
                        model: win.categories
                        delegate: EmojiButton {
                            required property var modelData
                            text: modelData.name
                            checked: !Emoji.query.trim() && Emoji.category === modelData.id
                            onClicked: { Emoji.query = ""; Emoji.category = modelData.id; win.selectAt(0); }
                            onActiveFocusChanged: {
                                if (activeFocus)
                                    categoryScroll.contentX = Math.max(0, Math.min(x, categoryScroll.contentWidth - categoryScroll.width));
                            }
                        }
                    }
                }
            }

            Rectangle { Layout.fillWidth: true; implicitHeight: 1; color: Tokens.separator }

            Item {
                Layout.fillWidth: true
                Layout.fillHeight: true
                GridView {
                    id: grid
                    anchors.fill: parent
                    clip: true
                    model: Emoji.results
                    currentIndex: Emoji.selected
                    cellWidth: width / win.columns
                    cellHeight: Tokens.emojiCellSize
                    cacheBuffer: Tokens.emojiCellSize
                    boundsBehavior: Flickable.StopAtBounds
                    activeFocusOnTab: true
                    keyNavigationEnabled: false
                    visible: !Emoji.loading && !Emoji.error
                    Keys.onPressed: event => win.handleKey(event, true)
                    onCurrentIndexChanged: positionViewAtIndex(currentIndex, GridView.Contain)
                    ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                    delegate: Item {
                        id: tile
                        required property string modelData
                        required property int index
                        readonly property var entry: Emoji.catalog.entries[modelData]
                        readonly property bool hasVariants: Emoji.catalog.families[entry.familyId].variants.length > 1
                        width: grid.cellWidth
                        height: grid.cellHeight
                        Rectangle {
                            anchors.fill: parent
                            anchors.margins: 2
                            radius: Tokens.radiusSm
                            color: tile.index === Emoji.selected ? Tokens.selection : hit.containsMouse ? Tokens.hover : "transparent"
                            border.width: tile.index === Emoji.selected ? 1 : 0
                            border.color: Tokens.accent
                        }
                        Text {
                            anchors.centerIn: parent
                            text: tile.entry.text
                            textFormat: Text.PlainText
                            font.family: Tokens.emojiFontFamily
                            font.pixelSize: Tokens.emojiGlyphSize
                        }
                        Accessible.role: Accessible.Button
                        Accessible.name: tile.entry.name
                        Accessible.onPressAction: { win.selectAt(tile.index); win.copy(); }
                        MouseArea {
                            id: hit
                            anchors.fill: parent
                            hoverEnabled: true
                            acceptedButtons: Qt.LeftButton | Qt.RightButton
                            cursorShape: Qt.PointingHandCursor
                            onClicked: mouse => {
                                win.selectAt(tile.index);
                                if (mouse.button === Qt.RightButton)
                                    win.variants();
                                else
                                    win.copy();
                            }
                        }
                        ToolTip.visible: hit.containsMouse
                        ToolTip.delay: 600
                        ToolTip.text: tile.entry.name
                        EmojiButton {
                            visible: tile.hasVariants
                            anchors.right: parent.right
                            anchors.bottom: parent.bottom
                            width: 18
                            height: 18
                            text: "·"
                            focusPolicy: Qt.NoFocus
                            Accessible.name: "Choose skin tone for " + tile.entry.name
                            onClicked: { win.selectAt(tile.index); win.variants(); }
                        }
                    }
                }

                Column {
                    anchors.centerIn: parent
                    width: parent.width - 24
                    spacing: 12
                    visible: Emoji.loading || !!Emoji.error || !Emoji.results.length
                    Text {
                        width: parent.width
                        text: Emoji.error || (Emoji.loading ? "Loading emoji…" : Emoji.category === "recent" && !Emoji.query ? "Your copied emoji will appear here." : "No emoji match your search.")
                        wrapMode: Text.Wrap
                        horizontalAlignment: Text.AlignHCenter
                        color: Emoji.error ? Tokens.danger : Tokens.subtext
                        font.family: Tokens.fontFamily
                        font.pixelSize: Tokens.fontMd
                    }
                    EmojiButton { anchors.horizontalCenter: parent.horizontalCenter; visible: !!Emoji.error; text: "Retry"; onClicked: Emoji.load() }
                }
            }

            Text {
                visible: !!Emoji.saveError
                Layout.fillWidth: true
                text: Emoji.saveError
                wrapMode: Text.Wrap
                color: Tokens.warning
                font.family: Tokens.fontFamily
                font.pixelSize: Tokens.fontSm
            }
            Rectangle { Layout.fillWidth: true; implicitHeight: 1; color: Tokens.separator }
            RowLayout {
                Layout.fillWidth: true
                Text {
                    Layout.fillWidth: true
                    text: Emoji.current ? Emoji.current.name : "Select an emoji"
                    elide: Text.ElideRight
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: Tokens.fontSm
                }
                EmojiButton { text: "Variants"; enabled: !!win.selectedFamily && win.selectedFamily.variants.length > 1; onClicked: win.variants() }
            }
            Text {
                text: "Enter  Copy     Shift+Enter  Skin tone     Ctrl+F  Search     Esc  Close"
                Layout.fillWidth: true
                elide: Text.ElideRight
                color: Tokens.overlay
                font.family: Tokens.fontFamily
                font.pixelSize: Tokens.fontSm
            }
        }

        MouseArea { anchors.fill: parent; visible: !!win.variantId; onClicked: win.variantId = "" }
        Loader {
            id: chooser
            anchors.centerIn: parent
            width: parent.width - 24
            height: Math.min(parent.height - 24, win.selectedFamily && win.selectedFamily.slots === 2 ? 400 : 250)
            active: !!win.variantId
            onActiveChanged: { if (!active) grid.forceActiveFocus(); }
            sourceComponent: VariantChooser {
                catalog: Emoji.catalog
                initialId: win.chooserSeed
                Component.onCompleted: { Emoji.chooserCount++; forceActiveFocus(); }
                Component.onDestruction: Emoji.chooserCount--
                onApplied: id => {
                    Emoji.setVariant(family.id, id);
                    win.variantId = "";
                }
                onDismissed: win.variantId = ""
            }
        }
    }
}
