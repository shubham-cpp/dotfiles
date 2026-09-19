pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import qs.Common
import "../../Common/EmojiCatalog.js" as Catalog

FocusScope {
    id: root
    required property var catalog
    required property string initialId
    readonly property var family: catalog.families[catalog.entries[initialId].familyId]
    property string pendingId: initialId
    property bool initialized: false
    readonly property var pending: catalog.entries[pendingId]
    signal applied(string id)
    signal dismissed()

    implicitWidth: 508
    implicitHeight: family.slots === 2 ? 400 : 250

    function choosePair() {
        const id = family.tuples[[person1.currentIndex + 1, person2.currentIndex + 1].join(",")];
        if (id)
            pendingId = id;
    }

    function syncTones() {
        const entry = catalog.entries[pendingId];
        if (!initialized || !entry)
            return;
        person1.currentIndex = Math.max(0, (entry.tones[0] || 1) - 1);
        person2.currentIndex = Math.max(0, (entry.tones[1] || entry.tones[0] || 1) - 1);
        choices.currentIndex = family.variants.indexOf(pendingId);
    }

    onPendingIdChanged: syncTones()
    Component.onCompleted: { initialized = true; syncTones(); choices.forceActiveFocus(); }
    Keys.onEscapePressed: dismissed()

    Rectangle {
        anchors.fill: parent
        color: Tokens.bg
        radius: Tokens.radius
        border.width: 1
        border.color: Tokens.border
        MouseArea { anchors.fill: parent }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 16
        spacing: 10

        Text {
            text: root.family.name
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: Tokens.fontLg
            elide: Text.ElideRight
            Layout.fillWidth: true
        }

        RowLayout {
            visible: root.family.slots === 2
            Layout.fillWidth: true
            spacing: 8
            Label { text: "Person 1"; color: Tokens.subtext }
            ToneSelect {
                id: person1
                objectName: "person1"
                model: Catalog.toneNames.slice(1)
                Layout.fillWidth: true
                Accessible.name: "Person 1 skin tone"
                onActivated: root.choosePair()
            }
            Label { text: "Person 2"; color: Tokens.subtext }
            ToneSelect {
                id: person2
                objectName: "person2"
                model: Catalog.toneNames.slice(1)
                Layout.fillWidth: true
                Accessible.name: "Person 2 skin tone"
                onActivated: root.choosePair()
            }
        }

        GridView {
            id: choices
            objectName: "choices"
            Layout.fillWidth: true
            Layout.fillHeight: true
            cellWidth: width / Math.max(1, Math.floor(width / 60))
            cellHeight: 56
            clip: true
            boundsBehavior: Flickable.StopAtBounds
            model: root.family.variants
            keyNavigationEnabled: true
            activeFocusOnTab: true
            onCurrentIndexChanged: {
                if (root.initialized && currentIndex >= 0 && currentIndex < count)
                    root.pendingId = root.family.variants[currentIndex];
                positionViewAtIndex(currentIndex, GridView.Contain);
            }
            Keys.onReturnPressed: root.applied(root.pendingId)
            Keys.onEnterPressed: root.applied(root.pendingId)
            delegate: EmojiButton {
                required property string modelData
                required property int index
                width: choices.cellWidth - 4
                height: choices.cellHeight - 4
                text: root.catalog.entries[modelData].text
                font.family: Tokens.emojiFontFamily
                font.pixelSize: Tokens.emojiGlyphSize
                checked: modelData === root.pendingId
                Accessible.name: root.catalog.entries[modelData].name
                ToolTip.visible: hovered
                ToolTip.delay: 500
                ToolTip.text: Accessible.name
                onClicked: { choices.currentIndex = index; choices.forceActiveFocus(); }
            }
        }

        Text {
            Layout.fillWidth: true
            text: root.pending.name
            color: Tokens.subtext
            font.family: Tokens.fontFamily
            font.pixelSize: Tokens.fontSm
            elide: Text.ElideRight
        }

        RowLayout {
            Layout.fillWidth: true
            EmojiButton { text: "Default"; onClicked: root.pendingId = root.family.id }
            EmojiButton { text: "Use preference"; onClicked: root.applied("") }
            Item { Layout.fillWidth: true }
            EmojiButton { text: "Cancel"; onClicked: root.dismissed() }
            EmojiButton { text: "Apply"; checked: true; onClicked: root.applied(root.pendingId) }
        }
    }
}
