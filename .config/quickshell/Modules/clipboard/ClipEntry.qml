import QtQuick
import qs.Common
import qs.Services

MouseArea {
    id: root
    required property var entry
    property bool selected: false
    signal selectionRequested()
    signal copyRequested()
    height: Tokens.clipboardRowHeight
    hoverEnabled: true
    onClicked: selectionRequested()
    onDoubleClicked: copyRequested()
    onEntryChanged: Clipboard.requestThumbnail(entry)
    Component.onCompleted: Clipboard.requestThumbnail(entry)

    Rectangle {
        anchors.fill: parent
        anchors.topMargin: 2
        anchors.bottomMargin: 2
        radius: Tokens.radiusSm
        color: root.selected ? Tokens.selection : (root.containsMouse ? Tokens.surface : "transparent")
        border.width: root.selected ? 1 : 0
        border.color: "#4c393e"
    }
    Rectangle {
        id: tile
        x: 12
        anchors.verticalCenter: parent.verticalCenter
        width: 42
        height: 42
        radius: 6
        color: root.entry.kind === "color" ? root.entry.preview.trim() : Tokens.bgAlt
        border.width: 1
        border.color: Tokens.separator
        Image {
            id: thumbnail
            anchors.fill: parent
            anchors.margins: 3
            source: root.entry.kind === "image" ? Clipboard.thumbnailFor(root.entry) : ""
            sourceSize.width: 84
            sourceSize.height: 84
            fillMode: Image.PreserveAspectFit
            asynchronous: true
            cache: false
        }
        Glyph {
            anchors.centerIn: parent
            visible: root.entry.kind !== "color" && thumbnail.status !== Image.Ready
            name: root.entry.kind === "image" ? "image" : (root.entry.kind === "link" ? "arrowRight" : "text")
            color: root.selected ? Tokens.text : Tokens.subtext
            font.pixelSize: 18
        }
    }
    Column {
        anchors.left: tile.right
        anchors.leftMargin: 12
        anchors.right: parent.right
        anchors.rightMargin: root.entry.source === "pin" ? 32 : 14
        anchors.verticalCenter: parent.verticalCenter
        spacing: 5
        Text {
            width: parent.width
            text: root.entry.title || root.entry.label || "Clipboard item"
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 16
            elide: Text.ElideRight
            textFormat: Text.PlainText
        }
        Text {
            width: parent.width
            text: root.entry.subtitle || "Text"
            color: Tokens.overlay
            font.family: Tokens.fontFamily
            font.pixelSize: 12
            elide: Text.ElideRight
            textFormat: Text.PlainText
        }
    }
    Glyph {
        anchors.right: parent.right
        anchors.rightMargin: 12
        anchors.verticalCenter: parent.verticalCenter
        name: "pin"
        visible: root.entry.source === "pin"
        color: Tokens.accent
        font.pixelSize: 11
    }
}
