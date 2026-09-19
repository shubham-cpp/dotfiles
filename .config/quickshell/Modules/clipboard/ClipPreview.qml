import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

Item {
    id: root
    property bool expanded: false
    required property var keyHandler
    signal toggleExpanded()

    function scrollPage(direction) {
        scroll.contentY = Math.max(0, Math.min(scroll.contentHeight - scroll.height, scroll.contentY + direction * scroll.height * 0.8));
    }

    Rectangle {
        anchors.fill: parent
        color: Tokens.bgAlt
    }
    Item {
        id: heading
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 20
        height: 36
        Text {
            anchors.left: parent.left
            anchors.right: expand.left
            anchors.rightMargin: 12
            anchors.verticalCenter: parent.verticalCenter
            text: Clipboard.current ? Clipboard.current.subtitle : "Preview"
            color: Tokens.overlay
            font.family: Tokens.fontFamily
            font.pixelSize: 12
            elide: Text.ElideRight
            textFormat: Text.PlainText
        }
        ClipAction {
            id: expand
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            text: root.expanded ? "Back" : "Expand"
            hint: "Tab"
            onTriggered: root.toggleExpanded()
        }
    }
    Item {
        id: content
        anchors.top: heading.bottom
        anchors.topMargin: 12
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.leftMargin: 22
        anchors.rightMargin: 22
        anchors.bottomMargin: 22

        Image {
            anchors.fill: parent
            visible: Clipboard.previewKind === "image" && !Clipboard.previewLoading
            source: visible ? Clipboard.previewImage : ""
            sourceSize.width: Math.ceil(width * 2)
            sourceSize.height: Math.ceil(height * 2)
            fillMode: Image.PreserveAspectFit
            asynchronous: true
            cache: false
        }
        Flickable {
            id: scroll
            anchors.fill: parent
            visible: Clipboard.previewKind !== "image" && !Clipboard.previewLoading && !Clipboard.previewError
            contentWidth: width
            contentHeight: previewColumn.height
            boundsBehavior: Flickable.StopAtBounds
            clip: true
            ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

            Column {
                id: previewColumn
                width: scroll.width
                spacing: 20
                Rectangle {
                    visible: Clipboard.previewKind === "color"
                    width: parent.width
                    height: visible ? 150 : 0
                    radius: Tokens.radiusSm
                    color: visible ? Clipboard.previewText.trim() : "transparent"
                }
                TextEdit {
                    width: parent.width
                    height: Math.max(implicitHeight, 24)
                    text: Clipboard.previewText
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 16
                    wrapMode: TextEdit.Wrap
                    textFormat: TextEdit.PlainText
                    Keys.onPressed: event => root.keyHandler(event)
                    readOnly: true
                    selectByMouse: true
                    selectionColor: Tokens.selection
                    selectedTextColor: Tokens.text
                    persistentSelection: true
                }
            }
        }
        Text {
            anchors.centerIn: parent
            width: parent.width - 24
            horizontalAlignment: Text.AlignHCenter
            wrapMode: Text.WordWrap
            text: Clipboard.previewLoading ? "Loading preview…" : Clipboard.previewError
            visible: Clipboard.previewLoading || !!Clipboard.previewError
            color: Tokens.overlay
            font.family: Tokens.fontFamily
            font.pixelSize: 14
        }
    }
    Connections {
        target: Clipboard
        function onCurrentChanged() { scroll.contentY = 0; }
    }
}
