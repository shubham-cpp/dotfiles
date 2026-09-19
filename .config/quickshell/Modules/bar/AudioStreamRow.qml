pragma ComponentBehavior: Bound

import Quickshell.Widgets
import QtQuick
import QtQuick.Controls
import qs.Common
import "../../Common/AudioList.js" as AudioList

Item {
    id: root
    required property var node
    required property string appName
    required property string subtitle
    required property string group
    required property string groupLabel
    required property string routeDetail
    required property bool routeChanging
    required property int index
    property bool narrow: width < 356
    readonly property bool interacting: volume.interacting
    readonly property var metadata: AudioList.props(node)
    readonly property string suggestedIcon: String(metadata["application.icon-name"] || "")
    readonly property string appId: String(metadata["application.id"] || metadata["application.process.binary"] || appName)
    property bool triedDesktopIcon: false
    signal interactionChanged()
    signal focused(int rowIndex)
    signal advanceRow(int rowIndex, bool backwards)
    signal targetLost(int rowIndex)

    implicitHeight: narrow ? 76 : (subtitle || routeChanging ? 60 : 44)
    onInteractingChanged: interactionChanged()
    onSuggestedIconChanged: triedDesktopIcon = false
    Component.onDestruction: interactionChanged()

    function focusMute() { volume.focusMute(); }
    function focusSlider() { volume.focusSlider(); }
    function cancelGesture() { volume.cancelGesture(); }

    IconImage {
        id: appIcon
        x: 0
        y: root.narrow ? 8 : (parent.height - height) / 2
        implicitSize: 20
        source: !root.node || !root.node.ready ? "" : (!root.triedDesktopIcon && root.suggestedIcon
            ? Icons.src(root.suggestedIcon) : Icons.fromAppId(root.appId))
        asynchronous: true
        onStatusChanged: {
            if (status === Image.Error && !root.triedDesktopIcon)
                root.triedDesktopIcon = true;
        }
    }
    Glyph {
        anchors.centerIn: appIcon
        visible: appIcon.status === Image.Error || !appIcon.source.toString().length
        name: "audioApp"
        color: Tokens.subtext
    }
    Column {
        id: identity
        x: 28
        y: root.narrow ? 7 : (parent.height - height) / 2
        width: root.narrow ? root.width - x : Math.min(122, root.width * 0.28)
        spacing: 3
        Text {
            width: parent.width
            text: root.appName
            textFormat: Text.PlainText
            elide: Text.ElideRight
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 13
        }
        Text {
            visible: !!root.subtitle || root.routeChanging
            width: parent.width
            text: root.routeChanging ? "Output changing" : root.subtitle
            textFormat: Text.PlainText
            elide: Text.ElideRight
            color: Tokens.subtext
            font.family: Tokens.fontFamily
            font.pixelSize: 11
        }
    }
    HoverHandler { id: identityHover }
    ToolTip.visible: identityHover.hovered && !volume.interacting
    ToolTip.delay: 800
    ToolTip.text: root.appName + (root.subtitle ? " · " + root.subtitle : "") + "\n" + root.routeDetail

    AudioVolumeControl {
        id: volume
        x: root.narrow ? 0 : identity.x + identity.width + 8
        y: root.narrow ? root.height - height : (root.height - height) / 2
        width: root.width - x
        node: root.node
        label: root.appName + (root.subtitle ? " " + root.subtitle : "")
        destination: root.routeDetail
        navigationHandled: true
        onFocusEntered: root.focused(root.index)
        onAdvance: backwards => root.advanceRow(root.index, backwards)
        onTargetLost: root.targetLost(root.index)
    }
}
