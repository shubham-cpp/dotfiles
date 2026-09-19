pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls
import qs.Common
import "../../Common/PopupGeometry.js" as PopupGeometry
import qs.Services
import "../../Common/AudioList.js" as AudioList

PanelWindow {
    id: win
    required property var anchorWindow
    required property var anchorItem
    required property var anchorControl
    property real panelX: 8
    property int gestures: 0
    property var focusedNode: null
    property int pendingFocusIndex: -1
    property var initialAnchor: null
    property bool initialized: false
    readonly property int panelWidth: Math.max(1, Math.min(Tokens.audioWidth, width - 16))
    readonly property int maxHeight: Math.max(1, Math.min(Tokens.audioMaxHeight, height - panel.y - 8))
    readonly property bool frozen: gestures > 0 || master.interacting || list.moving || input.popupVisible || output.popupVisible

    visible: Audio.open
    screen: anchorWindow ? anchorWindow.screen : Quickshell.screens[0]
    color: "transparent"
    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-audio"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    function updatePosition() {
        if (!anchorWindow)
            return;
        const center = anchorItem ? anchorItem.mapToItem(anchorWindow.contentItem, anchorItem.width / 2, 0).x : anchorWindow.width / 2;
        panelX = PopupGeometry.popupX(center, panelWidth, width);
    }
    function overVolume(x, y) {
        if (!anchorWindow || !anchorControl)
            return false;
        const point = anchorControl.mapToItem(anchorWindow.contentItem, 0, 0);
        return x >= point.x && x < point.x + anchorControl.width && y >= point.y && y < point.y + anchorControl.height;
    }
    function updateGestures() {
        let count = 0;
        for (let i = 0; i < list.count; i++) {
            const item = list.itemAtIndex(i) as AudioStreamRow;
            if (item && item.interacting) count++;
        }
        gestures = count;
    }
    function focusRow(index, backwards) {
        if (index < 0) {
            if (!output.focusSelector() && !input.focusSelector()) master.focusSlider();
        } else if (index >= list.count) {
            master.focusMute();
        } else {
            list.currentIndex = index;
            list.positionViewAtIndex(index, ListView.Contain);
            list.forceLayout();
            const row = list.itemAtIndex(index) as AudioStreamRow;
            if (row) {
                if (backwards) row.focusSlider();
                else row.focusMute();
            }
        }
    }
    function restoreFocus() {
        if (pendingFocusIndex >= 0) {
            const index = Math.min(pendingFocusIndex, list.count - 1);
            pendingFocusIndex = -1;
            focusedNode = null;
            focusRow(index, false);
            return;
        }
        if (!focusedNode)
            return;
        for (let i = 0; i < mixer.rows.count; i++) {
            if (mixer.rows.get(i).node === focusedNode) {
                list.currentIndex = i;
                return;
            }
        }
        focusedNode = null;
        if (list.count) {
            list.currentIndex = Math.min(Math.max(0, list.currentIndex), list.count - 1);
            list.positionViewAtIndex(list.currentIndex, ListView.Contain);
            const row = list.itemAtIndex(list.currentIndex) as AudioStreamRow;
            if (row) row.focusMute();
        } else {
            master.focusMute();
        }
    }
    onWidthChanged: Qt.callLater(win.updatePosition)
    onPanelWidthChanged: Qt.callLater(win.updatePosition)
    onAnchorItemChanged: Qt.callLater(win.updatePosition)
    onAnchorWindowChanged: {
        if (!anchorWindow || (initialized && anchorWindow !== initialAnchor)) Audio.close();
        else Qt.callLater(win.updatePosition);
    }
    Connections {
        target: win.anchorWindow
        function onScreenChanged() { if (!win.anchorWindow.screen) Audio.close(); }
    }

    AudioMixerModel {
        id: mixer
        freeze: win.frozen
        onReconciled: Qt.callLater(win.restoreFocus)
        onRemovingFocusedTarget: node => {
            for (let i = 0; i < list.count; i++) {
                const item = list.itemAtIndex(i) as AudioStreamRow;
                if (item && item.node === node) item.cancelGesture();
            }
            Qt.callLater(win.restoreFocus);
        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.RightButton && win.overVolume(mouse.x, mouse.y)) Audio.toggleMute();
            else Audio.close();
        }
        onWheel: event => {
            if (win.overVolume(event.x, event.y) && event.angleDelta.y !== 0)
                Audio.adjust(event.angleDelta.y > 0 ? 0.05 : -0.05);
            event.accepted = true;
        }
    }

    Rectangle {
        id: panel
        x: win.panelX
        y: (win.anchorWindow ? win.anchorWindow.height : Tokens.barHeight) + Tokens.overlayGap
        width: win.panelWidth
        height: Math.min(win.maxHeight, fixed.implicitHeight + Math.max(64, mixer.contentHeight + (width < 380 ? mixer.rows.count * 32 : 0)) + 24)
        color: Tokens.bg
        radius: Tokens.radius
        border.width: 1
        border.color: Tokens.border
        focus: true
        Keys.onEscapePressed: event => {
            if (input.popupVisible) input.closeMenu();
            else if (output.popupVisible) output.closeMenu();
            else Audio.close();
            event.accepted = true;
        }
        MouseArea { anchors.fill: parent }

        Column {
            id: fixed
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.margins: 12
            spacing: 10
            Text {
                width: parent.width
                text: !Audio.ready ? "Audio service unavailable" : output.waiting ? "Switching output…" : AudioList.deviceName(Audio.sink) || "No output device"
                textFormat: Text.PlainText
                elide: Text.ElideRight
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: 14
                font.weight: Font.DemiBold
            }
            AudioVolumeControl {
                id: master
                width: parent.width
                node: Audio.sink
                label: "Output"
                navigationHandled: true
                onFocusEntered: win.focusedNode = null
                onAdvance: backwards => {
                    if (backwards) win.focusRow(list.count - 1, true);
                    else if (!input.focusSelector() && !output.focusSelector()) win.focusRow(0, false);
                }
            }
            Rectangle { width: parent.width; height: 1; color: Tokens.separator }
            Row {
                width: parent.width
                spacing: 12
                AudioDeviceSelector {
                    id: input
                    width: (parent.width - parent.spacing) / 2
                    input: true
                    devices: mixer.inputs
                    current: Audio.source
                    navigationHandled: true
                    onOpening: output.closeMenu()
                    onFocusEntered: win.focusedNode = null
                    onAdvance: backwards => {
                        if (backwards) master.focusSlider();
                        else if (!output.focusSelector()) win.focusRow(0, false);
                    }
                }
                AudioDeviceSelector {
                    id: output
                    width: (parent.width - parent.spacing) / 2
                    devices: mixer.outputs
                    current: Audio.sink
                    navigationHandled: true
                    onOpening: input.closeMenu()
                    onFocusEntered: win.focusedNode = null
                    onAdvance: backwards => {
                        if (backwards) { if (!input.focusSelector()) master.focusSlider(); }
                        else win.focusRow(0, false);
                    }
                }
            }
            Rectangle { width: parent.width; height: 1; color: Tokens.separator }
            Text {
                text: "Applications"
                color: Tokens.subtext
                font.family: Tokens.fontFamily
                font.pixelSize: 12
            }
        }

        ListView {
            id: list
            anchors.top: fixed.bottom
            anchors.topMargin: 4
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.margins: 12
            clip: true
            model: mixer.rows
            cacheBuffer: 80
            boundsBehavior: Flickable.StopAtBounds
            keyNavigationEnabled: false
            highlightMoveDuration: 0
            onCountChanged: { Qt.callLater(win.updateGestures); Qt.callLater(win.restoreFocus); }
            section.property: "group"
            section.delegate: Item {
                id: sectionHeader
                required property string section
                width: list.width
                height: 28
                Text {
                    anchors.verticalCenter: parent.verticalCenter
                    width: parent.width
                    text: {
                        for (let i = 0; i < mixer.rows.count; i++) {
                            const row = mixer.rows.get(i);
                            if (row.group === sectionHeader.section) return row.groupLabel;
                        }
                        return "";
                    }
                    textFormat: Text.PlainText
                    elide: Text.ElideRight
                    color: Tokens.subtext
                    font.family: Tokens.fontFamily
                    font.pixelSize: 11
                }
            }
            delegate: AudioStreamRow {
                width: list.width - (scrollbar.visible ? 8 : 0)
                onInteractionChanged: Qt.callLater(win.updateGestures)
                onAdvanceRow: (rowIndex, backwards) => win.focusRow(rowIndex + (backwards ? -1 : 1), backwards)
                onTargetLost: rowIndex => { win.pendingFocusIndex = rowIndex; }
                onFocused: rowIndex => {
                    win.focusedNode = node;
                    list.currentIndex = rowIndex;
                    list.positionViewAtIndex(rowIndex, ListView.Contain);
                }
            }
            ScrollBar.vertical: ScrollBar { id: scrollbar; policy: ScrollBar.AsNeeded }
            Text {
                anchors.fill: parent
                visible: list.count === 0
                text: Audio.ready ? "No playback applications" : "Audio service unavailable"
                color: Tokens.subtext
                font.family: Tokens.fontFamily
                font.pixelSize: 13
                verticalAlignment: Text.AlignVCenter
                horizontalAlignment: Text.AlignHCenter
                wrapMode: Text.Wrap
            }
        }
    }
    Component.onCompleted: {
        initialAnchor = anchorWindow;
        initialized = true;
        Qt.callLater(win.updatePosition);
        master.focusMute();
    }
}
