pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls
import qs.Common
import "../../Common/PopupGeometry.js" as PopupGeometry
import qs.Services
import "../../Common/NetworkList.js" as NetworkList

PanelWindow {
    id: win
    required property var anchorWindow
    required property var anchorItem
    property var forgetTarget: null
    property bool settingsAvailable: false
    readonly property int maxHeight: Math.max(1, Math.min(520, (screen ? screen.height : 1080) - (anchorWindow ? anchorWindow.height : Tokens.barHeight) - 22))
    readonly property int rowHeight: entries.count * 52 + (Network.savedNets.length && Network.otherNets.length ? 26 : 0)

    property real panelX: 8
    readonly property int panelWidth: Math.max(1, Math.min(380, width - 16))
    readonly property int panelHeight: Math.min(maxHeight, 56 + adapter.height + Math.max(76, rowHeight) + footer.implicitHeight + 28)

    visible: Network.open
    screen: anchorWindow ? anchorWindow.screen : Quickshell.screens[0]
    color: "transparent"
    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-network"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    function updatePosition() {
        if (!anchorWindow)
            return;
        const center = anchorItem ? anchorItem.mapToItem(anchorWindow.contentItem, anchorItem.width / 2, 0).x : anchorWindow.width / 2;
        panelX = PopupGeometry.popupX(center, panelWidth, win.width);
    }
    onWidthChanged: Qt.callLater(win.updatePosition)
    onAnchorItemChanged: Qt.callLater(win.updatePosition)
    onAnchorWindowChanged: {
        if (!anchorWindow)
            Network.close();
        else
            Qt.callLater(win.updatePosition);
    }

    MouseArea { anchors.fill: parent; onClicked: Network.close() }

    function syncList() {
        NetworkList.sync(entries, NetworkList.rows(Network.grouped), !!(Network.promptNet || forgetTarget || list.moving));
        if (forgetTarget && Network.networkValues().indexOf(forgetTarget) === -1)
            forgetTarget = null;
        updateSelection(false);
    }

    function updateSelection(scroll) {
        for (let i = 0; i < entries.count; i++) {
            if (entries.get(i).net === Network.selected) {
                list.currentIndex = i;
                if (scroll)
                    list.positionViewAtIndex(i, ListView.Contain);
                return;
            }
        }
        list.currentIndex = -1;
    }

    function moveSelection(delta) {
        let index = list.currentIndex;
        index += delta;
        if (index >= 0 && index < entries.count) {
            Network.selected = entries.get(index).net;
            updateSelection(true);
        }
    }

    function handleKey(event) {
        if (event.key === Qt.Key_Escape) {
            if (Network.promptNet)
                Network.promptNet = null;
            else if (forgetTarget)
                forgetTarget = null;
            else
                Network.close();
            event.accepted = true;
        } else if (!Network.promptNet && !forgetTarget && (event.key === Qt.Key_Down || event.key === Qt.Key_Up)) {
            moveSelection(event.key === Qt.Key_Down ? 1 : -1);
            list.forceActiveFocus();
            event.accepted = true;
        }
    }

    function openSettings() {
        if (!settingsAvailable)
            return;
        Network.close();
        Quickshell.execDetached(["foot", "-e", "nmtui", "edit"]);
    }

    ListModel { id: entries }
    onForgetTargetChanged: Qt.callLater(win.syncList)

    Connections {
        target: Network
        function onGroupedChanged() { Qt.callLater(win.syncList); }
        function onSelectedChanged() {
            win.forgetTarget = null;
            win.updateSelection(false);
        }
        function onPromptNetChanged() {
            Qt.callLater(win.syncList);
            if (!Network.promptNet)
                list.forceActiveFocus();
        }
        function onUsableWifiChanged() {
            win.forgetTarget = null;
            Qt.callLater(win.syncList);
        }
    }

    // List-only observers are destroyed with the popup. Connection outcomes are
    // owned by the service's single pending-network observer.
    Instantiator {
        model: Network.usableWifi ? Network.wifiDevice.networks : null
        delegate: Connections {
            required property var modelData
            target: modelData
            function onConnectedChanged() { Network.bumpList(); Network.refreshIdentity(); }
            function onKnownChanged() { Network.bumpList(); }
        }
        onObjectAdded: Qt.callLater(Network.bumpList)
        onObjectRemoved: Qt.callLater(Network.bumpList)
    }

    Process {
        command: ["sh", "-c", "command -v foot >/dev/null && command -v nmtui >/dev/null"]
        running: true
        onExited: exitCode => win.settingsAvailable = exitCode === 0
    }

    Rectangle {
        id: panel
        x: win.panelX
        y: (win.anchorWindow ? win.anchorWindow.height : Tokens.barHeight) + Tokens.overlayGap
        width: win.panelWidth
        height: win.panelHeight
        radius: Tokens.radius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border
        clip: true
        Keys.onPressed: event => win.handleKey(event)
        MouseArea { anchors.fill: parent }

        Item {
            id: header
            anchors.left: parent.left
            anchors.right: parent.right
            height: 56
            Glyph {
                id: headerIcon
                anchors.left: parent.left
                anchors.leftMargin: 16
                anchors.verticalCenter: parent.verticalCenter
                name: Network.wifiRadio ? "wifi" : "wifiOff"
                color: Tokens.accent
                font.pixelSize: 18
            }
            Text {
                anchors.left: headerIcon.right
                anchors.leftMargin: 10
                anchors.verticalCenter: parent.verticalCenter
                text: "Wi-Fi"
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: 16
                font.weight: Font.DemiBold
            }
            Switch {
                id: radio
                anchors.right: closeButton.left
                anchors.rightMargin: 8
                anchors.verticalCenter: parent.verticalCenter
                implicitWidth: 44
                implicitHeight: 30
                padding: 0
                checked: Network.wifiRadio
                enabled: Network.backendAvailable && Network.wifiHardwareEnabled && !!Network.wifiDevice
                focusPolicy: Qt.StrongFocus
                hoverEnabled: true
                Accessible.name: "Wi-Fi"
                ToolTip.visible: hovered
                ToolTip.text: !Network.wifiHardwareEnabled ? "Wi-Fi blocked by hardware" : (checked ? "Turn Wi-Fi off" : "Turn Wi-Fi on")
                onToggled: Network.setWifiEnabled(checked)

                indicator: Rectangle {
                    anchors.centerIn: parent
                    width: 40
                    height: 22
                    radius: height / 2
                    color: radio.checked ? Tokens.accent : (radio.hovered ? Tokens.hover : Tokens.surface)
                    border.width: radio.visualFocus ? 2 : 1
                    border.color: radio.visualFocus ? Tokens.text : (radio.checked ? Tokens.accent : Tokens.border)
                    opacity: radio.enabled ? 1 : 0.45

                    Rectangle {
                        x: radio.checked ? parent.width - width - 3 : 3
                        anchors.verticalCenter: parent.verticalCenter
                        width: 16
                        height: 16
                        radius: width / 2
                        color: radio.checked ? Tokens.bg : Tokens.subtext
                        Behavior on x { NumberAnimation { duration: 120 } }
                    }
                }
                background: null
                contentItem: null
            }
            NetworkButton {
                id: closeButton
                anchors.right: parent.right
                anchors.rightMargin: 12
                anchors.verticalCenter: parent.verticalCenter
                text: "×"
                Accessible.name: "Close Wi-Fi"
                onClicked: Network.close()
            }
            Rectangle { anchors.bottom: parent.bottom; width: parent.width; height: 1; color: Tokens.separator }
        }

        ComboBox {
            id: adapter
            anchors.top: header.bottom
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.margins: 12
            anchors.topMargin: visible ? 8 : 0
            height: visible ? 34 : 0
            visible: Network.wifiDevices.length > 1
            enabled: !Network.busy
            model: Network.wifiDevices.map(device => device.name)
            currentIndex: Network.wifiDevices.indexOf(Network.wifiDevice)
            onActivated: index => {
                Network.promptNet = null;
                Network.preferredDevice = Network.wifiDevices[index];
            }
        }

        ListView {
            id: list
            anchors.top: adapter.bottom
            anchors.topMargin: 6
            anchors.bottom: footer.top
            anchors.bottomMargin: 10
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.leftMargin: 10
            anchors.rightMargin: 10
            model: entries
            clip: true
            focus: true
            activeFocusOnTab: true
            cacheBuffer: 52
            boundsBehavior: Flickable.StopAtBounds
            keyNavigationEnabled: false
            highlightMoveDuration: 0
            onMovementEnded: Qt.callLater(win.syncList)
            Keys.onReturnPressed: event => {
                if (!event.isAutoRepeat)
                    Network.activateSelected();
                event.accepted = true;
            }
            Keys.onEnterPressed: event => {
                if (!event.isAutoRepeat)
                    Network.activateSelected();
                event.accepted = true;
            }
            delegate: NetworkRow { width: ListView.view.width; height: implicitHeight }
            section.property: "group"
            section.delegate: Item {
                id: sectionHeader
                required property string section
                width: list.width
                height: section === "Nearby" && Network.savedNets.length ? 26 : 0
                visible: height > 0
                Rectangle {
                    visible: sectionHeader.height > 0
                    anchors.verticalCenter: parent.verticalCenter
                    width: parent.width
                    height: 1
                    color: Tokens.separator
                }
                Text {
                    visible: sectionHeader.height > 0
                    anchors.centerIn: parent
                    text: "  Nearby  "
                    color: Tokens.overlay
                    font.family: Tokens.fontFamily
                    font.pixelSize: 11
                    Rectangle { anchors.fill: parent; color: Tokens.bg; z: -1 }
                }
            }
            ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

            Text {
                visible: entries.count === 0
                anchors.fill: parent
                anchors.margins: 10
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                wrapMode: Text.Wrap
                text: {
                    if (!Network.backendAvailable) return "NetworkManager is unavailable";
                    if (!Network.wifiDevice) return "No Wi-Fi adapter found";
                    if (!Network.wifiHardwareEnabled) return "Wi-Fi is blocked by a hardware switch";
                    if (!Network.wifiRadio) return "Turn Wi-Fi on to see networks";
                    if (!Network.wifiDevice.nmManaged) return "This adapter is not managed by NetworkManager";
                    return Network.discovering ? "Searching for networks…" : "No networks found yet";
                }
                color: Tokens.subtext
                font.family: Tokens.fontFamily
                font.pixelSize: 13
            }
        }

        Column {
            id: footer
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            anchors.margins: 12
            spacing: 8

            Text {
                visible: Network.failMessage.length > 0
                width: parent.width
                text: (Network.failNet ? Network.failNet.name + ": " : "") + Network.failMessage
                color: Tokens.danger
                font.family: Tokens.fontFamily
                font.pixelSize: 12
                wrapMode: Text.Wrap
                textFormat: Text.PlainText
            }
            Loader {
                id: passwordLoader
                width: parent.width
                active: !!Network.promptNet
                visible: active
                sourceComponent: NetworkPassword {
                    net: Network.promptNet
                    width: passwordLoader.width
                }
            }
            Column {
                visible: !!win.forgetTarget
                width: parent.width
                spacing: 8
                Text {
                    width: parent.width
                    text: {
                        if (!win.forgetTarget) return "";
                        const count = win.forgetTarget.nmSettings.length;
                        return "Forget " + win.forgetTarget.name + "?" + (count > 1 ? " This removes all " + count + " saved profiles." : " Its saved settings and password will be removed.");
                    }
                    color: Tokens.subtext
                    font.family: Tokens.fontFamily
                    font.pixelSize: 12
                    wrapMode: Text.Wrap
                    textFormat: Text.PlainText
                }
                Row {
                    spacing: 8
                    NetworkButton { text: "Cancel"; onClicked: win.forgetTarget = null }
                    NetworkButton {
                        text: "Forget"
                        enabled: !Network.busy
                        onClicked: {
                            const target = win.forgetTarget;
                            win.forgetTarget = null;
                            Network.forgetNet(target);
                        }
                    }
                }
            }
            Row {
                spacing: 8
                Row {
                    visible: !!Network.selected && Network.usableWifi && !Network.promptNet && !win.forgetTarget && !Network.busy
                    spacing: 8
                    NetworkButton {
                        text: Network.selected && Network.selected.connected ? "Disconnect" : "Connect"
                        enabled: !!Network.selected && !Network.selected.stateChanging
                        onClicked: {
                            if (Network.selected.connected)
                                Network.disconnectNet(Network.selected);
                            else
                                Network.activateSelected();
                        }
                    }
                    NetworkButton {
                        visible: !!Network.selected && Network.selected.known
                        text: "Forget…"
                        enabled: !!Network.selected && !Network.selected.stateChanging
                        onClicked: win.forgetTarget = Network.selected
                    }
                }
                NetworkButton {
                    text: "Connection settings…"
                    enabled: win.settingsAvailable
                    onClicked: win.openSettings()
                    ToolTip.visible: hovered && !win.settingsAvailable
                    ToolTip.text: "Requires foot and nmtui"
                }
            }
        }
    }

    Component.onCompleted: {
        updatePosition();
        Network.bumpList();
        syncList();
        list.forceActiveFocus();
    }
}
