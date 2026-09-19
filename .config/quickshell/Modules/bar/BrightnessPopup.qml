pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls
import qs.Common
import "../../Common/PopupGeometry.js" as PopupGeometry
import qs.Services

PanelWindow {
    id: win

    required property var anchorWindow
    required property var anchorItem
    required property var anchorControl

    property real panelX: 8
    readonly property int panelWidth: Math.max(1, Math.min(Tokens.brightnessWidth, width - 16))

    visible: Brightness.open
    screen: anchorWindow ? anchorWindow.screen : Quickshell.screens[0]
    color: "transparent"
    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-brightness"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    function updatePosition() {
        if (!anchorWindow)
            return;
        const center = anchorItem ? anchorItem.mapToItem(anchorWindow.contentItem, anchorItem.width / 2, 0).x : anchorWindow.width / 2;
        panelX = PopupGeometry.popupX(center, panelWidth, width);
    }

    function overControl(x, y) {
        if (!anchorWindow || !anchorControl)
            return false;
        const point = anchorControl.mapToItem(anchorWindow.contentItem, 0, 0);
        return x >= point.x && x < point.x + anchorControl.width && y >= point.y && y < point.y + anchorControl.height;
    }

    onWidthChanged: Qt.callLater(win.updatePosition)
    onPanelWidthChanged: Qt.callLater(win.updatePosition)
    onAnchorItemChanged: Qt.callLater(win.updatePosition)
    onAnchorWindowChanged: {
        if (!anchorWindow)
            Brightness.close();
        else
            Qt.callLater(win.updatePosition);
    }
    Component.onCompleted: {
        NightLight.refresh();
        Qt.callLater(win.updatePosition);
    }

    MouseArea {
        anchors.fill: parent
        onClicked: mouse => {
            if (!win.overControl(mouse.x, mouse.y))
                Brightness.close();
        }
        onWheel: event => {
            if (win.overControl(event.x, event.y) && event.angleDelta.y !== 0)
                Brightness.adjust(event.angleDelta.y > 0 ? 5 : -5);
            event.accepted = true;
        }
    }

    Rectangle {
        id: panel
        x: win.panelX
        y: (win.anchorWindow ? win.anchorWindow.height : Tokens.barHeight) + Tokens.overlayGap
        width: win.panelWidth
        implicitHeight: col.implicitHeight + 24
        height: implicitHeight
        color: Tokens.bg
        radius: Tokens.radius
        border.width: 1
        border.color: Tokens.border
        focus: true
        Keys.onEscapePressed: Brightness.close()

        MouseArea {
            anchors.fill: parent
        }

        Column {
            id: col
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.margins: 12
            spacing: 10

            Item {
                width: parent.width
                height: 18

                Text {
                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Brightness"
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 14
                    font.weight: Font.DemiBold
                }

                Text {
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    text: Math.round((slider.pressed ? slider.value : Brightness.percent) * 100) + "%"
                    color: Tokens.subtext
                    font.family: Tokens.fontFamily
                    font.pixelSize: 13
                }
            }

            Slider {
                id: slider
                objectName: "brightnessSlider"
                width: parent.width
                height: 28
                from: 0.01
                to: 1
                stepSize: 0.01
                live: true
                wheelEnabled: false
                focusPolicy: Qt.StrongFocus
                hoverEnabled: true
                Accessible.name: "Brightness"
                Accessible.description: Math.round(Brightness.percent * 100) + " percent"

                Binding {
                    target: slider
                    property: "value"
                    value: Math.max(0.01, Math.min(1, Brightness.percent))
                    when: !slider.pressed
                    restoreMode: Binding.RestoreNone
                }
                onMoved: {
                    if (pressed)
                        Brightness.setPercent(value * 100);
                }
                Keys.onPressed: event => {
                    let p = Math.round(Brightness.percent * 100);
                    if (event.key === Qt.Key_Left || event.key === Qt.Key_Down)
                        p -= 1;
                    else if (event.key === Qt.Key_Right || event.key === Qt.Key_Up)
                        p += 1;
                    else if (event.key === Qt.Key_PageDown)
                        p -= 5;
                    else if (event.key === Qt.Key_PageUp)
                        p += 5;
                    else if (event.key === Qt.Key_Home)
                        p = 1;
                    else if (event.key === Qt.Key_End)
                        p = 100;
                    else
                        return;
                    Brightness.setPercent(p);
                    event.accepted = true;
                }
                background: Rectangle {
                    x: slider.leftPadding
                    y: (slider.height - height) / 2
                    width: slider.availableWidth
                    height: 4
                    radius: 2
                    color: Tokens.border
                    Rectangle {
                        width: slider.visualPosition * parent.width
                        height: parent.height
                        radius: 2
                        color: Tokens.accent
                    }
                }
                handle: Rectangle {
                    x: slider.leftPadding + slider.visualPosition * (slider.availableWidth - width)
                    y: (slider.height - height) / 2
                    width: 12
                    height: 12
                    radius: 6
                    color: Tokens.text
                    border.width: slider.visualFocus ? 2 : 0
                    border.color: Tokens.accent
                }
            }

            Rectangle {
                visible: NightLight.present
                width: parent.width
                height: 1
                color: Tokens.separator
            }

            Item {
                visible: NightLight.present
                width: parent.width
                height: visible ? 30 : 0

                Glyph {
                    id: nightIcon
                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                    name: "moon"
                    color: NightLight.enabled ? Tokens.accent : Tokens.subtext
                }

                Text {
                    anchors.left: nightIcon.right
                    anchors.leftMargin: 8
                    anchors.verticalCenter: parent.verticalCenter
                    text: "Night light"
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 13
                }

                Switch {
                    id: nightSwitch
                    objectName: "nightLightSwitch"
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    implicitWidth: 44
                    implicitHeight: 30
                    padding: 0
                    checked: NightLight.enabled
                    enabled: NightLight.present
                    focusPolicy: Qt.StrongFocus
                    hoverEnabled: true
                    Accessible.name: "Night light"
                    onToggled: NightLight.setEnabled(checked)

                    indicator: Rectangle {
                        anchors.centerIn: parent
                        width: 40
                        height: 22
                        radius: height / 2
                        color: nightSwitch.checked ? Tokens.accent : (nightSwitch.hovered ? Tokens.hover : Tokens.surface)
                        border.width: nightSwitch.visualFocus ? 2 : 1
                        border.color: nightSwitch.visualFocus ? Tokens.text : (nightSwitch.checked ? Tokens.accent : Tokens.border)

                        Rectangle {
                            x: nightSwitch.checked ? parent.width - width - 3 : 3
                            anchors.verticalCenter: parent.verticalCenter
                            width: 16
                            height: 16
                            radius: width / 2
                            color: nightSwitch.checked ? Tokens.bg : Tokens.subtext
                            Behavior on x {
                                NumberAnimation {
                                    duration: 120
                                }
                            }
                        }
                    }
                    background: null
                    contentItem: null
                }
            }
        }
    }
}
