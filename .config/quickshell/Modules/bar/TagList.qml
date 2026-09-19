pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.WindowManager
import QtQuick
import qs.Common
import qs.Services

Rectangle {
    id: root

    required property var screen

    implicitWidth: tags.implicitWidth > 0 ? tags.implicitWidth + 8 : 0
    implicitHeight: Tokens.barModuleHeight
    radius: 4
    color: Tokens.barModule

    readonly property var mangoTags: {
        const all = Workspaces.tagsFor(screen.name);
        const shown = [];
        for (let i = 0; i < all.length; i++) {
            const t = all[i];
            if (t.occupied || t.active || t.urgent)
                shown.push(t);
        }
        return shown;
    }
    readonly property bool useMango: Workspaces.tagsFor(screen.name).length > 0

    WheelHandler {
        acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad
        onWheel: event => {
            Workspaces.step(event.angleDelta.y > 0 ? -1 : 1);
            event.accepted = true;
        }
    }

    Row {
        id: tags
        anchors.centerIn: parent
        spacing: 0

        Repeater {
            model: {
                if (root.useMango)
                    return root.mangoTags;
                const proj = WindowManager.screenProjection(root.screen);
                const sets = proj ? proj.windowsets : [];
                const shown = [];
                for (let i = 0; i < sets.length; i++) {
                    const ws = sets[i];
                    if (ws.active || ws.urgent || ws.shouldDisplay)
                        shown.push(ws);
                }
                return shown;
            }

            delegate: MouseArea {
                id: cell

                required property var modelData
                required property int index

                readonly property int tagIndex: root.useMango ? modelData.index : Number(modelData.name)
                readonly property bool isActive: modelData.active
                readonly property bool isUrgent: root.useMango ? modelData.urgent : modelData.urgent
                readonly property bool isOccupied: root.useMango ? modelData.occupied : modelData.shouldDisplay
                readonly property string label: {
                    if (root.useMango)
                        return modelData.name;
                    if (modelData.name && modelData.name.length)
                        return modelData.name;
                    return String(cell.index + 1);
                }

                implicitWidth: 32
                implicitHeight: Tokens.barModuleHeight
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                    if (root.useMango)
                        Workspaces.activate(cell.tagIndex);
                    else if (modelData.canActivate)
                        modelData.activate();
                }

                Rectangle {
                    anchors.fill: parent
                    radius: 4
                    color: cell.isActive || cell.containsMouse ? Tokens.barHover : "transparent"
                    border.width: cell.isUrgent ? 1 : 0
                    border.color: Tokens.danger
                }

                Text {
                    anchors.centerIn: parent
                    text: cell.label
                    color: cell.isUrgent ? Tokens.danger : (cell.isActive ? Tokens.barAccent : (cell.isOccupied ? Tokens.barText : Tokens.overlay))
                    font.family: Tokens.barFontFamily
                    font.pixelSize: Tokens.fontSm
                    font.weight: cell.isActive || cell.isOccupied ? Font.DemiBold : Font.Normal
                    renderType: Text.NativeRendering
                }
            }
        }
    }
}
