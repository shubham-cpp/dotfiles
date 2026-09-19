pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Services.SystemTray
import Quickshell.Widgets
import QtQuick
import qs.Common

Row {
    id: root

    spacing: Tokens.barGap
    height: Tokens.barModuleHeight

    Repeater {
        model: SystemTray.items

        delegate: MouseArea {
            id: cell

            required property var modelData

            implicitWidth: Tokens.barModuleHeight
            implicitHeight: Tokens.barModuleHeight
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
            onClicked: mouse => {
                if (mouse.button === Qt.MiddleButton) {
                    modelData.secondaryActivate();
                    return;
                }
                if (mouse.button === Qt.RightButton || modelData.onlyMenu) {
                    if (modelData.hasMenu)
                        menuAnchor.open();
                    return;
                }
                modelData.activate();
            }
            onWheel: event => {
                modelData.scroll(event.angleDelta.y, false);
                event.accepted = true;
            }

            Rectangle {
                anchors.fill: parent
                radius: 4
                color: cell.containsMouse ? Tokens.barHover : Tokens.barModule
            }

            IconImage {
                anchors.centerIn: parent
                implicitSize: 16
                source: modelData.icon
                asynchronous: true
            }

            QsMenuAnchor {
                id: menuAnchor
                menu: cell.modelData.menu
                anchor.item: cell
                anchor.edges: Edges.Bottom | Edges.Right
                anchor.gravity: Edges.Bottom | Edges.Left
            }
        }
    }
}
