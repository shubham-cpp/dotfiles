pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import Quickshell.Widgets
import QtQuick
import qs.Common

Rectangle {
    id: root

    required property var screen

    implicitWidth: tasks.implicitWidth
    implicitHeight: Tokens.barWorkspaceHeight
    radius: 8
    color: Tokens.barModule

    Row {
        id: tasks
        anchors.centerIn: parent
        spacing: 0

        Repeater {
            model: ToplevelManager.toplevels

            delegate: Loader {
                id: task
                required property var modelData

                readonly property bool onThisScreen: {
                    const screens = modelData.screens;
                    if (!screens || screens.length === 0)
                        return true;
                    for (let i = 0; i < screens.length; i++) {
                        if (screens[i] === root.screen)
                            return true;
                    }
                    return false;
                }
                active: modelData.parent === null && onThisScreen
                visible: active

                sourceComponent: MouseArea {
                    id: cell
                    readonly property string appId: task.modelData.appId || ""
                    readonly property string iconSrc: Icons.fromAppId(appId)

                    implicitWidth: Tokens.barWorkspaceHeight
                    implicitHeight: Tokens.barWorkspaceHeight
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: task.modelData.activate()

                    Rectangle {
                        anchors.fill: parent
                        radius: root.radius
                        color: task.modelData.activated || cell.containsMouse ? Tokens.barHover : "transparent"
                    }

                    IconImage {
                        id: appIcon
                        visible: cell.iconSrc.length > 0
                        anchors.centerIn: parent
                        implicitSize: 16
                        source: cell.iconSrc
                        asynchronous: true
                    }

                    Text {
                        visible: cell.iconSrc.length === 0 || appIcon.status === Image.Error
                        anchors.centerIn: parent
                        text: cell.appId.length ? cell.appId.charAt(0).toUpperCase() : "?"
                        color: Tokens.subtext
                        font.family: Tokens.barFontFamily
                        font.pixelSize: Tokens.fontSm
                        renderType: Text.NativeRendering
                    }
                }
            }
        }
    }
}
