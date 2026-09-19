pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import qs.Common
import qs.Services

Scope {
    id: root

    property var primaryWindow: null
    property var primaryNetworkAnchor: null
    property var primaryVolumeAnchor: null
    property var primaryVolumeControl: null
    property var primaryBrightnessAnchor: null
    property var primaryBrightnessControl: null
    property var primarySystemAnchor: null
    signal networkAnchorMoved
    signal volumeAnchorMoved
    signal brightnessAnchorMoved
    signal systemAnchorMoved

    Variants {
        model: Quickshell.screens

        PanelWindow {
            id: barWindow
            required property var modelData

            screen: modelData
            Component.onCompleted: {
                if (modelData === Quickshell.screens[0]) {
                    root.primaryWindow = barWindow;
                    root.primaryNetworkAnchor = networkStatus.popupAnchor;
                    root.primaryVolumeAnchor = volumeMeter.popupAnchor;
                    root.primaryVolumeControl = volumeMeter;
                    root.primaryBrightnessAnchor = brightnessMeter.popupAnchor;
                    root.primaryBrightnessControl = brightnessMeter;
                    root.primarySystemAnchor = systemStats.popupAnchor;
                }
            }
            Component.onDestruction: {
                if (root.primaryWindow === barWindow) {
                    root.primaryWindow = null;
                    root.primaryNetworkAnchor = null;
                    root.primaryVolumeAnchor = null;
                    root.primaryVolumeControl = null;
                    root.primaryBrightnessAnchor = null;
                    root.primaryBrightnessControl = null;
                    root.primarySystemAnchor = null;
                }
            }
            anchors {
                top: true
                left: true
                right: true
            }
            implicitHeight: Tokens.barHeight
            exclusiveZone: Tokens.barHeight
            color: Tokens.barBg

            IdleInhibitor {
                window: barWindow
                enabled: Idle.enabled
            }

            Row {
                id: leftGroup
                anchors.left: parent.left
                anchors.leftMargin: Tokens.padSm
                anchors.verticalCenter: parent.verticalCenter
                spacing: Tokens.barGap

                TagList {
                    id: tags
                    screen: modelData
                }

                Flickable {
                    width: Math.min(tasks.implicitWidth, Math.max(0, clockWidget.x - leftGroup.x - tags.width - systemStats.width - 2 * leftGroup.spacing - Tokens.padSm))
                    height: Tokens.barModuleHeight
                    contentWidth: tasks.implicitWidth
                    contentHeight: height
                    clip: true
                    flickableDirection: Flickable.HorizontalFlick
                    boundsBehavior: Flickable.StopAtBounds

                    TaskList {
                        id: tasks
                        screen: modelData
                    }
                }

                SystemMeter {
                    id: systemStats
                    onXChanged: root.systemAnchorMoved()
                    onWidthChanged: root.systemAnchorMoved()
                }
            }

            ClockWidget {
                id: clockWidget
                anchors.centerIn: parent
                anchorWindow: barWindow
            }

            Flickable {
                anchors.right: parent.right
                anchors.rightMargin: Tokens.padSm
                anchors.verticalCenter: parent.verticalCenter
                width: Math.min(rightGroup.implicitWidth, Math.max(0, barWindow.width - clockWidget.x - clockWidget.width - Tokens.padSm - 12))
                height: Tokens.barModuleHeight
                contentWidth: rightGroup.implicitWidth
                contentHeight: height
                contentX: Math.max(0, contentWidth - width)
                clip: true
                flickableDirection: Flickable.HorizontalFlick
                boundsBehavior: Flickable.StopAtBounds
                onXChanged: {
                    root.networkAnchorMoved();
                    root.volumeAnchorMoved();
                    root.brightnessAnchorMoved();
                }
                onWidthChanged: {
                    root.networkAnchorMoved();
                    root.volumeAnchorMoved();
                    root.brightnessAnchorMoved();
                }
                onContentXChanged: {
                    root.networkAnchorMoved();
                    root.volumeAnchorMoved();
                    root.brightnessAnchorMoved();
                }

                Row {
                    id: rightGroup
                    spacing: Tokens.barGap

                    NetworkStatus {
                        id: networkStatus
                        showRates: barWindow.width >= 1920
                        onWidthChanged: root.networkAnchorMoved()
                    }
                    BrightnessMeter {
                        id: brightnessMeter
                        onXChanged: root.brightnessAnchorMoved()
                        onWidthChanged: root.brightnessAnchorMoved()
                    }
                    VolumeMeter {
                        id: volumeMeter
                        onXChanged: root.volumeAnchorMoved()
                        onWidthChanged: root.volumeAnchorMoved()
                    }
                    BatteryMeter {}
                    Caffeine {}
                    NotifBell {}
                    Tray {}
                }
            }
        }
    }
}
