pragma ComponentBehavior: Bound

import QtQuick
import qs.Common
import qs.Services

Column {
    id: root
    width: parent.width
    spacing: 12

    Component.onCompleted: Football.ensure()

    component ClubMark: Item {
        property string source: ""
        property string fallback: ""
        implicitWidth: 22
        implicitHeight: 22

        Image {
            id: img
            anchors.fill: parent
            source: parent.source
            fillMode: Image.PreserveAspectFit
            asynchronous: true
            cache: true
            sourceSize.width: 44
            sourceSize.height: 44
            visible: status === Image.Ready
        }

        Text {
            anchors.centerIn: parent
            visible: img.status !== Image.Ready
            text: parent.fallback
            color: Tokens.overlay
            font.family: Tokens.fontFamily
            font.pixelSize: 9
            font.weight: Font.DemiBold
        }
    }

    Row {
        spacing: 16

        Repeater {
            model: [{ key: "All", label: "All" }, { key: "PL", label: "PL" }, { key: "PD", label: "La Liga" }, { key: "CL", label: "UCL" }]

            delegate: MouseArea {
                id: filter
                required property var modelData
                implicitWidth: filterLbl.implicitWidth
                implicitHeight: 28
                cursorShape: Qt.PointingHandCursor
                onClicked: Football.setChip(modelData.key)

                Text {
                    id: filterLbl
                    anchors.centerIn: parent
                    text: filter.modelData.label
                    color: Football.chip === filter.modelData.key ? Tokens.text : Tokens.overlay
                    font.family: Tokens.fontFamily
                    font.pixelSize: 14
                    font.weight: Font.Medium
                }

                Rectangle {
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.bottom: parent.bottom
                    height: 2
                    radius: 1
                    visible: Football.chip === filter.modelData.key
                    color: Tokens.accent
                }
            }
        }
    }

    Text {
        visible: Football.status.length > 0
        width: parent.width
        wrapMode: Text.Wrap
        text: Football.status
        color: Tokens.warning
        font.family: Tokens.fontFamily
        font.pixelSize: 13
    }

    Repeater {
        model: Football.groups

        delegate: Column {
            id: day
            required property var modelData
            width: root.width
            spacing: 0
            readonly property bool open: Football.expanded.indexOf(modelData.key) !== -1

            MouseArea {
                width: parent.width
                height: 36
                cursorShape: Qt.PointingHandCursor
                hoverEnabled: true
                onClicked: Football.toggleGroup(day.modelData.key)

                Rectangle {
                    anchors.fill: parent
                    color: parent.containsMouse ? Tokens.hover : "transparent"
                }

                Text {
                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                    text: day.modelData.label
                    color: Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 14
                    font.weight: Font.DemiBold
                }

                Row {
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 10

                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        text: day.modelData.count
                        color: Tokens.overlay
                        font.family: Tokens.fontFamily
                        font.pixelSize: 13
                    }

                    Glyph {
                        anchors.verticalCenter: parent.verticalCenter
                        name: day.open ? "chevronDown" : "chevronRight"
                        color: Tokens.overlay
                        font.pixelSize: 12
                    }
                }
            }

            Column {
                visible: day.open
                width: parent.width
                spacing: 0

                Repeater {
                    model: day.open ? day.modelData.matches : []

                    delegate: Item {
                        id: row
                        required property var modelData
                        width: day.width
                        height: 56

                        Rectangle {
                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.top: parent.top
                            height: 1
                            color: Tokens.separator
                        }

                        Text {
                            id: league
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            width: 28
                            text: row.modelData.code
                            color: Tokens.overlay
                            font.family: Tokens.fontFamily
                            font.pixelSize: 11
                            font.weight: Font.DemiBold
                        }

                        Item {
                            id: homeSide
                            anchors.left: league.right
                            anchors.right: mid.left
                            anchors.leftMargin: 8
                            anchors.rightMargin: 8
                            anchors.verticalCenter: parent.verticalCenter
                            height: 22

                            ClubMark {
                                id: homeMark
                                anchors.left: parent.left
                                source: row.modelData.homeCrest || ""
                                fallback: row.modelData.homeTla || row.modelData.home.slice(0, 3)
                            }

                            Text {
                                anchors.left: homeMark.right
                                anchors.leftMargin: 8
                                anchors.right: parent.right
                                anchors.verticalCenter: parent.verticalCenter
                                text: row.modelData.home
                                color: Tokens.text
                                font.family: Tokens.fontFamily
                                font.pixelSize: 14
                                elide: Text.ElideRight
                                horizontalAlignment: Text.AlignRight
                            }
                        }

                        Item {
                            id: mid
                            anchors.horizontalCenter: parent.horizontalCenter
                            anchors.verticalCenter: parent.verticalCenter
                            width: 64
                            height: parent.height

                            Text {
                                anchors.horizontalCenter: parent.horizontalCenter
                                anchors.verticalCenter: parent.verticalCenter
                                anchors.verticalCenterOffset: row.modelData.chip.length ? -7 : 0
                                text: {
                                    if (row.modelData.hs === null || row.modelData.hs === undefined)
                                        return row.modelData.time;
                                    return row.modelData.hs + "–" + row.modelData.as;
                                }
                                color: row.modelData.live ? Tokens.success : Tokens.text
                                font.family: Tokens.fontFamily
                                font.pixelSize: 15
                                font.weight: Font.DemiBold
                            }

                            Text {
                                visible: row.modelData.chip.length > 0
                                anchors.horizontalCenter: parent.horizontalCenter
                                anchors.bottom: parent.bottom
                                anchors.bottomMargin: 8
                                text: row.modelData.chip
                                color: row.modelData.live ? Tokens.success : Tokens.overlay
                                font.family: Tokens.fontFamily
                                font.pixelSize: 10
                                font.weight: Font.DemiBold
                            }
                        }

                        Item {
                            id: awaySide
                            anchors.left: mid.right
                            anchors.right: parent.right
                            anchors.leftMargin: 8
                            anchors.verticalCenter: parent.verticalCenter
                            height: 22

                            ClubMark {
                                id: awayMark
                                anchors.right: parent.right
                                source: row.modelData.awayCrest || ""
                                fallback: row.modelData.awayTla || row.modelData.away.slice(0, 3)
                            }

                            Text {
                                anchors.left: parent.left
                                anchors.right: awayMark.left
                                anchors.rightMargin: 8
                                anchors.verticalCenter: parent.verticalCenter
                                text: row.modelData.away
                                color: Tokens.text
                                font.family: Tokens.fontFamily
                                font.pixelSize: 14
                                elide: Text.ElideRight
                            }
                        }
                    }
                }
            }
        }
    }

    Text {
        visible: Football.groups.length === 0 && Football.status.length === 0
        text: "No matches in this window"
        color: Tokens.overlay
        font.family: Tokens.fontFamily
        font.pixelSize: 14
    }
}
