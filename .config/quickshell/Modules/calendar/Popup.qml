pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Controls
import qs.Common
import qs.Services

PanelWindow {
    id: win

    required property var anchorWindow
    screen: anchorWindow ? anchorWindow.screen : (Quickshell.screens[0] || null)
    anchors {
        left: true
        right: true
        top: true
        bottom: true
    }
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-calendar"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
    onVisibleChanged: {
        if (visible)
            panel.forceActiveFocus();
        else if (Agenda.open)
            Agenda.close();
    }

    visible: Agenda.open
    color: "transparent"
    readonly property int panelWidth: Math.min(Tokens.calendarWidth, (screen ? screen.width : 1920) - 32)
    readonly property int panelHeight: Math.min(Tokens.calendarMaxHeight, (screen ? screen.height : 1080) - Tokens.barHeight - 24)

    property bool addingReminder: false
    readonly property var dayReminders: Reminders.upcomingFor(Agenda.selectedKey)

    Connections {
        target: Reminders
        function onCommandAdded(reminderId) {
            commandIn.text = "";
            win.addingReminder = false;
        }
    }

    component CalendarButton: MouseArea {
        id: button
        property string text: ""
        property bool prominent: false
        signal triggered()
        implicitWidth: label.implicitWidth + 20
        implicitHeight: 32
        hoverEnabled: true
        cursorShape: enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
        onClicked: triggered()
        Rectangle {
            anchors.fill: parent
            radius: Tokens.radiusSm
            color: button.containsMouse ? Tokens.hover : (button.prominent ? Tokens.selection : "transparent")
        }
        Text {
            id: label
            anchors.centerIn: parent
            text: button.text
            color: !button.enabled ? Tokens.overlay : (button.prominent ? Tokens.accent : Tokens.subtext)
            font.family: Tokens.fontFamily
            font.pixelSize: 13
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: Agenda.close()
    }

    Rectangle {
        id: panel
        x: Math.round((win.width - width) / 2)
        y: (win.anchorWindow ? win.anchorWindow.height : Tokens.barHeight) + Tokens.overlayGap
        width: win.panelWidth
        height: win.panelHeight
        radius: Tokens.radius
        color: Tokens.bg
        border.width: 1
        border.color: Tokens.border
        focus: true
        Keys.onEscapePressed: Agenda.close()

        MouseArea {
            anchors.fill: parent
        }

        Flickable {
            id: scroll
            anchors.fill: parent
            anchors.margins: 20
            contentWidth: width
            contentHeight: col.implicitHeight
            clip: true
            boundsBehavior: Flickable.StopAtBounds

            Column {
                id: col
                width: scroll.width
                spacing: 16

                Item {
                    width: parent.width
                    height: 32

                    Row {
                        spacing: 16
                        Repeater {
                            model: [{ key: "cal", label: "Calendar" }, { key: "fb", label: "Football" }]
                            delegate: MouseArea {
                                id: tab
                                required property var modelData
                                implicitWidth: tabLabel.implicitWidth
                                implicitHeight: 32
                                cursorShape: Qt.PointingHandCursor
                                onClicked: Agenda.tab = modelData.key
                                Text {
                                    id: tabLabel
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: tab.modelData.label
                                    color: Agenda.tab === tab.modelData.key ? Tokens.text : Tokens.overlay
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 15
                                    font.weight: Font.Medium
                                }
                                Rectangle {
                                    anchors.left: parent.left
                                    anchors.right: parent.right
                                    anchors.bottom: parent.bottom
                                    height: 2
                                    radius: 1
                                    visible: Agenda.tab === tab.modelData.key
                                    color: Tokens.accent
                                }
                            }
                        }
                    }

                    CalendarButton {
                        anchors.right: parent.right
                        text: "×"
                        onTriggered: Agenda.close()
                    }
                }

                Column {
                    visible: Agenda.tab === "cal"
                    width: parent.width
                    spacing: 16

                    Item {
                        width: parent.width
                        height: 36
                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: Agenda.monthLabel
                            color: Tokens.text
                            font.family: Tokens.fontFamily
                            font.pixelSize: 22
                            font.weight: Font.Medium
                        }
                        Row {
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: 2
                            CalendarButton {
                                text: "Today"
                                onTriggered: {
                                    const today = new Date();
                                    Agenda.year = today.getFullYear();
                                    Agenda.month = today.getMonth();
                                    Agenda.selectDate(today);
                                }
                            }
                            CalendarButton { text: "‹"; onTriggered: Agenda.prevMonth() }
                            CalendarButton { text: "›"; onTriggered: Agenda.nextMonth() }
                        }
                    }

                    Column {
                        width: parent.width
                        spacing: 8
                        Row {
                            width: parent.width
                            Repeater {
                                model: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
                                delegate: Text {
                                    required property var modelData
                                    width: grid.width / 7
                                    horizontalAlignment: Text.AlignHCenter
                                    text: modelData
                                    color: Tokens.overlay
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 12
                                }
                            }
                        }

                        MonthGrid {
                            id: grid
                            width: parent.width
                            height: 252
                            month: Agenda.month
                            year: Agenda.year
                            locale: Qt.locale("en_IN")
                            spacing: 0
                            delegate: MouseArea {
                                id: cell
                                required property var model
                                implicitWidth: grid.width / 7
                                implicitHeight: 42
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                readonly property bool inMonth: model.month === grid.month
                                readonly property string hkind: Agenda.holidayKind(model.date)
                                readonly property bool isSelected: {
                                    const selected = Agenda.selected;
                                    return selected && model.date.getFullYear() === selected.getFullYear() && model.date.getMonth() === selected.getMonth() && model.date.getDate() === selected.getDate();
                                }
                                onClicked: {
                                    Agenda.selectDate(model.date);
                                    if (!inMonth) {
                                        Agenda.month = model.date.getMonth();
                                        Agenda.year = model.date.getFullYear();
                                    }
                                }
                                Rectangle {
                                    anchors.centerIn: parent
                                    width: 36
                                    height: 36
                                    radius: 10
                                    color: cell.isSelected ? Tokens.selection : (cell.containsMouse ? Tokens.hover : "transparent")
                                    border.width: cell.model.today ? 1 : 0
                                    border.color: Tokens.accent
                                }
                                Text {
                                    anchors.centerIn: parent
                                    text: cell.model.day
                                    color: cell.isSelected ? Tokens.accent : (cell.hkind === "national" ? Tokens.danger : (cell.hkind === "mh" ? Tokens.warning : Tokens.text))
                                    opacity: cell.inMonth ? 1 : 0.3
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 15
                                    font.weight: cell.isSelected || cell.model.today ? Font.DemiBold : Font.Normal
                                }
                            }
                        }
                    }

                    Rectangle {
                        width: parent.width
                        height: 1
                        color: Tokens.separator
                    }

                    Item {
                        width: parent.width
                        height: 32
                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: Qt.formatDateTime(Agenda.selected, "ddd, d MMM")
                            color: Tokens.text
                            font.family: Tokens.fontFamily
                            font.pixelSize: 16
                            font.weight: Font.Medium
                        }
                        CalendarButton {
                            anchors.right: parent.right
                            text: win.addingReminder ? "Cancel" : "+ Add reminder"
                            prominent: !win.addingReminder
                            onTriggered: {
                                win.addingReminder = !win.addingReminder;
                                if (win.addingReminder)
                                    commandIn.forceActiveFocus();
                            }
                        }
                    }

                    Text {
                        visible: Agenda.selectedHolidays.length > 0
                        width: parent.width
                        wrapMode: Text.Wrap
                        text: Agenda.holidayNames(Agenda.selected)
                        color: Tokens.warning
                        font.family: Tokens.fontFamily
                        font.pixelSize: 14
                    }

                    Text {
                        visible: !win.dayReminders.length && !win.addingReminder
                        text: "No reminders for this day"
                        color: Tokens.overlay
                        font.family: Tokens.fontFamily
                        font.pixelSize: 14
                    }

                    Repeater {
                        model: win.dayReminders
                        delegate: Item {
                            id: reminder
                            required property var modelData
                            width: col.width
                            height: reminder.modelData.description.length > 0 ? 52 : 36
                            Column {
                                anchors.left: parent.left
                                anchors.right: deleteButton.left
                                anchors.verticalCenter: parent.verticalCenter
                                spacing: 2
                                Text {
                                    width: parent.width
                                    text: reminder.modelData.title
                                    textFormat: Text.PlainText
                                    color: Tokens.text
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 14
                                    elide: Text.ElideRight
                                }
                                Text {
                                    visible: reminder.modelData.description.length > 0
                                    width: parent.width
                                    text: reminder.modelData.description || ""
                                    textFormat: Text.PlainText
                                    color: Tokens.overlay
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 12
                                    elide: Text.ElideRight
                                }
                            }
                            CalendarButton {
                                id: deleteButton
                                anchors.right: parent.right
                                text: "Delete"
                                onTriggered: Reminders.remove(reminder.modelData.id)
                            }
                        }
                    }

                    Column {
                        visible: win.addingReminder
                        width: parent.width
                        spacing: 10

                        Row {
                            width: parent.width
                            height: 40
                            spacing: 8
                            Rectangle {
                                width: parent.width - saveButton.width - 8
                                height: parent.height
                                radius: Tokens.radiusSm
                                color: Tokens.bgAlt
                                border.width: 1
                                border.color: commandIn.activeFocus ? Tokens.accent : Tokens.border
                                TextInput {
                                    id: commandIn
                                    anchors.fill: parent
                                    anchors.leftMargin: 12
                                    anchors.rightMargin: 12
                                    verticalAlignment: Text.AlignVCenter
                                    clip: true
                                    color: Tokens.text
                                    font.family: Tokens.fontFamily
                                    font.pixelSize: 13
                                    Keys.onReturnPressed: saveButton.save()
                                }
                                Text {
                                    anchors.fill: commandIn
                                    verticalAlignment: Text.AlignVCenter
                                    visible: !commandIn.text.length
                                    text: "10m \"Take a break\" or --urgent \"tomorrow 3pm\" \"Call Mum\""
                                    color: Tokens.overlay
                                    font: commandIn.font
                                    elide: Text.ElideRight
                                }
                            }
                            CalendarButton {
                                id: saveButton
                                text: Reminders.commandBusy ? "Checking…" : "Save"
                                prominent: true
                                height: 40
                                enabled: commandIn.text.trim().length > 0 && !Reminders.commandBusy
                                function save() {
                                    if (!enabled)
                                        return;
                                    Reminders.addCommand(commandIn.text.trim());
                                }
                                onTriggered: save()
                            }
                        }
                        Text {
                            width: parent.width
                            text: "Use the same syntax as the remind command. Quote times and titles that contain spaces."
                            color: Tokens.overlay
                            font.family: Tokens.fontFamily
                            font.pixelSize: 12
                            wrapMode: Text.Wrap
                        }
                    }
                    Text {
                        visible: Reminders.error.length > 0
                        width: parent.width
                        wrapMode: Text.Wrap
                        text: Reminders.error
                        color: Tokens.warning
                        font.family: Tokens.fontFamily
                        font.pixelSize: 13
                    }
                }

                Loader {
                    active: Agenda.tab === "fb"
                    width: parent.width
                    source: "FootballTab.qml"
                }
            }
        }
    }
}
