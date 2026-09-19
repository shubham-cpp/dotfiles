pragma Singleton

import Quickshell
import QtQuick

Singleton {
    id: root

    readonly property bool ready: true
    readonly property date date: clock.date
    readonly property string text: Qt.formatDateTime(clock.date, "h:mm AP  d MMM")

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }
}
