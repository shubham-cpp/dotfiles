pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick
import "../Common/Holidays.js" as Holidays

Singleton {
    id: root

    readonly property bool ready: true
    property bool open: false
    property var anchorWindow: null
    property string tab: "cal"
    property int year: (new Date()).getFullYear()
    property int month: (new Date()).getMonth()
    property var selected: new Date()
    property int gen: 0

    readonly property string selectedKey: Holidays.keyFromDate(selected)
    readonly property var selectedHolidays: Holidays.onDay(selected)
    readonly property string monthLabel: {
        const names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
        return names[month] + " " + year;
    }

    function toggle(window): bool {
        if (window)
            anchorWindow = window;
        open = !open;
        if (open) {
            const n = new Date();
            year = n.getFullYear();
            month = n.getMonth();
            selected = n;
            tab = "cal";
            gen++;
        }
        return open;
    }

    function close(): void {
        open = false;
    }

    function prevMonth() {
        if (month === 0) {
            month = 11;
            year--;
        } else {
            month--;
        }
        gen++;
    }

    function nextMonth() {
        if (month === 11) {
            month = 0;
            year++;
        } else {
            month++;
        }
        gen++;
    }

    function selectDate(d) {
        selected = d;
        gen++;
    }

    function holidayKind(d) {
        return Holidays.kindOnDay(d);
    }

    function holidayNames(d) {
        const hits = Holidays.onDay(d);
        const names = [];
        for (let i = 0; i < hits.length; i++)
            names.push(hits[i].name);
        return names.join(" · ");
    }

    IpcHandler {
        target: "calendar"

        function toggle(): bool {
            return root.toggle();
        }

        function close(): void {
            root.close();
        }
    }
}
