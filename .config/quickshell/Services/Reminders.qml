pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    readonly property bool ready: true
    property var items: []
    property int gen: 0
    readonly property int maxTimerMs: 2147483647
    property string error: ""
    property var scheduledCommand: []
    property var delivery: null
    property real retryAfter: 0
    property var commandJob: null
    readonly property bool commandBusy: commandJob !== null
    signal commandAdded(string reminderId)

    function upcomingFor(key) {
        const out = [];
        for (let i = 0; i < items.length; i++) {
            const r = items[i];
            if (!r.fired && String(r.at).indexOf(key) === 0)
                out.push(r);
        }
        return out;
    }

    function add(title, atIso, urgent, description) {
        const at = parseLocal(atIso);
        if (!at) {
            error = "Enter a valid date and time.";
            return "";
        }
        if (Date.parse(at) <= Date.now()) {
            error = "Reminder time must be in the future.";
            return "";
        }
        error = "";
        const baseId = "r_" + Date.now();
        let id = baseId;
        for (let suffix = 1; items.some(item => item.id === id); suffix++)
            id = baseId + "_" + suffix;
        const next = items.slice();
        next.push({
            id: id,
            title: (String(title || "reminder").trim() || "reminder").slice(0, 200),
            description: String(description || "").trim().slice(0, 1000),
            at: at,
            urgent: !!urgent,
            fired: false
        });
        items = next;
        persist();
        schedule();
        gen++;
        return id;
    }

    function remove(id) {
        const next = [];
        for (let i = 0; i < items.length; i++) {
            if (items[i].id !== id)
                next.push(items[i]);
        }
        if (next.length === items.length)
            return false;
        items = next;
        persist();
        schedule();
        gen++;
        return true;
    }

    function activeJson() {
        return JSON.stringify(items.filter(item => !item.fired).slice().sort((a, b) => Date.parse(a.at) - Date.parse(b.at)));
    }

    function addCommand(commandLine) {
        if (commandJob) {
            error = "A reminder command is already being checked.";
            return false;
        }
        if (!String(commandLine || "").trim().length) {
            error = "Enter a reminder command.";
            return false;
        }
        error = "";
        commandJob = { output: "", message: "", outputDone: false, errorDone: false,
            exited: false, exitCode: -1 };
        commandParser.command = [Quickshell.env("HOME") + "/.local/bin/myscripts/remind",
            "parse", String(commandLine).trim()];
        commandParser.running = true;
        return true;
    }

    function finishCommandParse() {
        const job = commandJob;
        if (!job || !job.outputDone || !job.errorDone || !job.exited)
            return;
        commandJob = null;
        if (job.exitCode !== 0) {
            error = job.message.trim().replace(/^Error:\s*/, "") || "Could not parse the reminder command.";
            return;
        }
        let reminder;
        try {
            reminder = JSON.parse(job.output);
        } catch (e) {
            error = "The reminder parser returned invalid data.";
            return;
        }
        if (!reminder || typeof reminder.title !== "string" || typeof reminder.description !== "string"
                || typeof reminder.at !== "string" || typeof reminder.urgent !== "boolean") {
            error = "The reminder parser returned invalid data.";
            return;
        }
        const id = add(reminder.title, reminder.at, reminder.urgent, reminder.description);
        if (id)
            commandAdded(id);
    }

    function snooze(id, minutes) {
        const next = items.slice();
        const when = new Date(Date.now() + minutes * 60000);
        for (let i = 0; i < next.length; i++) {
            if (next[i].id === id) {
                next[i].fired = false;
                next[i].at = isoIst(when);
            }
        }
        items = next;
        persist();
        schedule();
        gen++;
    }

    function handleAction(id, action) {
        if (!items.some(item => item.id === id && item.fired))
            return;
        if (action === "snooze")
            snooze(id, 10);
        else if (action === "done")
            remove(id);
    }

    function isoIst(d) {
        return isFinite(d.getTime()) ? new Date(d.getTime() + 330 * 60000).toISOString().slice(0, 19) + "+05:30" : "";
    }

    function parseLocal(s) {
        const t = String(s || "").trim().replace(" ", "T");
        const parts = /^(\d{4}-\d{2}-\d{2})T(\d{2}):(\d{2})(?::(\d{2})(?:\.\d{1,3})?)?(Z|[+-]\d{2}:\d{2})?$/.exec(t);
        if (!parts || Number(parts[2]) > 23 || Number(parts[3]) > 59 || Number(parts[4] || 0) > 59)
            return "";
        const day = new Date(parts[1] + "T00:00:00Z");
        if (!isFinite(day.getTime()) || day.toISOString().slice(0, 10) !== parts[1])
            return "";
        return isoIst(new Date(parts[5] ? t : t + "+05:30"));
    }

    function nextUnfired() {
        let best = null;
        let bestMs = Infinity;
        for (let i = 0; i < items.length; i++) {
            const r = items[i];
            if (r.fired)
                continue;
            const ms = Date.parse(r.at);
            if (isNaN(ms))
                continue;
            if (ms < bestMs) {
                bestMs = ms;
                best = r;
            }
        }
        return best;
    }

    function fire(): void {
        if (delivery || notifier.running)
            return;
        const due = nextUnfired();
        if (!due || Date.parse(due.at) > Date.now() || retryAfter > Date.now()) {
            schedule();
            return;
        }
        wake.stop();
        delivery = Object.assign({}, due);
        notifier.command = [Quickshell.shellPath(".local/bin/qs-reminder"),
            "notify", due.id, due.title, due.at, due.urgent ? "critical" : "normal", due.description || ""];
        notifier.running = true;
    }

    function finishDelivery(exitCode) {
        const sent = delivery;
        delivery = null;
        if (!sent)
            return;
        const pending = items.some(item => item.id === sent.id && item.at === sent.at && !item.fired);
        if (pending && exitCode !== 0) {
            error = "Could not deliver a reminder. Retrying in 30 seconds.";
            retryAfter = Date.now() + 30000;
            schedule();
            return;
        }
        if (pending) {
            items = items.map(item => item.id === sent.id && item.at === sent.at
                ? Object.assign({}, item, { fired: true }) : item);
            error = "";
            retryAfter = 0;
            persist();
            gen++;
        }
        Qt.callLater(root.fire);
    }

    function schedule() {
        const n = nextUnfired();
        const stop = "systemctl --user stop qs-reminders.timer qs-reminders.service 2>/dev/null || true";
        if (!n) {
            wake.stop();
            scheduledCommand = ["sh", "-c", stop];
        } else {
            const deadline = Math.max(Date.parse(n.at), retryAfter);
            const ms = deadline - Date.now();
            wake.interval = Math.max(250, Math.min(ms, maxTimerMs));
            wake.restart();
            const cal = new Date(deadline).toISOString().slice(0, 19).replace("T", " ") + " UTC";
            const helper = Quickshell.shellPath(".local/bin/qs-reminder");
            const shell = Quickshell.shellPath("");
            const quote = value => "'" + String(value).replace(/'/g, "'\\''") + "'";
            scheduledCommand = ["sh", "-c", stop + "; systemd-run --user --collect --unit=qs-reminders --on-calendar="
                + quote(cal) + " --timer-property=Persistent=true --timer-property=AccuracySec=1s "
                + quote(helper) + " wake " + quote(shell)];
        }
        runSchedule();
    }

    function runSchedule() {
        if (scheduler.running || !scheduledCommand.length)
            return;
        scheduler.command = scheduledCommand;
        scheduledCommand = [];
        scheduler.running = true;
    }

    function persist() {
        store.setText(JSON.stringify({ items: items }));
    }

    function load() {
        try {
            const raw = store.text();
            if (raw && raw.length) {
                const obj = JSON.parse(raw);
                const rows = Array.isArray(obj) ? obj : obj && obj.items;
                if (!Array.isArray(rows))
                    throw new Error("Invalid reminders");
                items = rows.filter(row => row && typeof row.id === "string" && typeof row.title === "string" && parseLocal(row.at))
                    .map(row => ({
                        id: row.id,
                        title: row.title.slice(0, 200),
                        description: typeof row.description === "string" ? row.description.slice(0, 1000) : "",
                        at: parseLocal(row.at),
                        urgent: row.urgent === true,
                        fired: row.fired === true
                    }));
            }
        } catch (e) {
            items = [];
        }
        schedule();
        gen++;
    }

    Connections {
        target: Notifications
        function onReminderAction(reminderId, action) { root.handleAction(reminderId, action); }
    }

    Connections {
        target: Logind
        function onPreparingForSleepChanged() { if (!Logind.preparingForSleep) root.fire(); }
    }

    Process {
        id: notifier
        onExited: (exitCode, exitStatus) => root.finishDelivery(exitStatus === 0 ? exitCode : -1)
        onRunningChanged: { if (!running && root.delivery) root.finishDelivery(-1); }
    }

    Process {
        id: scheduler
        onExited: root.runSchedule()
    }

    Process {
        id: commandParser
        stdout: StdioCollector {
            onStreamFinished: {
                if (!root.commandJob)
                    return;
                root.commandJob.output = this.text;
                root.commandJob.outputDone = true;
                root.finishCommandParse();
            }
        }
        stderr: StdioCollector {
            onStreamFinished: {
                if (!root.commandJob)
                    return;
                root.commandJob.message = this.text;
                root.commandJob.errorDone = true;
                root.finishCommandParse();
            }
        }
        onExited: (exitCode, exitStatus) => {
            if (!root.commandJob)
                return;
            root.commandJob.exitCode = exitStatus === 0 ? exitCode : -1;
            root.commandJob.exited = true;
            root.finishCommandParse();
        }
        onRunningChanged: {
            const job = root.commandJob;
            if (running || !job || job.exited)
                return;
            job.outputDone = true;
            job.errorDone = true;
            job.exited = true;
            job.exitCode = -1;
            root.finishCommandParse();
        }
    }

    IpcHandler {
        target: "reminders"

        function fire(): void {
            root.fire();
        }

        function add(title: string, atIso: string, urgent: bool, description: string): string {
            return root.add(title, atIso, urgent, description);
        }

        function list(): string {
            return root.activeJson();
        }

        function cancel(id: string): bool {
            return root.remove(id);
        }
    }

    Timer {
        id: wake
        repeat: false
        onTriggered: root.fire()
    }

    FileView {
        id: store
        path: `${Quickshell.dataDir}/reminders.json`
        blockLoading: true
        printErrors: false
        Component.onCompleted: root.load()
    }
}
