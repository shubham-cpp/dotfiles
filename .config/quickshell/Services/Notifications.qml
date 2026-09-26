pragma Singleton

import Quickshell
import Quickshell.Io
import Quickshell.Services.Notifications
import QtQuick
import qs.Common
import "../Common/NotificationPolicy.js" as Policy

Singleton {
    id: root

    readonly property bool ready: true
    property bool dnd: false
    property bool centerOpen: false
    onCenterOpenChanged: {
        if (centerOpen) {
            for (const n of root.toasts.slice())
                expireToast(n);
        } else {
            markAllRead();
        }
    }
    property var history: []
    property var toasts: []
    property var lockNotifications: []
    property int unread: 0
    property int gen: 0
    signal toastUpdated(int notificationId)
    signal reminderAction(string reminderId, string action)

    readonly property string imageCacheDir: Quickshell.cacheDir + "/notif-images"
    property string imageEpoch: Date.now().toString(36)
    property int imageSerial: 0
    property var imageJob: null
    property bool imagesDirty: false

    readonly property int toastCap: Tokens.toastCap
    readonly property int historyCap: 80
    readonly property int defaultExpireSec: 6

    function strip(s) {
        return String(s || "").replace(/<[^>]*>/g, "").replace(/&nbsp;/g, " ").replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, "\"");
    }

    function isAllowlisted(n) {
        const name = String(n.appName || "").toLowerCase();
        const desk = String(n.desktopEntry || "").toLowerCase();
        const keys = ["notify-send", "quickshell", "qs-shell", "reminders"];
        for (let i = 0; i < keys.length; i++) {
            if (name.indexOf(keys[i]) !== -1 || desk.indexOf(keys[i]) !== -1)
                return true;
        }
        return false;
    }

    function shouldToast(n) {
        if (centerOpen || Lock.locked)
            return false;
        if (!dnd)
            return true;
        if (n.urgency === NotificationUrgency.Critical && isAllowlisted(n))
            return true;
        return false;
    }

    function lockStateChanged() {
        if (!Lock.locked) {
            lockNotifications = [];
            return;
        }
        for (const n of toasts.slice())
            expireToast(n);
    }

    function recordLockNotification(n) {
        if (!Lock.locked || Policy.isOsd(n)
                || (dnd && !(n.urgency === NotificationUrgency.Critical && isAllowlisted(n))))
            return;
        lockNotifications = [String(n.appName || n.desktopEntry || "Unknown app"), ...lockNotifications].slice(0, 5);
    }

    function shouldHistory(n) {
        if (n.transient)
            return false;
        return true;
    }

    function imageKeys(entries) {
        return entries.map(entry => entry.imageKey || "").filter(key => key.length);
    }

    function runImageWork() {
        if (imageWorker.running || root.imageJob)
            return;
        const entry = root.history.find(entry => entry.imageKey && entry.imageSource && !entry.image && !entry.imageFailed);
        if (!root.imagesDirty && !entry)
            return;
        root.imagesDirty = false;
        root.imageJob = { key: entry ? entry.imageKey : "", source: entry ? entry.imageSource : "",
            keep: imageKeys(root.history), output: "", outputDone: false, exited: false, exitCode: 0 };
        imageWorker.command = [Quickshell.shellPath(".local/bin/qs-notification-images"),
            root.imageCacheDir, JSON.stringify(root.imageJob)];
        imageWorker.running = true;
    }

    function finishImageWork() {
        const job = root.imageJob;
        if (!job || !job.outputDone || !job.exited)
            return;
        let result = null;
        try { result = JSON.parse(job.output); } catch (_) {}
        const valid = job.exitCode === 0 && result && Array.isArray(result.kept);
        const destination = root.imageCacheDir + "/" + job.key;
        const next = root.history.map(entry => {
            let image = entry.image;
            let failed = entry.imageFailed;
            if (valid && image.indexOf(root.imageCacheDir + "/") === 0 && result.kept.indexOf(entry.imageKey) === -1) {
                image = "";
                failed = true;
            }
            if (entry.imageKey === job.key && job.key) {
                image = valid && result.path === destination ? destination : "";
                failed = !image.length;
            }
            return image === entry.image && failed === entry.imageFailed ? entry
                : Object.assign({}, entry, { image: image, imageFailed: failed });
        });
        if (next.some((entry, index) => entry !== root.history[index]))
            saveHistory(next);
        root.imageJob = null;
        if (root.imagesDirty || root.history.some(entry => entry.imageKey && entry.imageSource && !entry.image && !entry.imageFailed))
            imageTimer.restart();
    }

    function actionLabels(n) {
        const out = [];
        const acts = n.actions;
        for (let i = 0; i < acts.length; i++)
            out.push({
                identifier: acts[i].identifier,
                text: acts[i].text
            });
        return out;
    }

    function snapshot(n, imageChanged) {
        const source = String(n.image || "");
        const local = source.indexOf("file:") === 0;
        const previous = imageChanged ? null
            : root.history.find(entry => entry.live && entry.id === n.id && entry.imageSource === source);
        return {
            id: n.id,
            appName: n.appName,
            appIcon: n.appIcon,
            desktopEntry: n.desktopEntry,
            summary: strip(n.summary),
            body: strip(n.body),
            urgency: n.urgency,
            image: local ? (previous ? previous.image : "") : source,
            imageSource: local ? source : "",
            imageKey: local ? (previous ? previous.imageKey : "img-" + root.imageEpoch + "-" + (++root.imageSerial)) : "",
            imageFailed: local && previous ? previous.imageFailed : false,
            actions: actionLabels(n),
            stackKey: Policy.stackKey(n),
            time: Date.now(),
            read: root.centerOpen,
            live: true
        };
    }

    function pushHistory(snap) {
        const next = root.history.slice();
        for (let i = 0; i < next.length; i++) {
            if ((next[i].live && next[i].id === snap.id) || (snap.stackKey && next[i].stackKey === snap.stackKey)) {
                snap.read = next[i].read || snap.read;
                next[i] = snap;
                saveHistory(next);
                return;
            }
        }
        next.unshift(snap);
        while (next.length > root.historyCap)
            next.pop();
        saveHistory(next);
    }

    function saveHistory(next) {
        const imagesChanged = imageKeys(root.history).join("\n") !== imageKeys(next).join("\n");
        root.history = next;
        root.unread = next.filter(entry => !entry.read).length;
        persistSoon();
        root.gen++;
        if (imagesChanged) {
            root.imagesDirty = true;
            imageTimer.restart();
        }
    }

    function removeHistory(id, stackKey) {
        const next = root.history.filter(entry => !(entry.live && entry.id === id) && !(stackKey && entry.stackKey === stackKey));
        if (next.length !== root.history.length)
            saveHistory(next);
    }

    function showToast(n) {
        const next = root.toasts.slice();
        for (let i = 0; i < next.length; i++) {
            if (next[i].id === n.id) {
                root.toastUpdated(n.id);
                return;
            }
        }
        next.unshift(n);
        next.splice(root.toastCap);
        root.toasts = next;
        root.gen++;
    }

    function hideToast(id) {
        const next = [];
        for (let i = 0; i < root.toasts.length; i++) {
            if (root.toasts[i].id !== id)
                next.push(root.toasts[i]);
        }
        if (next.length !== root.toasts.length) {
            root.toasts = next;
            root.gen++;
        }
    }

    function expireToast(n) {
        hideToast(n.id);
        releaseUnowned();
    }

    function releaseUnowned() {
        // History keeps actions alive; an evicted entry can still own a visible toast.
        const owned = new Set(root.toasts.map(n => n.id));
        for (const entry of root.history) {
            if (entry.live)
                owned.add(entry.id);
        }
        for (const n of server.trackedNotifications.values.slice()) {
            if (!owned.has(n.id))
                n.expire();
        }
    }

    function markDead(id) {
        const next = root.history.slice();
        let changed = false;
        for (let i = 0; i < next.length; i++) {
            if (next[i].live && next[i].id === id) {
                next[i].live = false;
                changed = true;
            }
        }
        if (changed)
            saveHistory(next);
    }

    function liveById(id) {
        const vals = server.trackedNotifications.values;
        for (let i = 0; i < vals.length; i++) {
            if (vals[i] && vals[i].id === id)
                return vals[i];
        }
        return null;
    }

    function ingest(n) {
        let closed = false;
        let imageChanged = false;
        const id = n.id;
        const refresh = () => {
            if (!closed) {
                root.refreshNotification(n, imageChanged);
                imageChanged = false;
            }
        };
        const schedule = () => Qt.callLater(refresh);
        n.imageChanged.connect(() => {
            imageChanged = true;
            schedule();
        });
        // Replacements mutate this object; NotificationServer.notification only signals new IDs.
        const changes = ["summaryChanged", "bodyChanged", "appNameChanged", "appIconChanged",
            "urgencyChanged", "expireTimeoutChanged", "hintsChanged",
            "actionsChanged", "residentChanged", "transientChanged", "desktopEntryChanged",
            "hasActionIconsChanged", "hasInlineReplyChanged", "inlineReplyPlaceholderChanged"];
        for (const change of changes)
            n[change].connect(schedule);
        n.closed.connect(() => {
            closed = true;
            root.hideToast(id);
            root.markDead(id);
        });

        if (n.lastGeneration) {
            const next = root.history.slice();
            const entry = next.find(entry => entry.id === id);
            if (entry) {
                entry.live = true;
                saveHistory(next);
            } else
                n.expire();
        } else {
            recordLockNotification(n);
            refreshNotification(n);
        }
    }

    function refreshNotification(n, imageChanged) {
        const key = Policy.stackKey(n);
        if (Policy.isOsd(n)) {
            removeHistory(n.id, "");
            n.expire();
            return;
        }

        if (key) {
            // Stack tags group presentation; each sender still receives its own protocol ID.
            const tracked = server.trackedNotifications.values.slice();
            for (const old of tracked) {
                if (old.id !== n.id && Policy.stackKey(old) === key)
                    old.expire();
            }
        }

        const keepHistory = shouldHistory(n);
        const keepToast = shouldToast(n);
        if (keepHistory)
            pushHistory(snapshot(n, imageChanged));
        else
            removeHistory(n.id, key);
        if (keepToast)
            showToast(n);
        else
            hideToast(n.id);
        releaseUnowned();
    }

    function invoke(id, identifier) {
        const n = liveById(id);
        if (!n)
            return;
        const acts = n.actions;
        for (let i = 0; i < acts.length; i++) {
            if (acts[i].identifier === identifier) {
                const reminderId = n.hints["x-quickshell-reminder-id"];
                if (n.appName === "reminders" && typeof reminderId === "string"
                        && reminderId.length && (identifier === "snooze" || identifier === "done")) {
                    root.reminderAction(reminderId, identifier);
                    dismissLive(n);
                    return;
                }
                acts[i].invoke();
                return;
            }
        }
    }

    function invokeDefault(n) {
        const acts = n.actions;
        for (let i = 0; i < acts.length; i++) {
            if (acts[i].identifier === "default") {
                acts[i].invoke();
                return;
            }
        }
    }

    function dismissLive(n) {
        removeHistory(n.id, "");
        hideToast(n.id);
        n.dismiss();
    }

    function dismissHistory(entry) {
        if (root.history.indexOf(entry) === -1)
            return;
        const n = entry.live ? liveById(entry.id) : null;
        saveHistory(root.history.filter(candidate => candidate !== entry));
        if (n)
            n.dismiss();
    }

    function clearHistory() {
        const tracked = server.trackedNotifications.values.slice();
        saveHistory([]);
        for (const n of tracked)
            n.dismiss();
    }

    function markAllRead() {
        if (!root.history.some(entry => !entry.read))
            return;
        saveHistory(root.history.map(entry => Object.assign({}, entry, {read: true})));
    }

    function copy(text) {
        Quickshell.clipboardText = String(text || "");
    }

    function toggleCenter() {
        centerOpen = !centerOpen;
        gen++;
    }

    function toggleDnd() {
        dnd = !dnd;
        persistSoon();
        gen++;
        return dnd;
    }

    function expireSec(n) {
        const t = n.expireTimeout;
        if (t === 0)
            return 0;
        if (t > 0)
            // Quickshell 0.3.1 exposes the D-Bus timeout in milliseconds.
            return t / 1000;
        if (n.urgency === NotificationUrgency.Critical)
            return 10;
        return defaultExpireSec;
    }

    function persistSoon() {
        persistTimer.restart();
    }

    function persist() {
        const payload = JSON.stringify({
            dnd: root.dnd,
            unread: root.unread,
            history: root.history.map(entry => Object.assign({}, entry, { imageSource: "" }))
        });
        store.setText(payload);
    }

    function load() {
        root.imagesDirty = true;
        imageTimer.restart();
        try {
            const raw = store.text();
            if (!raw || !raw.length)
                return;
            const obj = JSON.parse(raw);
            if (typeof obj.dnd === "boolean")
                root.dnd = obj.dnd;
            if (Array.isArray(obj.history))
                root.history = obj.history.filter(entry => entry && typeof entry === "object" && Number.isInteger(entry.id))
                    .slice(0, root.historyCap).map(entry => Object.assign({}, entry, {
                        image: typeof entry.image === "string" ? entry.image : "",
                        imageKey: typeof entry.imageKey === "string" ? entry.imageKey : "",
                        imageSource: "", live: false
                    }));
            root.unread = 0;
            for (let i = 0; i < root.history.length; i++) {
                if (!root.history[i].read)
                    root.unread++;
            }
            root.gen++;
        } catch (e) {
        }
    }

    IpcHandler {
        target: "notifs"

        function toggleCenter(): void {
            root.toggleCenter();
        }

        function toggleDnd(): bool {
            return root.toggleDnd();
        }
    }

    FileView {
        id: store
        path: `${Quickshell.stateDir}/notifs.json`
        blockLoading: true
        printErrors: false
        Component.onCompleted: root.load()
    }

    Timer {
        id: persistTimer
        interval: 400
        repeat: false
        onTriggered: root.persist()
    }

    Timer {
        id: imageTimer
        interval: 1
        onTriggered: root.runImageWork()
    }

    Process {
        id: imageWorker
        stdout: StdioCollector {
            onStreamFinished: {
                if (!root.imageJob)
                    return;
                root.imageJob.output = text;
                root.imageJob.outputDone = true;
                root.finishImageWork();
            }
        }
        onExited: (exitCode, exitStatus) => {
            if (!root.imageJob)
                return;
            root.imageJob.exitCode = exitCode;
            root.imageJob.exited = true;
            root.finishImageWork();
        }
        onRunningChanged: {
            const job = root.imageJob;
            if (running || !job || job.exited) return;
            job.outputDone = true;
            job.exited = true;
            job.exitCode = -1;
            root.finishImageWork();
        }
    }

    Connections {
        target: Lock
        function onLockedChanged() { root.lockStateChanged(); }
    }

    NotificationServer {
        id: server
        actionsSupported: true
        actionIconsSupported: true
        imageSupported: true
        bodySupported: true
        bodyMarkupSupported: false
        persistenceSupported: true
        inlineReplySupported: true
        keepOnReload: true
        onNotification: n => {
            n.tracked = true;
            root.ingest(n);
        }
    }
}
