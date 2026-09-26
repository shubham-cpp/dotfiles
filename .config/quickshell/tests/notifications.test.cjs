const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const policy = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(__dirname, "../Common/NotificationPolicy.js"), "utf8").replace(/^\.pragma library\s*/, ""), policy);

function signal() {
    const callbacks = [];
    return { connect(callback) { callbacks.push(callback); }, emit() { callbacks.forEach(callback => callback()); } };
}

function service() {
    const pending = new Set();
    const updates = [];
    const tracked = [];
    const reminderActions = [];
    const context = vm.createContext({
        Policy: policy,
        NotificationUrgency: { Critical: 2 },
        Qt: { callLater(callback) { pending.add(callback); } },
        server: { trackedNotifications: { values: tracked } },
        persistTimer: { restart() {} },
        imageTimer: { restart() {} },
        imageWorker: { running: false, command: [] },
        imageCacheDir: "/tmp/notification-fixture/notif-images",
        imageEpoch: "fixture", imageSerial: 0, imageJob: null, imagesDirty: false,
        Quickshell: { shellPath: value => value },
        Lock: { locked: false },
        reminderAction(id, action) { reminderActions.push([id, action]); },
        toastUpdated(id) { updates.push(id); },
        history: [], toasts: [], lockNotifications: [], unread: 0, gen: 0, dnd: false, centerOpen: false,
        historyCap: 80, toastCap: 4, defaultExpireSec: 6
    });
    context.root = context;
    const source = fs.readFileSync(path.join(__dirname, "../Services/Notifications.qml"), "utf8");
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0], context);

    function receive(id, overrides = {}) {
        const n = {
            id, appName: "Test app", desktopEntry: "", appIcon: "", image: "",
            summary: "Message", body: "First", hints: {}, actions: [], urgency: 1,
            transient: false, expireTimeout: 3000, lastGeneration: false, expired: false,
            ...overrides
        };
        for (const name of ["closed", "summaryChanged", "bodyChanged", "appNameChanged", "appIconChanged",
            "imageChanged", "urgencyChanged", "expireTimeoutChanged", "hintsChanged", "actionsChanged",
            "residentChanged", "transientChanged", "desktopEntryChanged", "hasActionIconsChanged",
            "hasInlineReplyChanged", "inlineReplyPlaceholderChanged"])
            n[name] = signal();
        n.expire = () => {
            assert.equal(n.expired, false, "must not close an already closed object");
            n.expired = true;
            tracked.splice(tracked.indexOf(n), 1);
            n.closed.emit();
        };
        n.dismiss = n.expire;
        tracked.push(n);
        context.ingest(n);
        return n;
    }

    function flush() {
        const callbacks = [...pending];
        pending.clear();
        callbacks.forEach(callback => callback());
    }
    return { context, receive, flush, updates, tracked, reminderActions };
}

function tagged(tag, extra = {}) {
    return { hints: { "x-canonical-private-synchronous": tag }, ...extra };
}

test("locking hides existing toasts without dismissing their history", () => {
    const { context, receive } = service();
    receive(1, { appName: "Mail" });
    context.Lock.locked = true;
    context.lockStateChanged();
    assert.equal(context.toasts.length, 0);
    assert.equal(context.history.length, 1);
    assert.deepEqual(Array.from(context.lockNotifications), []);
});

test("new locked notifications show app names only, stay in history, and never toast", () => {
    const { context, receive } = service();
    context.Lock.locked = true;
    context.lockStateChanged();
    receive(1, { appName: "Mail", summary: "Secret title", body: "Secret body" });
    assert.deepEqual(Array.from(context.lockNotifications), ["Mail"]);
    assert.equal(context.toasts.length, 0);
    assert.equal(context.history[0].summary, "Secret title");
});

test("lock list retains five newest names across repeated lock requests and clears on unlock", () => {
    const { context, receive } = service();
    context.Lock.locked = true;
    context.lockStateChanged();
    for (let id = 1; id <= 6; id++)
        receive(id, { appName: `App ${id}` });
    assert.deepEqual(Array.from(context.lockNotifications), ["App 6", "App 5", "App 4", "App 3", "App 2"]);
    context.lockStateChanged();
    assert.equal(context.lockNotifications[0], "App 6");
    context.Lock.locked = false;
    context.lockStateChanged();
    assert.deepEqual(Array.from(context.lockNotifications), []);
    context.Lock.locked = true;
    context.lockStateChanged();
    assert.deepEqual(Array.from(context.lockNotifications), []);
});

test("locked list obeys DND and ignores OSD, but keeps ordinary notifications in history", () => {
    const { context, receive } = service();
    context.Lock.locked = true;
    context.dnd = true;
    receive(1, { appName: "Mail" });
    receive(2, { appName: "System OSD", hints: { category: "device", "x-dunst-stack-tag": "volume" } });
    receive(3, { appName: "reminders", urgency: 2 });
    assert.deepEqual(Array.from(context.lockNotifications), ["reminders"]);
    assert.equal(context.toasts.length, 0);
    assert.equal(context.history.length, 2);
});

test("updates to an existing notification cannot add a second lock row", () => {
    const { context, receive, flush } = service();
    const old = receive(1, { appName: "Mail" });
    context.Lock.locked = true;
    old.bodyChanged.emit();
    flush();
    assert.deepEqual(Array.from(context.lockNotifications), []);
    const incoming = receive(2, { appName: "Chat" });
    incoming.bodyChanged.emit();
    flush();
    assert.deepEqual(Array.from(context.lockNotifications), ["Chat"]);
});

test("only known noncritical volume and backlight events are filtered", () => {
    for (const key of ["x-canonical-private-synchronous", "x-dunst-stack-tag"]) {
        for (const tag of ["volume", "backlight", "microphone"]) {
            const { context, receive, tracked } = service();
            const n = receive(1, { appName: "System OSD", hints: { category: "device", [key]: tag } });
            const filtered = tag !== "microphone";
            assert.equal(n.expired, filtered);
            assert.equal(context.toasts.length, filtered ? 0 : 1);
            assert.equal(context.history.length, filtered ? 0 : 1);
            assert.equal(tracked.length, filtered ? 0 : 1);
        }
    }
    for (const overrides of [{ appName: "Other app" }, { urgency: 2 }, { hints: { category: "error", "x-dunst-stack-tag": "volume" } }]) {
        const { context, receive } = service();
        receive(1, { appName: "System OSD", hints: { category: "device", "x-dunst-stack-tag": "volume" }, ...overrides });
        assert.equal(context.toasts.length, 1);
    }
});

test("filtered events do not rebuild unrelated toasts or history", () => {
    const { context, receive } = service();
    receive(1);
    const toasts = context.toasts, history = context.history;
    receive(2, { appName: "System OSD", hints: { category: "device", "x-dunst-stack-tag": "volume" } });
    assert.equal(context.toasts, toasts);
    assert.equal(context.history, history);
});

test("filtered volume events cannot remove a preserved critical error with the same tag", () => {
    const { context, receive } = service();
    const hints = { category: "device", "x-dunst-stack-tag": "volume" };
    receive(1, { appName: "System OSD", urgency: 2, hints });
    receive(2, { appName: "System OSD", hints });
    assert.equal(context.history.length, 1);
    assert.equal(context.history[0].id, 1);
    assert.equal(context.toasts[0].id, 1);
});

test("a valid-ID update refreshes history and its timer once without rebuilding the card", () => {
    const { context, receive, flush, updates } = service();
    const n = receive(1);
    const toasts = context.toasts;
    n.summary = "Updated";
    n.body = "Second";
    n.summaryChanged.emit();
    n.bodyChanged.emit();
    n.hintsChanged.emit();
    flush();
    assert.deepEqual(updates, [1]);
    assert.equal(context.toasts, toasts);
    assert.equal(context.history.length, 1);
    assert.equal(context.history[0].body, "Second");
    assert.equal(context.unread, 1);
    context.history[0].read = true;
    n.bodyChanged.emit();
    flush();
    assert.equal(context.unread, 0);
});

test("stack tags replace older objects and history only within the same application", () => {
    const { context, receive, tracked } = service();
    const old = receive(1, tagged("progress"));
    receive(2, tagged("progress", { appName: "Another app" }));
    receive(3, { hints: { "x-dunst-stack-tag": "progress" }, body: "Latest" });
    assert.equal(old.expired, true);
    assert.deepEqual(tracked.map(n => n.id), [2, 3]);
    assert.equal(context.history.length, 2);
    assert.equal(context.history.find(entry => entry.appName === "Test app").body, "Latest");
    assert.equal(context.unread, 2);
});

test("pending replacement callbacks cannot resurrect a closed notification", () => {
    const { context, receive, flush, updates } = service();
    const n = receive(1);
    n.bodyChanged.emit();
    n.expire();
    flush();
    assert.equal(context.toasts.length, 0);
    assert.equal(context.history[0].live, false);
    assert.deepEqual(updates, []);
});

test("a replacement can reshow an expired card or remove it under DND", () => {
    const { context, receive, flush } = service();
    const n = receive(1);
    context.expireToast(n);
    assert.equal(context.toasts.length, 0);
    n.bodyChanged.emit();
    flush();
    assert.equal(context.toasts.length, 1);
    context.dnd = true;
    n.bodyChanged.emit();
    flush();
    assert.equal(context.toasts.length, 0);
    assert.equal(context.history.length, 1);
});

test("transient updates remove old grouped history and release objects on expiry", () => {
    const { context, receive, tracked } = service();
    receive(1, tagged("progress"));
    const n = receive(2, tagged("progress", { transient: true }));
    assert.equal(context.history.length, 0);
    context.expireToast(n);
    assert.equal(tracked.length, 0);
    assert.equal(context.toasts.length, 0);
});

test("reused IDs do not overwrite closed history and reload survivors do not duplicate it", () => {
    const { context, receive, flush } = service();
    receive(1).expire();
    receive(1, { body: "New lifetime" });
    assert.equal(context.history.length, 2);
    context.toasts = [];
    context.history[0].live = false;
    const restored = receive(1, { body: "After reload", lastGeneration: true });
    assert.equal(context.toasts.length, 0);
    restored.bodyChanged.emit();
    flush();
    assert.equal(context.history.length, 2);
    assert.equal(context.history[0].body, "After reload");
});

test("transient overflow releases discarded objects", () => {
    const { context, receive, tracked } = service();
    context.toastCap = 1;
    const old = receive(1, { transient: true });
    receive(2, { transient: true });
    assert.equal(old.expired, true);
    assert.deepEqual(tracked.map(n => n.id), [2]);
});

test("timeout units retain fractional seconds and the persistent/default cases", () => {
    const { context } = service();
    assert.equal(context.expireSec({ expireTimeout: 1500 }), 1.5);
    assert.equal(context.expireSec({ expireTimeout: 0 }), 0);
    assert.equal(context.expireSec({ expireTimeout: -1, urgency: 1 }), 6);
});

test("dismissing archived history cannot close a live notification that reused its ID", () => {
    const { context, receive } = service();
    receive(1).expire();
    const archived = context.history[0];
    const live = receive(1, { body: "New lifetime" });
    context.dismissHistory(archived);
    assert.equal(live.expired, false);
    assert.equal(context.history.length, 1);
    assert.equal(context.history[0].body, "New lifetime");
    context.dismissHistory(archived);
    assert.equal(context.history.length, 1);
});

test("explicit dismissal removes history, while toast timeout keeps it", () => {
    const { context, receive } = service();
    const first = receive(1);
    context.expireToast(first);
    assert.equal(context.history.length, 1);
    context.dismissHistory(context.history[0]);
    assert.equal(first.expired, true);
    assert.equal(context.history.length, 0);
    const second = receive(2);
    context.dismissLive(second);
    assert.equal(context.toasts.length, 0);
    assert.equal(context.history.length, 0);
    assert.equal(context.unread, 0);
});

test("clear all closes tracked notifications and cancels pending replacements", () => {
    const { context, receive, flush, tracked } = service();
    const n = receive(1);
    receive(2, { transient: true });
    n.bodyChanged.emit();
    context.clearHistory();
    flush();
    assert.equal(context.history.length, 0);
    assert.equal(context.toasts.length, 0);
    assert.equal(context.unread, 0);
    assert.equal(tracked.length, 0);
});

test("an open centre receives history without duplicate toasts and can mark it read", () => {
    const { context, receive } = service();
    receive(1);
    context.centerOpen = true;
    receive(2);
    assert.equal(context.toasts.length, 1);
    assert.equal(context.history.length, 2);
    assert.equal(context.unread, 1);
    assert.equal(context.shouldToast({ urgency: 2, appName: "notify-send" }), false);
    context.markAllRead();
    assert.equal(context.unread, 0);
    assert.equal(context.history.every(entry => entry.read), true);
});

test("ordinary notification retention stays bounded after repeated toast expiry", () => {
    const { context, receive, tracked } = service();
    for (let id = 1; id <= 1000; id++)
        context.expireToast(receive(id));
    assert.equal(context.history.length, 80);
    assert.equal(context.toasts.length, 0);
    assert.equal(tracked.length, 80);
    assert.equal(context.history.every(entry => entry.live), true);
});

test("history eviction preserves a visible toast until its final owner releases it", () => {
    const { context, receive, tracked } = service();
    context.historyCap = 1;
    const old = receive(1, { resident: true, expireTimeout: 0 });
    receive(2);
    assert.equal(old.expired, false);
    context.expireToast(old);
    assert.equal(old.expired, true);
    assert.deepEqual(tracked.map(n => n.id), [2]);
});

test("DND history eviction and toast overflow release unowned ordinary objects", () => {
    const { context, receive, tracked } = service();
    context.historyCap = 1;
    context.toastCap = 1;
    const first = receive(1);
    const second = receive(2);
    assert.equal(first.expired, true);
    context.dnd = true;
    const third = receive(3);
    assert.equal(second.expired, false);
    context.expireToast(second);
    assert.equal(second.expired, true);
    receive(4);
    assert.equal(third.expired, true);
    assert.deepEqual(tracked.map(n => n.id), [4]);
});

test("local images publish only after copy success and unchanged replacements reuse them", () => {
    const { context, receive, flush } = service();
    const n = receive(1, { image: "file:///tmp/first.png" });
    assert.equal(context.history[0].image, "");
    context.runImageWork();
    const key = context.imageJob.key;
    const destination = context.imageCacheDir + "/" + key;
    Object.assign(context.imageJob, { output: JSON.stringify({ path: destination, kept: [key] }), outputDone: true });
    context.finishImageWork();
    assert.equal(context.history[0].image, "");
    assert.notEqual(context.imageJob, null);
    Object.assign(context.imageJob, { exited: true, exitCode: 0 });
    context.imageWorker.running = false;
    context.finishImageWork();
    assert.equal(context.history[0].image, destination);
    n.bodyChanged.emit();
    flush();
    assert.equal(context.history[0].image, destination);
    assert.equal(context.history[0].imageKey, key);
    n.imageChanged.emit();
    flush();
    assert.notEqual(context.history[0].imageKey, key);
    assert.equal(context.history[0].image, "");
});

test("old history normalizes missing images and reload drops unowned native objects", () => {
    const { context, receive } = service();
    context.store = { text: () => JSON.stringify({ history: [null, { id: 1, body: "Archived" }] }) };
    context.load();
    assert.equal(context.history.length, 1);
    assert.equal(context.history[0].image, "");
    const retained = receive(1, { lastGeneration: true });
    const orphan = receive(2, { lastGeneration: true });
    assert.equal(retained.expired, false);
    assert.equal(orphan.expired, true);
    context.runImageWork();
    Object.assign(context.imageJob, { output: JSON.stringify({ path: "", kept: [] }), outputDone: true, exited: true, exitCode: 0 });
    context.imageWorker.running = false;
    assert.doesNotThrow(() => context.finishImageWork());
});

test("stale and failed image jobs cannot publish into replacement or evicted history", () => {
    const { context, receive, flush } = service();
    const n = receive(1, { image: "file:///tmp/old.png" });
    context.runImageWork();
    const oldKey = context.imageJob.key;
    Object.assign(context.imageJob, { exited: true, exitCode: 0 });
    context.finishImageWork();
    assert.notEqual(context.imageJob, null);
    n.image = "file:///tmp/new.png";
    n.imageChanged.emit();
    flush();
    Object.assign(context.imageJob, { output: JSON.stringify({ path: context.imageCacheDir + "/" + oldKey, kept: [oldKey] }), outputDone: true, exited: true, exitCode: 0 });
    context.imageWorker.running = false;
    context.finishImageWork();
    assert.equal(context.history[0].image, "");
    assert.notEqual(context.history[0].imageKey, oldKey);
    context.runImageWork();
    Object.assign(context.imageJob, { output: "", outputDone: true, exited: true, exitCode: 1 });
    context.imageWorker.running = false;
    context.finishImageWork();
    assert.equal(context.history[0].image, "");
    assert.equal(context.history[0].imageFailed, true);
    n.image = "file:///tmp/third.png";
    n.imageChanged.emit();
    flush();
    context.runImageWork();
    const key = context.imageJob.key;
    context.clearHistory();
    Object.assign(context.imageJob, { output: JSON.stringify({ path: context.imageCacheDir + "/" + key, kept: [key] }), outputDone: true, exited: true, exitCode: 0 });
    context.imageWorker.running = false;
    context.finishImageWork();
    assert.equal(context.history.length, 0);
    assert.equal(context.imagesDirty, true);
});

test("reminder actions route through the shell while other senders retain native actions", () => {
    const { context, receive, reminderActions } = service();
    let nativeCalls = 0;
    const actions = [{ identifier: "done", invoke() { nativeCalls++; } }];
    const n = receive(1, { appName: "reminders", hints: { "x-quickshell-reminder-id": "r1" }, actions });
    context.expireToast(n);
    context.invoke(1, "done");
    assert.deepEqual(reminderActions, [["r1", "done"]]);
    assert.equal(n.expired, true);
    receive(2, { appName: "Other app", hints: { "x-quickshell-reminder-id": "r1" }, actions });
    context.invoke(2, "done");
    assert.equal(nativeCalls, 1);
});

test("toast and history action buttons route through the shell even when invocation closes the object", () => {
    for (const file of ["ToastCard.qml", "HistoryCard.qml"]) {
        const source = fs.readFileSync(path.join(__dirname, "../Modules/notifications", file), "utf8");
        const button = source.slice(source.indexOf("id: actionButton"));
        const handler = button.match(/onClicked: (\{[\s\S]*?\n {20}\}|[^\n]+)/)[1];
        const calls = [];
        const root = { n: { id: 7, resident: false }, entry: { id: 7 } };
        vm.runInNewContext(handler, {
            root,
            modelData: { identifier: "done", invoke() { assert.fail("action bypassed shell routing"); } },
            Notifications: {
                invoke(id, action) { calls.push([id, action]); root.n = null; },
                hideToast(id) { assert.equal(id, 7); }
            }
        });
        assert.deepEqual(calls, [[7, "done"]]);
    }
});
