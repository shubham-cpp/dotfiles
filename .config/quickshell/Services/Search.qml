pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

// One attached child for all active search profiles. Callers own Qt objects;
// this module owns transport, revisions and the validity of published results.
Singleton {
    id: root
    property var _profiles: ({})
    property var _releases: []
    property var _flight: null
    property int _serial: 0
    property int _cursor: 0
    property int _retries: 0
    property bool _ready: false
    property bool _stopping: false
    property bool _broken: false
    property string _instance: ""
    property var _timing: ({})
    signal resultsReady(string profile, var keys)
    signal failed(string profile, string message)
    signal pending(string profile)

    function setCatalog(profile, rows) {
        if (!["launcher", "clipboard", "emoji", "files"].includes(profile)) return;
        const chunks = [];
        let batch = [], size = 0, total = 0;
        for (const row of rows) {
            const text = JSON.stringify(row);
            if (text.length > 64000 || total + text.length > 4000000 || rows.length > 10000) {
                release(profile);
                failed(profile, "Search data exceeds the supported size.");
                return;
            }
            if (size + text.length > 16000 && batch.length) {
                chunks.push("[" + batch.join(",") + "]");
                batch = []; size = 0;
            }
            batch.push(text); size += text.length; total += text.length;
        }
        if (batch.length) chunks.push("[" + batch.join(",") + "]");
        const epoch = ++_serial;
        _profiles[profile] = { epoch: epoch, revision: epoch, chunks: chunks, chunk: 0,
            phase: "begin", desired: 0, accepted: -1, query: null };
        pending(profile);
        Qt.callLater(root.pump);
    }

    function search(profile, query) {
        const state = _profiles[profile];
        if (!state) return;
        state.query = query;
        state.desired = ++_serial;
        pending(profile);
        Qt.callLater(root.pump);
    }

    // A local variant choice changes recovery inputs without reordering the
    // visible results. It is allowed only after the current result completed.
    function updateReplay(profile, changes) {
        const state = _profiles[profile];
        if (state && state.accepted === state.desired)
            state.query = Object.assign({}, state.query, changes);
    }

    function release(profile) {
        const state = _profiles[profile];
        if (!state) return;
        delete _profiles[profile];
        if (worker.running) _releases.push({ profile: profile, epoch: state.epoch, revision: state.revision });
        if (!Object.keys(_profiles).length) {
            _broken = false; _retries = 0;
            _releases = [];
            if (worker.running) {
                _stopping = true;
                worker.running = false;
                watchdog.interval = 1000;
                watchdog.restart();
            }
        }
        Qt.callLater(root.pump);
    }

    function send(profile, state, type, query, chunk) {
        const request = ++_serial;
        const message = Object.assign({ v: 1, type: type, profile: profile, epoch: state.epoch,
            revision: state.revision, request: request }, query || {});
        _flight = { profile: profile, epoch: state.epoch, revision: state.revision,
            request: request, type: type, desired: state.desired, sent: Date.now() };
        let line = JSON.stringify(message);
        if (chunk !== undefined) line = line.slice(0, -1) + ',"rows":' + chunk + '}';
        worker.write(line + "\n");
        watchdog.interval = type === "search" ? 1000 : 5000;
        watchdog.restart();
    }

    function pump() {
        const names = Object.keys(_profiles);
        if (!names.length || _broken || _stopping || _flight) return;
        if (!worker.running) {
            _ready = false; _instance = "";
            worker.running = true;
            watchdog.interval = 2000;
            watchdog.restart();
            return;
        }
        if (!_ready) return;
        if (_releases.length) {
            const item = _releases.shift();
            send(item.profile, item, "release");
            return;
        }
        for (let i = 0; i < names.length; i++) {
            const index = (_cursor + i) % names.length;
            const profile = names[index], state = _profiles[profile];
            if (state.phase === "ready" && (!state.query || state.accepted === state.desired)) continue;
            _cursor = (index + 1) % names.length;
            if (state.phase === "begin") send(profile, state, "begin");
            else if (state.phase === "chunks") {
                if (state.chunk < state.chunks.length) send(profile, state, "chunk", null, state.chunks[state.chunk]);
                else send(profile, state, "commit");
            } else send(profile, state, "search", state.query);
            return;
        }
    }

    function handleLine(line) {
        const readAt = Date.now();
        let message;
        try { message = JSON.parse(line); } catch (_) { protocolFailure(); return; }
        const parsedAt = Date.now();
        if (!message || typeof message !== "object" || Array.isArray(message) || message.v !== 1) {
            protocolFailure(); return;
        }
        if (message.type === "ready") {
            if (_ready || !message.instance) { protocolFailure(); return; }
            _instance = message.instance;
            _ready = true;
            watchdog.stop();
            Qt.callLater(root.pump);
            return;
        }
        const flight = _flight;
        if (!flight || message.instance !== _instance || message.request !== flight.request
                || message.profile !== flight.profile || message.epoch !== flight.epoch
                || message.revision !== flight.revision) return;
        // Completion and publication differ: an obsolete result still retires
        // its request so a newer pending query can run.
        _flight = null;
        watchdog.stop();
        const state = _profiles[flight.profile];
        if (state && state.epoch === flight.epoch) {
            if (message.type === "error") { protocolFailure(); return; }
            if (flight.type === "search") {
                const keys = message.keys === undefined ? [] : message.keys;
                const limit = flight.profile === "clipboard" ? 40 : flight.profile === "emoji" ? 20000 : 50;
                if (message.type !== "results" || !Array.isArray(keys) || keys.length > limit) {
                    protocolFailure(); return;
                }
                const seen = Object.create(null);
                for (const key of keys) {
                    if (typeof key !== "string" || !key.length || seen[key]) {
                        protocolFailure(); return;
                    }
                    seen[key] = true;
                }
                if (flight.desired === state.desired) {
                    state.accepted = state.desired;
                    _timing = { replyMs: readAt - flight.sent, parseMs: parsedAt - readAt,
                        validationMs: Date.now() - parsedAt, profile: flight.profile, characters: line.length };
                    resultsReady(flight.profile, keys);
                }
            } else if (message.type !== flight.type) { protocolFailure(); return; }
            else if (flight.type === "begin") { state.phase = "chunks"; state.chunk = 0; }
            else if (flight.type === "chunk") state.chunk++;
            else if (flight.type === "commit") state.phase = "ready";
        }
        Qt.callLater(root.pump);
    }

    function protocolFailure() {
        _ready = false;
        if (worker.running) worker.signal(9);
        else stopOrRecover();
    }

    function stopOrRecover() {
        watchdog.stop();
        _flight = null; _ready = false; _instance = ""; _releases = [];
        const expected = _stopping;
        _stopping = false;
        const names = Object.keys(_profiles);
        if (!names.length) return;
        if (!expected && ++_retries > 1) {
            _broken = true;
            for (const profile of names) failed(profile, "Search stopped. Close and reopen to retry.");
            return;
        }
        for (const profile of names) {
            const state = _profiles[profile];
            state.phase = "begin"; state.chunk = 0; state.accepted = -1;
            pending(profile);
        }
        retry.restart();
    }

    Process {
        id: worker
        stdinEnabled: true
        command: [Quickshell.shellPath(".local/bin/qs-search"), "--emoji-catalog", Quickshell.shellPath("data/emoji.json")]
        stdout: SplitParser { onRead: data => root.handleLine(data) }
        stderr: SplitParser { onRead: data => console.warn("Search helper:", data.slice(0, 256)) }
        onExited: root.stopOrRecover()
    }
    Timer { id: retry; interval: 100; onTriggered: root.pump() }
    IpcHandler {
        target: "search"
        function status(): string {
            return JSON.stringify({ profiles: Object.keys(root._profiles), running: worker.running,
                ready: root._ready, stopping: root._stopping, failed: root._broken,
                pending: root._flight !== null, processId: worker.processId, timing: root._timing });
        }
    }
    Timer {
        id: watchdog
        onTriggered: {
            if (root._stopping) worker.signal(9);
            else root.protocolFailure();
        }
    }
}
