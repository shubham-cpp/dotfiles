pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick
import qs.Common

Singleton {
    id: root

    readonly property bool ready: true
    property bool open: false
    property string query: ""
    property int selected: 0
    property var results: []
    property var stats: ({})
    property var pins: []
    property bool _statsReadOnly: false
    property bool _pinsReadOnly: false
    property int gen: 0
    property bool searching: false
    property string searchError: ""
    property var _entries: []
    property real lastSearchMs: 0
    property real _searchStarted: 0

    function toggle(): bool {
        if (open) {
            close();
            return false;
        }
        query = "";
        selected = 0;
        open = true;
        return true;
    }

    function close(): void {
        open = false;
    }

    function isPinned(id) {
        return pins.indexOf(id) !== -1;
    }

    function togglePin(id) {
        if (searching) return;
        const next = pins.slice();
        const i = next.indexOf(id);
        if (i === -1)
            next.push(id);
        else
            next.splice(i, 1);
        pins = next;
        persistPins();
        refreshCatalog();
    }

    function recordLaunch(id) {
        const next = Object.assign(Object.create(null), stats);
        const cur = next[id] || { count: 0, last: 0 };
        next[id] = { count: Math.min(Number.MAX_SAFE_INTEGER, (cur.count || 0) + 1), last: Math.floor(Date.now() / 1000) };
        stats = next;
        persistStatsSoon();
    }

    function launch(row) {
        if (!open || searching || !row || !row.entry || results.indexOf(row) < 0
                || DesktopEntries.applications.values.indexOf(row.entry) < 0)
            return;
        row.entry.execute();
        recordLaunch(row.id);
        close();
    }

    function launchSelected() {
        if (selected >= 0 && selected < results.length)
            launch(results[selected]);
    }

    function move(delta) {
        if (!results.length)
            return;
        selected = (selected + delta + results.length) % results.length;
        gen++;
    }

    function refreshCatalog() {
        if (!open) return;
        const apps = DesktopEntries.applications.values;
        const rows = [];
        for (let i = 0; i < apps.length; i++) {
            const e = apps[i];
            if (!e || e.noDisplay)
                continue;
            if (!e.command || e.command.length === 0)
                continue;
            const id = e.id || e.name;
            const pinned = isPinned(id);
            rows.push({
                entry: e,
                id: id,
                name: e.name,
                comment: e.comment || e.genericName || "",
                icon: e.icon,
                pinned: pinned
            });
        }
        _entries = rows;
        const order = rows.map((row, index) => index).sort((a, b) => rows[a].name.localeCompare(rows[b].name));
        const ties = {};
        order.forEach((index, tie) => ties[index] = tie);
        Search.setCatalog("launcher", rows.map((row, index) => {
            const e = row.entry, usage = stats[row.id] || {};
            return { key: String(index), id: e.id || "", name: e.name || "", genericName: e.genericName || "",
                comment: e.comment || "", keywords: e.keywords || [], pinned: row.pinned,
                count: usage.count || 0, last: usage.last || 0, tie: ties[index] };
        }));
        rebuild();
    }

    function rebuild() {
        if (!open) return;
        _searchStarted = Date.now();
        Search.search("launcher", { query: query, now: Date.now() / 1000 });
    }

    function acceptSearch(keys) {
        if (!open) return;
        const rows = [];
        for (const key of keys) {
            if (typeof key !== "string" || !/^(0|[1-9][0-9]*)$/.test(key)) { Search.protocolFailure(); return; }
            const index = Number(key);
            if (!Number.isInteger(index) || index < 0 || index >= _entries.length) { Search.protocolFailure(); return; }
            rows.push(_entries[index]);
        }
        results = rows;
        searching = false;
        searchError = "";
        lastSearchMs = Date.now() - _searchStarted;
        if (selected >= results.length) selected = Math.max(0, results.length - 1);
        gen++;
    }

    Connections {
        target: Search
        function onPending(profile) { if (profile === "launcher") root.searching = true; }
        function onResultsReady(profile, keys) { if (profile === "launcher") root.acceptSearch(keys); }
        function onFailed(profile, message) {
            if (profile !== "launcher") return;
            root.searching = true;
            root.results = [];
            root.searchError = message;
        }
    }

    Connections {
        target: DesktopEntries.applications
        function onValuesChanged() { root.refreshCatalog(); }
    }

    onOpenChanged: {
        if (open) refreshCatalog();
        else {
            Search.release("launcher");
            _entries = [];
            results = [];
            searching = false;
            searchError = "";
        }
    }

    function persistStatsSoon() {
        statsTimer.restart();
    }

    function persistStats() {
        if (!_statsReadOnly) statsFile.setText(JSON.stringify(stats));
    }

    function persistPins() {
        if (!_pinsReadOnly) pinsFile.setText(JSON.stringify(pins));
    }

    function loadStats() {
        try {
            const raw = statsFile.text();
            if (raw.length > 4000000) throw new Error("Usage file exceeds limit");
            const loaded = raw.length ? JSON.parse(raw) : {};
            if (!loaded || typeof loaded !== "object" || Array.isArray(loaded))
                throw new Error("Invalid usage file");
            const ids = Object.keys(loaded);
            if (ids.length > 10000) throw new Error("Too many usage records");
            const now = Date.now() / 1000;
            const kept = Object.create(null);
            for (const id of ids) {
                const usage = loaded[id];
                if (!id.length || id.length > 4096 || !usage || typeof usage !== "object" || Array.isArray(usage))
                    continue;
                const count = usage.count === undefined ? 0 : usage.count;
                const last = usage.last === undefined ? 0 : usage.last;
                if (!Number.isSafeInteger(count) || count < 0 || !Number.isFinite(last) || last < 0 || last > Number.MAX_SAFE_INTEGER)
                    continue;
                if (now - last < 90 * 86400 || count >= 3)
                    kept[id] = { count: count, last: last };
            }
            stats = kept;
        } catch (e) {
            stats = ({});
            _statsReadOnly = true;
        }
    }

    function loadPins() {
        try {
            const raw = pinsFile.text();
            if (raw.length > 4000000) throw new Error("Pins file exceeds limit");
            const loaded = raw.length ? JSON.parse(raw) : [];
            if (!Array.isArray(loaded) || loaded.length > 10000)
                throw new Error("Invalid pins file");
            pins = [...new Set(loaded.filter(id => typeof id === "string" && id.length > 0 && id.length <= 4096))];
        } catch (e) {
            pins = [];
            _pinsReadOnly = true;
        }
    }

    IpcHandler {
        target: "launcher"

        function toggle(): bool {
            return root.toggle();
        }

        function close(): void {
            root.close();
        }

        function search(text: string): void {
            if (!root.open) root.toggle();
            root.query = text;
        }

        function status(): string {
            return JSON.stringify({ open: root.open, searching: root.searching, count: root.results.length,
                error: root.searchError, searchMs: root.lastSearchMs,
                selected: root.results[root.selected] ? root.results[root.selected].id : "" });
        }
    }

    FileView {
        id: statsFile
        path: `${Quickshell.dataDir}/launcher-stats.json`
        blockLoading: true
        printErrors: false
        Component.onCompleted: root.loadStats()
    }

    FileView {
        id: pinsFile
        path: `${Quickshell.dataDir}/launcher-pins.json`
        blockLoading: true
        printErrors: false
        Component.onCompleted: root.loadPins()
    }

    Timer {
        id: statsTimer
        interval: 750
        repeat: false
        onTriggered: root.persistStats()
    }

    onQueryChanged: {
        selected = 0;
        if (open)
            rebuild();
    }
}
