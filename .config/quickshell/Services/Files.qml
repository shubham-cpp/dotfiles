pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick
import "../Common/FilePath.js" as FilePath

Singleton {
    id: root

    readonly property bool ready: true
    readonly property bool unavailable: Lock.locked || Lock.unlockInProgress || Logind.preparingForSleep
    property bool open: false
    property string query: ""
    property int selected: 0
    property var results: []
    property var stats: ({})
    property bool _statsReadOnly: false
    property int gen: 0
    property bool searching: false
    property string searchError: ""
    property real lastSearchMs: 0
    property real _searchStarted: 0
    property string screenName: ""
    property int _epoch: 0
    property int _cursorEpoch: 0

    function toggle(): bool {
        if (open) {
            close();
            return false;
        }
        if (unavailable)
            return false;
        closePeers();
        _epoch++;
        query = "";
        selected = 0;
        searchError = "";
        screenName = "";
        open = true;
        _cursorEpoch = _epoch;
        cursor.running = true;
        return true;
    }

    function close(): void {
        open = false;
        _epoch++;
        cursor.running = false;
        screenName = "";
    }

    function closePeers() {
        LauncherStats.close();
        Clipboard.close();
        Emoji.close();
        Agenda.close();
        Network.close();
        Power.close();
        Audio.close();
        Brightness.close();
        Resources.close();
        Notifications.centerOpen = false;
    }

    function recordOpen(path) {
        const next = Object.assign(Object.create(null), stats);
        const cur = next[path] || {
            count: 0,
            last: 0
        };
        next[path] = {
            count: Math.min(Number.MAX_SAFE_INTEGER, (cur.count || 0) + 1),
            last: Math.floor(Date.now() / 1000)
        };
        stats = next;
        persistSoon();
    }

    function recentRows() {
        let xbel = "";
        try {
            xbel = recentFile.text() || "";
        } catch (e) {
            xbel = "";
        }
        return FilePath.emptyRows(stats, xbel, Quickshell.env("HOME") || "", 50);
    }

    function openRow(row) {
        if (!open || !row)
            return;
        const path = row.path;
        if (!path || path.charAt(0) !== "/" || path.indexOf("\0") !== -1 || path.length > 4096)
            return;
        if (!results.some(item => item && item.path === path))
            return;
        Quickshell.execDetached(["xdg-open", path]);
        recordOpen(path);
        close();
    }

    function openSelected() {
        if (selected >= 0 && selected < results.length)
            openRow(results[selected]);
    }

    function move(delta) {
        if (!results.length)
            return;
        selected = (selected + delta + results.length) % results.length;
        gen++;
    }

    function rebuild() {
        if (!open)
            return;
        _searchStarted = Date.now();
        Search.search("files", {
            query: query
        });
    }

    function acceptSearch(keys) {
        if (!open)
            return;
        if (!String(query).trim()) {
            results = recentRows();
        } else {
            const home = Quickshell.env("HOME") || "";
            const rows = [];
            for (const path of keys) {
                if (typeof path !== "string" || path.charAt(0) !== "/" || path.length > 4096 || path.indexOf("\0") !== -1) {
                    Search.protocolFailure();
                    return;
                }
                rows.push(FilePath.fromPath(path, home));
            }
            results = rows;
        }
        searching = false;
        searchError = "";
        lastSearchMs = Date.now() - _searchStarted;
        if (selected >= results.length)
            selected = Math.max(0, results.length - 1);
        gen++;
    }

    function loadStats() {
        try {
            const raw = statsFile.text();
            if (raw.length > 4000000)
                throw new Error("Usage file exceeds limit");
            const loaded = raw.length ? JSON.parse(raw) : {};
            if (!loaded || typeof loaded !== "object" || Array.isArray(loaded))
                throw new Error("Invalid usage file");
            const ids = Object.keys(loaded);
            if (ids.length > 10000)
                throw new Error("Too many usage records");
            const now = Date.now() / 1000;
            const kept = Object.create(null);
            for (const id of ids) {
                const usage = loaded[id];
                if (!id.length || id.length > 4096 || id.charAt(0) !== "/" || !usage || typeof usage !== "object" || Array.isArray(usage))
                    continue;
                const count = usage.count === undefined ? 0 : usage.count;
                const last = usage.last === undefined ? 0 : usage.last;
                if (!Number.isSafeInteger(count) || count < 0 || !Number.isFinite(last) || last < 0 || last > Number.MAX_SAFE_INTEGER)
                    continue;
                if (now - last < 90 * 86400 || count >= 3)
                    kept[id] = {
                        count: count,
                        last: last
                    };
            }
            const ordered = Object.keys(kept).sort((a, b) => kept[b].last - kept[a].last);
            if (ordered.length > 2000) {
                const trimmed = Object.create(null);
                for (let i = 0; i < 2000; i++)
                    trimmed[ordered[i]] = kept[ordered[i]];
                stats = trimmed;
            } else {
                stats = kept;
            }
        } catch (e) {
            stats = ({});
            _statsReadOnly = true;
        }
    }

    function persistSoon() {
        statsTimer.restart();
    }

    function persistStats() {
        if (!_statsReadOnly)
            statsFile.setText(JSON.stringify(stats));
    }

    Connections {
        target: Search
        function onPending(profile) {
            if (profile === "files" && String(root.query).trim())
                root.searching = true;
        }
        function onResultsReady(profile, keys) {
            if (profile === "files")
                root.acceptSearch(keys);
        }
        function onFailed(profile, message) {
            if (profile !== "files")
                return;
            root.searching = false;
            root.searchError = message;
            root.results = String(root.query).trim() ? [] : root.recentRows();
        }
    }

    Connections {
        target: Lock
        function onLockedChanged() {
            if (Lock.locked)
                root.close();
        }
    }

    Connections {
        target: LauncherStats
        function onOpenChanged() {
            if (LauncherStats.open && root.open)
                root.close();
        }
    }

    Connections {
        target: Clipboard
        function onOpenChanged() {
            if (Clipboard.open && root.open)
                root.close();
        }
    }

    onOpenChanged: {
        if (open) {
            try {
                recentFile.reload();
            } catch (e) {}
            results = recentRows();
            Search.setCatalog("files", []);
            rebuild();
        } else {
            Search.release("files");
            results = [];
            searching = false;
            searchError = "";
        }
    }

    onQueryChanged: {
        selected = 0;
        if (open)
            rebuild();
    }

    FileView {
        id: statsFile
        path: `${Quickshell.dataDir}/files-stats.json`
        blockLoading: true
        printErrors: false
        Component.onCompleted: root.loadStats()
    }

    FileView {
        id: recentFile
        path: `${Quickshell.env("HOME")}/.local/share/recently-used.xbel`
        blockLoading: true
        printErrors: false
    }

    Process {
        id: cursor
        command: ["mmsg", "get", "cursorpos"]
        stdout: StdioCollector {
            onStreamFinished: {
                if (!root.open || root._cursorEpoch !== root._epoch)
                    return;
                try {
                    root.screenName = JSON.parse(text).monitor || "";
                } catch (e) {}
            }
        }
    }

    Timer {
        id: statsTimer
        interval: 750
        repeat: false
        onTriggered: root.persistStats()
    }

    IpcHandler {
        target: "files"

        function toggle(): bool {
            return root.toggle();
        }

        function close(): void {
            root.close();
        }

        function search(text: string): void {
            if (!root.open)
                root.toggle();
            root.query = text;
        }

        function status(): string {
            return JSON.stringify({
                open: root.open,
                searching: root.searching,
                count: root.results.length,
                error: root.searchError,
                searchMs: root.lastSearchMs,
                selected: root.results[root.selected] ? root.results[root.selected].path : ""
            });
        }
    }
}
