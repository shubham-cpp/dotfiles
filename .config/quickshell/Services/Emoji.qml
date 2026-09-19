pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick
import qs.Services
import "../Common/EmojiCatalog.js" as Catalog

Singleton {
    id: root

    readonly property bool ready: true
    property bool open: false
    property string query: ""
    property string category: "all"
    property int selected: 0
    property var results: []
    property var catalog: null
    property var preferences: Catalog.emptyState()
    property string screenName: ""
    property bool loading: false
    property string error: ""
    property string saveError: ""
    property bool variantOpen: false
    property int uiCount: 0
    property int chooserCount: 0
    property real lastSearchMs: 0
    property real _publishMs: 0
    property real _validationMs: 0
    property real _modelMs: 0
    property bool searching: false
    property bool _searchInstalled: false
    property real _searchStarted: 0
    property var _chosenVariants: ({})
    readonly property var current: catalog && results[selected] ? catalog.entries[results[selected]] : null
    readonly property bool unavailable: Lock.locked || Lock.unlockInProgress || Logind.preparingForSleep
    property bool _stateRead: false
    property bool _stateReady: false
    property var _rawState: null
    property bool _readOnly: false
    property bool _saving: false
    property bool _dirty: false
    property bool _copying: false
    property int _epoch: 0
    property int _cursorEpoch: 0
    property string _corrupt: ""

    function toggle(): bool {
        if (open) {
            close();
            return false;
        }
        if (unavailable)
            return false;
        LauncherStats.close();
        Clipboard.close();
        Files.close();
        Agenda.close();
        Network.close();
        Power.close();
        Audio.close();
        Brightness.close();
        Resources.close();
        Notifications.centerOpen = false;
        _epoch++;
        query = "";
        category = preferences.recents.length ? "recent" : "all";
        selected = 0;
        screenName = "";
        open = true;
        _copying = false;
        _cursorEpoch = _epoch;
        cursor.running = true;
        load();
        return true;
    }

    function close(): void {
        open = false;
        Search.release("emoji");
        _searchInstalled = false;
        searching = false;
        // This bounded, public display index is reusable across openings.
        // Go owns search data and exits when the final search profile closes.
        _chosenVariants = ({});
        loading = false;
        _epoch++;
        cursor.running = false;
        results = [];
        selected = 0;
        variantOpen = false;
        query = "";
        screenName = "";
    }

    function load() {
        error = "";
        if (!catalog) {
            loading = true;
            catalogFile.path = Quickshell.shellPath(".local/emoji-display.json");
        }
        if (!_stateRead && !stateFile.path)
            stateFile.path = Quickshell.dataPath("emoji-state.json");
        finishLoad();
    }

    function finishLoad() {
        if (!catalog || !_stateRead)
            return;
        if (!_stateReady) {
            try {
                preferences = Catalog.sanitizeState(_rawState || Catalog.emptyState(), catalog, Date.now());
            } catch (e) {
                preferences = Catalog.emptyState();
                _readOnly = true;
                saveError = "Preferences use an unsupported format; changes will not be saved.";
            }
            _rawState = null;
            _stateReady = true;
            if (open)
                category = preferences.recents.length ? "recent" : "all";
        }
        loading = false;
        if (open)
            rebuild();
    }

    function rebuild() {
        if (!open || !catalog || !_stateReady)
            return;
        if (!_searchInstalled) {
            _searchInstalled = true;
            Search.setCatalog("emoji", []);
        }
        _searchStarted = Date.now();
        Search.search("emoji", {
            query: query,
            category: category,
            preferences: preferences
        });
    }

    function acceptSearch(keys) {
        const started = Date.now();
        if (!open || !catalog)
            return;
        const entries = catalog.entries, choices = _chosenVariants;
        const hasChoices = Object.keys(choices).length > 0;
        const next = hasChoices ? [] : keys;
        for (const key of keys) {
            const entry = entries[key];
            if (!entry) {
                Search.protocolFailure();
                return;
            }
            if (hasChoices)
                next.push(choices[entry.familyId] || key);
        }
        const validated = Date.now();
        results = next;
        const published = Date.now();
        searching = false;
        selected = Math.min(selected, Math.max(0, results.length - 1));
        lastSearchMs = Date.now() - _searchStarted;
        _publishMs = Date.now() - started;
        _validationMs = validated - started;
        _modelMs = published - validated;
    }

    Connections {
        target: Search
        function onPending(profile) {
            if (profile === "emoji")
                root.searching = true;
        }
        function onResultsReady(profile, keys) {
            if (profile === "emoji")
                root.acceptSearch(keys);
        }
        function onFailed(profile, message) {
            if (profile !== "emoji")
                return;
            root.searching = true;
            root.results = [];
            root.error = message;
        }
    }

    function move(delta) {
        selected = Math.max(0, Math.min(results.length - 1, selected + delta));
    }

    function setTone(tone) {
        if (!_stateReady || !Number.isInteger(tone) || tone < 0 || tone > 5)
            return;
        _chosenVariants = ({});
        preferences = {
            schema: 1,
            tone: tone,
            overrides: preferences.overrides,
            recents: preferences.recents
        };
        persist();
        rebuild();
    }

    function setVariant(familyId, id) {
        const family = catalog ? catalog.families[familyId] : null;
        if (searching || !_stateReady || !family || (id && !family.variants.includes(id)))
            return;
        const overrides = Object.assign({}, preferences.overrides);
        if (id)
            overrides[familyId] = id;
        else
            delete overrides[familyId];
        preferences = {
            schema: 1,
            tone: preferences.tone,
            overrides: overrides,
            recents: preferences.recents
        };
        persist();
        // Keep the current query/recents stable, but visibly apply this choice.
        const next = results.slice();
        const resolved = id || Catalog.resolve(catalog, familyId, preferences);
        _chosenVariants[familyId] = resolved;
        Search.updateReplay("emoji", {
            preferences: preferences
        });
        if (current && current.familyId === familyId)
            next[selected] = resolved;
        results = next;
    }

    function copyId(id) {
        if (!open || unavailable || loading || searching || !_stateReady || _copying || !catalog || !catalog.entries[id])
            return;
        _copying = true;
        Quickshell.clipboardText = catalog.entries[id].text;
        preferences = Catalog.recentState(preferences, id, Date.now());
        persist();
        close();
    }

    function persist() {
        if (!_stateReady || _readOnly)
            return;
        _dirty = true;
        if (_corrupt.length) {
            if (!recoveryFile.path) {
                recoveryFile.path = Quickshell.dataPath("emoji-state-recovery-" + Date.now() + ".json");
                recoveryFile.setText(_corrupt);
            }
            return;
        }
        saveNext();
    }

    function saveNext() {
        if (!_dirty || _saving || _readOnly)
            return;
        _dirty = false;
        _saving = true;
        stateFile.setText(JSON.stringify(preferences));
    }

    onQueryChanged: {
        _chosenVariants = ({});
        selected = 0;
        if (open)
            rebuild();
    }
    onCategoryChanged: {
        _chosenVariants = ({});
        selected = 0;
        if (open)
            rebuild();
    }
    onUnavailableChanged: {
        if (unavailable)
            close();
    }

    Connections {
        target: LauncherStats
        function onOpenChanged() {
            if (LauncherStats.open)
                root.close();
        }
    }
    Connections {
        target: Clipboard
        function onOpenChanged() {
            if (Clipboard.open)
                root.close();
        }
    }
    Connections {
        target: Agenda
        function onOpenChanged() {
            if (Agenda.open)
                root.close();
        }
    }
    Connections {
        target: Power
        function onOpenChanged() {
            if (Power.open)
                root.close();
        }
    }
    Connections {
        target: Network
        function onOpenChanged() {
            if (Network.open)
                root.close();
        }
    }
    Connections {
        target: Notifications
        function onCenterOpenChanged() {
            if (Notifications.centerOpen)
                root.close();
        }
    }
    Connections {
        target: Audio
        function onOpenChanged() {
            if (Audio.open)
                root.close();
        }
    }
    Connections {
        target: Resources
        function onOpenChanged() {
            if (Resources.open)
                root.close();
        }
    }
    Connections {
        target: Quickshell
        function onScreensChanged() {
            if (root.open && (!Quickshell.screens.length || (root.screenName && !Quickshell.screens.some(screen => screen.name === root.screenName))))
                root.close();
        }
    }

    FileView {
        id: catalogFile
        printErrors: false
        onLoaded: {
            if (!root.open) {
                path = "";
                return;
            }
            try {
                root.catalog = Catalog.index(JSON.parse(text()));
                root.finishLoad();
            } catch (e) {
                root.error = "Could not load the emoji catalog. " + e.message;
                root.loading = false;
            }
            path = "";
        }
        onLoadFailed: {
            if (!root.open) {
                path = "";
                return;
            }
            root.error = "Emoji catalog is unavailable. Retry or close the picker.";
            root.loading = false;
            path = "";
        }
    }

    FileView {
        id: stateFile
        printErrors: false
        onLoaded: {
            if (root._stateRead)
                return;
            try {
                root._rawState = JSON.parse(text());
            } catch (e) {
                root._corrupt = text();
                root.saveError = "Unreadable preferences were reset; a recovery copy will be kept.";
            }
            root._stateRead = true;
            root.finishLoad();
        }
        onLoadFailed: error => {
            if (root._stateRead)
                return;
            if (error !== FileViewError.FileNotFound) {
                root._readOnly = true;
                root.saveError = "Cannot read emoji preferences; changes will not be saved.";
            }
            root._stateRead = true;
            root.finishLoad();
        }
        onSaved: {
            root._saving = false;
            root.saveError = "";
            root.saveNext();
        }
        onSaveFailed: {
            root._saving = false;
            root.saveError = "Could not save emoji preferences. Changes are kept for this session.";
        }
    }

    FileView {
        id: recoveryFile
        printErrors: false
        onSaved: {
            root._corrupt = "";
            root.saveNext();
            path = "";
        }
        onSaveFailed: {
            root._readOnly = true;
            root._dirty = false;
            root.saveError = "Could not preserve old preferences; changes will not be saved.";
        }
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

    IpcHandler {
        target: "emoji"
        function toggle(): bool {
            return root.toggle();
        }
        function close(): void {
            root.close();
        }
        function search(text: string): void {
            if (!root.open)
                root.toggle();
            root.category = "all";
            root.query = text;
        }
        function status(): string {
            return JSON.stringify({
                open: root.open,
                loading: root.loading,
                searching: root.searching,
                error: root.error,
                query: root.query,
                category: root.category,
                saveError: root.saveError,
                count: root.results.length,
                selected: root.current ? root.current.id : "",
                variantOpen: root.variantOpen,
                uiCount: root.uiCount,
                chooserCount: root.chooserCount,
                searchMs: root.lastSearchMs,
                publishMs: root._publishMs,
                validationMs: root._validationMs,
                modelMs: root._modelMs,
                saving: root._saving,
                cursorRunning: cursor.running,
                catalogVersion: root.catalog ? root.catalog.version : ""
            });
        }
    }
}
