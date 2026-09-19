pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick
import "../Common/ClipboardFormat.js" as Format

Singleton {
    id: root

    readonly property bool ready: true
    property bool open: false
    property string query: ""
    property string filter: "all"
    property int selected: 0
    property var items: []
    property var pins: []
    property var results: []
    property int gen: 0
    property bool searching: false
    property var _searchRows: []
    property bool _catalogReady: false
    property string _selectionKey: ""
    property bool _resetSelection: false
    property string previewImage: ""
    property string previewText: ""
    property string previewKind: ""
    readonly property var current: (selected >= 0 && selected < results.length) ? results[selected] : null
    readonly property bool showPreview: current !== null
    property bool previewLoading: false
    property string previewError: ""
    property string actionError: ""
    property bool copying: false
    property bool _pinsReadOnly: false
    property int _epoch: 0
    property int _copyEpoch: -1
    property var _listJob: null
    property bool _listAgain: false
    property int _thumbnailSlot: 0
    readonly property string pinDirectory: Quickshell.dataDir + "/clipboard-pins"
    property var thumbnails: ({})

    property var _decodeQueue: []
    property var _decodeJob: null
    property var _decodedText: ({})
    property var _decodeErrors: ({})
    property var _pendingPin: null

    readonly property var secretRe: [
        /ghp_[A-Za-z0-9]{20,}/,
        /sk-[A-Za-z0-9]{20,}/,
        /xox[baprs]-/,
        /-----BEGIN [A-Z ]*PRIVATE KEY-----/
    ]

    function isSecret(text) {
        const s = String(text || "");
        for (let i = 0; i < secretRe.length; i++) {
            if (secretRe[i].test(s))
                return true;
        }
        return false;
    }

    function kindOf(preview) {
        return Format.kindOf(preview);
    }

    function toggle(): bool {
        open = !open;
        if (open) {
            actionError = _pinsReadOnly ? "Pinned items could not be loaded. The original file was kept." : "";
            query = "";
            filter = "all";
            selected = 0;
            refresh();
        }
        return open;
    }

    function close(): void {
        open = false;
    }

    function refresh() {
        if (!open) return;
        _listAgain = true;
        startList();
    }

    function startList() {
        if (!open || !_listAgain || _listJob || lister.running) return;
        _listAgain = false;
        _listJob = { epoch: _epoch, output: "", outputDone: false, exited: false, exitCode: 0 };
        lister.running = true;
    }

    function finishList() {
        const job = _listJob;
        if (!job || !job.outputDone || !job.exited) return;
        _listJob = null;
        if (open && job.epoch === _epoch) {
            if (job.exitCode === 0) parseList(job.output);
            else actionError = "Clipboard history could not be loaded.";
        }
        Qt.callLater(root.startList);
    }

    function parseList(text) {
        if (!open) return;
        const lines = String(text || "").split("\n");
        const out = [];
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            if (!line.length)
                continue;
            const tab = line.indexOf("\t");
            if (tab <= 0)
                continue;
            const id = line.slice(0, tab);
            const preview = line.slice(tab + 1);
            if (!/^[0-9]+$/.test(id) || isSecret(preview))
                continue;
            out.push({
                id: id,
                preview: preview,
                kind: kindOf(preview),
                source: "clip",
                line: line
            });
        }
        items = out;
        rebuild();
    }

    function rebuild(resetSelection) {
        if (!open) return;
        _resetSelection = resetSelection === true || _resetSelection;
        _selectionKey = _resetSelection ? "" : rowKey(current);
        if (resetSelection !== true || !_catalogReady) {
            const rows = [], records = [];
            const sources = [pins, items];
            for (let sourceIndex = 0; sourceIndex < sources.length; sourceIndex++) {
                for (const item of sources[sourceIndex]) {
                    if (isSecret(item.preview)) continue;
                    const row = { id: item.id, preview: item.preview, kind: kindOf(item.preview),
                        source: sourceIndex === 0 ? "pin" : "clip", file: item.file || "",
                        line: item.line || "", pinned: sourceIndex === 0 };
                    row.title = Format.title(row);
                    row.subtitle = Format.subtitle(row);
                    row.label = row.title;
                    records.push({ key: String(rows.length), text: row.preview + " " + row.title + " " + row.subtitle,
                        kind: row.kind, pinned: row.pinned });
                    rows.push(row);
                }
            }
            _searchRows = rows;
            _catalogReady = true;
            Search.setCatalog("clipboard", records);
        }
        Search.search("clipboard", { query: query.trim(), filter: filter });
    }

    function acceptSearch(keys) {
        if (!open) return;
        const rows = [];
        for (const key of keys) {
            if (typeof key !== "string" || !/^(0|[1-9][0-9]*)$/.test(key)) { Search.protocolFailure(); return; }
            const index = Number(key);
            if (!Number.isInteger(index) || index < 0 || index >= _searchRows.length) { Search.protocolFailure(); return; }
            rows.push(_searchRows[index]);
        }
        results = rows;
        searching = false;
        const previousIndex = results.findIndex(row => rowKey(row) === _selectionKey);
        if (_resetSelection) selected = 0;
        else if (previousIndex >= 0) selected = previousIndex;
        else if (selected >= results.length) selected = Math.max(0, results.length - 1);
        _resetSelection = false;
        _decodeQueue = _decodeQueue.filter(job => results.some(row => rowKey(row) === job.key));
        const text = {}, errors = {};
        for (const row of results) {
            const key = rowKey(row);
            if (_decodedText[key] !== undefined) text[key] = _decodedText[key];
            if (_decodeErrors[key]) errors[key] = _decodeErrors[key];
        }
        _decodedText = text;
        _decodeErrors = errors;
        gen++;
        loadPreview();
    }

    Connections {
        target: Search
        function onPending(profile) { if (profile === "clipboard") root.searching = true; }
        function onResultsReady(profile, keys) { if (profile === "clipboard") root.acceptSearch(keys); }
        function onFailed(profile, message) {
            if (profile !== "clipboard") return;
            root.searching = true;
            root.results = [];
            root.actionError = message;
        }
    }

    function move(delta) {
        if (!results.length)
            return;
        selected = (selected + delta + results.length) % results.length;
        gen++;
    }

    function rowKey(row) {
        return row ? row.source + ":" + row.id + ":" + row.preview : "";
    }

    function thumbnailFor(row) {
        if (!row || row.kind !== "image")
            return "";
        return thumbnails[rowKey(row)] || "";
    }

    function requestThumbnail(row) {
        if (!open || !row || row.kind !== "image" || thumbnailFor(row).length)
            return;
        queueDecode(row, rowKey(row) === rowKey(current));
    }

    function queueDecode(row, priority) {
        const key = rowKey(row);
        if (!open || _decodeErrors[key] || (_decodeJob && _decodeJob.key === key))
            return;
        const queue = [];
        for (let i = 0; i < _decodeQueue.length; i++) {
            const queued = _decodeQueue[i];
            if (queued.key !== key && (queued.row.kind === "image" || queued.key === rowKey(current)))
                queue.push(queued);
        }
        const job = { key: key, row: row, epoch: _epoch, output: "", outputDone: false, exited: false, exitCode: 0 };
        if (priority)
            queue.unshift(job);
        else
            queue.push(job);
        _decodeQueue = queue;
        decodeNext.restart();
    }

    function startDecode() {
        if (!open || _decodeJob || decoder.running || !_decodeQueue.length)
            return;
        const queue = _decodeQueue.slice();
        const job = queue.shift();
        _decodeQueue = queue;
        _decodeJob = job;
        const row = job.row;
        if (row.kind === "image") {
            const directory = Quickshell.cacheDir + "/clip-preview-slots/";
            if (previewImage === "file://" + directory + _thumbnailSlot)
                _thumbnailSlot = (_thumbnailSlot + 1) % 16;
            const dest = directory + _thumbnailSlot;
            _thumbnailSlot = (_thumbnailSlot + 1) % 16;
            // A slot is reused only after its old image reference is released.
            const next = {};
            for (const key of Object.keys(thumbnails)) {
                if (thumbnails[key] !== "file://" + dest) next[key] = thumbnails[key];
            }
            thumbnails = next;
            decoder.command = contentCommand("image", row, dest);
        } else {
            decoder.command = contentCommand("text", row, "");
        }
        decoder.running = true;
    }

    function finishDecode() {
        const job = _decodeJob;
        if (!job || !job.outputDone || !job.exited)
            return;
        if (!open || job.epoch !== _epoch) {
            _decodeJob = null;
            if (open) decodeNext.restart();
            return;
        }
        let error = "";
        if (job.exitCode !== 0)
            error = "Preview unavailable";
        else if (job.row.kind !== "image" && isSecret(job.output))
            error = "Sensitive content hidden";
        else if (job.row.kind === "image" && !job.output.trim().length)
            error = "Preview unavailable";
        if (error.length) {
            const errors = Object.assign({}, _decodeErrors);
            errors[job.key] = error;
            _decodeErrors = errors;
        } else if (job.row.kind === "image") {
            const next = Object.assign({}, thumbnails);
            next[job.key] = "file://" + job.output.trim();
            thumbnails = next;
        } else if (open) {
            const next = Object.assign({}, _decodedText);
            next[job.key] = job.output;
            _decodedText = next;
        }
        _decodeJob = null;
        if (open && rowKey(current) === job.key)
            loadPreview();
        if (open)
            decodeNext.restart();
    }

    function loadPreview() {
        previewImage = "";
        previewText = "";
        previewKind = "";
        previewLoading = false;
        previewError = "";
        const row = current;
        if (!open || !row)
            return;
        previewKind = row.kind;
        const key = rowKey(row);
        if (_decodeErrors[key]) {
            previewError = _decodeErrors[key];
            return;
        }
        if (row.kind === "color") {
            previewText = row.preview.trim();
            return;
        }
        if (row.kind === "image") {
            previewImage = thumbnailFor(row);
            if (previewImage.length)
                return;
        } else if (_decodedText[key] !== undefined) {
            previewText = _decodedText[key];
            return;
        }
        previewLoading = true;
        queueDecode(row, true);
    }

    function copySelected() {
        if (selected >= 0 && selected < results.length)
            copyRow(results[selected]);
    }

    function contentCommand(action, row, destination) {
        return [Quickshell.shellPath(".local/bin/qs-clipboard"), "content",
            action, row.source, row.id, pinDirectory, destination];
    }

    function copyRow(row) {
        if (!open || searching || !row || results.indexOf(row) < 0 || copier.running || copying) return;
        actionError = "";
        _copyEpoch = _epoch;
        copying = true;
        copier.command = contentCommand("copy", row, "");
        copier.running = true;
    }

    function finishCopy(exitCode) {
        copying = false;
        if (!open || _copyEpoch !== _epoch) return;
        if (exitCode === 0) close();
        else actionError = "Copy failed. Try again.";
    }

    function finishPin(exitCode) {
        if (exitCode === 0 && _pendingPin) {
            pins = [_pendingPin].concat(pins);
            persistPins();
            rebuild();
        }
        if (exitCode !== 0 && open) actionError = "The item could not be pinned.";
        _pendingPin = null;
    }

    function quote(s) {
        return "'" + String(s).replace(/'/g, "'\\''") + "'";
    }

    function deleteSelected() {
        if (searching) return;
        if (selected < 0 || selected >= results.length)
            return;
        const row = results[selected];
        if (row.source === "pin") {
            if (_pinsReadOnly || !Format.validPin(row, pinDirectory)) return;
            const next = [];
            for (let i = 0; i < pins.length; i++) {
                if (pins[i].id !== row.id)
                    next.push(pins[i]);
            }
            pins = next;
            persistPins();
            Quickshell.execDetached(["gio", "trash", row.file]);
            rebuild();
            return;
        }
        deleter.command = ["sh", "-c", "printf '%s\\n' " + quote(row.id) + " | cliphist delete"];
        deleter.running = false;
        deleter.running = true;
    }

    function togglePinSelected() {
        if (searching) return;
        if (selected < 0 || selected >= results.length)
            return;
        const row = results[selected];
        if (row.source === "pin") {
            deleteSelected();
            return;
        }
        if (isSecret(row.preview))
            return;
        if (pinner.running || _pinsReadOnly) return;
        if (pins.length >= 100) {
            actionError = "Unpin an item before adding another pin.";
            return;
        }
        const pid = "p_" + Date.now() + "_" + Math.floor(Math.random() * 1000000);
        const file = pinDirectory + "/" + pid;
        _pendingPin = {
            id: pid,
            preview: row.preview,
            kind: row.kind,
            file: file
        };
        pinner.command = contentCommand("pin", row, pid);
        pinner.running = true;
    }

    function persistPins() {
        if (!_pinsReadOnly) pinsFile.setText(JSON.stringify(pins));
    }

    function loadPins() {
        try {
            const raw = pinsFile.text();
            if (raw && raw.length)
                pins = Format.validatePins(JSON.parse(raw), pinDirectory);
        } catch (e) {
            pins = [];
            _pinsReadOnly = true;
            actionError = "Pinned items could not be loaded. The original file was kept.";
        }
    }

    IpcHandler {
        target: "clipboard"

        function toggle(): bool {
            return root.toggle();
        }

        function close(): void {
            root.close();
        }
        function search(text: string): void { if (!root.open) root.toggle(); root.query = text; }
        function status(): string {
            return JSON.stringify({ open: root.open, searching: root.searching, count: root.results.length,
                error: root.actionError, decoding: root.previewLoading });
        }
    }

    Process {
        id: lister
        command: ["cliphist", "list"]
        stdout: StdioCollector {
            onStreamFinished: {
                if (!root._listJob) return;
                root._listJob.output = text;
                root._listJob.outputDone = true;
                root.finishList();
            }
        }
        onExited: (exitCode, exitStatus) => {
            if (!root._listJob) return;
            root._listJob.exitCode = exitCode;
            root._listJob.exited = true;
            root.finishList();
        }
        // FailedToStart has no exited/streamFinished signals. Normal exits set
        // job.exited before runningChanged, even if output completion is later.
        onRunningChanged: {
            const job = root._listJob;
            if (running || !job || job.exited) return;
            job.outputDone = true;
            job.exited = true;
            job.exitCode = -1;
            root.finishList();
        }
    }

    Process {
        id: copier
        command: ["true"]
        onExited: (exitCode, exitStatus) => root.finishCopy(exitStatus === 0 ? exitCode : -1)
        onRunningChanged: { if (!running && root.copying) root.finishCopy(-1); }
    }

    Process {
        id: deleter
        command: ["true"]
        onExited: root.refresh()
    }

    Process {
        id: pinner
        command: ["true"]
        onExited: (exitCode, exitStatus) => root.finishPin(exitStatus === 0 ? exitCode : -1)
        onRunningChanged: { if (!running && root._pendingPin) root.finishPin(-1); }
    }

    Process {
        id: decoder
        command: ["true"]
        stdout: StdioCollector {
            onStreamFinished: {
                if (!root._decodeJob)
                    return;
                root._decodeJob.output = text;
                root._decodeJob.outputDone = true;
                root.finishDecode();
            }
        }
        onExited: (exitCode, exitStatus) => {
            if (!root._decodeJob)
                return;
            root._decodeJob.exitCode = exitCode;
            root._decodeJob.exited = true;
            root.finishDecode();
        }
        onRunningChanged: {
            const job = root._decodeJob;
            if (running || !job || job.exited) return;
            job.outputDone = true;
            job.exited = true;
            job.exitCode = -1;
            root.finishDecode();
        }
    }

    Timer {
        id: decodeNext
        interval: 1
        onTriggered: root.startDecode()
    }

    FileView {
        id: pinsFile
        path: `${Quickshell.dataDir}/clipboard-pins.json`
        blockLoading: true
        printErrors: false
        Component.onCompleted: root.loadPins()
    }

    onQueryChanged: {
        selected = 0;
        if (open)
            rebuild(true);
    }

    onFilterChanged: {
        if (["all", "text", "image", "link", "pinned"].indexOf(filter) === -1) {
            filter = "all";
            return;
        }
        selected = 0;
        if (open)
            rebuild(true);
    }

    onOpenChanged: {
        _epoch++;
        if (!open) {
            Search.release("clipboard");
            _searchRows = [];
            _catalogReady = false;
            _resetSelection = false;
            _selectionKey = "";
            searching = false;
            decodeNext.stop();
            _listAgain = false;
            items = [];
            results = [];
            thumbnails = ({});
            _decodeQueue = [];
            _decodedText = ({});
            _decodeErrors = ({});
            loadPreview();
        }
    }

    onCurrentChanged: {
        if (open)
            loadPreview();
    }
}
