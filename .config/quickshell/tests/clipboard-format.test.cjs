const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const common = path.join(__dirname, "../Common/ClipboardFormat.js");
const format = vm.createContext({});
vm.runInContext(fs.readFileSync(common, "utf8").replace(/^\.pragma library\s*/, ""), format);

function service() {
    const source = fs.readFileSync(path.join(__dirname, "../Services/Clipboard.qml"), "utf8");
    const context = vm.createContext({
        Format: format,
        searching: false, _searchRows: [], _catalogReady: false, _selectionKey: "", _resetSelection: false,
        Quickshell: { cacheDir: "/tmp/clipboard-test", shellPath: x => x },
        pinDirectory: "/tmp/clipboard-pins", _epoch: 1, _thumbnailSlot: 0,
        _copyEpoch: -1, copying: false, copier: {running: false, command: []},
        _listJob: null, _listAgain: false, lister: {running: false},
        Qt: {callLater() {}}, actionError: "",
        decoder: { running: false, command: [] },
        decodeNext: { restart() {}, stop() {} },
        open: true, query: "", filter: "all", selected: 0, pins: [], items: [], results: [], gen: 0,
        previewImage: "", previewText: "", previewKind: "", previewLoading: false, previewError: "",
        thumbnails: {}, _decodeQueue: [], _decodeJob: null, _decodedText: {}, _decodeErrors: {},
        secretRe: [/ghp_[A-Za-z0-9]{20,}/]
    });
    context.root = context;
    context.Search = require("./search-fixture.cjs").adapter(context);
    Object.defineProperty(context, "current", { get: () => context.results[context.selected] || null });
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm)) {
        vm.runInContext(match[0].replace(/\): (bool|void) \{/, ") {"), context);
    }
    return context;
}

function row(id, preview) {
    return { id, preview, kind: format.kindOf(preview), source: "clip" };
}

function finish(context, output, code = 0) {
    context._decodeJob.output = output;
    context._decodeJob.outputDone = true;
    context._decodeJob.exited = true;
    context._decodeJob.exitCode = code;
    context.decoder.running = false;
    context.finishDecode();
}

test("image labels use cliphist metadata and omit unavailable metadata", () => {
    const image = row("1", "[[ binary data 53 KiB png 482x654 ]]");
    assert.equal(format.title(image), "Image · 482 × 654");
    assert.equal(format.subtitle(image), "PNG · 53 KiB");
    assert.equal(format.title(row("2", "[[ binary data ]]")), "Image");
});

test("links, paths, colors and multiline text receive useful labels", () => {
    assert.equal(format.title(row("1", "https://www.example.com/docs?q=ui")), "example.com");
    assert.equal(format.subtitle(row("1", "https://www.example.com/docs?q=ui")), "/docs?q=ui");
    assert.equal(format.title(row("2", "/home/user/Documents/notes.txt")), "notes.txt");
    assert.equal(format.title(row("3", "First line\nSecond line")), "First line");
    assert.equal(format.subtitle(row("3", "First line\nSecond line")), "Second line");
    assert.equal(format.matchesFilter(row("4", "#ff6363"), "text"), true);
    assert.equal(format.matchesFilter(row("5", "https://example.com"), "text"), false);
    assert.equal(format.matchesFilter({ source: "pin", preview: "hello" }, "pinned"), true);
});

test("filtering happens before the 40-result cap", () => {
    const context = service();
    context.filter = "text";
    context.items = Array.from({ length: 45 }, (_, i) => row(String(i), "[[ binary data 1 KiB png 20x20 ]]"));
    context.items.push(row("100", "A short message"));
    context.rebuild();
    assert.equal(context.results.length, 1);
    assert.equal(context.results[0].title, "A short message");
    assert.equal(context.previewLoading, true);
});

test("rapid navigation cannot publish an old image as a text preview", () => {
    const context = service();
    context.results = [row("1", "[[ binary data 1 KiB png 20x20 ]]"), row("2", "hello")];
    context.loadPreview();
    context.startDecode();
    context.selected = 1;
    context.loadPreview();
    finish(context, "/tmp/clipboard-test/clip-thumbs/1");
    assert.equal(context.previewImage, "");
    assert.equal(context.previewText, "");
    assert.equal(context.previewLoading, true);
    assert.equal(context.thumbnailFor(context.results[0]), "file:///tmp/clipboard-test/clip-thumbs/1");
    context.startDecode();
    finish(context, "hello");
    assert.equal(context.previewText, "hello");
    assert.equal(context.previewLoading, false);
});

test("refresh preserves selection identity while a new filter resets it", () => {
    const context = service();
    context.items = [row("1", "first"), row("2", "second")];
    context.rebuild();
    context.selected = 1;
    context.items.unshift(row("3", "newest"));
    context.rebuild();
    assert.equal(context.current.id, "2");
    context.rebuild(true);
    assert.equal(context.current.id, "3");
});

test("completion waits for both exit status and stdout in either order", () => {
    for (const outputFirst of [true, false]) {
        const context = service();
        context.results = [row("1", "short")];
        context.loadPreview();
        context.startDecode();
        context._decodeJob.output = "short";
        context._decodeJob.outputDone = outputFirst;
        context._decodeJob.exited = !outputFirst;
        context.finishDecode();
        assert.equal(context.previewLoading, true);
        finish(context, "short");
        assert.equal(context.previewText, "short");
    }
});

test("selected images take priority and thumbnail jobs are deduplicated", () => {
    const context = service();
    context.results = [row("1", "[[ binary data 1 KiB png 20x20 ]]"), row("2", "[[ binary data 2 KiB png 30x30 ]]")];
    context.requestThumbnail(context.results[0]);
    context.requestThumbnail(context.results[0]);
    context.requestThumbnail(context.results[1]);
    context.selected = 1;
    context.loadPreview();
    assert.equal(context._decodeQueue.length, 2);
    context.startDecode();
    assert.equal(context._decodeJob.row.id, "2");
});

test("closed panels and sensitive or failed decodes do not expose preview text", () => {
    for (const [output, code, closed] of [["private", 0, true], ["ghp_abcdefghijklmnopqrstuvwxyz", 0, false], ["", 1, false]]) {
        const context = service();
        context.results = [row("1", "short")];
        context.loadPreview();
        context.startDecode();
        context.open = !closed;
        finish(context, output, code);
        assert.equal(context.previewText, "");
        if (!closed)
            assert.ok(context.previewError.length > 0);
    }
});


test("late decoder and list output cannot populate a reopened picker", () => {
    const c = service();
    c.results = [row("1", "[[ binary data 1 KiB png 20x20 ]]")];
    c.loadPreview(); c.startDecode();
    c._epoch++;
    c.results = []; c.items = [];
    finish(c, "/tmp/clipboard-test/clip-preview-slots/0");
    assert.equal(Object.keys(c.thumbnails).length, 0);
    c._listJob = {epoch: c._epoch - 1, output: "1\told text", outputDone: true, exited: true, exitCode: 0};
    c.finishList();
    assert.equal(c.items.length, 0);
    assert.equal(c.results.length, 0);
});

test("thumbnail references remain bounded while different searches decode images", () => {
    const c = service();
    for (let i = 0; i < 50; i++) {
        c.results = [row(String(i), "[[ binary data 1 KiB png 20x20 ]]")];
        c.loadPreview(); c.startDecode();
        const destination = c.decoder.command.at(-1);
        finish(c, destination);
        assert.ok(Object.keys(c.thumbnails).length <= 16);
    }
});

test("copy waits for its process and uses argument arrays", () => {
    const c = service();
    c.results = [row("22", "hello")];
    c.copyRow(c.results[0]);
    assert.equal(c.open, true);
    assert.equal(c.copying, true);
    assert.deepEqual(Array.from(c.copier.command).slice(-5), ["copy", "clip", "22", "/tmp/clipboard-pins", ""]);
});

test("pin records reject malformed arrays, duplicate IDs and paths outside their directory", () => {
    const pin = {id: "p_123", preview: "hello", file: "/pins/p_123"};
    assert.equal(format.validatePins([pin], "/pins")[0].kind, "text");
    for (const raw of [{}, null, [pin, pin], [{...pin, id: "../secret"}], [{...pin, file: "/etc/passwd"}], [{...pin, preview: null}]])
        assert.throws(() => format.validatePins(raw, "/pins"));
});
