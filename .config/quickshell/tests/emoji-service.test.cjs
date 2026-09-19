const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const test = require("node:test");
const path = require("node:path");
const root = path.join(__dirname, "..");
const Catalog = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(root, "Common/EmojiCatalog.js"), "utf8").replace(/^\.(?:pragma|import).*\n/gm, ""), Catalog);
const displayData = JSON.parse(fs.readFileSync(path.join(root, ".local/emoji-display.json")));
const catalog = Catalog.index(displayData);

function service() {
    const writes = [], copies = [];
    const Quickshell = { shellPath: x => x, dataPath: x => x };
    Object.defineProperty(Quickshell, "clipboardText", { set: x => copies.push(x) });
    const context = vm.createContext({ Catalog, Quickshell, catalog, preferences: Catalog.emptyState(),
        open: false, unavailable: false, query: "", category: "all", selected: 0, results: [], loading: false,
        error: "", saveError: "", screenName: "", variantOpen: false, _stateRead: true, _stateReady: true,
        _rawState: null, _readOnly: false, _saving: false, _dirty: false, _copying: false,
        _epoch: 0, _cursorEpoch: 0, _corrupt: "", lastSearchMs: 0, _publishMs: 0, searching: false, _searchInstalled: false, _searchStarted: 0, _chosenVariants: {},
        cursor: { running: false }, catalogFile: { path: "" }, stateFile: { path: "prefs", setText: x => writes.push(x) },
        recoveryFile: { path: "", setText: x => writes.push(x) }, Notifications: { centerOpen: false } });
    for (const name of ["LauncherStats", "Clipboard", "Files", "Agenda", "Network", "Power", "Audio", "Brightness", "Resources"])
        context[name] = { open: true, close() { this.open = false; } };
    context.root = context;
    context.Search = require("./search-fixture.cjs").adapter(context);
    Object.defineProperty(context, "current", { get: () => context.catalog?.entries[context.results[context.selected]] || null });
    const source = fs.readFileSync(path.join(root, "Services/Emoji.qml"), "utf8");
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0].replace(/\): (bool|void) \{/, ") {"), context);
    return { c: context, writes, copies };
}

test("lock/sleep guard blocks opening and copying", () => {
    const { c, copies } = service(); c.unavailable = true;
    assert.equal(c.toggle(), false); assert.equal(c.open, false);
    c.open = true; c.copyId("1f44d"); assert.equal(copies.length, 0);
});
test("opening emoji also closes the audio and resource popups", () => {
    const { c } = service();
    c.toggle();
    assert.equal(c.Audio.open, false);
    assert.equal(c.Resources.open, false);
    assert.equal(c.Files.open, false);
});
test("copy emits exact sequence once and closes without retained results", () => {
    const { c, copies, writes } = service(); c.toggle(); c.copyId("1f44d-1f3fd"); c.copyId("1f44d-1f3fd");
    assert.deepEqual(copies, ["👍🏽"]); assert.equal(writes.length, 1);
    assert.equal(c.open, false); assert.equal(c.results.length, 0); assert.equal(c.cursor.running, false);
    assert.equal(c.preferences.recents[0].id, "1f44d-1f3fd");
});
test("invalid IDs and incomplete state cannot copy or update recents", () => {
    const { c, copies } = service(); c.open = true; c.copyId("unknown");
    c._stateReady = false; c.copyId("1f44d"); assert.equal(copies.length, 0);
});
test("writes coalesce to latest preference without an interval timer", () => {
    const { c, writes } = service(); c.setTone(1); c.setTone(3); c.setTone(5);
    assert.equal(writes.length, 1); c._saving = false; c.saveNext();
    assert.equal(writes.length, 2); assert.equal(JSON.parse(writes[1]).tone, 5);
});
test("family override changes displayed selection and can be cleared", () => {
    const { c } = service(); c.toggle(); c.query = "thumbs up"; c.rebuild();
    c.setVariant("1f44d", "1f44d-1f3ff"); assert.equal(c.current.text, "👍🏿");
    c.setVariant("1f44d", "1f44e"); assert.equal(c.current.text, "👍🏿");
    c.setVariant("1f44d", ""); assert.equal(c.current.text, "👍");
});
test("late catalog/state completion cannot reopen a closed picker", () => {
    const { c } = service(); c.toggle(); c.close(); c.finishLoad();
    assert.equal(c.open, false); assert.equal(c.results.length, 0);
});
test("future preferences are read only; corrupt bytes are preserved before replacement", () => {
    const { c, writes } = service(); c._stateReady = false; c._rawState = { schema: 2 }; c.finishLoad();
    c.setTone(3); assert.equal(writes.length, 0); assert.equal(c._readOnly, true);
    const next = service(); next.c._corrupt = "broken json"; next.c.setTone(3);
    assert.deepEqual(next.writes, ["broken json"]); assert.equal(next.c._saving, false);
});

test("production display catalog publishes all browse results in family order", () => {
    assert.equal(displayData.displayOnly, true);
    const { c } = service();
    c.toggle();
    const expected = Array.from(c.catalog.order, id => Catalog.resolve(c.catalog, id, c.preferences));
    assert.equal(c.results.length, 1914);
    assert.deepEqual(Array.from(c.results), expected);
    assert.equal(c.searching, false);
});

test("local variant choice survives replay of an explicit tone query", () => {
    const { c } = service();
    c.toggle();
    c.query = "thumbs up dark";
    c.rebuild();
    assert.equal(c.current.text, "👍🏿");
    const replay = Array.from(c.results);
    c.setVariant("1f44d", "1f44d-1f3fb");
    assert.equal(c.current.text, "👍🏻");
    c.searching = true;
    c.acceptSearch(replay);
    assert.equal(c.current.text, "👍🏻");
    assert.equal(c._chosenVariants["1f44d"], "1f44d-1f3fb");
    assert.equal(c.searching, false);
});

test("invalid final result rejects the entire publication", () => {
    const { c } = service();
    c.toggle();
    const previous = c.results;
    let failures = 0;
    c.Search.protocolFailure = () => failures++;
    c.searching = true;
    c.acceptSearch(Array.from(previous).concat("missing-emoji"));
    assert.equal(failures, 1);
    assert.equal(c.results, previous);
    assert.equal(c.results.length, 1914);
    assert.equal(c.searching, true);
});

test("close and reopen retain the static display catalog and restore browsing", () => {
    const { c } = service();
    c.toggle();
    const retained = c.catalog;
    const previous = Array.from(c.results);
    c.close();
    assert.equal(c.catalog, retained);
    assert.equal(c.results.length, 0);
    assert.equal(c._searchInstalled, false);
    c.toggle();
    assert.equal(c.catalog, retained);
    assert.equal(c.catalogFile.path, "");
    assert.deepEqual(Array.from(c.results), previous);
    assert.equal(c.loading, false);
});
