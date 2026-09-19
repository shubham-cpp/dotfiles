const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const test = require("node:test");
const path = require("node:path");
const { spawnSync } = require("node:child_process");
const root = path.join(__dirname, "..");
const { search: goSearch } = require("./search-fixture.cjs");
const c = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(root, "Common/EmojiCatalog.js"), "utf8").replace(/^\.(?:pragma|import).*\n/gm, ""), c);
const data = JSON.parse(fs.readFileSync(path.join(root, "data/emoji.json")));
const catalog = c.index(structuredClone(data));
const search = (state, query, category) => goSearch("emoji", [], { preferences: state, query, category });

test("every pinned sequence is reachable once, with exact bytes", () => {
    assert.equal(data.entries.length, 3944);
    assert.equal(catalog.order.length, 1914);
    const ids = catalog.order.flatMap(id => catalog.families[id].variants);
    assert.equal(new Set(ids).size, data.entries.length);
    for (const text of ["👍🏽", "👩🏽‍💻", "🫱🏻‍🫲🏿", "❤️", "1️⃣", "🇮🇳", "🏴󠁧󠁢󠁥󠁮󠁧󠁿"])
        assert.equal(catalog.entries[catalog.byText[text]].text, text);
});

test("global tones and per-family overrides preserve recents exactly", () => {
    const state = c.emptyState();
    state.tone = 3;
    assert.equal(c.resolve(catalog, "1f44d", state), "1f44d-1f3fd");
    state.overrides["1f44d"] = "1f44d-1f3ff";
    assert.equal(c.resolve(catalog, "1f44d", state), "1f44d-1f3ff");
    state.overrides["1f44d"] = "1f44d";
    assert.equal(c.resolve(catalog, "1f44d", state), "1f44d");
    const recent = c.recentState(state, "1f44d-1f3fb", 42);
    assert.deepEqual(search(recent, "", "recent"), ["1f44d-1f3fb"]);
});

test("paired legacy defaults and distinct mixed-tone combinations resolve", () => {
    const family = catalog.families["1f91d"];
    assert.equal(family.tuples["1,5"], "1faf1-1f3fb-200d-1faf2-1f3ff");
    assert.equal(family.tuples["5,1"], "1faf1-1f3ff-200d-1faf2-1f3fb");
    assert.equal(family.tuples["3,3"], "1f91d-1f3fd");
    assert.equal(family.tuples["0,5"], undefined);
    for (const id of ["1f48f", "1f491", "1f91d"]) {
        const f = catalog.families[id];
        assert.equal(f.slots, 2);
        assert.equal(Object.keys(f.tuples).length, 26);
        for (let tone = 1; tone <= 5; tone++) {
            const s = c.emptyState(); s.tone = tone;
            assert.equal(c.resolve(catalog, id, s), f.tuples[[tone, tone].join(",")]);
        }
    }
});

test("exact emoji and explicit tone search beat saved preference", () => {
    const state = c.emptyState(); state.tone = 1;
    for (const [query, text] of [["👍🏿", "👍🏿"], ["thumbs up dark", "👍🏿"], ["woman technologist", "👩🏻‍💻"], [":thumbsup:", "👍🏻"], ["RED_HEART", "❤️"]]) {
        const ids = search(state, query, "recent");
        assert.equal(catalog.entries[ids[0]].text, text, query);
    }
    assert.equal(search(state, "zzzzzzzzzzzzzzzzzzz", "all").length, 0);
    assert.equal(catalog.entries[search(state, "handshake light dark", "all")[0]].text, "🫱🏻‍🫲🏿");
    assert.equal(catalog.entries[search(state, "light bulb", "all")[0]].text, "💡");
});

test("recent and preference input is bounded, deduplicated and validated", () => {
    const state = c.emptyState();
    state.tone = 99;
    state.overrides = { "1f44d": "1f44e", "1f44e": "1f44e-1f3ff" };
    state.recents = [{ id: "absent", at: 1 }, { id: "1f44d", at: 1e20 }, { id: "1f44d", at: 2 }]
        .concat(data.entries.slice(0, 100).map(e => ({ id: e.id, at: -1 })));
    const s = c.sanitizeState(state, catalog, 100);
    assert.equal(s.tone, 0);
    assert.equal(s.overrides["1f44d"], undefined);
    assert.equal(s.overrides["1f44e"], "1f44e-1f3ff");
    assert.equal(s.recents.length, 48);
    assert.equal(s.recents[0].at, 100);
    assert.equal(c.recentState(s, "1f44d", 200).recents.length, 48);
    assert.throws(() => c.sanitizeState({ schema: 2 }, catalog, 100));
});

test("catalog validation rejects corruption and missing family links", () => {
    const broken = structuredClone(data); broken.entries[0].text = "wrong";
    assert.throws(() => c.index(broken), /Invalid emoji entry/);
    const missing = structuredClone(data); missing.families.pop();
    assert.throws(() => c.index(missing), /Unreachable/);
});

test("Go display catalog supports production validation and variant resolution", () => {
    const result = spawnSync(path.join(root, ".local/bin/qs-search"),
        ["--emoji-catalog", path.join(root, "data/emoji.json"), "--export-display"],
        { encoding: "utf8", timeout: 10000, maxBuffer: 4 * 1024 * 1024 });
    assert.equal(result.status, 0, result.stderr || String(result.error || ""));
    const display = JSON.parse(result.stdout);
    const indexed = c.index(display);
    assert.equal(indexed.order.length, catalog.order.length);
    assert.equal(Object.keys(indexed.entries).length, data.entries.length);
    assert.equal(c.resolve(indexed, "1f44d", { tone: 3, overrides: {} }), "1f44d-1f3fd");
    for (const entry of display.entries) {
        assert.equal(entry.search, "");
        assert.equal(entry.aliases.length, 0);
        assert.equal(indexed.entries[entry.id].text, catalog.entries[entry.id].text);
    }
    const ids = search(c.emptyState(), "woman technologist", "all");
    assert.equal(indexed.entries[ids[0]].text, "👩‍💻");
});

test("category browsing is complete without duplicating tone families", () => {
    const state = c.emptyState();
    assert.equal(search(state, "", "all").length, 1914);
    const union = catalog.groups.flatMap(group => search(state, "", group));
    assert.equal(new Set(union).size, 1914);
});
