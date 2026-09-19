const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

function service(statsText = "{}", pinsText = "[]") {
    const statsFile = { text: () => statsText, setText(value) { statsText = value; } };
    const pinsFile = { text: () => pinsText, setText(value) { pinsText = value; } };
    const context = vm.createContext({
        stats: {}, pins: [], _statsReadOnly: false, _pinsReadOnly: false,
        statsFile, pinsFile, statsTimer: { restart() {} }
    });
    const source = fs.readFileSync(path.join(__dirname, "../Services/LauncherStats.qml"), "utf8");
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0].replace(/\): (?:void|bool)/g, ")"), context);
    context.loadStats();
    context.loadPins();
    return { context, statsFile, pinsFile };
}

test("unsupported launcher state cannot throw or overwrite the original file", () => {
    for (const raw of ["null", "[]", '"text"', "1", "{"]) {
        const { context: c, statsFile } = service(raw);
        assert.equal(c._statsReadOnly, true, raw);
        assert.deepEqual(Object.keys(c.stats), []);
        c.recordLaunch("firefox.desktop");
        c.persistStats();
        assert.equal(statsFile.text(), raw);
    }
    for (const raw of ["null", "{}", '"text"', "1", "{"]) {
        const { context: c, pinsFile } = service("{}", raw);
        assert.equal(c._pinsReadOnly, true, raw);
        assert.equal(c.isPinned("firefox.desktop"), false);
        c.pins = ["firefox.desktop"];
        c.persistPins();
        assert.equal(pinsFile.text(), raw);
    }
});

test("usage validation retains valid entries and the existing retention policy", () => {
    const now = Math.floor(Date.now() / 1000);
    const { context: c } = service(JSON.stringify({
        recent: { count: 1, last: now }, frequent: { count: 3 },
        stale: { count: 2, last: 1 }, missingCount: { last: now },
        nullEntry: null, arrayEntry: [], stringCount: { count: "4", last: now },
        fractionalCount: { count: 1.5, last: now }, negativeCount: { count: -1, last: now },
        overflowCount: { count: Number.MAX_SAFE_INTEGER + 1, last: now },
        stringTime: { count: 3, last: "123" }, negativeTime: { count: 3, last: -1 }
    }));
    assert.deepEqual(JSON.parse(JSON.stringify(c.stats)), {
        recent: { count: 1, last: now }, frequent: { count: 3, last: 0 },
        missingCount: { count: 0, last: now }
    });
    assert.equal(c._statsReadOnly, false);
    c.recordLaunch("recent");
    assert.equal(c.stats.recent.count, 2);
});

test("pin validation keeps valid order, removes duplicates and rejects non-string IDs", () => {
    const { context: c } = service("{}", JSON.stringify(["firefox.desktop", null, 4, {}, "", "firefox.desktop", "org.kde.kate.desktop"]));
    assert.deepEqual(Array.from(c.pins), ["firefox.desktop", "org.kde.kate.desktop"]);
    assert.equal(c._pinsReadOnly, false);
    assert.equal(c.isPinned("firefox.desktop"), true);
});

test("usage IDs remain dictionary keys when recording another launch", () => {
    const { context: c } = service('{"__proto__":{"count":3,"last":1}}');
    c.recordLaunch("other");
    c.recordLaunch("__proto__");
    assert.equal(c.stats.__proto__.count, 4);
    assert.equal(Object.getPrototypeOf(c.stats), null);
});

test("oversized state is retained on disk and launch counts remain representable", () => {
    const tooMany = JSON.stringify(Array.from({ length: 10001 }, (_, i) => `app-${i}`));
    const pins = service("{}", tooMany);
    assert.equal(pins.context._pinsReadOnly, true);
    pins.context.persistPins();
    assert.equal(pins.pinsFile.text(), tooMany);
    const stats = service(" ".repeat(4000001));
    assert.equal(stats.context._statsReadOnly, true);
    stats.context.persistStats();
    assert.equal(stats.statsFile.text().length, 4000001);
    const { context: c } = service(JSON.stringify({ app: { count: Number.MAX_SAFE_INTEGER, last: 1 } }));
    c.recordLaunch("app");
    assert.equal(c.stats.app.count, Number.MAX_SAFE_INTEGER);
});
