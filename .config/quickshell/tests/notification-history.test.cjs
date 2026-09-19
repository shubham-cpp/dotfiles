const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const history = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(__dirname, "../Common/NotificationHistory.js"), "utf8").replace(/^\.pragma library\s*/, ""), history);

test("history is sorted newest first with one heading per date group", () => {
    const now = new Date(2026, 8, 12, 14);
    const entries = [
        { time: new Date(2026, 8, 10, 12).getTime() },
        { time: new Date(2026, 8, 12, 10).getTime() },
        { time: new Date(2026, 8, 11, 20).getTime() },
        { time: new Date(2026, 8, 12, 13).getTime() }
    ];
    const rows = history.rows(entries, now);
    assert.deepEqual(Array.from(rows, row => row.section), ["Today", "", "Yesterday", "Earlier"]);
    assert.equal(rows[0].entry, entries[3]);
    assert.equal(entries[0].time, new Date(2026, 8, 10, 12).getTime());
});

test("midnight and year boundaries use local calendar dates", () => {
    const now = new Date(2027, 0, 1, 0, 1);
    assert.equal(history.dayGroup(new Date(2027, 0, 1, 0, 0).getTime(), now), "Today");
    assert.equal(history.dayGroup(new Date(2026, 11, 31, 0, 0).getTime(), now), "Yesterday");
    assert.equal(history.dayGroup(new Date(2026, 11, 30, 23, 59).getTime(), now), "Earlier");
});

test("a daylight-saving transition does not move early yesterday into Earlier", () => {
    const previous = process.env.TZ;
    process.env.TZ = "America/New_York";
    try {
        const now = new Date(2026, 10, 2, 0, 30);
        const yesterday = new Date(2026, 10, 1, 0, 15);
        assert.equal(history.dayGroup(yesterday.getTime(), now), "Yesterday");
    } finally {
        if (previous === undefined)
            delete process.env.TZ;
        else
            process.env.TZ = previous;
    }
});

test("reconciling rows preserves existing cards when entries arrive or change", () => {
    const original = { rowKey: "live:1", section: "Today" };
    const items = [original];
    const model = {
        get count() { return items.length; },
        get(i) { return items[i]; },
        insert(i, item) { items.splice(i, 0, item); },
        move(from, to) { items.splice(to, 0, items.splice(from, 1)[0]); },
        setProperty(i, name, value) { items[i][name] = value; },
        remove(i, count) { items.splice(i, count); }
    };
    history.sync(model, [{ rowKey: "live:2", section: "Today" }, { rowKey: "live:1", section: "" }]);
    assert.equal(items[1], original);
    history.sync(model, [{ rowKey: "live:1", section: "Today" }]);
    assert.equal(items.length, 1);
    assert.equal(items[0], original);
    assert.equal(history.key({id: 1, live: true, time: 1}), history.key({id: 1, live: true, time: 2}));
    assert.notEqual(history.key({id: 1, live: false, time: 1}), history.key({id: 1, live: false, time: 2}));
});
