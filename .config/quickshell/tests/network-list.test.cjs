const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const list = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(__dirname, "../Common/NetworkList.js"), "utf8").replace(/^\.pragma library\s*/, ""), list);

function net(name, extra = {}) {
    return { name, connected: false, known: false, signalStrength: 0, ...extra };
}

function names(arr) {
    return Array.prototype.map.call(arr, n => n.name).join(",");
}

test("partition puts connected first, then known, then others by signal", () => {
    const grouped = list.partition([
        net("Cafe", { signalStrength: 0.9 }),
        net("Home", { known: true, signalStrength: 0.4 }),
        net("Office", { known: true, signalStrength: 0.8 }),
        net("Current", { connected: true, known: true, signalStrength: 0.5 }),
        net("Weak", { signalStrength: 0.2 }),
        net("", { signalStrength: 1 }),
        null
    ]);
    assert.equal(names(grouped.connected), "Current");
    assert.equal(names(grouped.known), "Office,Home");
    assert.equal(names(grouped.other), "Cafe,Weak");
    assert.equal(names(list.saved(grouped)), "Current,Office,Home");
});

test("empty and missing values yield empty groups", () => {
    const empty = list.partition(undefined);
    assert.equal(empty.connected.length + empty.known.length + empty.other.length, 0);
    assert.equal(list.saved(undefined).length, 0);
    assert.equal(list.wifiGlyph(0.9), "wifi4");
    assert.equal(list.wifiGlyph(0.5), "wifi3");
    assert.equal(list.wifiGlyph(0.25), "wifi2");
    assert.equal(list.wifiGlyph(0), "wifi1");
});

test("equal signal strength uses names consistently without combining native networks", () => {
    const grouped = list.partition([
        net("Zulu", { signalStrength: 0.5 }),
        net("Alpha", { signalStrength: 0.5 }),
        net("Alpha", { signalStrength: 0.5 })
    ]);
    assert.equal(names(grouped.other), "Alpha,Alpha,Zulu");
});

test("rows use real network objects and a section role, never a null separator", () => {
    const current = net("Current", { connected: true });
    const nearby = net("Nearby");
    const rows = list.rows(list.partition([nearby, current]));
    assert.equal(rows.length, 2);
    assert.equal(rows[0].net, current);
    assert.equal(rows[0].group, "Saved");
    assert.equal(rows[1].net, nearby);
    assert.equal(rows[1].group, "Nearby");
});

test("frozen lists remove vanished networks and populate only when empty", () => {
    const a = net("A"), b = net("B"), c = net("C");
    const rows = [];
    const writes = [];
    const model = { get count() { return rows.length; }, get: i => rows[i],
        insert: (i, row) => rows.splice(i, 0, { ...row }), remove: i => rows.splice(i, 1),
        move: (i, j) => rows.splice(j, 0, rows.splice(i, 1)[0]),
        setProperty(i, key, value) { writes.push({ i, key, value }); rows[i][key] = value; } };
    const initial = [{ net: a, group: "Nearby" }, { net: b, group: "Nearby" }];
    list.sync(model, initial, true);
    list.sync(model, initial, false);
    assert.equal(writes.length, 0);
    const changed = [{ net: c, group: "Saved" }, { net: b, group: "Saved" }];
    list.sync(model, changed, true);
    assert.deepEqual(rows, [{ net: b, group: "Nearby" }]);
    list.sync(model, changed, false);
    assert.deepEqual(rows, changed);
    assert.deepEqual(writes, [{ i: 1, key: "group", value: "Saved" }]);
    list.sync(model, [{ net: a, group: "Nearby" }], true);
    assert.deepEqual(rows, [{ net: a, group: "Nearby" }]);
});
