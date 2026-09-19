const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");
const context = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(__dirname, "../Common/ResourceList.js"), "utf8").replace(/^\.pragma library\n/, ""), context);

function app(key, memory, cpu) {
    return { key, name: key, icon: "", memory, cpu, count: 1, canEnd: true, state: "", members: [{ pid: memory, started: 10 }] };
}
function model() {
    const rows = [];
    const writes = [];
    return { rows, writes, get count() { return rows.length; }, get: i => rows[i],
        setProperty(i, key, value) { writes.push({i, key, value}); rows[i][key] = value; },
        insert: (i, row) => rows.splice(i, 0, {...row}),
        remove: i => rows.splice(i, 1), move: (from, to) => rows.splice(to, 0, rows.splice(from, 1)[0]) };
}

test("both rankings sort independently and cap after ranking", () => {
    const apps = Array.from({length: 12}, (_, i) => app(String(i), i, 12 - i));
    assert.equal(context.ranked(apps, "memory").length, 10);
    assert.equal(context.ranked(apps, "memory")[0].key, "11");
    assert.equal(context.ranked(apps, "cpu")[0].key, "0");
});

test("unchanged samples do not rewrite rows in either ranking, including frozen rows", () => {
    const apps = [app("a", 10, 20), app("b", 20, 10)];
    for (const metric of ["memory", "cpu"]) {
        const rows = model();
        context.sync(rows, apps, metric, false);
        context.sync(rows, apps, metric, false);
        context.sync(rows, apps, metric, true);
        assert.equal(rows.writes.length, 0);
        const changed = apps.map(a => ({...a, cpu: a.cpu + 1}));
        context.sync(rows, changed, metric, true);
        assert.equal(rows.writes.length, 2);
        assert.ok(rows.writes.every(write => write.key === "cpu"));
    }
});

test("only displayed membership lists are serialized and shared across columns", () => {
    const apps = Array.from({length: 12}, (_, i) => app(String(i), i, i));
    const memory = model(), cpu = model();
    context.sync(memory, apps, "memory", false);
    assert.ok(Array.isArray(apps[0].members));
    assert.equal(typeof apps[11].members, "string");
    context.sync(cpu, apps, "cpu", false);
    assert.equal(memory.get(0).members, cpu.get(0).members);
    assert.deepEqual(JSON.parse(cpu.get(0).members), [{pid: 11, started: 10}]);
});
test("frozen rows update values but never move, replace or remove action targets", () => {
    const rows = model();
    context.sync(rows, [app("a", 30, 1), app("b", 20, 2)], "memory", false);
    context.sync(rows, [app("b", 50, 2), app("c", 80, 3)], "memory", true);
    assert.deepEqual(rows.rows.map(row => row.appKey), ["a", "b"]);
    assert.equal(rows.get(0).gone, true);
    assert.equal(rows.get(0).canEnd, false);
    assert.equal(rows.get(1).memory, 50);
    context.sync(rows, [app("b", 50, 2), app("c", 80, 3)], "memory", false);
    assert.deepEqual(rows.rows.map(row => row.appKey), ["c", "b"]);
});
test("a frozen initial list still populates and a returning app clears the exited state", () => {
    const rows = model();
    context.sync(rows, [app("a", 10, -1)], "memory", true);
    assert.equal(rows.count, 1);
    context.sync(rows, [], "memory", true);
    context.sync(rows, [app("a", 20, 1)], "memory", true);
    assert.equal(rows.get(0).gone, false);
    assert.equal(rows.get(0).canEnd, true);
});
