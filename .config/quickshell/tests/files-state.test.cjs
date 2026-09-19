const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const filePath = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(__dirname, "../Common/FilePath.js"), "utf8").replace(/^\.pragma library\s*/, ""), filePath);

function service(statsText = "{}") {
    const statsFile = { text: () => statsText, setText(value) { statsText = value; } };
    const context = vm.createContext({
        stats: {}, _statsReadOnly: false, statsFile, statsTimer: { restart() {} },
        recentFile: { text: () => "", reload() {} },
        cursor: { running: false }, _epoch: 0, screenName: "",
        open: false, results: [], searching: false,
        FilePath: filePath, Date, Number, Math, Object, String, Array,
        Quickshell: { env: () => "/home/u", execDetached() {}, dataDir: "/tmp" }
    });
    const source = fs.readFileSync(path.join(__dirname, "../Services/Files.qml"), "utf8");
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0].replace(/\): (?:void|bool)/g, ")"), context);
    context.loadStats();
    return { context, statsFile };
}

test("unsupported files usage cannot throw or overwrite the original file", () => {
    for (const raw of ["null", "[]", '"text"', "1", "{"]) {
        const { context: c, statsFile } = service(raw);
        assert.equal(c._statsReadOnly, true, raw);
        assert.deepEqual(Object.keys(c.stats), []);
        c.recordOpen("/home/u/notes.md");
        c.persistStats();
        assert.equal(statsFile.text(), raw);
    }
});

test("files usage validation retains recent and frequent paths", () => {
    const now = Math.floor(Date.now() / 1000);
    const { context: c } = service(JSON.stringify({
        "/home/u/recent.md": { count: 1, last: now },
        "/home/u/frequent.md": { count: 3 },
        "relative.md": { count: 9, last: now },
        "/home/u/stale.md": { count: 2, last: 1 }
    }));
    assert.deepEqual(Object.keys(c.stats).sort(), ["/home/u/frequent.md", "/home/u/recent.md"]);
});

test("openRow matches by path and still works while searching", () => {
    const opened = [];
    const { context: c } = service();
    c.Quickshell.execDetached = args => opened.push(args);
    c.open = true;
    c.searching = true;
    c.results = [{ path: "/home/u/notes.md", name: "notes.md", dir: "~" }];
    c.openRow({ path: "/home/u/notes.md" });
    assert.equal(opened.length, 1);
    assert.equal(opened[0][0], "xdg-open");
    assert.equal(opened[0][1], "/home/u/notes.md");
    assert.equal(c.open, false);
    assert.equal(c.stats["/home/u/notes.md"].count, 1);
});
