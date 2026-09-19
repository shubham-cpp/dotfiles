const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const context = vm.createContext({});
vm.runInContext(fs.readFileSync(path.join(__dirname, "../Common/FilePath.js"), "utf8").replace(/^\.pragma library\s*/, ""), context);

function plain(value) {
    return JSON.parse(JSON.stringify(value));
}

test("basename and tilde parent match superp display", () => {
    const home = "/home/u";
    assert.deepEqual(plain(context.fromPath("/home/u/notes.md", home)), {
        path: "/home/u/notes.md", name: "notes.md", dir: "~"
    });
    assert.deepEqual(plain(context.fromPath("/home/u/src/hello.go", home)), {
        path: "/home/u/src/hello.go", name: "hello.go", dir: "~/src"
    });
    assert.deepEqual(plain(context.fromPath("/tmp/out.txt", home)), {
        path: "/tmp/out.txt", name: "out.txt", dir: "/tmp"
    });
    assert.equal(context.basename("file.txt"), "file.txt");
    assert.equal(context.parent("/"), "/");
});

test("recents prefer latest then count and cap the list", () => {
    const rows = context.recents({
        "/a": { count: 9, last: 10 },
        "/b": { count: 1, last: 30 },
        "/c": { count: 4, last: 20 },
        "/d": { count: 8, last: 30 }
    }, "/home/u", 3);
    assert.deepEqual(plain(rows.map(row => row.path)), ["/d", "/b", "/c"]);
});

test("gtk recents skip directories, decode URLs, and prefer visited time", () => {
    const xbel = `<?xml version="1.0"?>
<xbel>
  <bookmark href="file:///home/u/old.txt" visited="2026-01-01T00:00:00Z"/>
  <bookmark href="file:///run/media/disk/" visited="2026-09-14T00:00:00Z"/>
  <bookmark href="file:///home/u/docs/My%20Notes.md" visited="2026-09-14T12:00:00Z"/>
  <bookmark href="file:///home/u/old.txt" visited="2026-09-14T18:00:00Z"/>
</xbel>`;
    assert.deepEqual(plain(context.gtkPaths(xbel, 10)), [
        "/home/u/old.txt",
        "/home/u/docs/My Notes.md"
    ]);
});

test("empty rows put picker stats ahead of gtk recents", () => {
    const xbel = `<bookmark href="file:///home/u/gtk.md" visited="2026-09-14T12:00:00Z"/>
<bookmark href="file:///home/u/notes.md" visited="2026-09-14T18:00:00Z"/>`;
    const rows = context.emptyRows({ "/home/u/notes.md": { count: 1, last: 50 } }, xbel, "/home/u", 50);
    assert.deepEqual(plain(rows.map(row => row.path)), ["/home/u/notes.md", "/home/u/gtk.md"]);
});
