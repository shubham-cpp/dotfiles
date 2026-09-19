// Differential tests retain the pre-migration JS policies as executable references.
// Qt collation is represented by the explicit tie ordinal supplied by the QML adapter.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const vm = require("node:vm");
const { spawn, spawnSync } = require("node:child_process");
const { createInterface } = require("node:readline");
const { performance } = require("node:perf_hooks");
const { test, before, after } = require("node:test");

const root = path.resolve(__dirname, "..");
const temporary = fs.mkdtempSync(path.join(os.homedir(), ".cache", "qs-search-differential-"));
const binary = path.join(temporary, "qs-search");
function qtContext(globals = {}) {
    const context = vm.createContext(globals);
    // Measured with the frozen oracle in Qt 6.11.2: String.toLowerCase does
    // full lowercasing without contextual final sigma. V8 normally changes
    // word-final capital sigma to final sigma. Adjust the reference engine,
    // keeping the saved production JS source unchanged. Other full mappings,
    // including capital dotted I's expansion, still use V8's conversion.
    vm.runInContext(`{
        const original = String.prototype.toLowerCase;
        String.prototype.toLowerCase = function() {
            return original.call(String(this).replace(/\\u03a3/g, "\\u03c3"));
        };
    }`, context);
    return context;
}
const Fuzzy = qtContext();
vm.runInContext(fs.readFileSync(path.join(__dirname, "reference/Fuzzy.js"), "utf8").replace(/^\.pragma.*\n/, ""), Fuzzy);
const Emoji = qtContext({ Fuzzy });
vm.runInContext(fs.readFileSync(path.join(__dirname, "reference/EmojiCatalog.js"), "utf8").replace(/^\.(?:pragma|import).*\n/gm, ""), Emoji);
const Format = qtContext();
vm.runInContext(fs.readFileSync(path.join(root, "Common/ClipboardFormat.js"), "utf8").replace(/^\.pragma.*\n/, ""), Format);
const plain = value => JSON.parse(JSON.stringify(value));

before(() => {
    const build = spawnSync("mise", ["exec", "--", "go", "build", "-o", binary, "./cmd/qs-search"], { cwd: root, encoding: "utf8", timeout: 60000 });
    assert.equal(build.status, 0, build.stderr || build.error?.message);
});
after(() => {
    // Only this test's generated executable directory is moved to Trash.
    const cleanup = spawnSync("gio", ["trash", temporary], { encoding: "utf8", timeout: 10000 });
    assert.equal(cleanup.status, 0, cleanup.stderr);
});

function run(profile, rows, queries) {
    const envelope = { v: 1, profile, epoch: 7, revision: 3 };
    const requests = [{ ...envelope, type: "begin" }];
    // Real chunks exercise incremental installation, including UTF-8 strings.
    for (let i = 0; i < rows.length; i += 100)
        requests.push({ ...envelope, type: "chunk", rows: rows.slice(i, i + 100) });
    requests.push({ ...envelope, type: "commit" });
    queries.forEach((query, index) => requests.push({ ...envelope, type: "search", request: index + 1, ...query }));
    requests.push({ ...envelope, type: "release" });
    const result = spawnSync(binary, ["--emoji-catalog", path.join(root, "data/emoji.json")], {
        input: requests.map(request => JSON.stringify(request) + "\n").join(""), encoding: "utf8", timeout: 30000, maxBuffer: 32 * 1024 * 1024
    });
    assert.equal(result.status, 0, result.stderr || result.error?.message);
    const replies = result.stdout.trim().split("\n").map(line => JSON.parse(line));
    assert.equal(replies.length, requests.length + 1, "each command must terminate with one reply");
    assert.equal(replies[0].type, "ready");
    assert.ok(replies[0].instance);
    replies.slice(1).forEach((reply, index) => {
        const request = requests[index];
        assert.equal(reply.v, 1);
        assert.equal(reply.instance, replies[0].instance);
        assert.equal(reply.profile, profile);
        assert.equal(reply.epoch, envelope.epoch);
        assert.equal(reply.revision, envelope.revision);
        assert.notEqual(reply.type, "error", reply.error);
        if (request.type === "search") assert.equal(reply.request, request.request);
    });
    return replies.filter(reply => reply.type === "results").map(reply => reply.keys || []);
}

function launcherReference(rows, query, now) {
    const matches = rows.flatMap((row, order) => {
        const hit = Fuzzy.scoreDesktop(query, row);
        if (!hit) return [];
        const age = Math.max(0, now - row.last);
        const frec = row.last ? Math.log(1 + (row.count || 0)) + 4 * Math.exp(-age / 604800) : 0;
        return [{ key: row.key, tier: row.pinned ? 0 : hit.tier, score: hit.score, frec, tie: row.tie, order }];
    });
    return matches.sort((a, b) => a.tier - b.tier || b.score - a.score || b.frec - a.frec || a.tie - b.tie || a.order - b.order).slice(0, 50).map(row => row.key);
}

test("launcher retains JS compatibility outside the new contiguous-name ordering", () => {
    const now = 1800000000;
    const data = [
        { id: "firefox.desktop", name: "Firefox", genericName: "Web browser", keywords: ["Internet", "Navigation"] },
        { id: "exact", name: "fire", count: 2, last: now - 100 },
        { id: "prefix", name: "Firewall", pinned: true },
        { id: "unmatched-pin", name: "Calculator", pinned: true },
        { id: "case", name: "fooBar" },
        { id: "upper", name: "FooBar" },
        { id: "metadata", name: "Something", comment: "web browser", keywords: ["foo", "bar"] },
        { id: "split", name: "Other", comment: "foo", genericName: "bar", keywords: ["something"] },
        { id: "arbitrary-id-subsequence", name: "ZZZ" },
        { id: "unicode", name: "İstanbul 😀", keywords: ["ΣΟΣ", "👩🏽‍💻"] },
        { id: "duplicate-a", name: "Same", tie: 700 },
        { id: "duplicate-b", name: "Same", tie: 600 },
        { id: "duplicate-c", name: "Same", tie: 600 },
    ];
    for (let i = 0; i < 100; i++) data.push({ id: `bulk-${i}`, name: `Application ${i}`, count: i % 4, last: i % 3 ? now - i * 10000 : 0 });
    const rows = data.map((entry, i) => ({ key: `a${i}`, count: 0, last: 0, tie: i, ...entry }));
    const queries = ["", "fire", "FIRE", "firefox.desktop", "foB", "FB", "web browser", "foo bar", "foo", "arbidsub", "Same", "İ", "😀", "σος", "application", "zzz-no-match", " \tfire\n"];
    const actual = run("launcher", rows, queries.map(query => ({ query, now })));
    queries.forEach((query, i) => assert.deepEqual(actual[i], launcherReference(rows, query, now), query));
});

test("launcher prioritizes contiguous names and word starts through the helper protocol", () => {
    const now = 1800000000;
    const names = ["Heroic Games Launcher", "Hardware Locality lstopo", "Shelly", "CachyOS Hello",
        "Application 90", "Application 9", "Application 19"];
    const rows = names.map((name, tie) => ({ key: name, name, tie, count: 100, last: now }));
    assert.deepEqual(run("launcher", rows, [{ query: "hel", now }, { query: "app 9", now }]), [
        ["CachyOS Hello", "Shelly", "Heroic Games Launcher", "Hardware Locality lstopo"],
        ["Application 9", "Application 90", "Application 19"]
    ]);
});

test("clipboard preserves display-text matching, pin/history order and filtering before cap", () => {
    const items = [
        { preview: "#ff6363", pinned: true },
        { preview: "/home/user/Documents/notes.txt", pinned: true },
        { preview: "https://www.example.com/docs?q=ui", pinned: false },
        { preview: "First line\nSecond line", pinned: false },
        { preview: "[[ binary data 53 KiB png 482x654 ]]", pinned: true },
        { preview: "fooBar 😀", pinned: false },
    ];
    for (let i = 0; i < 45; i++) items.push({ preview: `[[ binary data ${i} KiB png 20x20 ]]`, pinned: false });
    for (let i = 0; i < 60; i++) items.push({ preview: `Plain ${i}`, pinned: false });
    items.sort((a, b) => Number(b.pinned) - Number(a.pinned));
    const rows = items.map((item, i) => {
        const row = { ...item, source: item.pinned ? "pin" : "clip", kind: Format.kindOf(item.preview) };
        return { key: `c${i}`, ...row, text: item.preview + " " + Format.title(row) + " " + Format.subtitle(row) };
    });
    const queries = ["", "color", "notes", "file path", "example", "second line", "482", "KiB", "FB", "😀", "Plain", "Plain 59", "missing"];
    const searches = ["all", "pinned", "text", "image", "link"].flatMap(filter => queries.map(query => ({ filter, query })));
    const actual = run("clipboard", rows, searches);
    searches.forEach(({ query, filter }, i) => {
        const expected = rows.filter(row => Format.matchesFilter(row, filter) && (!query.trim() || Fuzzy.scoreMultiTokenAND(query.trim(), row.text))).slice(0, 40).map(row => row.key);
        assert.deepEqual(actual[i], expected, `${filter}/${query}`);
    });
});

test("emoji matches the complete real JS catalog for browse, recents, aliases, tones and fuzzy fallback", () => {
    const data = JSON.parse(fs.readFileSync(path.join(root, "data/emoji.json"), "utf8"));
    const catalog = Emoji.index(data);
    assert.equal(catalog.order.length, 1914);
    assert.equal(Object.keys(catalog.entries).length, 3944);
    const preferences = plain(Emoji.emptyState());
    preferences.tone = 3;
    preferences.overrides = { "1f44d": "1f44d-1f3ff" };
    preferences.recents = [{ id: "1f44d-1f3fb", at: 100 }, { id: "1f600", at: 99 }];
    const queries = ["", "👍🏿", "thumbs up dark", "woman technologist", ":thumbsup:", "RED_HEART", "handshake light dark", "light bulb", "medium dark", "handshake medium light medium dark", "zzzzzzzzzzzzzzzzzzz", "tmbsup", "grnng", "😀", "flag india", "  flag\tindia  ", "person running", "family", "👩🏽‍💻", "İ", "ΟΣ", "💻", "birthday cake"];
    // Search many real names and deterministic shortened names, not only handpicked aliases.
    for (let i = 0; i < catalog.order.length; i += 71) {
        const entry = catalog.entries[catalog.order[i]];
        queries.push(entry.name, entry.name.replace(/[aeiou]/g, ""));
    }
    const searches = ["all", "recent", ...catalog.groups].map(category => ({ query: "", category, preferences }));
    for (const query of queries) {
        searches.push({ query, category: "all", preferences });
        searches.push({ query, category: "recent", preferences });
    }
    const actual = run("emoji", [], searches);
    searches.forEach(({ query, category, preferences }, i) => assert.deepEqual(actual[i], plain(Emoji.search(catalog, preferences, query, category)), `${category}/${query}`));
});

test("emoji display projection retains all exact sequences, families and tone metadata", () => {
    const original = JSON.parse(fs.readFileSync(path.join(root, "data/emoji.json"), "utf8"));
    const exported = spawnSync(binary, ["--emoji-catalog", path.join(root, "data/emoji.json"), "--export-display"], { encoding: "utf8", timeout: 10000, maxBuffer: 8 * 1024 * 1024 });
    assert.equal(exported.status, 0, exported.stderr);
    const display = JSON.parse(exported.stdout);
    assert.equal(display.displayOnly, true);
    assert.equal(display.entries.length, original.entries.length);
    assert.equal(display.families.length, original.families.length);
    display.families.forEach((family, i) => {
        for (const field of ["id", "name", "group", "slots", "variants"]) assert.deepEqual(family[field], original.families[i][field], `${family.id}/${field}`);
    });
    assert.deepEqual(display.groups, original.groups);
    display.entries.forEach((entry, i) => {
        const expected = original.entries[i];
        for (const field of ["id", "text", "name", "familyId", "tones"]) assert.deepEqual(entry[field], expected[field], `${entry.id}/${field}`);
        assert.equal(entry.search, "");
        assert.deepEqual(entry.aliases, []);
    });
});

test("optional warm Node-reference versus Go-pipe measurement", { skip: process.env.QS_SEARCH_BENCHMARK !== "1", timeout: 60000 }, async t => {
    const quantiles = values => {
        const sorted = [...values].sort((a, b) => a - b);
        const at = percentile => Number(sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * percentile))].toFixed(4));
        return { p50_ms: at(0.5), p95_ms: at(0.95), p99_ms: at(0.99) };
    };
    const now = 1800000000;
    const workloads = [100, 500, 2000].map(count => {
        const rows = Array.from({ length: count }, (_, i) => ({ key: `app-${i}`, id: `application-${i}.desktop`, name: `Application ${i}`, genericName: "Synthetic editor", keywords: ["graphics", "development"], count: i % 4, last: now - i * 10000, tie: i, pinned: i % 71 === 0 }));
        return { name: `launcher-${count}`, profile: "launcher", rows, queries: ["", "app", "application 2", "editor", "grp", "missing", "app 9"].map(query => ({ query, now })), reference: q => launcherReference(rows, q.query, now) };
    });
    const catalog = Emoji.index(JSON.parse(fs.readFileSync(path.join(root, "data/emoji.json"), "utf8")));
    const preferences = plain(Emoji.emptyState());
    workloads.push({ name: "emoji-1914-families", profile: "emoji", rows: [], queries: ["", "thumbs up", "thumbs up dark", "tmbsup", "grnng", "missingzzzz", "woman technologist", "flag india"].map(query => ({ query, category: "all", preferences })), reference: q => Emoji.search(catalog, preferences, q.query, q.category) });
    for (const workload of workloads) {
        const started = performance.now();
        const child = spawn(binary, ["--emoji-catalog", path.join(root, "data/emoji.json")], { stdio: ["pipe", "pipe", "pipe"] });
        const exited = new Promise(resolve => child.once("exit", (code, signal) => resolve({ code, signal })));
        let stderr = "";
        child.stderr.on("data", data => { stderr += data; });
        const lines = createInterface({ input: child.stdout })[Symbol.asyncIterator]();
        const ready = JSON.parse((await lines.next()).value);
        assert.equal(ready.type, "ready");
        const startup = performance.now() - started;
        const envelope = { v: 1, profile: workload.profile, epoch: 1, revision: 1 };
        const send = async command => {
            child.stdin.write(JSON.stringify({ ...envelope, ...command }) + "\n");
            const frame = await lines.next();
            assert.equal(frame.done, false, stderr);
            const reply = JSON.parse(frame.value);
            assert.notEqual(reply.type, "error", reply.error);
            return reply;
        };
        try {
            await send({ type: "begin" });
            for (let i = 0; i < workload.rows.length; i += 100) await send({ type: "chunk", rows: workload.rows.slice(i, i + 100) });
            await send({ type: "commit" });
            let request = 0;
            for (let i = 0; i < 50; i++) {
                const query = workload.queries[i % workload.queries.length];
                workload.reference(query);
                await send({ type: "search", request: ++request, ...query });
            }
            const js = [], pipe = [];
            for (let i = 0; i < 500; i++) {
                const query = workload.queries[i % workload.queries.length];
                const measureJS = () => { const start = performance.now(); workload.reference(query); js.push(performance.now() - start); };
                const measureGo = async () => { const start = performance.now(); await send({ type: "search", request: ++request, ...query }); pipe.push(performance.now() - start); };
                if (i % 2) { await measureGo(); measureJS(); } else { measureJS(); await measureGo(); }
            }
            const memory = fs.readFileSync(`/proc/${child.pid}/smaps_rollup`, "utf8");
            const number = name => Number(memory.match(new RegExp(`^${name}:\\s+(\\d+)`, "m"))[1]);
            t.diagnostic(JSON.stringify({ workload: workload.name, observations: 500, node_reference: quantiles(js), go_pipe_roundtrip: quantiles(pipe), startup_ready_one_observation_ms: Number(startup.toFixed(3)), helper_pss_kib: number("Pss"), helper_rss_kib: number("Rss"), caveat: "Node reference and attached-pipe timing; excludes Qt, QML projection/publication/rendering; helper memory only" }));
        } finally {
            child.stdin.end();
            const termination = await exited;
            assert.equal(termination.code, 0, stderr || termination.signal);
        }
    }
});
