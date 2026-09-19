// Synchronous adapter for existing domain tests. It crosses the real Go wire
// interface; separate Qt tests exercise asynchronous scheduling and lifecycle.
const {spawnSync} = require("node:child_process");
const path = require("node:path");
const root = path.join(__dirname, "..");
function search(profile, rows, query) {
    let request = 0;
    const envelope = type => ({v: 1, type, profile, epoch: 1, revision: 1, request: ++request});
    const frames = [envelope("begin")];
    let chunk = [], bytes = 0;
    for (const row of rows) {
        const size = Buffer.byteLength(JSON.stringify(row));
        if (chunk.length && bytes + size > 32000) {
            frames.push({...envelope("chunk"), rows: chunk}); chunk = []; bytes = 0;
        }
        chunk.push(row); bytes += size;
    }
    if (chunk.length) frames.push({...envelope("chunk"), rows: chunk});
    frames.push(envelope("commit"), {...envelope("search"), ...query});
    const result = spawnSync(path.join(root, ".local/bin/qs-search"), ["--emoji-catalog", path.join(root, "data/emoji.json")], {
        input: frames.map(JSON.stringify).join("\n") + "\n", encoding: "utf8", timeout: 10000, maxBuffer: 4 * 1024 * 1024
    });
    if (result.status !== 0) throw new Error(result.stderr || result.error || "search helper failed");
    const reply = JSON.parse(result.stdout.trim().split("\n").at(-1));
    if (reply.type !== "results") throw new Error(JSON.stringify(reply));
    return reply.keys || [];
}
function adapter(context) {
    const catalogs = {};
    return {
        setCatalog(profile, rows) { catalogs[profile] = rows; context.searching = true; },
        search(profile, query) { context.searching = true; context.acceptSearch(search(profile, catalogs[profile] || [], query)); },
        updateReplay(profile, changes) {},
        release(profile) { delete catalogs[profile]; }
    };
}
module.exports = {search, adapter};
