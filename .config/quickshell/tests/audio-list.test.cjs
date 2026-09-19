const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const test = require("node:test");
const list = vm.createContext({});
vm.runInContext(fs.readFileSync(`${__dirname}/../Common/AudioList.js`, "utf8").replace(/^\.pragma library\s*/, ""), list);
let serial = 0;
function node(name, extra = {}) {
    return { id: ++serial, name, description: name, ready: true, audio: { volume: .5, muted: false },
        isStream: false, isSink: true, properties: {}, ...extra };
}
function stream(name, extra = {}) { return node(name, { isStream: true, isSink: true, ...extra }); }
function link(source, target) { return { source, target }; }
function model() {
    const rows = [];
    const writes = [];
    return { writes, get count() { return rows.length; }, get: i => rows[i],
        insert: (i, row) => rows.splice(i, 0, {...row}), remove: i => rows.splice(i, 1),
        move: (i, j) => rows.splice(j, 0, rows.splice(i, 1)[0]),
        setProperty(i, key, value) { writes.push({ i, key, value }); rows[i][key] = value; } };
}
test("playback keeps paused, muted and unknown streams but excludes capture, monitors and filters", () => {
    assert.equal(list.playback(stream("unknown", { ready: false })), true);
    assert.equal(list.playback(stream("muted", { audio: { volume: .4, muted: true } })), true);
    assert.equal(list.playback(stream("capture", { isSink: false })), false);
    assert.equal(list.playback(stream("monitor", { properties: { "stream.monitor": "true" } })), false);
    assert.equal(list.playback(stream("monitor", { properties: { "media.category": "Monitor" } })), false);
    assert.equal(list.playback(stream("filter", { properties: { "media.role": "Filter" } })), false);
    assert.equal(list.playback(node("device")), false);
    assert.equal(list.playback(stream("video", { audio: null })), false);
});
test("routes follow connections rather than defaults and retain distinct same-app streams", () => {
    const a=stream("Firefox"), b=stream("Firefox"), headphones=node("Headphones"), hdmi=node("HDMI");
    const rows=list.rows([a,b,headphones,hdmi], [link(a,hdmi),link(b,headphones)], headphones);
    assert.equal(rows.length,2);
    assert.equal(rows[0].node,b);
    assert.equal(rows[0].groupLabel,"Headphones · Default");
    assert.equal(rows[1].groupLabel,"HDMI");
    assert.notEqual(rows[0].subtitle,rows[1].subtitle);
});
test("fan-out has one row and deduplicates stereo links", () => {
    const a=stream("App"), x=node("X"), y=node("Y");
    const links=[link(a,x),link(a,x),link(a,y)];
    const rows=list.rows([a,x,y],links,x);
    assert.equal(rows.length,1);
    assert.equal(rows[0].group,"multiple");
    assert.equal(rows[0].routeDetail,"X, Y");
});

test("converging processing paths are not mistaken for unknown routing", () => {
    const a=stream("App"), x=node("Filter X",{isSink:false}), y=node("Filter Y",{isSink:false});
    const merge=node("Merge",{isSink:false}), sink=node("Speakers");
    const result=list.destination(a,[a,x,y,merge,sink],[link(a,x),link(a,y),link(x,merge),link(y,merge),link(merge,sink)]);
    assert.equal(result.output,sink);
});
test("intermediate routing stops at virtual sinks and cycles are unresolved", () => {
    const a=stream("App"), filter=node("Filter",{isSink:false}), sink=node("Virtual"), physical=node("Speaker");
    assert.equal(list.destination(a,[a,filter,sink,physical],[link(a,filter),link(filter,sink),link(sink,physical)]).output,sink);
    assert.equal(list.destination(a,[a,filter],[link(a,filter),link(filter,a)]).key,"other");
    assert.equal(list.destination(a,[a,filter],[link(a,filter)]).key,"other");
    assert.equal(list.destination(a,[a],[]).key,"unconnected");
    assert.equal(list.destination(a,[a],[link(a,null)]).key,"other");
});
test("frozen reconciliation defers moves but removes stale objects and marks changed routes", () => {
    const a=stream("Alpha"), b=stream("Beta"), x=node("X"), y=node("Y"), m=model();
    list.sync(m,list.rows([a,b,x,y],[link(a,x),link(b,y)],x),false);
    list.sync(m,list.rows([a,b,x,y],[link(a,y),link(b,x)],x),true);
    assert.equal(m.get(0).node,a);
    assert.equal(m.get(0).routeChanging,true);
    const replacement=stream("Replacement",{id:a.id});
    list.sync(m,list.rows([replacement,b,x,y],[link(replacement,x),link(b,x)],x),true);
    assert.equal(m.count,1);
    assert.equal(m.get(0).node,b);
    list.sync(m,list.rows([replacement,b,x,y],[link(replacement,x),link(b,x)],x),false);
    assert.equal(m.count,2);
    assert.ok([m.get(0).node,m.get(1).node].includes(replacement));
});
test("device labels disambiguate identical descriptions and exclude input monitors", () => {
    const a=node("a",{description:"USB"}),b=node("b",{description:"USB"}),mic=node("mic",{isSink:false});
    const monitor=node("sink.monitor",{isSink:false});
    assert.equal(list.devices([a,b,mic,monitor],false).map(d=>d.label).join(","),"USB · a,USB · b");
    assert.equal(list.devices([a,b,mic,monitor],true).length,1);
});

test("unchanged device snapshots retain menu identity across stream refreshes", () => {
    const a=node("Output"), b=node("Other"), before=list.devices([a,b],false);
    assert.equal(list.sameDevices(before,list.devices([a,b,stream("New app")],false)),true);
    assert.equal(list.sameDevices(before,list.devices([a],false)),false);
    const replacement=node("Other",{id:b.id});
    assert.equal(list.sameDevices(before,list.devices([a,replacement],false)),false);
});

test("frozen route flags publish only changes and clear after reconciliation", () => {
    const a = stream("App"), x = node("X"), y = node("Y"), m = model();
    const initial = list.rows([a, x, y], [link(a, x)], x);
    list.sync(m, initial, false);
    list.sync(m, initial, true);
    assert.equal(m.writes.length, 0);
    const rerouted = list.rows([a, x, y], [link(a, y)], x);
    list.sync(m, rerouted, true);
    list.sync(m, rerouted, true);
    assert.deepEqual(m.writes, [{ i: 0, key: "routeChanging", value: true }]);
    list.sync(m, rerouted, false);
    assert.equal(m.get(0).routeChanging, false);
    const writes = m.writes.length;
    list.sync(m, rerouted, true);
    assert.equal(m.writes.length, writes);
});
