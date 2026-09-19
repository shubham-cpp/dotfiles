const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const source = fs.readFileSync(path.join(__dirname, "../Services/Resources.qml"), "utf8");
const handlers = source.match(/^    function \w+\([^\n]*\) \{[\s\S]*?^    \}/gm).join("\n");

function fixture() {
    const writes = [];
    const service = {open: true, available: true, paused: false, apps: [], errorText: "",
        reader: {running: true, write: line => writes.push(JSON.parse(line))}};
    service.root = service;
    vm.createContext(service);
    vm.runInContext(handlers, service);
    return {service, writes};
}

test("ending from a paused view submits its captured identities unchanged", () => {
    const {service, writes} = fixture();
    service.paused = true;
    const captured = JSON.stringify([{pid: 12, started: 34}]);
    service.apps = [{key: "app", members: JSON.stringify([{pid: 56, started: 78}])}];
    service.endApplication("app", captured, false);
    assert.deepEqual(writes, [
        {action: "pause", paused: false},
        {action: "end", key: "app", members: [{pid: 12, started: 34}], force: false}
    ]);
});
