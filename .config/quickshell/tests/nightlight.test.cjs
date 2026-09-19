const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const source = fs.readFileSync(path.join(__dirname, "../Services/NightLight.qml"), "utf8");

function service() {
    const launched = [];
    const killer = { command: [], running: false };
    Object.defineProperty(killer, "running", {
        get() { return this.active || false; },
        set(on) { this.active = on; }
    });
    const context = vm.createContext({
        present: true, running: false, starting: false, pid: 0, argvPid: 0,
        command: ["wlsunset", "-l", "18.5204", "-L", "73.8567", "-T", "5800", "-t", "2700"],
        killer, probe: { running: false }, which: { running: false },
        argvReader: { running: false, command: [] },
        afterStart: { restart() { context.restarted = true; } },
        Quickshell: { execDetached(cmd) { launched.push(Array.from(cmd)); } }
    });
    context.root = context;
    Object.defineProperty(context, "enabled", { get: () => context.running || context.starting });
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0], context);
    return { context, launched, killer };
}

test("pid snapshot marks running and remembers argv", () => {
    const { context: c } = service();
    c.handlePid(" 288421\n");
    assert.equal(c.running, true);
    assert.equal(c.pid, 288421);
    assert.equal(c.present, true);
    assert.match(c.argvReader.command.join(" "), /\/proc\/288421\/cmdline/);
    c.handleArgv("wlsunset\n-l\n18.5204\n-L\n73.8567\n-T\n5800\n-t\n2700\n");
    assert.equal([].concat(c.command).join(" "), "wlsunset -l 18.5204 -L 73.8567 -T 5800 -t 2700");
    assert.equal(c.argvPid, 288421);
    c.handlePid("");
    assert.equal(c.running, false);
    assert.equal(c.pid, 0);
});

test("start uses the remembered command and stop sends SIGTERM", () => {
    const { context: c, launched, killer } = service();
    c.command = ["wlsunset", "-l", "1", "-L", "2"];
    assert.equal(c.start(), true);
    assert.equal(c.starting, true);
    assert.equal(c.enabled, true);
    assert.deepEqual(launched[0], ["wlsunset", "-l", "1", "-L", "2"]);
    assert.equal(c.start(), false, "do not spawn a second process");
    assert.equal(c.stop(), true);
    assert.equal([].concat(killer.command).join(" "), "pkill -TERM -x wlsunset");
    assert.equal(killer.running, true);
    assert.equal(c.running, false);
    assert.equal(c.starting, false);
});

test("missing binary cannot start; missing pidof output is paused", () => {
    const { context: c, launched } = service();
    c.present = false;
    assert.equal(c.start(), false);
    assert.equal(launched.length, 0);
    c.handleWhich("", 1);
    assert.equal(c.present, false);
    c.handleWhich("/usr/bin/wlsunset\n", 0);
    assert.equal(c.present, true);
    c.handlePid("not-a-pid");
    assert.equal(c.running, false);
});

test("toggle pause and resume match a two-state switch", () => {
    const { context: c, launched } = service();
    c.running = true;
    c.pid = 9;
    assert.equal(c.toggle(), false);
    assert.equal(c.running, false);
    assert.equal(c.toggle(), true);
    assert.equal(c.starting, true);
    assert.equal(launched.length, 1);
});
