const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const source = fs.readFileSync(path.join(__dirname, "../Services/Brightness.qml"), "utf8");

function service() {
    const writes = [];
    const hardware = { value: 50, max: 100, reads: 0 };
    const writer = { command: [], running: false };
    Object.defineProperty(writer, "running", {
        get() { return this.active || false; },
        set(on) {
            if (on) {
                assert.equal(this.active || false, false, "writes must not overlap");
                writes.push(Array.from(this.command));
            }
            this.active = on;
        }
    });
    const context = vm.createContext({
        device: "amdgpu_bl1", present: true, value: 50, maxValue: 100,
        writing: false, pendingValue: -1, rescanPending: false,
        writer, discovery: { running: false },
        valFile: { reload() { hardware.reads++; }, text() { return String(hardware.value); } },
        maxFile: { reload() {}, text() { return String(hardware.max); } },
        console: { warn() {} },
        Quickshell: { execDetached(cmd) { writes.push(Array.from(cmd)); } }
    });
    context.root = context;
    Object.defineProperty(context, "percent", { get: () => context.value / context.maxValue });
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0], context);
    return { context, hardware, writes, finish(code = 0) {
        writer.running = false;
        context.finishWrite(code);
    } };
}

function event(action = "change", device = "amdgpu_bl1") {
    return `KERNEL[19633.335587] ${action} /devices/pci/drm/card1/${device} (backlight)`;
}

test("external changes update immediately on a kernel event without a timer", () => {
    const { context: c, hardware } = service();
    hardware.value = 60;
    c.handleEvent(event());
    assert.equal(c.value, 60);
    assert.equal(hardware.reads, 1);
    assert.doesNotMatch(source, /\bTimer\s*\{/);
});

test("rapid adjustments update the UI immediately and coalesce pending writes", () => {
    const { context: c, hardware, writes, finish } = service();
    c.adjust(10);
    c.adjust(10);
    c.adjust(-5);
    assert.equal(c.value, 65);
    assert.equal(writes.length, 1);
    assert.deepEqual(writes[0], ["brightnessctl", "--quiet", "--device=amdgpu_bl1", "set", "60"]);
    hardware.value = 60;
    c.handleEvent(event());
    assert.equal(c.value, 65, "old hardware event must not overwrite the requested value");
    finish();
    assert.equal(writes.length, 2);
    assert.equal(writes[1].at(-1), "65");
    hardware.value = 65;
    finish();
    assert.equal(c.writing, false);
    assert.equal(c.value, 65);
});

test("failed writes discard pending targets and restore the hardware value", () => {
    const { context: c, writes, finish } = service();
    c.adjust(10);
    c.adjust(10);
    finish(1);
    assert.equal(c.value, 50);
    assert.equal(c.pendingValue, -1);
    assert.equal(c.writing, false);
    assert.equal(writes.length, 1);
    c.adjust(-10);
    assert.equal(writes.length, 2, "later input can retry after failure");
});

test("bounds and missing devices do not create redundant writes", () => {
    const { context: c, writes } = service();
    c.present = false;
    c.adjust(10);
    assert.equal(writes.length, 0);
    c.present = true;
    c.value = 100;
    c.adjust(10);
    assert.equal(writes.length, 0);
    c.value = 1;
    c.adjust(-10);
    assert.equal(writes.length, 0);
    c.adjust(NaN);
    assert.equal(writes.length, 0);
});

test("monitor headers and unrelated devices do not refresh brightness", () => {
    const { context: c, hardware } = service();
    c.handleEvent("monitor will print the received events for:");
    c.handleEvent(event("change", "other_backlight"));
    assert.equal(hardware.reads, 0);
    c.handleEvent("KERNEL - the kernel uevent");
    assert.equal(c.discovery.running, true, "discover only after the listener is ready");
});

test("hotplug rediscovers devices and invalidates targets for removed devices", () => {
    const { context: c, writes, finish } = service();
    c.adjust(10);
    c.adjust(10);
    c.handleEvent(event("remove"));
    assert.equal(c.discovery.running, true);
    c.selectDevice("");
    assert.equal(c.present, false);
    assert.equal(c.pendingValue, -1);
    finish(1);
    assert.equal(writes.length, 1);
    c.selectDevice("amdgpu_bl2\n");
    assert.equal(c.present, true);
    assert.equal(c.device, "amdgpu_bl2");
});

test("setPercent writes an absolute value, floors at 1%, and rejects junk", () => {
    const { context: c, writes, finish } = service();
    c.setPercent(25);
    assert.equal(c.value, 25);
    assert.equal(writes[0].at(-1), "25");
    finish();
    c.setPercent(0);
    assert.equal(c.value, 1);
    assert.equal(writes[1].at(-1), "1");
    c.setPercent(NaN);
    assert.equal(c.value, 1);
    assert.equal(writes.length, 2);
});

test("popup toggle closes peers, refuses a lock, and clears anchors", () => {
    const { context: c } = service();
    c.open = false;
    c.Lock = { locked: false };
    c.Audio = { close() { c.audioClosed = true; } };
    c.Network = { close() { c.netClosed = true; } };
    c.Power = { close() { c.powerClosed = true; } };
    c.Resources = { close() { c.resClosed = true; } };
    c.NightLight = { refresh() { c.nightRefreshed = true; } };
    const win = {};
    assert.equal(c.toggle(win, win, win), true);
    assert.equal(c.open, true);
    assert.equal(c.anchorControl, win);
    assert.equal(c.audioClosed, true);
    assert.equal(c.nightRefreshed, true);
    c.close();
    assert.equal(c.open, false);
    assert.equal(c.anchorItem, null);
    c.Lock.locked = true;
    assert.equal(c.toggle(win, win, win), false);
    c.Lock.locked = false;
    c.present = false;
    assert.equal(c.toggle(win, win, win), false);
});
