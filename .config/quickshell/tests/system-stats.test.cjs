const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

const source = fs.readFileSync(path.join(__dirname, "../Services/SystemStats.qml"), "utf8");

function service() {
    const context = vm.createContext({ previousCpu: null, cpuPercent: -1, memoryUsedKiB: -1, temperature: NaN });
    context.root = context;
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0], context);
    return context;
}

test("RAM excludes available memory and changes units at one GB", () => {
    const stats = service();
    stats.updateMemory("MemTotal: 2097152 kB\nMemFree: 100 kB\nMemAvailable: 1572864 kB\n");
    assert.equal(stats.memoryUsedKiB, 524288);
    assert.equal(stats.memoryTotalKiB, 2097152);
    assert.equal(stats.formatMemory(stats.memoryUsedKiB), "512 MB");
    assert.equal(stats.formatMemory(1048575), "1023 MB");
    assert.equal(stats.formatMemory(1048576), "1.0 GB");
    assert.equal(stats.formatMemory(1572864), "1.5 GB");
    assert.equal(stats.formatMemory(0), "0 MB");
    stats.updateMemory("MemTotal: 2097152 kB\n");
    assert.equal(stats.formatMemory(stats.memoryUsedKiB), "--");
});

test("CPU uses successive aggregate samples and does not double count guest time", () => {
    const stats = service();
    stats.updateCpu("cpu 100 0 100 800 0 0 0 0 50 0\ncpu0 100 0 100 800 0 0 0 0 50 0\n");
    assert.equal(stats.cpuPercent, -1, "one sample cannot measure current CPU usage");
    stats.updateCpu("cpu 140 0 110 840 10 0 0 0 70 0\n");
    assert.equal(stats.cpuPercent, 50);
    stats.updateCpu("cpu 140 0 110 940 10 0 0 0 70 0\n");
    assert.equal(stats.cpuPercent, 0);
    stats.updateCpu("cpu 240 0 110 940 10 0 0 0 70 0\n");
    assert.equal(stats.cpuPercent, 100);
});

test("invalid or reset CPU counters require a new baseline and recover", () => {
    const stats = service();
    stats.updateCpu("cpu 100 0 100 800 0 0 0 0\n");
    stats.updateCpu("cpu 10 0 10 80 0 0 0 0\n");
    assert.equal(stats.cpuPercent, -1);
    stats.updateCpu("cpu 20 0 20 160 0 0 0 0\n");
    assert.ok(Math.abs(stats.cpuPercent - 20) < 0.001);
    stats.updateCpu("cpu broken\n");
    assert.equal(stats.cpuPercent, -1);
    assert.equal(stats.previousCpu, null);
    stats.updateCpu("cpu 30 0 30 240 0 0 0 0\n");
    assert.equal(stats.cpuPercent, -1);
});

test("temperature converts millidegrees and clears unavailable readings", () => {
    const stats = service();
    stats.updateTemperature(" 51250\n");
    assert.equal(stats.temperature, 51.25);
    stats.updateTemperature("0\n");
    assert.equal(stats.temperature, 0);
    for (const text of ["", "unknown", "50000 broken"]) {
        stats.updateTemperature(text);
        assert.ok(Number.isNaN(stats.temperature));
    }
});
