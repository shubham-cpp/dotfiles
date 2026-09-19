const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const test = require("node:test");

// Execute the service's actual handlers. Native objects and QML timers are fakes;
// focus, property bindings and real NetworkManager behavior need runtime checks.
const source = fs.readFileSync(path.join(__dirname, "../Services/Network.qml"), "utf8");
const handlers = source.match(/^    function \w+\([^\n]*\) \{[\s\S]*?^    \}/gm).join("\n");

function fixture() {
    const device = { name: "wlan-test", nmManaged: true, networks: { values: [] }, scannerEnabled: false };
    const context = {
        open: true, usableWifi: true, wifiDevice: device, wifiDevices: [device],
        wifiRadio: true, wifiHardwareEnabled: true, backendAvailable: true,
        selected: null, promptNet: null, pendingNet: null, pendingKind: "", pendingFailure: -1,
        pendingDevice: null,
        pendingObserved: false, pendingSlow: false, failNet: null, retryNet: null,
        failMessage: "", scannerDevice: null, listGen: 0,
        connected: false, rateConsumers: [], iface: "wlan-test",
        downBps: 0, upBps: 0, _lastRx: -1, _lastTx: -1, _lastMs: 0,
        savedNets: [], otherNets: [], anchorWindow: null, anchorItem: null,
        watchdog: { interval: 0, restart() {}, stop() {} },
        discovery: { restart() {}, stop() {} },
        WifiSecurityType: { Open: 0, Owe: 1, WpaPsk: 2, Wpa2Psk: 3, Sae: 4 },
        ConnectionState: { Connected: 1, Connecting: 2, Disconnected: 3, Disconnecting: 4 },
        ConnectionFailReason: { Unknown: 0, NoSecrets: 1, WifiAuthTimeout: 2, WifiNetworkLost: 3 },
        Networking: { wifiEnabled: true, devices: { values: [device] } },
        Qt: { callLater() {} }
    };
    context.root = context;
    Object.defineProperty(context, "busy", { get: () => context.pendingKind.length > 0 });
    Object.defineProperty(context, "samplingRates", { get: () => context.connected && (context.open || context.rateConsumers.length > 0) });
    vm.createContext(context);
    vm.runInContext(handlers, context);
    context.refreshIdentity = () => {};
    context.bumpList = () => {};
    const calls = [];
    function net(extra = {}) {
        const n = { name: "Test", device, connected: false, known: false,
            stateChanging: false, state: 3, security: 3,
            connect() { calls.push("connect"); },
            connectWithPsk() { calls.push("psk"); },
            disconnect() { calls.push("disconnect"); },
            forget() { calls.push("forget"); }, ...extra };
        device.networks.values.push(n);
        return n;
    }
    return { context, device, calls, net };
}

test("accepted password collapses the editor before the backend call and submits once", () => {
    const { context: s, net, calls } = fixture();
    const n = net({ connectWithPsk() {
        assert.equal(s.promptNet, null);
        assert.equal(s.pendingNet, n);
        calls.push("psk");
    } });
    s.promptNet = n;
    assert.equal(s.submitPsk("test-password"), true);
    assert.equal(s.submitPsk("test-password"), false);
    assert.deepEqual(calls, ["psk"]);
});

test("invalid WPA input remains editable; raw PSK and short SAE are treated separately", () => {
    const { context: s, net, calls } = fixture();
    s.promptNet = net();
    assert.equal(s.submitPsk("short"), false);
    assert.ok(s.promptNet);
    assert.equal(calls.length, 0);
    assert.equal(s.submitPsk("a".repeat(64)), true);
    const sae = fixture();
    sae.context.promptNet = sae.net({ security: 4 });
    assert.equal(sae.context.submitPsk("short"), true);
});

test("close preserves pending ownership and failures are scoped to the target", () => {
    const { context: s, net } = fixture();
    const a = net(), b = net({ name: "Other" });
    s.promptNet = a;
    s.submitPsk("test-password");
    s.close();
    assert.equal(s.pendingNet, a);
    s.handleFail(b, 1);
    assert.equal(s.pendingNet, a);
    assert.equal(s.failMessage, "");
    s.handleFail(a, 1);
    assert.equal(s.pendingNet, null);
    assert.equal(s.retryNet, a);
    assert.equal(s.promptNet, null);
    assert.ok(s.failMessage.length);
});

test("selecting a connected network never disconnects; explicit action does", () => {
    const { context: s, net, calls } = fixture();
    const n = net({ connected: true, known: true, state: 1 });
    s.activate(n);
    assert.deepEqual(calls, []);
    s.disconnectNet(n);
    assert.deepEqual(calls, ["disconnect"]);
});

test("success clears pending and errors even while the popup is closed", () => {
    const { context: s, net } = fixture();
    const n = net({ known: true });
    s.activate(n);
    s.close();
    n.connected = true;
    n.state = 1;
    s.observePending();
    assert.equal(s.pendingNet, null);
    assert.equal(s.failMessage, "");
});

test("an unresolved watchdog expiry never enables a duplicate activation", () => {
    const { context: s, net, calls } = fixture();
    const n = net({ known: true });
    s.activate(n);
    s.pendingTimeout();
    s.activate(n);
    assert.equal(s.pendingNet, n);
    assert.equal(s.pendingSlow, true);
    assert.deepEqual(calls, ["connect"]);
});

test("scanner ownership transfers and releases the old device", () => {
    const { context: s, device } = fixture();
    s.setScanning(true);
    assert.equal(device.scannerEnabled, true);
    const replacement = { scannerEnabled: false };
    s.wifiDevice = replacement;
    s.setScanning(true);
    assert.equal(device.scannerEnabled, false);
    assert.equal(replacement.scannerEnabled, true);
    s.close();
    assert.equal(replacement.scannerEnabled, false);
});

test("failure reason arriving before terminal state does not permit early retry", () => {
    const { context: s, net, calls } = fixture();
    const n = net({ known: true });
    s.activate(n);
    n.state = 2;
    n.stateChanging = true;
    s.handleFail(n, 1);
    assert.equal(s.busy, true);
    assert.equal(s.promptNet, null);
    s.activate(n);
    assert.deepEqual(calls, ["connect"]);
    n.state = 3;
    n.stateChanging = false;
    s.observePending();
    assert.equal(s.busy, false);
    assert.equal(s.promptNet, n);
});

test("a late detailed failure upgrades a generic result until another attempt starts", () => {
    const { context: s, net } = fixture();
    const n = net({ known: true });
    s.activate(n);
    s.pendingObserved = true;
    s.observePending();
    assert.equal(s.pendingNet, null);
    s.handleFail(n, 1);
    assert.equal(s.retryNet, n);
    const other = net({ name: "Other", known: true });
    s.activate(other);
    s.handleFail(n, 1);
    assert.equal(s.pendingNet, other);
    assert.equal(s.promptNet, null);
});

test("loss of the target object does not cancel its outstanding native request", () => {
    const { context: s, device, net, calls } = fixture();
    s.activate(net({ known: true }));
    s.losePendingTarget();
    assert.equal(s.pendingNet, null);
    assert.equal(s.pendingDevice, device);
    assert.equal(s.busy, true);
    s.activate(net({ known: true }));
    assert.deepEqual(calls, ["connect"]);
    s.usableWifi = false;
    s.reconcileDevice();
    assert.equal(s.busy, false);
});

test("traffic demand is shared by visible bars and the popup", () => {
    const { context: s } = fixture();
    s.connected = true;
    s.open = false;
    const firstBar = {}, secondBar = {};
    s.setRateConsumer(firstBar, true);
    s.setRateConsumer(firstBar, true);
    s.setRateConsumer(secondBar, true);
    assert.equal(s.rateConsumers.length, 2);
    assert.equal(s.samplingRates, true);
    s.setRateConsumer(firstBar, false);
    assert.equal(s.samplingRates, true);
    s.setRateConsumer(secondBar, false);
    assert.equal(s.samplingRates, false);
    s.open = true;
    assert.equal(s.samplingRates, true);
    s.connected = false;
    assert.equal(s.samplingRates, false);
});

test("hidden rates do no reads and resuming starts with a fresh interval", () => {
    const { context: s } = fixture();
    let reads = 0, now = 1000, rx = 100, tx = 200;
    s.Date = {now: () => now};
    s.rxFile = {reload() { reads++; }, text: () => String(rx)};
    s.txFile = {reload() { reads++; }, text: () => String(tx)};
    s.connected = true;
    s.open = false;
    s.pollBytes();
    assert.equal(reads, 0);
    s.open = true;
    s.pollBytes();
    assert.equal(s.downBps, 0);
    now += 2000; rx += 400; tx += 600;
    s.pollBytes();
    assert.equal(s.downBps, 200);
    assert.equal(s.upBps, 300);
    s.open = false;
    s.pollBytes();
    assert.equal(reads, 4);
    assert.equal(s._lastRx, -1);
    now += 20000; rx += 100000;
    s.open = true;
    s.pollBytes();
    assert.equal(s.downBps, 0);
    now += 2000; rx += 100;
    s.pollBytes();
    assert.equal(s.downBps, 50);
});
