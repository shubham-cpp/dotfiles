const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { test } = require('node:test');

const source = fs.readFileSync(path.join(__dirname, '../Services/Logind.qml'), 'utf8');
const handler = source.match(/function handleLine\(line\): void \{([\s\S]*?)\n    \}\n\n    Process/)[1];

function fixture() {
    const reports = [];
    const context = {
        ready: false, preparingForSleep: false, token: '', errorText: '',
        bridge: { running: true },
        Lock: { activations: 0, activate() { this.activations++; } },
        reportState() {
            reports.push({ ready: context.ready, preparingForSleep: context.preparingForSleep });
        }
    };
    vm.createContext(context);
    vm.runInContext(`function handleLine(line) {${handler}}`, context);
    return { context, reports, send(message) { context.handleLine(JSON.stringify(message)); } };
}

test('awake replacement bridge clears a missed resume before acknowledging ready', () => {
    const { context, reports, send } = fixture();
    send({ event: 'sleep', token: 'old' });
    send({ event: 'lost', token: 'old' });
    assert.equal(context.preparingForSleep, true);
    send({ event: 'ready', token: 'replacement', recover: true, preparingForSleep: false });
    assert.equal(context.ready, true);
    assert.equal(context.preparingForSleep, false);
    assert.equal(context.errorText, '');
    assert.equal(context.token, 'replacement');
    assert.deepEqual(reports.at(-1), { ready: true, preparingForSleep: false });
    assert.equal(context.Lock.activations, 3);
});

test('initial sleeping state locks before its acknowledgement', () => {
    const { context, reports, send } = fixture();
    send({ event: 'ready', token: 'initial', recover: false, preparingForSleep: true });
    assert.equal(context.Lock.activations, 1);
    assert.deepEqual(reports, [{ ready: true, preparingForSleep: true }]);
    send({ event: 'resume', token: 'resumed' });
    assert.equal(context.preparingForSleep, false);
    assert.equal(context.Lock.activations, 2);
});

test('old or malformed readiness cannot clear the sleep guard', () => {
    for (const preparingForSleep of [undefined, null, 0, 'false']) {
        const { context, reports, send } = fixture();
        send({ event: 'ready', token: 'old-helper', preparingForSleep });
        assert.equal(context.ready, false);
        assert.equal(context.preparingForSleep, true);
        assert.equal(context.bridge.running, false);
        assert.equal(context.Lock.activations, 1);
        assert.equal(reports.length, 0);
    }
});

test('null and tokenless frames leave session state unchanged', () => {
    const { context, reports, send } = fixture();
    for (const message of [null, {}, [], { event: 'ready', preparingForSleep: false }])
        send(message);
    assert.equal(context.ready, false);
    assert.equal(context.preparingForSleep, false);
    assert.equal(context.Lock.activations, 0);
    assert.equal(reports.length, 0);
});
