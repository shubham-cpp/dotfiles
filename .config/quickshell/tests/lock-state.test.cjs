const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const { test } = require('node:test');
const source = fs.readFileSync(path.join(__dirname, '../Services/Lock.qml'), 'utf8');

function body(marker) {
    const start = source.indexOf(marker);
    assert.notEqual(start, -1, marker);
    const open = source.indexOf('{', start);
    let depth = 1;
    for (let i = open + 1; i < source.length; i++) {
        if (source[i] === '{') depth++;
        if (source[i] === '}') depth--;
        if (!depth) return source.slice(open + 1, i);
    }
    throw new Error('Unclosed block');
}

function fixture() {
    const context = {
        locked: false, secure: false, currentText: '', unlockInProgress: false,
        showFailure: false, errorText: '', focusGen: 0, epoch: 0, attemptEpoch: -1,
        authReply: '', auth: { running: false },
        authTimeout: { stop() {}, restart() {} },
        Logind: { preparingForSleep: false, reportState() {} }
    };
    context.root = context;
    vm.createContext(context);
    for (const [name, args] of Object.entries({
        cancelAuthentication: '', activate: '', setSecure: 'on', focus: '',
        tryUnlock: '', finishAttempt: 'exitCode'
    })) vm.runInContext(`function ${name}(${args}) { ${body('function ' + name + '(')} }`, context);
    return context;
}

function attempt(c) {
    c.activate();
    c.setSecure(true);
    c.currentText = 'test fixture';
    c.tryUnlock();
}

test('only a successful current process result can unlock', () => {
    for (const [exit, reply, expected] of [[0, 'QS_AUTH_SUCCESS', false], [1, 'QS_AUTH_SUCCESS', true], [0, '', true], [2, 'QS_AUTH_UNAVAILABLE', true]]) {
        const c = fixture(); attempt(c); c.authReply = reply; c.auth.running = false; c.finishAttempt(exit);
        assert.equal(c.locked, expected);
        assert.equal(c.currentText, '');
        assert.equal(c.unlockInProgress, false);
    }
});

test('renewed lock invalidates an earlier successful attempt', () => {
    const c = fixture(); attempt(c); c.activate(); c.authReply = 'QS_AUTH_SUCCESS'; c.finishAttempt(0);
    assert.equal(c.locked, true);
    assert.equal(c.unlockInProgress, false);
    assert.equal(c.currentText, '');
});

test('sleep and lost compositor ownership reject otherwise successful auth', () => {
    for (const state of ['sleep', 'insecure']) {
        const c = fixture(); attempt(c); c.authReply = 'QS_AUTH_SUCCESS';
        if (state === 'sleep') c.Logind.preparingForSleep = true;
        else c.setSecure(false);
        c.finishAttempt(0);
        assert.equal(c.locked, true);
    }
});

test('no authentication starts while unlocked, insecure, sleeping or empty', () => {
    for (const state of ['unlocked', 'insecure', 'sleep', 'empty']) {
        const c = fixture(); c.locked = state !== 'unlocked'; c.secure = state !== 'insecure';
        c.Logind.preparingForSleep = state === 'sleep'; c.currentText = state === 'empty' ? '' : 'fixture';
        c.tryUnlock(); assert.equal(c.auth.running, false);
    }
});

test('timed-out or duplicate completions cannot unlock', () => {
    const c = fixture(); attempt(c);
    vm.runInContext(body('onTriggered:'), c);
    c.authReply = 'QS_AUTH_SUCCESS'; c.finishAttempt(0);
    assert.equal(c.locked, true);
    assert.equal(c.unlockInProgress, false);
    assert.match(c.errorText, /timed out/);
});

test('production reload is explicit and denied during lock or sleep', () => {
    const shell = fs.readFileSync(path.join(__dirname, '../shell.qml'), 'utf8');
    assert.match(shell, /Quickshell\.watchFiles = false/);
    const guarded = shell.match(/function reload\(\): bool \{([\s\S]*?)\n        \}/)[1];
    for (const flag of ['locked', 'unlockInProgress', 'sleep', 'notReady', 'safe']) {
        let calls = 0;
        const context = {Lock: {locked: flag === 'locked', unlockInProgress: flag === 'unlockInProgress'},
            Logind: {ready: flag !== 'notReady', preparingForSleep: flag === 'sleep'}, Quickshell: {reload() {calls++;}}};
        vm.createContext(context);
        const result = vm.runInContext(`(() => {${guarded}})()`, context);
        assert.equal(result, flag === 'safe'); assert.equal(calls, flag === 'safe' ? 1 : 0);
    }
});
