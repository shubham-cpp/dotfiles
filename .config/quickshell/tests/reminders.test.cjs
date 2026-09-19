const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const test = require('node:test');

function service() {
    const commands = [];
    const c = vm.createContext({
        items: [], gen: 0, maxTimerMs: 2147483647,
        Quickshell: { execDetached(command) { commands.push(command); }, shellPath(p) { return '/shell/' + p; } },
        Qt: { formatDateTime() { return 'time'; }, callLater() {} },
        store: { setText() {} }, wake: { stop() {}, restart() {} },
        scheduler: { running: false }, scheduledCommand: [], error: '',
        notifier: { running: false }, delivery: null, retryAfter: 0
    });
    c.root = c;
    for (const m of fs.readFileSync('Services/Reminders.qml', 'utf8').matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(m[0].replace(/\): void/g, ')'), c);
    return { c, commands };
}

test('reminder timestamps preserve explicit offsets and use IST for local input', () => {
    const { c } = service();
    for (const text of ['2026-09-12T18:30:00Z', '2026-09-12T18:30:00-04:00', '2026-09-12T18:30:00+05:30'])
        assert.equal(Date.parse(c.parseLocal(text)), Date.parse(text));
    assert.equal(Date.parse(c.parseLocal('2026-09-12 18:30')), Date.parse('2026-09-12T18:30:00+05:30'));
    assert.match(c.isoIst(new Date('2026-09-12T18:30:00Z')), /^2026-09-13T00:00:00/);
});

test('invalid reminder dates are rejected instead of silently rescheduled', () => {
    const { c } = service();
    for (const text of ['nonsense', '2026-02-30 12:00', '2026-09-12 25:00']) {
        assert.equal(c.parseLocal(text), '');
        assert.equal(c.add('bad', text, false), '');
    }
    assert.equal(c.items.length, 0);
});

test('new reminders reject past deadlines and retain bounded descriptions', () => {
    const { c } = service();
    assert.equal(c.add('past', '2026-01-01 00:00', false, ''), '');
    assert.match(c.error, /future/);
    const id = c.add('future', '2099-01-01 00:00', true, 'details');
    assert.ok(id);
    assert.equal(c.items[0].description, 'details');
    assert.equal(JSON.parse(c.activeJson())[0].id, id);
    assert.equal(c.remove('missing'), false);
    assert.equal(c.remove(id), true);
});

test('reminder actions only change an existing fired reminder', () => {
    const { c } = service();
    c.items = [{ id: 'r_1', title: 'test', at: '2026-09-12T00:00:00+05:30', fired: true }];
    c.handleAction('missing', 'snooze');
    assert.equal(c.items.length, 1);
    c.handleAction('r_1', 'snooze');
    assert.equal(c.items[0].fired, false);
    assert.ok(Math.abs(Date.parse(c.items[0].at) - Date.now() - 600000) < 1000);
    c.handleAction('r_1', 'done');
    assert.equal(c.items.length, 1, 'stale action must not delete a snoozed reminder');
    c.items[0].fired = true;
    c.handleAction('r_1', 'done');
    assert.equal(c.items.length, 0);
});

test('systemd deadline uses the absolute instant, independent of host timezone', () => {
    const { c } = service();
    c.items = [{ id: 'r_1', title: 'test', at: '2099-09-12T18:30:00-04:00', fired: false }];
    c.schedule();
    assert.ok(c.scheduler.command.some(arg => String(arg).includes('2099-09-12 22:30:00 UTC')));
});

test('reminders remain pending until delivery succeeds and failures retry once after a delay', () => {
    const { c } = service();
    c.items = [{ id: 'r_1', title: 'test', at: '2026-01-01T00:00:00Z', fired: false }];
    c.fire();
    assert.equal(c.items[0].fired, false);
    assert.equal(c.notifier.running, true);
    c.notifier.running = false;
    c.finishDelivery(1);
    assert.equal(c.items[0].fired, false);
    assert.ok(c.retryAfter > Date.now());
    c.fire();
    assert.equal(c.notifier.running, false);
    c.retryAfter = 0;
    c.fire();
    c.notifier.running = false;
    c.finishDelivery(0);
    assert.equal(c.items[0].fired, true);
    assert.equal(c.error, '');
});
