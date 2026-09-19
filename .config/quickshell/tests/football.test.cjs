const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const test = require('node:test');

function service() {
    const requests = [];
    class Request {
        static DONE = 4;
        constructor() { requests.push(this); }
        open() {}
        setRequestHeader() {}
        send() {}
        abort() { this.aborted = true; }
        complete(matches) {
            this.readyState = 4;
            this.status = 200;
            this.responseText = JSON.stringify({ matches });
            if (this.onreadystatechange) this.onreadystatechange();
        }
    }
    const context = vm.createContext({
        token: 'test', chip: 'All', status: '', matches: [], groups: [],
        expanded: [], expandedSeeded: false, gen: 0,
        fetchedAt: 0, staleMs: 3600000, codes: ['PL', 'PD', 'CL'], request: null,
        XMLHttpRequest: Request, requestTimeout: { restart() {}, stop() {} },
        cacheFile: { setText() {} }, Qt: { formatDate() { return 'date'; } }
    });
    context.root = context;
    const source = fs.readFileSync('Services/Football.qml', 'utf8');
    for (const match of source.matchAll(/^    function [\s\S]*?^    }/gm))
        vm.runInContext(match[0], context);
    return { context, requests };
}

function match(over) {
    return Object.assign({
        id: 1,
        competition: { code: 'PL' },
        utcDate: new Date().toISOString(),
        status: 'TIMED',
        homeTeam: { shortName: 'Arsenal', tla: 'ARS', crest: 'https://crests.football-data.org/57.svg' },
        awayTeam: { shortName: 'Chelsea', tla: 'CHE', crest: 'https://crests.football-data.org/61.png' },
        score: { fullTime: { home: null, away: null } }
    }, over);
}

test('only one football request is active and an empty successful result is cached', () => {
    const { context: c, requests } = service();
    c.ensure();
    c.ensure();
    assert.equal(requests.length, 1);
    requests[0].complete([]);
    c.ensure();
    assert.equal(requests.length, 1);
    assert.equal(c.status, '');
});

test('an expired football request cannot overwrite the next response or status', () => {
    const { context: c, requests } = service();
    c.fetchNow();
    const late = requests[0].onreadystatechange;
    c.cancelRequest();
    assert.equal(requests[0].aborted, true);
    c.fetchNow();
    requests[1].complete([]);
    const fetched = c.fetchedAt;
    requests[0].readyState = 4;
    requests[0].status = 429;
    late();
    assert.equal(c.fetchedAt, fetched);
    assert.equal(c.status, '');
});

test('same football filter and unchanged results keep existing groups', () => {
    const { context: c } = service();
    c.ingest([]);
    const groups = c.groups;
    c.setChip('All');
    c.ingest([]);
    assert.equal(c.groups, groups);
});

test('fresh cache within an hour skips a new request', () => {
    const { context: c, requests } = service();
    c.fetchedAt = Date.now();
    c.ensure();
    assert.equal(requests.length, 0);
});

test('stale cache after an hour fetches again', () => {
    const { context: c, requests } = service();
    c.fetchedAt = Date.now() - 3600001;
    c.ensure();
    assert.equal(requests.length, 1);
});

test('refresh with cached matches does not show a loading status', () => {
    const { context: c, requests } = service();
    c.ingest([match()]);
    c.status = '';
    c.fetchedAt = 0;
    c.fetchNow();
    assert.equal(requests.length, 1);
    assert.equal(c.status, '');
});

test('ingest keeps crests, short names, and expands today', () => {
    const { context: c } = service();
    c.ingest([match(), match({
        id: 2,
        utcDate: new Date(Date.now() + 86400000).toISOString(),
        homeTeam: { shortName: 'Liverpool', tla: 'LIV', crest: 'http://evil.example/1.svg' }
    })]);
    assert.equal(c.matches[0].home, 'Arsenal');
    assert.equal(c.matches[0].homeCrest, 'https://crests.football-data.org/57.svg');
    assert.equal(c.matches[0].awayCrest, 'https://crests.football-data.org/61.png');
    assert.equal(c.matches[1].homeCrest, '');
    assert.equal(c.matches[0].chip, '');
    const today = c.groups.find(g => g.today);
    assert.ok(today);
    assert.equal(today.label, 'Today');
    assert.equal(c.expanded.length, 1);
    assert.equal(c.expanded[0], today.key);
    assert.equal(c.isOpen(today.key), true);
});

test('fetch window covers IST yesterday', () => {
    const { context: c } = service();
    const w = c.window();
    const ist = new Date(Date.now() + 330 * 60000);
    const start = new Date(Date.UTC(ist.getUTCFullYear(), ist.getUTCMonth(), ist.getUTCDate() - 1) - 330 * 60000);
    const pad = n => (n < 10 ? '0' : '') + n;
    const expected = start.getUTCFullYear() + '-' + pad(start.getUTCMonth() + 1) + '-' + pad(start.getUTCDate());
    assert.equal(w.from, expected);
    assert.ok(w.to > w.from);
});

test('yesterday matches are kept, labelled, and collapsed', () => {
    const { context: c } = service();
    c.ingest([
        match({ id: 1 }),
        match({
            id: 2,
            utcDate: new Date(Date.now() - 86400000).toISOString(),
            status: 'FINISHED',
            score: { fullTime: { home: 1, away: 0 } }
        }),
        match({
            id: 3,
            utcDate: new Date(Date.now() - 3 * 86400000).toISOString(),
            status: 'FINISHED',
            score: { fullTime: { home: 2, away: 2 } }
        })
    ]);
    assert.equal(c.matches.some(m => m.id === 3), false);
    const yesterday = c.groups.find(g => g.yesterday);
    assert.ok(yesterday);
    assert.equal(yesterday.label, 'Yesterday');
    assert.equal(c.isOpen(yesterday.key), false);
    const today = c.groups.find(g => g.today);
    assert.ok(today);
    assert.equal(c.isOpen(today.key), true);
});

test('several date groups can stay open at once', () => {
    const { context: c } = service();
    c.ingest([
        match({ id: 1 }),
        match({ id: 2, utcDate: new Date(Date.now() + 86400000).toISOString() })
    ]);
    assert.equal(c.groups.length, 2);
    const first = c.groups[0].key;
    const second = c.groups[1].key;
    assert.equal(c.isOpen(first), true);
    assert.equal(c.isOpen(second), false);
    c.toggleGroup(second);
    assert.equal(c.isOpen(first), true);
    assert.equal(c.isOpen(second), true);
    c.toggleGroup(first);
    assert.equal(c.isOpen(first), false);
    assert.equal(c.isOpen(second), true);
});
