.pragma library
.import "Fuzzy.js" as Fuzzy

var toneNames = ["Default", "Light", "Medium-light", "Medium", "Medium-dark", "Dark"];

function normalize(text) {
    return String(text || "").toLowerCase().replace(/[_:,\-]/g, " ").replace(/\s+/g, " ").trim();
}

function sequenceId(text) {
    const codes = [];
    // Explicit code-point iteration also works on Qt's UTF-16 string iterator.
    for (let i = 0; i < text.length; i++) {
        const code = text.codePointAt(i);
        codes.push(code.toString(16));
        if (code > 0xffff)
            i++;
    }
    return codes.join("-");
}

function index(data) {
    if (!data || data.schema !== 1 || !Array.isArray(data.entries) || !Array.isArray(data.families)
            || !Array.isArray(data.groups) || !data.entries.length || data.entries.length > 20000)
        throw new Error("Unsupported emoji catalog");
    const out = { entries: {}, families: {}, byText: {}, order: [], groups: data.groups, version: data.unicode };
    for (const entry of data.entries) {
        indexEntry(entry, out.entries, out.byText);
    }
    const seen = new Set();
    for (const family of data.families)
        indexFamily(family, out, seen);
    if (seen.size !== data.entries.length)
        throw new Error("Unreachable emoji entries");
    return out;
}

function indexEntry(entry, entries, byText) {
    if (!entry || !/^[0-9a-f]+(?:-[0-9a-f]+)*$/.test(entry.id) || entries[entry.id]
            || typeof entry.text !== "string" || typeof entry.name !== "string"
            || typeof entry.search !== "string" || !Array.isArray(entry.aliases)
            || !Array.isArray(entry.tones) || entry.tones.length > 2
            || entry.tones.some(t => !Number.isInteger(t) || t < 1 || t > 5)
            || sequenceId(entry.text) !== entry.id)
        throw new Error("Invalid emoji entry");
    entries[entry.id] = entry;
    entry.nameKey = normalize(entry.name);
    byText[entry.text] = entry.id;
}

function indexFamily(family, out, seen) {
    if (!family || !out.entries[family.id] || out.families[family.id]
            || !Array.isArray(family.variants) || family.variants.indexOf(family.id) < 0
            || out.groups.indexOf(family.group) < 0 || ![0, 1, 2].includes(family.slots))
        throw new Error("Invalid emoji family");
    const tuples = {};
    for (const id of family.variants) {
        const entry = out.entries[id];
        if (!entry || entry.familyId !== family.id || seen.has(id))
            throw new Error("Invalid emoji variant");
        seen.add(id);
        // Some legacy paired emoji encode equal tones with one modifier.
        const tones = entry.tones.length === 1 && family.slots === 2 ? [entry.tones[0], entry.tones[0]] : entry.tones;
        const key = tones.join(",");
        if (tuples[key])
            throw new Error("Ambiguous emoji tones");
        tuples[key] = id;
    }
    if (tuples[""] !== family.id)
        throw new Error("Missing default emoji");
    family.tuples = tuples;
    out.families[family.id] = family;
    out.order.push(family.id);
}

function emptyState() {
    return { schema: 1, tone: 0, overrides: {}, recents: [] };
}

function sanitizeState(raw, catalog, now) {
    if (!raw || raw.schema !== 1 || !raw.overrides || typeof raw.overrides !== "object" || !Array.isArray(raw.recents))
        throw new Error("Unsupported emoji preferences");
    const state = emptyState();
    state.tone = Number.isInteger(raw.tone) && raw.tone >= 0 && raw.tone <= 5 ? raw.tone : 0;
    for (const key of Object.keys(raw.overrides).slice(0, catalog.order.length)) {
        const entry = catalog.entries[raw.overrides[key]];
        if (entry && entry.familyId === key)
            state.overrides[key] = entry.id;
    }
    const seen = new Set();
    for (const recent of raw.recents) {
        if (recent && catalog.entries[recent.id] && !seen.has(recent.id)) {
            seen.add(recent.id);
            state.recents.push({ id: recent.id, at: Math.max(0, Math.min(now, Number(recent.at) || 0)) });
        }
        if (state.recents.length === 48)
            break;
    }
    return state;
}

function resolve(catalog, familyId, state) {
    const family = catalog.families[familyId];
    if (!family)
        return "";
    const override = state.overrides[familyId];
    if (family.variants.includes(override))
        return override;
    const key = state.tone ? (family.slots === 2 ? [state.tone, state.tone] : [state.tone]).join(",") : "";
    return family.tuples[key] || family.id;
}

function recentState(state, id, now) {
    return { schema: 1, tone: state.tone, overrides: state.overrides,
        recents: [{ id: id, at: now }].concat(state.recents.filter(row => row.id !== id)).slice(0, 48) };
}

function rank(query, tokens, entry, allowFuzzy) {
    const name = entry.nameKey;
    if (name === query || entry.aliases.includes(query))
        return 10000;
    if (tokens.every(token => name.includes(token)))
        return 8000 - name.length;
    if (tokens.every(token => entry.search.includes(token)))
        return 6000 - name.length;
    const hit = allowFuzzy ? Fuzzy.scoreMultiTokenAND(query, entry.search) : null;
    return hit ? Math.min(4000, hit.score || 1) : -1;
}

function toneQuery(query) {
    const tones = [];
    const names = { "light": 1, "medium light": 2, "medium": 3, "medium dark": 4, "dark": 5 };
    const base = normalize(query.replace(/\b(medium light|medium dark|light|medium|dark)(?: skin tones?)?\b/g, (match, name) => {
        tones.push(names[name]);
        return " ";
    }));
    return { tones: tones, base: base, tokens: base ? base.split(" ") : [] };
}

function search(catalog, state, query, category) {
    const raw = String(query || "").trim();
    const exact = catalog.byText[raw];
    if (exact)
        return [exact];
    const q = normalize(raw);
    if (!q && category === "recent")
        return state.recents.map(row => row.id);
    if (!q)
        return catalog.order.filter(id => category === "all" || catalog.families[id].group === category)
            .map(id => resolve(catalog, id, state));
    const tokens = q.split(" ");
    const tone = toneQuery(q);
    const rows = [];
    // Normal searches need no fuzzy allocations. Use fuzzy matching only when
    // no name/keyword match exists, so typo recovery stays available.
    for (let pass = 0; pass < 2 && !rows.length; pass++) {
        for (const familyId of catalog.order) {
            const family = catalog.families[familyId];
            let matched = resolve(catalog, familyId, state);
            let score;
            if (tone.tones.length && family.slots) {
                const tuple = tone.tones.length === 1 && family.slots === 2 ? [tone.tones[0], tone.tones[0]] : tone.tones;
                matched = family.tuples[tuple.join(",")];
                if (!matched)
                    continue;
                score = tone.base ? rank(tone.base, tone.tokens, catalog.entries[familyId], pass === 1) : 6000;
            } else {
                score = rank(q, tokens, catalog.entries[familyId], pass === 1);
            }
            if (score >= 0)
                rows.push({ id: matched, score: score, order: rows.length });
        }
    }
    const recent = {};
    state.recents.forEach((row, i) => recent[row.id] = 48 - i);
    rows.sort((a, b) => b.score - a.score || (recent[b.id] || 0) - (recent[a.id] || 0) || a.order - b.order);
    return rows.map(row => row.id);
}
