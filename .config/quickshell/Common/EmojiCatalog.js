.pragma library

var toneNames = ["Default", "Light", "Medium-light", "Medium", "Medium-dark", "Dark"];

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
