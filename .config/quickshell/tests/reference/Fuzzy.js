.pragma library

// fzf v2 default-scheme constants (junegunn/fzf algo.go).
var SCORE_MATCH = 16;
var SCORE_GAP_START = -3;
var SCORE_GAP_EXT = -1;
var BONUS_BOUNDARY = 8;
var BONUS_CAMEL = 7;
var BONUS_CONSEC = 4;
var BONUS_FIRST = 2;
var BONUS_WHITE = 10;
var BONUS_DELIM = 9;
var BONUS_NONWORD = 8;

function charClass(ch) {
    if (ch === undefined || ch === "")
        return "white";
    if (" \t\n\r".indexOf(ch) !== -1)
        return "white";
    if ("/,:;|".indexOf(ch) !== -1)
        return "delim";
    if (ch >= "0" && ch <= "9")
        return "number";
    if (ch >= "a" && ch <= "z")
        return "lower";
    if (ch >= "A" && ch <= "Z")
        return "upper";
    return "nonword";
}

function bonusFor(prev, cls) {
    if (prev === "white")
        return BONUS_WHITE;
    if (prev === "delim")
        return BONUS_DELIM;
    if (prev === "nonword")
        return BONUS_BOUNDARY;
    if ((prev === "lower" && cls === "upper") || (prev !== "number" && cls === "number"))
        return BONUS_CAMEL;
    if (cls === "nonword")
        return BONUS_NONWORD;
    if (cls === "white")
        return BONUS_WHITE;
    return 0;
}

function isSubseq(query, text) {
    let qi = 0;
    for (let i = 0; i < text.length && qi < query.length; i++) {
        if (text[i] === query[qi])
            qi++;
    }
    return qi === query.length;
}

function score(query, text) {
    if (!query || !query.length)
        return { score: 0 };
    if (!text || !text.length)
        return null;

    const qRaw = String(query);
    const tRaw = String(text);
    const sensitive = qRaw !== qRaw.toLowerCase();
    const q = sensitive ? qRaw : qRaw.toLowerCase();
    const t = sensitive ? tRaw : tRaw.toLowerCase();
    if (!isSubseq(q, t))
        return null;

    let qi = 0;
    let total = 0;
    let consec = 0;
    let firstBonus = 0;
    let prev = "white";

    for (let i = 0; i < t.length && qi < q.length; i++) {
        const cls = charClass(tRaw[i]);
        if (t[i] === q[qi]) {
            let b = i === 0 ? BONUS_WHITE : bonusFor(prev, cls);
            if (qi === 0)
                b *= BONUS_FIRST;
            consec++;
            if (consec === 1)
                firstBonus = b;
            else if (b >= BONUS_BOUNDARY && b > firstBonus)
                consec = 1, firstBonus = b;
            else
                b = Math.max(b, BONUS_CONSEC, firstBonus);
            total += SCORE_MATCH + b;
            qi++;
        } else if (qi > 0) {
            if (consec > 0)
                total += SCORE_GAP_START;
            else
                total += SCORE_GAP_EXT;
            consec = 0;
        }
        prev = cls;
    }

    if (qi < q.length)
        return null;
    if (total < 0)
        total = 0;
    return { score: total };
}

function scoreMultiTokenAND(query, text) {
    const parts = String(query).trim().split(/\s+/);
    if (parts.length <= 1)
        return score(query, text);
    let total = 0;
    for (let i = 0; i < parts.length; i++) {
        const r = score(parts[i], text);
        if (!r)
            return null;
        total += r.score;
    }
    return { score: total };
}

function scoreDesktop(query, entry) {
    const name = entry.name || "";
    const id = entry.id || "";
    const q = String(query || "").trim();
    if (!q.length)
        return { tier: 5, score: 0 };

    const ql = q.toLowerCase();
    const nl = name.toLowerCase();
    const il = id.toLowerCase();
    if (nl === ql || il === ql)
        return { tier: 1, score: 10000 };
    if (nl.indexOf(ql) === 0 || il.indexOf(ql) === 0)
        return { tier: 2, score: 8000 + Math.max(0, 80 - nl.length) };

    const nameHit = scoreMultiTokenAND(q, name);
    if (nameHit)
        return { tier: 3, score: nameHit.score };

    const extra = [entry.genericName || "", entry.comment || ""];
    const kws = entry.keywords || [];
    for (let i = 0; i < kws.length; i++)
        extra.push(kws[i]);
    let best = null;
    for (let i = 0; i < extra.length; i++) {
        const hit = scoreMultiTokenAND(q, extra[i]);
        if (hit && (!best || hit.score > best.score))
            best = { tier: 4, score: hit.score };
    }
    return best;
}
