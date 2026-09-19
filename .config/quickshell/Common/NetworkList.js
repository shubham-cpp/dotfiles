.pragma library

function partition(values) {
    const connected = [];
    const known = [];
    const other = [];
    const list = values || [];
    for (let i = 0; i < list.length; i++) {
        const n = list[i];
        if (!n || !String(n.name || "").length)
            continue;
        if (n.connected)
            connected.push(n);
        else if (n.known)
            known.push(n);
        else
            other.push(n);
    }
    const bySignal = (a, b) => ((Number(b.signalStrength) || 0) - (Number(a.signalStrength) || 0)) || String(a.name).localeCompare(String(b.name));
    connected.sort(bySignal);
    known.sort(bySignal);
    other.sort(bySignal);
    return { connected, known, other };
}

function saved(part) {
    const grouped = part || { connected: [], known: [] };
    return (grouped.connected || []).concat(grouped.known || []);
}

function wifiGlyph(signal) {
    const s = Number(signal) || 0;
    if (s >= 0.75)
        return "wifi4";
    if (s >= 0.5)
        return "wifi3";
    if (s >= 0.25)
        return "wifi2";
    return "wifi1";
}

function rows(part) {
    const top = saved(part).map(net => ({ net, group: "Saved" }));
    const nearby = (part.other || []).map(net => ({ net, group: "Nearby" }));
    return top.concat(nearby);
}

function removeMissing(model, desired) {
    for (let i = model.count - 1; i >= 0; i--) {
        const net = model.get(i).net;
        if (!desired.some(row => row.net === net))
            model.remove(i);
    }
}

// Keep delegates across signal updates. Editing freezes ordering/additions,
// but vanished networks are removed immediately.
function sync(model, desired, freeze) {
    removeMissing(model, desired);
    if (freeze && model.count > 0)
        return;
    for (let i = 0; i < desired.length; i++) {
        let existing = i;
        while (existing < model.count && model.get(existing).net !== desired[i].net)
            existing++;
        if (existing === model.count)
            model.insert(i, desired[i]);
        else if (existing !== i)
            model.move(existing, i, 1);
        if (model.get(i).group !== desired[i].group)
            model.setProperty(i, "group", desired[i].group);
    }
}
