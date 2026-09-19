.pragma library

function props(node) {
    return node && node.ready ? (node.properties || {}) : {};
}

function flagged(value) {
    return value === true || value === "true" || value === 1;
}

function monitor(node) {
    const p = props(node);
    return flagged(p["stream.monitor"]) || flagged(p["node.monitor"])
        || p["media.category"] === "Monitor" || p["media.category"] === "Manager"
        || String(node.name || "").endsWith(".monitor");
}

function playback(node) {
    const p = props(node);
    // Quickshell 0.3.1 AudioOutStream = Audio | Sink | Stream.
    return !!(node && node.audio && node.isStream && node.isSink && !monitor(node)
        && !flagged(p["node.hidden"]) && p["media.role"] !== "Filter");
}

function device(node, input) {
    return !!(node && node.audio && !node.isStream && node.isSink !== input && !monitor(node));
}

function deviceName(node) {
    return node ? String(node.description || node.nickname || node.name || "Audio device") : "";
}

function appName(node) {
    const p = props(node);
    return String(p["application.name"] || node.description || node.name || "Unknown application");
}

function devices(nodes, input) {
    const selected = nodes.filter(n => device(n, input));
    return selected.map(node => {
        const name = deviceName(node);
        const duplicate = selected.some(n => n !== node && deviceName(n) === name);
        return { node, label: duplicate ? name + " · " + node.name : name };
    }).sort((a, b) => a.label.localeCompare(b.label) || a.node.id - b.node.id);
}

function sameDevices(a, b) {
    return a.length === b.length && a.every((entry, i) => entry.node === b[i].node && entry.label === b[i].label);
}

// Links come from Pipewire.linkGroups with monitor targets excluded.
// Stop at selectable virtual sinks as well as physical sinks.
function traceOutputs(node, nodes, links) {
    const visited = new Set([node]);
    const queue = [node];
    const targets = new Set();
    let unresolved = false;
    for (let i = 0; i < queue.length; i++) {
        const outgoing = links.filter(link => link.source === queue[i]);
        if (i > 0 && outgoing.length === 0)
            unresolved = true;
        for (const link of outgoing) {
            const target = link.target;
            if (!target || nodes.indexOf(target) === -1) {
                unresolved = true;
            } else if (device(target, false)) {
                targets.add(target);
            } else if (target.isStream) {
                unresolved = true;
            } else if (!visited.has(target)) {
                visited.add(target);
                queue.push(target);
            }
        }
    }
    return { outputs: Array.from(targets), unresolved };
}

function destination(node, nodes, links) {
    if (!links.some(link => link.source === node))
        return { key: "unconnected", label: "Not connected", detail: "No output connection", rank: 3 };
    const { outputs, unresolved } = traceOutputs(node, nodes, links);
    if (unresolved || outputs.length === 0)
        return { key: "other", label: "Other routes", detail: "Output unavailable", rank: 4 };
    if (outputs.length > 1)
        return { key: "multiple", label: "Multiple outputs", detail: outputs.map(deviceName).sort().join(", "), rank: 2 };
    return { key: "sink:" + outputs[0].id, label: deviceName(outputs[0]), detail: deviceName(outputs[0]), rank: 1, output: outputs[0] };
}

function rows(nodes, links, defaultSink) {
    const streams = nodes.filter(playback);
    return streams.map(node => {
        const route = destination(node, nodes, links);
        const name = appName(node);
        const p = props(node);
        const duplicates = streams.filter(n => appName(n) === name);
        const subtitle = duplicates.length > 1
            ? String(p["media.name"] || node.description || "Stream") + " · " + (duplicates.indexOf(node) + 1) : "";
        return { node, appName: name, subtitle, group: route.key,
            groupLabel: route.label + (route.output === defaultSink ? " · Default" : ""),
            routeDetail: route.detail, rank: route.output === defaultSink ? 0 : route.rank,
            routeChanging: false };
    }).sort((a, b) => a.rank - b.rank || a.groupLabel.localeCompare(b.groupLabel)
        || a.group.localeCompare(b.group) || a.appName.localeCompare(b.appName) || a.node.id - b.node.id);
}

function updateRouteFlags(model, desired) {
    for (let i = 0; i < model.count; i++) {
        const current = model.get(i);
        const row = desired.find(r => r.node === current.node);
        const changing = row.group !== current.group;
        if (current.routeChanging !== changing)
            model.setProperty(i, "routeChanging", changing);
    }
}

function updateRow(model, index, row) {
    const current = model.get(index);
    for (const key of ["appName", "subtitle", "group", "groupLabel", "routeDetail", "rank", "routeChanging"]) {
        if (current[key] !== row[key])
            model.setProperty(index, key, row[key]);
    }
}

// Never store null-object separator rows. Qt's QObject list roles require live objects.
function sync(model, desired, freeze) {
    for (let i = model.count - 1; i >= 0; i--) {
        if (!desired.some(row => row.node === model.get(i).node))
            model.remove(i);
    }
    if (freeze) {
        updateRouteFlags(model, desired);
        return;
    }
    for (let i = 0; i < desired.length; i++) {
        let existing = i;
        while (existing < model.count && model.get(existing).node !== desired[i].node)
            existing++;
        if (existing === model.count)
            model.insert(i, desired[i]);
        else if (existing !== i)
            model.move(existing, i, 1);
        updateRow(model, i, desired[i]);
    }
}
