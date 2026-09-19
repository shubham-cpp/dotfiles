.pragma library

function ranked(apps, metric) {
    return apps.slice().sort((a, b) => b[metric] - a[metric]
        || a.name.localeCompare(b.name) || a.key.localeCompare(b.key)).slice(0, 10);
}

function prepareRow(app) {
    // Serialize only displayed applications, once for both rankings per sample.
    if (Array.isArray(app.members))
        app.members = JSON.stringify(app.members);
    return { appKey: app.key, appName: app.name, iconName: app.icon,
        memory: app.memory, cpu: app.cpu, processCount: app.count,
        members: app.members, canEnd: app.canEnd,
        actionState: app.state, gone: false };
}

function update(model, index, values) {
    const current = model.get(index);
    for (const key of Object.keys(values)) {
        if (current[key] !== values[key])
            model.setProperty(index, key, values[key]);
    }
}

function updateFrozen(model, apps) {
    // Keep empty slots for exited apps while interacting. Even a removal could
    // otherwise put a different application's end button under the pointer.
    for (let i = 0; i < model.count; i++) {
        const app = apps.find(app => app.key === model.get(i).appKey);
        if (app) update(model, i, prepareRow(app));
        else update(model, i, { gone: true, canEnd: false });
    }
}

function sync(model, apps, metric, frozen) {
    if (frozen && model.count) {
        updateFrozen(model, apps);
        return;
    }
    const desired = ranked(apps, metric).map(prepareRow);
    for (let i = model.count - 1; i >= 0; i--) {
        if (!desired.some(row => row.appKey === model.get(i).appKey)) model.remove(i);
    }
    for (let i = 0; i < desired.length; i++) {
        let existing = i;
        while (existing < model.count && model.get(existing).appKey !== desired[i].appKey) existing++;
        if (existing === model.count) {
            model.insert(i, desired[i]);
        } else {
            if (existing !== i) model.move(existing, i, 1);
            update(model, i, desired[i]);
        }
    }
}

function memoryText(bytes) {
    if (bytes < 0) return "--";
    return bytes < 1073741824 ? Math.round(bytes / 1048576) + " MB" : (bytes / 1073741824).toFixed(1) + " GB";
}
