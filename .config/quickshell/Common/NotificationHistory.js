.pragma library

function key(entry) {
    return entry.live ? "live:" + entry.id : "history:" + entry.id + ":" + entry.time;
}

function dayGroup(timestamp, now) {
    const today = new Date(now);
    today.setHours(0, 0, 0, 0);
    const yesterday = new Date(today);
    yesterday.setDate(yesterday.getDate() - 1);
    if (timestamp >= today.getTime())
        return "Today";
    if (timestamp >= yesterday.getTime())
        return "Yesterday";
    return "Earlier";
}

function rows(history, now) {
    let previous = "";
    return history.slice().sort((a, b) => (b.time || 0) - (a.time || 0)).map(entry => {
        const group = dayGroup(entry.time || 0, now);
        const section = group === previous ? "" : group;
        previous = group;
        return { entry, section, rowKey: key(entry) };
    });
}

function sync(model, rows) {
    for (let i = 0; i < rows.length; i++) {
        const row = rows[i];
        let found = i;
        while (found < model.count && model.get(found).rowKey !== row.rowKey)
            found++;
        if (found === model.count)
            model.insert(i, {rowKey: row.rowKey, section: row.section});
        else {
            if (found !== i)
                model.move(found, i, 1);
            if (model.get(i).section !== row.section)
                model.setProperty(i, "section", row.section);
        }
    }
    if (model.count > rows.length)
        model.remove(rows.length, model.count - rows.length);
}
