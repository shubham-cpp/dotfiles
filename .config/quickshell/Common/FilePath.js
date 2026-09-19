.pragma library

function basename(path) {
    const text = String(path || "");
    const i = text.lastIndexOf("/");
    if (i === -1)
        return text;
    if (i === text.length - 1)
        return "";
    return text.slice(i + 1);
}

function parent(path) {
    const text = String(path || "");
    const i = text.lastIndexOf("/");
    if (i <= 0)
        return i === 0 ? "/" : ".";
    return text.slice(0, i);
}

function displayDir(path, home) {
    const dir = parent(path);
    const root = String(home || "");
    if (root && dir === root)
        return "~";
    if (root && dir.indexOf(root + "/") === 0)
        return "~" + dir.slice(root.length);
    return dir;
}

function fromPath(path, home) {
    const text = String(path || "");
    return { path: text, name: basename(text), dir: displayDir(text, home) };
}

function recents(stats, home, limit) {
    const cap = limit === undefined ? 50 : limit;
    const keys = Object.keys(stats || {});
    keys.sort((a, b) => {
        const left = stats[a] || {};
        const right = stats[b] || {};
        const last = (right.last || 0) - (left.last || 0);
        if (last)
            return last;
        return (right.count || 0) - (left.count || 0);
    });
    const rows = [];
    for (let i = 0; i < keys.length && rows.length < cap; i++)
        rows.push(fromPath(keys[i], home));
    return rows;
}

function fileUrlPath(href) {
    const text = String(href || "").replace(/&amp;/g, "&");
    if (text.indexOf("file://") !== 0)
        return "";
    let path = text.slice(7);
    if (path.charAt(0) !== "/") {
        const slash = path.indexOf("/");
        if (slash === -1)
            return "";
        path = path.slice(slash);
    }
    try {
        path = decodeURIComponent(path);
    } catch (e) {
        return "";
    }
    if (path.charAt(0) !== "/" || path.length > 4096 || path.indexOf("\0") !== -1)
        return "";
    if (path.charAt(path.length - 1) === "/")
        return "";
    return path;
}

function gtkBookmark(attrs) {
    const href = /href="(file:[^"]+)"/.exec(attrs);
    const path = href ? fileUrlPath(href[1]) : "";
    if (!path)
        return null;
    const visited = /visited="([^"]+)"/.exec(attrs);
    return { path: path, visited: visited ? visited[1] : "" };
}

function gtkBookmarks(xbel) {
    const text = String(xbel || "");
    if (!text.length || text.length > 2000000)
        return [];
    const found = [];
    const re = /<bookmark\b([^>]*)>/g;
    let match = re.exec(text);
    while (match && found.length < 200) {
        const row = gtkBookmark(match[1]);
        if (row)
            found.push(row);
        match = re.exec(text);
    }
    found.sort((a, b) => (a.visited < b.visited ? 1 : a.visited > b.visited ? -1 : 0));
    return found;
}

function gtkPaths(xbel, limit) {
    const cap = limit === undefined ? 50 : limit;
    const found = gtkBookmarks(xbel);
    const out = [];
    const seen = Object.create(null);
    for (let i = 0; i < found.length && out.length < cap; i++) {
        if (seen[found[i].path])
            continue;
        seen[found[i].path] = true;
        out.push(found[i].path);
    }
    return out;
}

function emptyRows(stats, xbel, home, limit) {
    const cap = limit === undefined ? 50 : limit;
    const rows = recents(stats, home, cap);
    const seen = Object.create(null);
    for (let i = 0; i < rows.length; i++)
        seen[rows[i].path] = true;
    const extra = gtkPaths(xbel, cap);
    for (let i = 0; i < extra.length && rows.length < cap; i++) {
        if (seen[extra[i]])
            continue;
        seen[extra[i]] = true;
        rows.push(fromPath(extra[i], home));
    }
    return rows;
}
