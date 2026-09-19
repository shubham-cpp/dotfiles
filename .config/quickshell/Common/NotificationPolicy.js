.pragma library

function stackTag(n) {
    const hints = n.hints || {};
    return String(hints["x-dunst-stack-tag"] || hints["x-canonical-private-synchronous"] || "").trim();
}

function stackKey(n) {
    const tag = stackTag(n);
    const app = String(n.desktopEntry || n.appName || "");
    return tag && app ? JSON.stringify([app, tag]) : "";
}

function isOsd(n) {
    const hints = n.hints || {};
    const tag = stackTag(n);
    return n.appName === "System OSD" && hints.category === "device"
        && (tag === "volume" || tag === "backlight") && n.urgency !== 2;
}
