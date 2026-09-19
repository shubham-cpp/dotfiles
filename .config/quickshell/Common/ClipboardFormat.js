.pragma library

function kindOf(preview) {
    const value = String(preview || "").trim();
    if (/\[\[ binary data /i.test(value))
        return "image";
    if (/^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$/.test(value))
        return "color";
    if (/^https?:\/\/[^\s]+$/i.test(value))
        return "link";
    if (/^(\/|\.\/|~\/)/.test(value) && value.indexOf("\n") === -1)
        return "path";
    return "text";
}

function kindLabel(row) {
    const kind = row.kind || kindOf(row.preview);
    return ({ image: "Image", color: "Color", path: "File path", link: "Link", text: "Text" })[kind] || "Text";
}

function imageInfo(preview) {
    const value = String(preview || "");
    const dimensions = value.match(/(\d+)\s*[x×]\s*(\d+)/);
    const format = value.match(/\b(png|jpe?g|gif|webp|bmp|tiff?|avif|svg)\b/i);
    const size = value.match(/(\d+(?:\.\d+)?)\s*(bytes?|[kmgt]i?b)\b/i);
    return {
        dimensions: dimensions ? dimensions[1] + " × " + dimensions[2] : "",
        format: format ? format[1].toUpperCase().replace("JPG", "JPEG") : "",
        size: size ? size[1] + " " + size[2].replace(/^([kmgt])i?b$/i, function (unit, prefix) {
            return prefix.toUpperCase() + (unit.toLowerCase().indexOf("i") >= 0 ? "iB" : "B");
        }) : ""
    };
}

function linkInfo(preview) {
    const value = String(preview || "").trim();
    const match = value.match(/^https?:\/\/([^/?#]+)(.*)$/i);
    return match ? { host: match[1].replace(/^.*@/, "").replace(/^www\./i, ""), path: match[2] || "/" } : { host: value, path: "" };
}

function title(row) {
    const value = String(row.preview || "").trim();
    const kind = row.kind || kindOf(value);
    if (kind === "image") {
        const info = imageInfo(value);
        return "Image" + (info.dimensions ? " · " + info.dimensions : "");
    }
    if (kind === "link")
        return linkInfo(value).host;
    if (kind === "path") {
        const path = value.replace(/\/+$/, "") || "/";
        return path.slice(path.lastIndexOf("/") + 1) || path;
    }
    return value.split(/\r?\n/)[0].replace(/\s+/g, " ").slice(0, 120) || "Empty text";
}

function subtitle(row) {
    const value = String(row.preview || "").trim();
    const kind = row.kind || kindOf(value);
    if (kind === "image") {
        const info = imageInfo(value);
        return [info.format, info.size].filter(function (part) { return part.length > 0; }).join(" · ") || "Image";
    }
    if (kind === "path")
        return value;
    if (kind === "link")
        return linkInfo(value).path;
    if (kind === "color")
        return "Color";
    const lines = value.split(/\r?\n/);
    if (lines.length > 1)
        return lines.slice(1).join(" ").replace(/\s+/g, " ").trim().slice(0, 160) || "Text";
    return "Text";
}

function matchesFilter(row, filter) {
    const kind = row.kind || kindOf(row.preview);
    if (filter === "pinned")
        return row.pinned === true || row.source === "pin";
    if (filter === "image" || filter === "link")
        return kind === filter;
    if (filter === "text")
        return kind !== "image" && kind !== "link";
    return true;
}

function validPin(pin, directory) {
    return !!pin && typeof pin === "object" && !Array.isArray(pin)
        && typeof pin.id === "string" && /^p_[0-9]+(?:_[0-9]+)?$/.test(pin.id)
        && typeof pin.preview === "string" && pin.preview.length <= 8000
        && pin.file === directory + "/" + pin.id;
}

function validatePins(raw, directory) {
    if (!Array.isArray(raw) || raw.length > 100)
        throw new Error("Invalid clipboard pins");
    const seen = new Set();
    return raw.map(pin => {
        if (!validPin(pin, directory) || seen.has(pin.id))
            throw new Error("Invalid clipboard pin");
        seen.add(pin.id);
        return { id: pin.id, preview: pin.preview, kind: kindOf(pin.preview), file: pin.file };
    });
}
