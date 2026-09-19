.pragma library

function popupX(center, width, availableWidth) {
    return Math.round(Math.max(8, Math.min(center - width / 2, availableWidth - width - 8)));
}
