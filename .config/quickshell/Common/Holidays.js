.pragma library

// Maharashtra GAD 2026 public holidays (NI Act), plus DoPT compulsory days.
// Nager.Date has no IN feed (HTTP 204).
var years = {
    "2026": [
        { date: "2026-01-26", name: "Republic Day", kind: "national" },
        { date: "2026-02-15", name: "Mahashivratri", kind: "mh" },
        { date: "2026-02-19", name: "Chhatrapati Shivaji Maharaj Jayanti", kind: "mh" },
        { date: "2026-03-03", name: "Holi (Second Day)", kind: "mh" },
        { date: "2026-03-19", name: "Gudi Padwa", kind: "mh" },
        { date: "2026-03-21", name: "Ramzan-Id (Id-Ul-Fitra)", kind: "national" },
        { date: "2026-03-26", name: "Ram Navami", kind: "mh" },
        { date: "2026-03-31", name: "Mahavir Janmakalyanak", kind: "national" },
        { date: "2026-04-03", name: "Good Friday", kind: "national" },
        { date: "2026-04-14", name: "Dr. Babasaheb Ambedkar Jayanti", kind: "mh" },
        { date: "2026-05-01", name: "Maharashtra Din / Buddha Pournima", kind: "mh" },
        { date: "2026-05-28", name: "Bakri Id (Id-Uz-Zuha)", kind: "national" },
        { date: "2026-06-26", name: "Moharum", kind: "national" },
        { date: "2026-08-15", name: "Independence Day", kind: "national" },
        { date: "2026-08-26", name: "Id-E-Milad", kind: "national" },
        { date: "2026-09-14", name: "Ganesh Chaturthi", kind: "mh" },
        { date: "2026-10-02", name: "Mahatma Gandhi Jayanti", kind: "national" },
        { date: "2026-10-20", name: "Dasara", kind: "national" },
        { date: "2026-11-08", name: "Diwali Amavasya (Laxmi Pujan)", kind: "national" },
        { date: "2026-11-10", name: "Diwali (Bali Pratipada)", kind: "mh" },
        { date: "2026-11-24", name: "Guru Nanak Jayanti", kind: "national" },
        { date: "2026-12-25", name: "Christmas", kind: "national" }
    ]
};

function keyFromDate(d) {
    const y = d.getFullYear();
    const m = d.getMonth() + 1;
    const day = d.getDate();
    return y + "-" + (m < 10 ? "0" : "") + m + "-" + (day < 10 ? "0" : "") + day;
}

function listYear(year) {
    return years[String(year)] || [];
}

function onDay(d) {
    const key = keyFromDate(d);
    const list = listYear(d.getFullYear());
    const out = [];
    for (let i = 0; i < list.length; i++) {
        if (list[i].date === key)
            out.push(list[i]);
    }
    return out;
}

function kindOnDay(d) {
    const hits = onDay(d);
    if (!hits.length)
        return "";
    for (let i = 0; i < hits.length; i++) {
        if (hits[i].kind === "national")
            return "national";
    }
    return hits[0].kind;
}
