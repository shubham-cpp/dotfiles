pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    readonly property bool ready: true
    property string token: ""
    property string chip: "All"
    property string status: ""
    property var matches: []
    property var groups: []
    property var expanded: []
    property bool expandedSeeded: false
    property int gen: 0
    property real fetchedAt: 0
    property var request: null

    readonly property var codes: ["PL", "PD", "CL"]
    readonly property int staleMs: 60 * 60 * 1000
    readonly property bool hasLive: {
        const _ = gen;
        for (let i = 0; i < matches.length; i++) {
            const m = matches[i];
            if ((chip === "All" || m.code === chip) && m.live)
                return true;
        }
        return false;
    }

    function pad(n) {
        return (n < 10 ? "0" : "") + n;
    }

    function istParts(utcIso) {
        const d = new Date(utcIso);
        const ist = new Date(d.getTime() + 330 * 60000);
        const y = ist.getUTCFullYear();
        const m = ist.getUTCMonth() + 1;
        const day = ist.getUTCDate();
        const hh = ist.getUTCHours();
        const mm = ist.getUTCMinutes();
        const key = y + "-" + pad(m) + "-" + pad(day);
        const time = pad(hh) + ":" + pad(mm);
        return { key: key, time: time, date: ist };
    }

    function todayKey() {
        return istParts(new Date().toISOString()).key;
    }

    function yesterdayKey() {
        return istParts(new Date(Date.now() - 86400000).toISOString()).key;
    }

    function utcYmd(d) {
        return d.getUTCFullYear() + "-" + pad(d.getUTCMonth() + 1) + "-" + pad(d.getUTCDate());
    }

    function isLive(st) {
        return st === "IN_PLAY" || st === "PAUSED" || st === "EXTRA_TIME" || st === "PENALTY_SHOOTOUT" || st === "LIVE";
    }

    function chipOf(st) {
        if (isLive(st))
            return "LIVE";
        if (st === "FINISHED")
            return "FT";
        if (st === "SCHEDULED" || st === "TIMED")
            return "";
        return st || "";
    }

    function teamLabel(team) {
        if (!team)
            return "";
        return team.shortName || team.tla || team.name || "";
    }

    function teamTla(team) {
        if (!team)
            return "";
        return team.tla || "";
    }

    function crestOf(team) {
        if (!team || !team.crest)
            return "";
        const u = String(team.crest);
        if (u.indexOf("https://crests.football-data.org/") !== 0)
            return "";
        return u;
    }

    function isOpen(key) {
        const _ = gen;
        return expanded.indexOf(key) !== -1;
    }

    function toggleGroup(key) {
        expandedSeeded = true;
        const i = expanded.indexOf(key);
        if (i === -1)
            expanded = expanded.concat([key]);
        else {
            const next = expanded.slice();
            next.splice(i, 1);
            expanded = next;
        }
        gen++;
    }

    function seedExpanded() {
        if (expandedSeeded)
            return;
        const today = todayKey();
        for (let i = 0; i < groups.length; i++) {
            if (groups[i].key === today) {
                expanded = [today];
                expandedSeeded = true;
                return;
            }
        }
        if (groups.length)
            expanded = [groups[0].key];
        expandedSeeded = groups.length > 0;
    }

    function rebuildGroups() {
        const list = [];
        for (let i = 0; i < matches.length; i++) {
            const m = matches[i];
            if (chip !== "All" && m.code !== chip)
                continue;
            list.push(m);
        }
        const by = {};
        const keys = [];
        for (let i = 0; i < list.length; i++) {
            const k = list[i].dateKey;
            if (!by[k]) {
                by[k] = [];
                keys.push(k);
            }
            by[k].push(list[i]);
        }
        keys.sort();
        const today = todayKey();
        const yesterday = yesterdayKey();
        const g = [];
        for (let i = 0; i < keys.length; i++) {
            const k = keys[i];
            const d = new Date(k + "T12:00:00");
            g.push({
                key: k,
                today: k === today,
                yesterday: k === yesterday,
                label: k === today ? "Today" : (k === yesterday ? "Yesterday" : Qt.formatDate(d, "ddd d MMM")),
                matches: by[k],
                count: by[k].length
            });
        }
        groups = g;
        seedExpanded();
        gen++;
    }

    function ingest(rawMatches) {
        const out = [];
        const minKey = yesterdayKey();
        for (let i = 0; i < rawMatches.length; i++) {
            const m = rawMatches[i];
            const code = (m.competition && m.competition.code) ? m.competition.code : "";
            if (codes.indexOf(code) === -1)
                continue;
            const st = m.status || "";
            const ist = istParts(m.utcDate);
            if (ist.key < minKey)
                continue;
            const ft = m.score && m.score.fullTime ? m.score.fullTime : {};
            const home = m.homeTeam || {};
            const away = m.awayTeam || {};
            out.push({
                id: m.id,
                code: code,
                status: st,
                live: isLive(st),
                chip: chipOf(st),
                home: teamLabel(home) || "HOME",
                away: teamLabel(away) || "AWAY",
                homeTla: teamTla(home),
                awayTla: teamTla(away),
                homeCrest: crestOf(home),
                awayCrest: crestOf(away),
                hs: ft.home,
                as: ft.away,
                dateKey: ist.key,
                time: ist.time,
                utcDate: m.utcDate
            });
        }
        if (JSON.stringify(out) !== JSON.stringify(matches)) {
            matches = out;
            rebuildGroups();
        }
        fetchedAt = Date.now();
        persistCache();
    }

    function window() {
        const ist = new Date(Date.now() + 330 * 60000);
        const from = new Date(Date.UTC(ist.getUTCFullYear(), ist.getUTCMonth(), ist.getUTCDate() - 1) - 330 * 60000);
        const to = new Date(Date.now() + 8 * 86400000);
        return { from: utcYmd(from), to: utcYmd(to) };
    }

    function ensure() {
        if (!token.length) {
            status = "set FOOTBALL_DATA_TOKEN or secrets.json";
            gen++;
            return;
        }
        if (fetchedAt > 0 && Date.now() - fetchedAt < staleMs) {
            return;
        }
        fetchNow();
    }

    function fetchNow() {
        if (!token.length || request)
            return;
        const w = window();
        const url = "https://api.football-data.org/v4/matches?dateFrom=" + w.from + "&dateTo=" + w.to;
        if (!matches.length) {
            status = "loading…";
            gen++;
        }
        const xhr = new XMLHttpRequest();
        request = xhr;
        xhr.open("GET", url);
        xhr.setRequestHeader("X-Auth-Token", token);
        xhr.onreadystatechange = function () {
            if (request !== xhr || xhr.readyState !== XMLHttpRequest.DONE)
                return;
            requestTimeout.stop();
            request = null;
            xhr.onreadystatechange = null;
            if (xhr.status === 429) {
                status = "rate limited";
                gen++;
                return;
            }
            if (xhr.status < 200 || xhr.status >= 300) {
                status = "http " + xhr.status;
                gen++;
                return;
            }
            try {
                const obj = JSON.parse(xhr.responseText);
                if (!obj || !Array.isArray(obj.matches))
                    throw new Error("Invalid match response");
                ingest(obj.matches);
                status = "";
            } catch (e) {
                status = "parse error";
                gen++;
            }
        };
        requestTimeout.restart();
        try {
            xhr.send();
        } catch (e) {
            cancelRequest();
            status = "request failed";
        }
    }

    function cancelRequest() {
        const xhr = request;
        request = null;
        requestTimeout.stop();
        if (xhr) {
            xhr.onreadystatechange = null;
            xhr.abort();
        }
    }

    function setChip(c) {
        if (chip === c || (c !== "All" && codes.indexOf(c) === -1))
            return;
        chip = c;
        rebuildGroups();
    }

    function persistCache() {
        cacheFile.setText(JSON.stringify({
            fetchedAt: fetchedAt,
            from: window().from,
            matches: matches
        }));
    }

    function loadCache() {
        try {
            const raw = cacheFile.text();
            if (!raw || !raw.length)
                return;
            const obj = JSON.parse(raw);
            if (!obj || !Array.isArray(obj.matches) || !isFinite(obj.fetchedAt))
                return;
            matches = obj.matches;
            rebuildGroups();
            const shapeOk = matches.length === 0 || matches[0].homeCrest !== undefined;
            const windowOk = obj.from === window().from;
            fetchedAt = (shapeOk && windowOk) ? Math.max(0, Math.min(Date.now(), obj.fetchedAt)) : 0;
        } catch (e) {
        }
    }

    function loadToken() {
        const env = Quickshell.env("FOOTBALL_DATA_TOKEN");
        if (env && env.length) {
            token = env;
            return;
        }
        try {
            const raw = secretsFile.text();
            if (raw && raw.length) {
                const obj = JSON.parse(raw);
                token = obj.footballDataToken || obj.token || "";
            }
        } catch (e) {
            token = "";
        }
    }

    Component.onCompleted: {
        loadToken();
        loadCache();
    }

    onTokenChanged: {
        cancelRequest();
        fetchedAt = 0;
    }

    Timer {
        id: requestTimeout
        interval: 25000
        onTriggered: {
            root.cancelRequest();
            root.status = "request timed out";
        }
    }

    FileView {
        id: secretsFile
        path: `${Quickshell.shellDir}/secrets.json`
        blockLoading: true
        printErrors: false
    }

    FileView {
        id: cacheFile
        path: `${Quickshell.cacheDir}/football-matches.json`
        blockLoading: true
        printErrors: false
    }

    Timer {
        interval: 75000
        running: Agenda.tab === "fb" && Agenda.open && root.hasLive && root.token.length > 0
        repeat: true
        onTriggered: root.fetchNow()
    }
}
