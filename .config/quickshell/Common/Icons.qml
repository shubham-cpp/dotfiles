pragma Singleton

import Quickshell

// Theme icons first. AppImage / AppManager drops unthemed pngs in
// ~/.local/share/icons/<name>.png which iconPath(..., check) skips.
Singleton {
    readonly property var glyphs: ({
        search: "\uf002",
        clock: "\uf017",
        calendar: "\uf073",
        wifi: "\uf1eb",
        wifiOff: "\udb81\uddaa",
        wifi1: "\udb82\udd1f",
        wifi2: "\udb82\udd22",
        wifi3: "\udb82\udd25",
        wifi4: "\udb82\udd28",
        ethernet: "\udb80\ude00",
        lock: "\uf023",
        eye: "\uf06e",
        eyeOff: "\uf070",
        download: "\uf0ab",
        upload: "\uf0aa",
        volume: "\uf028",
        microphone: "\uf130",
        audioApp: "\uf001",
        volumeOff: "\udb81\udd81",
        brightness: "\uf185",
        moon: "\uf186",
        memory: "\uefc5",
        cpu: "\uf2db",
        temperature: "\uf2c9",
        bolt: "\uf0e7",
        battery: "\uf240",
        battery0: "\udb80\udc8e",
        battery10: "\udb80\udc7a",
        battery20: "\udb80\udc7b",
        battery30: "\udb80\udc7c",
        battery40: "\udb80\udc7d",
        battery50: "\udb80\udc7e",
        battery60: "\udb80\udc7f",
        battery70: "\udb80\udc80",
        battery80: "\udb80\udc81",
        battery90: "\udb80\udc82",
        battery100: "\udb80\udc79",
        batteryAlert: "\udb80\udc83",
        batteryCharging: "\udb80\udc84",
        batteryCharging10: "\udb82\udc9c",
        batteryCharging20: "\udb80\udc86",
        batteryCharging30: "\udb80\udc87",
        batteryCharging40: "\udb80\udc88",
        batteryCharging50: "\udb82\udc9d",
        batteryCharging60: "\udb80\udc89",
        batteryCharging70: "\udb82\udc9e",
        batteryCharging80: "\udb80\udc8a",
        batteryCharging90: "\udb80\udc8b",
        batteryCharging100: "\udb80\udc85",
        powerSaver: "\udb80\udf2a",
        powerBalanced: "\udb81\udcc5",
        powerPerformance: "\uf135",
        coffee: "\uf0f4",
        bell: "\uf0f3",
        bellOff: "\uf1f6",
        pin: "\uf08d",
        clipboard: "\uf0ea",
        copy: "\uf0c5",
        check: "\uf00c",
        image: "\uf03e",
        text: "\uf15c",
        file: "\uf15b",
        arrowRight: "\uf061",
        chevronDown: "\uf078",
        chevronRight: "\uf054",
        close: "\uf00d"
    })

    function src(icon) {
        if (!icon || !String(icon).length)
            return "";
        const s = String(icon);
        if (s.indexOf("file:") === 0)
            return s;
        if (s.charAt(0) === "/")
            return "file://" + s;

        let themed = Quickshell.iconPath(s, true);
        if (themed && themed.length)
            return themed;

        // Reverse-DNS ids (com.t3tools.t3code) are not file extensions.
        const shortName = (!/\.(png|svg|xpm|jpg|jpeg)$/i.test(s) && s.indexOf(".") !== -1) ? s.split(".").pop() : s;
        if (shortName !== s) {
            themed = Quickshell.iconPath(shortName, true);
            if (themed && themed.length)
                return themed;
        }

        const home = Quickshell.env("HOME");
        return "file://" + home + "/.local/share/icons/" + shortName + ".png";
    }

    function fromAppId(appId) {
        const entry = DesktopEntries.heuristicLookup(appId);
        if (entry && entry.icon)
            return src(entry.icon);
        if (!appId)
            return "";
        return src(String(appId).toLowerCase());
    }
}
