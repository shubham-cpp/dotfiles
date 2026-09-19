pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    property bool ready: false
    property bool preparingForSleep: false
    property string token: ""
    property string errorText: ""

    function reportState(): void {
        if (ready && bridge.running && token !== "")
            bridge.write(JSON.stringify({ token: token, secure: Lock.secure, requested: Lock.locked }) + "\n");
    }

    function handleLine(line): void {
        let message;
        try { message = JSON.parse(line); } catch (_) { return; }
        if (!message || typeof message.token !== "string")
            return;
        // An older helper cannot establish whether a missed resume is safe.
        if (message.event === "ready" && typeof message.preparingForSleep !== "boolean") {
            preparingForSleep = true;
            message.event = "error";
        }
        token = message.token;
        switch (message.event) {
        case "ready":
            preparingForSleep = message.preparingForSleep;
            ready = true;
            errorText = "";
            if (message.recover || preparingForSleep)
                Lock.activate();
            reportState();
            break;
        case "sleep":
            preparingForSleep = true;
            Lock.activate();
            reportState();
            break;
        case "resume":
            preparingForSleep = false;
            Lock.activate();
            reportState();
            break;
        case "lock":
            Lock.activate();
            break;
        case "lost":
            ready = false;
            errorText = "Session integration unavailable";
            Lock.activate();
            break;
        case "error":
            ready = false;
            errorText = "Session integration unavailable";
            Lock.activate();
            bridge.running = false;
            break;
        }
    }

    Process {
        id: bridge
        running: true
        stdinEnabled: true
        command: [Quickshell.shellPath(".local/bin/qs-session")]
        stdout: SplitParser { onRead: data => root.handleLine(data) }
        onExited: {
            root.ready = false;
            root.errorText = "Session integration unavailable";
            Lock.activate();
        }
    }

    Timer {
        id: restartBridge
        interval: 1000
        running: !root.ready
        repeat: true
        onTriggered: {
            Lock.activate();
            if (!bridge.running)
                bridge.running = true;
        }
    }
}
