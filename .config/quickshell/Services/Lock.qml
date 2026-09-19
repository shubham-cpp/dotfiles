pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

// Lock ownership stays on WlSessionLock in shell.qml. Never expose unlock IPC.
Singleton {
    id: root

    readonly property bool ready: true
    property bool locked: false
    property bool secure: false
    property string currentText: ""
    property bool unlockInProgress: false
    property bool showFailure: false
    property string errorText: ""
    property int focusGen: 0
    property int epoch: 0
    property int attemptEpoch: -1
    property string authReply: ""

    onCurrentTextChanged: {
        if (!unlockInProgress) {
            showFailure = false;
            errorText = "";
        }
    }
    onLockedChanged: Logind.reportState()

    function cancelAuthentication(): void {
        epoch++;
        unlockInProgress = false;
        authTimeout.stop();
        if (auth.running)
            auth.running = false;
        currentText = "";
        authReply = "";
    }

    function activate(): void {
        cancelAuthentication();
        locked = true;
        showFailure = false;
        errorText = "";
        focusGen++;
        Logind.reportState();
    }

    function setSecure(on: bool): void {
        secure = on;
        if (!on && unlockInProgress)
            cancelAuthentication();
        Logind.reportState();
    }

    function focus(): void {
        if (locked)
            focusGen++;
    }

    function tryUnlock(): void {
        if (!locked || !secure || Logind.preparingForSleep || currentText === "" || unlockInProgress || auth.running)
            return;
        attemptEpoch = epoch;
        authReply = "";
        errorText = "";
        showFailure = false;
        unlockInProgress = true;
        authTimeout.restart();
        auth.running = true;
    }

    function finishAttempt(exitCode: int): void {
        if (attemptEpoch !== epoch || !unlockInProgress || !locked)
            return;
        authTimeout.stop();
        currentText = "";
        const success = exitCode === 0 && authReply === "QS_AUTH_SUCCESS" && secure && !Logind.preparingForSleep;
        unlockInProgress = false;
        authReply = "";
        if (success) {
            locked = false;
        } else {
            showFailure = true;
            errorText = exitCode === 1 ? "Incorrect password. Try again." : "Authentication unavailable. Try again.";
            focusGen++;
        }
        Logind.reportState();
    }

    IpcHandler {
        target: "lock"

        function activate(): void { root.activate(); }
        function focus(): void { root.focus(); }
        function status(): string { return root.secure && root.locked ? "secure" : root.locked ? "locking" : "unlocked"; }
    }

    Process {
        id: auth
        command: [Quickshell.shellPath(".local/bin/lock-auth")]
        stdinEnabled: true
        onStarted: {
            if (root.attemptEpoch !== root.epoch || !root.unlockInProgress) {
                running = false;
                return;
            }
            write(root.currentText + "\n");
            root.currentText = "";
        }
        stdout: SplitParser {
            onRead: data => root.authReply = data
        }
        onExited: (exitCode, exitStatus) => root.finishAttempt(exitStatus === 0 ? exitCode : 2)
    }

    Timer {
        id: authTimeout
        interval: 30000
        onTriggered: {
            root.cancelAuthentication();
            root.showFailure = true;
            root.errorText = "Authentication timed out. Try again.";
            root.focusGen++;
        }
    }
}
