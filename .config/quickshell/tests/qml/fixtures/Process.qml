import QtQuick

QtObject {
    property bool running: false
    property var command: []
    property QtObject stdout: null
    signal exited(int exitCode, int exitStatus)

    function failStart() { running = false; }

    function finishExit(code) {
        // Quickshell v0.3.1 emits exited before runningChanged.
        exited(code, 0);
        running = false;
    }

    function finishOutput(output) {
        if (!stdout) return;
        stdout.text = output;
        stdout.streamFinished();
    }

    function finish(output, code, outputFirst) {
        if (outputFirst) finishOutput(output);
        finishExit(code);
        if (!outputFirst) finishOutput(output);
    }
}
