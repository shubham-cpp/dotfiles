pragma Singleton
import QtQuick

// UI contract fake. Actual service handlers are tested in network-service.test.cjs.
QtObject {
    property var promptNet: null
    property var pendingNet: null
    property var failNet: null
    property string failMessage: ""
    property bool busy: false
    property int submissions: 0

    function submitPsk(text) {
        if (busy || !promptNet)
            return false;
        if (text.length < 8) {
            failNet = promptNet;
            failMessage = "Invalid password";
            return false;
        }
        busy = true;
        pendingNet = promptNet;
        promptNet = null;
        submissions++;
        return true;
    }
}
