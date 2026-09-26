import QtQuick
import QtTest
import "../../Modules/lock"

TestCase {
    name: "LockContent"
    when: windowShown
    width: 800
    height: 600

    LockContent {
        id: content
        anchors.fill: parent
        userName: "Fixture"
    }
    SignalSpy { id: submissions; target: content; signalName: "submitted" }

    function init() {
        content.secure = true;
        content.busy = false;
        content.sleeping = false;
        content.passwordText = "";
        content.focusPassword();
        submissions.clear();
    }

    function test_lockNotificationListOnlyShowsNames() {
        content.lockNotifications = ["Mail", "Chat"];
        const list = findChild(content, "lockNotificationList");
        const rows = findChild(content, "lockNotificationRows");
        verify(list.height > 0);
        compare(rows.count, 2);
        compare(rows.itemAt(0).text, "Mail");
        compare(rows.itemAt(1).text, "Chat");
        content.lockNotifications = [];
        compare(list.height, 0);
        const field = findChild(content, "lockPassword");
        verify(field.activeFocus);
    }

    function test_maskedPasswordAndEnter() {
        const field = findChild(content, "lockPassword");
        compare(field.echoMode, TextInput.Password);
        compare(field.passwordMaskDelay, 0);
        verify(field.inputMethodHints & Qt.ImhSensitiveData);
        keyClick(Qt.Key_A);
        keyClick(Qt.Key_B);
        compare(content.passwordText, "ab");
        verify(field.displayText.indexOf("a") === -1);
        verify(field.displayText.indexOf("b") === -1);
        keyClick(Qt.Key_Return);
        compare(submissions.count, 1);
    }

    function test_unavailableInputCannotSubmit_data() {
        return [{tag: "insecure", secure: false, busy: false, sleeping: false},
                {tag: "busy", secure: true, busy: true, sleeping: false},
                {tag: "sleep", secure: true, busy: false, sleeping: true}];
    }

    function test_unavailableInputCannotSubmit(data) {
        content.secure = data.secure;
        content.busy = data.busy;
        content.sleeping = data.sleeping;
        const field = findChild(content, "lockPassword");
        verify(!field.enabled);
        keyClick(Qt.Key_A);
        keyClick(Qt.Key_Return);
        compare(content.passwordText, "");
        compare(submissions.count, 0);
    }
}
