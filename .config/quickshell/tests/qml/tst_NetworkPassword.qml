import QtQuick
import QtTest
import qs.Services
import qs.Modules.bar

TestCase {
    id: root
    name: "NetworkPassword"
    when: windowShown
    width: 380
    height: 240

    QtObject { id: first; property string name: "First" }
    QtObject { id: second; property string name: "Second" }
    Loader {
        id: editor
        width: 356
        active: !!Network.promptNet
        sourceComponent: NetworkPassword { net: Network.promptNet }
    }

    function init() {
        Network.pendingNet = null;
        Network.failNet = null;
        Network.failMessage = "";
        Network.busy = false;
        Network.submissions = 0;
        Network.promptNet = first;
        tryVerify(() => editor.item !== null);
    }

    function cleanup() {
        Network.promptNet = null;
        tryCompare(editor, "item", null);
    }

    function input() {
        return findChild(editor.item, "networkPasswordInput");
    }

    function test_enterUnloadsEditorBeforeConnectionCompletes() {
        const field = input();
        field.forceActiveFocus();
        field.text = "synthetic-password";
        keyClick(Qt.Key_Return);
        tryCompare(editor, "item", null);
        compare(Network.pendingNet, first);
        compare(Network.busy, true);
        compare(Network.submissions, 1);
        keyClick(Qt.Key_Return);
        compare(Network.submissions, 1);
    }

    function test_invalidInputIsRetained() {
        const field = input();
        field.forceActiveFocus();
        field.text = "short";
        keyClick(Qt.Key_Return);
        compare(input(), field);
        compare(field.text, "short");
        compare(Network.submissions, 0);
        compare(Network.promptNet, first);
    }

    function test_switchingTargetClearsDraft() {
        input().text = "synthetic-password";
        Network.promptNet = second;
        tryCompare(input(), "text", "");
    }

    function test_escapeUnloadsEditor() {
        input().forceActiveFocus();
        input().text = "synthetic-password";
        keyClick(Qt.Key_Escape);
        tryCompare(editor, "item", null);
        compare(Network.submissions, 0);
    }
}
