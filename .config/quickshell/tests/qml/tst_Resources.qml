import QtQuick
import QtTest
import qs.Modules.bar
import qs.Services

TestCase {
    id: root
    name: "ResourceControls"
    when: windowShown
    visible: true
    width: 760
    height: 520

    ResourceColumn {
        id: column
        width: 360
        height: 440
        metric: "memory"
        title: "Memory"
        summary: "12 / 32 GB"
    }
    function app(key, memory) {
        return {key, name:key, icon:"", memory, cpu:3, count:1,
            canEnd:true, state:"", members:[{pid:memory,started:1}]};
    }
    function init() {
        Resources.apps = [];
        Resources.lastAction = null;
        Resources.paused = false;
        Resources.apps = [app("Browser", 200), app("Editor", 100)];
        mouseMove(root, 740, 500);
        root.forceActiveFocus();
        wait(20);
    }
    function row() { return findChild(column, "resourceEnd").parent; }
    function test_inline_confirmation_and_cancel() {
        const button = findChild(column, "resourceEnd");
        mouseClick(button, 16, 16);
        compare(row().confirming, true);
        compare(Resources.lastAction, null);
        mouseClick(findChild(row(), "resourceCancel"), 16, 16);
        compare(row().confirming, false);
        compare(Resources.lastAction, null);
    }
    function test_confirmation_keeps_original_members_as_values_refresh() {
        const selected = row();
        mouseClick(findChild(selected, "resourceEnd"), 16, 16);
        const key = selected.appKey;
        const captured = selected.capturedMembers;
        Resources.apps = [app("Editor", 900), app("Browser", 300)];
        wait(20);
        compare(selected.appKey, key);
        mouseClick(findChild(selected, "resourceConfirm"), 15, 15);
        compare(Resources.lastAction.key, key);
        compare(Resources.lastAction.members, captured);
        compare(Resources.lastAction.force, false);
    }
    function test_exited_app_disables_confirmation_without_shifting_rows() {
        const selected = row();
        mouseClick(findChild(selected, "resourceEnd"), 16, 16);
        const key = selected.appKey;
        Resources.apps = Resources.apps.filter(app => app.key !== key);
        wait(20);
        compare(selected.appKey, key);
        compare(selected.gone, true);
        compare(selected.confirming, false);
        compare(findChild(selected, "resourceEnd").enabled, false);
        compare(Resources.lastAction, null);
    }
    function test_escape_cancels_before_closing() {
        const selected = row();
        mouseClick(findChild(selected, "resourceEnd"), 16, 16);
        keyClick(Qt.Key_Escape);
        compare(selected.confirming, false);
        compare(Resources.lastAction, null);
    }
}
