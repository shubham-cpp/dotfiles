import QtQuick
import QtTest
import Quickshell.Wayland
import qs.Common
import qs.Modules.bar
import qs.Services

TestCase {
    id: root
    name: "BarLifetime"
    when: windowShown
    visible: true
    width: 700
    height: 300

    QtObject { id: screenOne; property string name: "test-screen" }
    QtObject { id: screenTwo }
    QtObject {
        id: browser
        property string appId: "browser"
        property var screens: [screenOne]
        property var parent: null
        property bool activated: false
        function activate() {}
    }
    TagList { id: tags; y: 100; screen: screenOne }
    TaskList { id: firstTasks; screen: screenOne }
    TaskList { id: secondTasks; y: 50; screen: screenTwo }
    Component { id: rates; NetworkStatus {} }
    Component { id: clockWidget; ClockWidget { anchorWindow: screenOne } }

    function task(list) {
        const children = list.children[0].children;
        for (const child of children) {
            if (child.modelData === browser)
                return child;
        }
        return null;
    }
    function test_workspace_highlight_fills_rounded_segment() {
        Workspaces.tagsByMonitor = {"test-screen": [{index: 1, name: "1", active: true, urgent: false, occupied: true}]};
        Workspaces.gen++;
        const cell = tags.children[0].children.find(child => child.modelData && child.modelData.index === 1);
        verify(cell !== undefined);
        const highlight = cell.children[0];
        compare(tags.width, cell.width);
        compare(tags.radius, 8);
        compare(highlight.x, 0);
        compare(highlight.y, 0);
        compare(highlight.width, cell.width);
        compare(highlight.height, cell.height);
        compare(highlight.radius, tags.radius);
        compare(highlight.color, Tokens.barHover);
    }
    function test_task_highlight_fills_rounded_segment() {
        browser.activated = true;
        const cell = task(firstTasks).item;
        const highlight = cell.children[0];
        compare(firstTasks.width, cell.width);
        compare(firstTasks.radius, 8);
        compare(highlight.x, 0);
        compare(highlight.y, 0);
        compare(highlight.width, cell.width);
        compare(highlight.height, cell.height);
        compare(highlight.radius, firstTasks.radius);
        compare(highlight.color, Tokens.barHover);
    }
    function init() {
        ToplevelManager.toplevels = [];
        browser.activated = false;
        Workspaces.tagsByMonitor = ({});
        Workspaces.gen++;
        browser.screens = [screenOne];
        browser.parent = null;
        ToplevelManager.toplevels = [browser];
        Network.connected = true;
    }
    function test_task_contents_follow_screen_changes_and_parenting() {
        verify(task(firstTasks).item !== null);
        compare(task(secondTasks).item, null);
        tryCompare(firstTasks, "implicitWidth", Tokens.barModuleHeight);
        compare(secondTasks.implicitWidth, 0);
        browser.screens = [screenTwo];
        tryCompare(task(firstTasks), "item", null);
        verify(task(secondTasks).item !== null);
        tryCompare(firstTasks, "implicitWidth", 0);
        tryCompare(secondTasks, "implicitWidth", Tokens.barModuleHeight);
        browser.parent = root;
        tryCompare(task(secondTasks), "item", null);
        browser.parent = null;
        browser.screens = [];
        verify(task(firstTasks).item !== null);
        verify(task(secondTasks).item !== null);
    }
    function test_rates_are_loaded_and_registered_only_while_visible() {
        const first = createTemporaryObject(rates, root, {showRates: false});
        const second = createTemporaryObject(rates, root, {showRates: true, y: 100});
        tryCompare(Network, "consumerCount", 1);
        const compactWidth = first.implicitWidth;
        first.showRates = true;
        tryCompare(Network, "consumerCount", 2);
        tryVerify(() => first.implicitWidth > compactWidth);
        first.visible = false;
        tryCompare(Network, "consumerCount", 1);
        Network.connected = false;
        tryCompare(Network, "consumerCount", 0);
        Network.connected = true;
        tryCompare(Network, "consumerCount", 1);
        second.showRates = false;
        tryCompare(Network, "consumerCount", 0);
        first.visible = true;
        tryCompare(Network, "consumerCount", 1);
        first.destroy();
        tryCompare(Network, "consumerCount", 0);
    }
    function test_clock_click_passes_its_window_to_calendar() {
        const widget = createTemporaryObject(clockWidget, root, {x: 250, y: 150});
        verify(widget !== null);
        Agenda.open = false;
        Agenda.anchorWindow = null;
        mouseClick(widget, widget.width / 2, widget.height / 2, Qt.LeftButton);
        compare(Agenda.open, true);
        compare(Agenda.anchorWindow, screenOne);
    }
}
