import QtQuick
import QtQuick.Controls
import QtTest
import qs.Services
import qs.Modules.bar
import "../../Common/AudioList.js" as AudioList

TestCase {
    id: root
    name: "AudioControls"
    when: windowShown
    visible: true
    width: 480
    height: 400

    Component {
        id: nodeComponent
        QtObject {
            property int id: 1
            property string name: "Test audio"
            property bool ready: true
            property bool isStream: true
            property bool isSink: true
            property var properties: ({})
            property QtObject audio: QtObject {
                property real volume: 0.5
                property bool muted: false
            }
        }
    }
    property var first: null
    property var second: null
    AudioVolumeControl { id: control; width: 340; label: "Test" }
    SignalSpy { id: advanceSpy; target: control; signalName: "advance" }
    SignalSpy { id: lostSpy; target: control; signalName: "targetLost" }
    AudioDeviceSelector { id: selector; y: 60; width: 220 }
    ListModel { id: entries }
    ListView {
        id: view
        y: 170
        width: 400
        height: 180
        model: entries
        delegate: Item {
            required property var node
            width: 400
            height: 44
            Text { text: parent.node ? parent.node.name : "" }
        }
        section.property: "group"
        section.delegate: Item { required property string section; height: 28 }
    }
    function init() {
        first = createTemporaryObject(nodeComponent, root);
        second = createTemporaryObject(nodeComponent, root, {id:2,name:"Second"});
        Audio.nodes = [first, second];
        Audio.ready = true;
        Audio.writes = 0;
        Audio.selections = 0;
        control.node = first;
        control.navigationHandled = false;
        advanceSpy.clear();
        lostSpy.clear();
        selector.current = first;
        selector.devices = [{node:first,label:"First"},{node:second,label:"Second"}];
        selector.waiting = false;
        selector.pending = null;
        selector.message = "";
        wait(10);
    }
    function cleanup() { selector.closeMenu(); entries.clear(); control.node = null; Audio.nodes = []; }
    function test_externalUpdatesNeverWriteAndPreserveBoost() {
        first.audio.volume = 1.4;
        first.audio.muted = true;
        wait(10);
        compare(Audio.writes, 0);
        compare(first.audio.volume, 1.4);
        const slider = findChild(control, "audioSlider");
        compare(slider.value, 1);
        first.audio.volume = 0.3;
        tryCompare(slider, "value", 0.3);
        compare(Audio.writes, 0);
    }
    function test_muteAndKeyboardAdjustments() {
        const mute = findChild(control, "audioMute");
        mouseClick(mute, 16, 16);
        compare(first.audio.muted, true);
        compare(first.audio.volume, 0.5);
        const slider = findChild(control, "audioSlider");
        slider.forceActiveFocus();
        keyClick(Qt.Key_Right);
        compare(first.audio.volume, 0.51);
        compare(first.audio.muted, false);
        keyClick(Qt.Key_Home); compare(first.audio.volume, 0);
        keyClick(Qt.Key_End); compare(first.audio.volume, 1);
        keyClick(Qt.Key_PageDown); compare(first.audio.volume, 0.95);
    }
    function test_dragDoesNotTransferToChangedDefault() {
        const slider = findChild(control, "audioSlider");
        mousePress(slider, slider.width / 2, 18);
        verify(slider.pressed);
        control.node = second;
        const before = Audio.writes;
        mouseMove(slider, slider.width - 10, 18);
        mouseRelease(slider, slider.width - 10, 18);
        compare(second.audio.volume, 0.5);
        compare(Audio.writes, before);
    }
    function test_pointerWritesAndDestructionCancels() {
        const slider = findChild(control, "audioSlider");
        mousePress(slider, slider.width / 4, 18);
        mouseMove(slider, slider.width * 0.7, 18);
        verify(Audio.writes > 0);
        Audio.nodes = [second];
        const before = Audio.writes;
        mouseMove(slider, slider.width - 10, 18);
        mouseRelease(slider, slider.width - 10, 18);
        compare(Audio.writes, before);
    }
    function test_selectorOnlyExplicitActivationWrites() {
        const combo = findChild(selector, "audioOutput");
        selector.current = second;
        compare(Audio.selections, 0);
        selector.devices = [{node:second,label:"Second"},{node:first,label:"First"}];
        compare(Audio.selections, 0);
        combo.activated(1);
        compare(Audio.selected, first);
        compare(Audio.selections, 1);
        verify(selector.waiting);
        selector.current = first;
        verify(!selector.waiting);
        compare(selector.message, "");
    }
    function test_dropdownEscapeAndRemoval() {
        const combo = findChild(selector, "audioOutput");
        mouseClick(combo, combo.width / 2, combo.height / 2);
        tryCompare(combo.popup, "visible", true);
        keyClick(Qt.Key_Escape);
        tryCompare(combo.popup, "visible", false);
        combo.activated(1);
        verify(selector.waiting);
        selector.devices = [{node:first,label:"First"}];
        verify(!selector.waiting);
        compare(selector.message, "Device unavailable");
    }
    function test_nativeObjectSectionsAndIdentityReuse() {
        const rows = [first, second].map((node,i) => ({node, appName:node.name, subtitle:"", group:"sink:"+i,
            groupLabel:"Output "+i, routeDetail:"Output", rank:i, routeChanging:false}));
        AudioList.sync(entries, rows, false);
        view.currentIndex = 1;
        tryVerify(() => view.currentItem !== null);
        compare(view.currentItem.node, second);
        AudioList.sync(entries, [rows[1],rows[0]], false);
        compare(entries.get(0).node, second);
        AudioList.sync(entries, [rows[0]], true);
        compare(entries.count, 1);
        compare(entries.get(0).node, first);
    }
    function test_managedKeyboardTraversalAndFocusedTargetLoss() {
        control.navigationHandled = true;
        control.focusMute();
        keyClick(Qt.Key_Tab);
        verify(findChild(control, "audioSlider").activeFocus);
        keyClick(Qt.Key_Tab);
        compare(advanceSpy.count, 1);
        compare(advanceSpy.signalArguments[0][0], false);
        keyClick(Qt.Key_Backtab);
        verify(findChild(control, "audioMute").activeFocus);
        keyClick(Qt.Key_Backtab);
        compare(advanceSpy.count, 2);
        compare(advanceSpy.signalArguments[1][0], true);
        first.destroy();
        tryVerify(() => lostSpy.count > 0);
        verify(!control.usable);
    }
    function test_deviceTimeoutAndLateBackendChange() {
        findChild(selector, "audioOutput").activated(1);
        verify(selector.waiting);
        tryCompare(selector, "waiting", false, 3500);
        compare(selector.message, "Switch could not be confirmed");
        selector.current = second;
        compare(findChild(selector, "audioOutput").currentIndex, 1);
        compare(selector.message, "");
        compare(Audio.selections, 1);
    }
}
