import QtQuick
import QtTest
import "../../Common/NetworkList.js" as NetworkList

TestCase {
    id: root
    name: "NetworkList"
    when: windowShown
    width: 380
    height: 300

    QtObject {
        id: current
        property string name: "Current"
        property bool connected: true
        property bool known: true
        property real signalStrength: 0.3
    }
    QtObject {
        id: nearby
        property string name: "Nearby"
        property bool connected: false
        property bool known: false
        property real signalStrength: 0.9
    }
    QtObject {
        id: saved
        property string name: "Saved"
        property bool connected: false
        property bool known: true
        property real signalStrength: 0.5
    }
    ListModel { id: entries }
    ListView {
        id: view
        anchors.fill: parent
        model: entries
        delegate: Item {
            required property var net
            width: 380
            height: 52
            Text { text: parent.net.name }
        }
        section.property: "group"
        section.delegate: Item {
            required property string section
            height: section === "Nearby" ? 26 : 0
        }
    }

    function cleanup() {
        entries.clear();
        saved.known = true;
    }

    function test_nativeObjectsAndSection() {
        const rows = NetworkList.rows(NetworkList.partition([nearby, saved, current]));
        NetworkList.sync(entries, rows, false);
        compare(entries.count, 3);
        compare(entries.get(0).net, current);
        compare(entries.get(1).net, saved);
        compare(entries.get(2).net, nearby);
        // Materialize delegates and select across the section boundary. A null
        // QObject separator previously crashed Qt's ListModel.data path here.
        view.currentIndex = 2;
        tryVerify(() => view.currentItem !== null);
        compare(view.currentItem.net, nearby);
        view.currentIndex = 0;
        tryCompare(view.currentItem, "net", current);
    }

    function test_stableDelegateAndDeferredOrdering() {
        NetworkList.sync(entries, NetworkList.rows(NetworkList.partition([current, saved, nearby])), false);
        view.currentIndex = 0;
        tryVerify(() => view.currentItem !== null);
        const delegate = view.currentItem;
        NetworkList.sync(entries, NetworkList.rows(NetworkList.partition([nearby, current, saved])), false);
        compare(view.currentItem, delegate);
        saved.known = false;
        const changed = NetworkList.rows(NetworkList.partition([current, saved, nearby]));
        NetworkList.sync(entries, changed, true);
        compare(entries.get(1).net, saved);
        NetworkList.sync(entries, changed, false);
        compare(entries.get(1).net, nearby);
        compare(entries.get(2).group, "Nearby");
        compare(view.currentItem, delegate);
    }

    function test_removalWhileEditing() {
        NetworkList.sync(entries, NetworkList.rows(NetworkList.partition([current, saved, nearby])), false);
        NetworkList.sync(entries, NetworkList.rows(NetworkList.partition([current, nearby])), true);
        compare(entries.count, 2);
        compare(entries.get(1).net, nearby);
    }
}
