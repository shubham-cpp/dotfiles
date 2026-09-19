pragma Singleton
import QtQuick

QtObject {
    property bool connected: true
    property bool wifi: true
    property bool wifiRadio: true
    property bool open: false
    property string ssid: "Test"
    property string iface: "wlan-test"
    property real downBps: 0
    property real upBps: 0
    property var consumers: []
    readonly property int consumerCount: consumers.length

    function fmt(value) { return String(value); }
    function setRateConsumer(consumer, enabled) {
        if (enabled)
            consumers = consumers.concat([consumer]);
        else
            consumers = consumers.filter(item => item !== consumer);
    }
}
