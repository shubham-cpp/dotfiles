pragma Singleton
import QtQuick

QtObject {
    property var apps: []
    property bool available: true
    property bool paused: false
    property string errorText: ""
    property var lastAction: null
    function endApplication(key, members, force) { lastAction = {key, members, force}; }
}
