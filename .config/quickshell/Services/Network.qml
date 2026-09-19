pragma Singleton
pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Io
import Quickshell.Networking
import QtQuick
import "../Common/NetworkList.js" as NetworkList

Singleton {
    id: root

    property int listGen: 0
    property string iface: ""
    property string ssid: ""
    property bool connected: false
    property bool wifi: false
    property real downBps: 0
    property real upBps: 0
    property var rateConsumers: []
    readonly property bool samplingRates: connected && (open || rateConsumers.length > 0)
    property bool ready: true
    property bool open: false
    property var anchorWindow: null
    property var anchorItem: null
    property var selected: null
    property var promptNet: null
    property var pendingNet: null
    property var pendingDevice: null
    property string pendingKind: ""
    readonly property bool busy: pendingKind.length > 0
    property int pendingFailure: -1
    property bool pendingObserved: false
    property bool pendingSlow: false
    property var failNet: null
    property var retryNet: null
    property string failMessage: ""
    property var scannerDevice: null
    property var preferredDevice: null
    property bool discovering: false

    property real _lastRx: -1
    property real _lastTx: -1
    property real _lastMs: 0

    readonly property bool wifiRadio: Networking.wifiEnabled
    readonly property bool wifiHardwareEnabled: Networking.wifiHardwareEnabled
    readonly property bool backendAvailable: Networking.backend !== NetworkBackendType.None
    readonly property var wifiDevices: Networking.devices.values.filter(d => d && d.type === DeviceType.Wifi)
    readonly property var wifiDevice: {
        const devices = wifiDevices;
        if (pendingDevice && devices.indexOf(pendingDevice) !== -1)
            return pendingDevice;
        if (preferredDevice && devices.indexOf(preferredDevice) !== -1)
            return preferredDevice;
        return devices.find(d => d.connected) || devices.find(d => d.nmManaged) || devices[0] || null;
    }
    readonly property bool usableWifi: !!(backendAvailable && wifiRadio && wifiHardwareEnabled && wifiDevice && wifiDevice.nmManaged)
    readonly property var grouped: {
        if (!open || !usableWifi)
            return {
                connected: [],
                known: [],
                other: []
            };
        const _ = listGen;
        return NetworkList.partition(networkValues());
    }
    readonly property var savedNets: NetworkList.saved(grouped)
    readonly property var otherNets: grouped.other || []

    function fmt(n) {
        if (n < 1024)
            return Math.round(n) + "B";
        if (n < 1024 * 1024)
            return Math.round(n / 1024) + "K";
        return (n / (1024 * 1024)).toFixed(1) + "M";
    }

    function networkValues() {
        const dev = wifiDevice;
        if (!dev)
            return [];
        return dev.networks.values;
    }

    function wifiGlyph(net) {
        return NetworkList.wifiGlyph(net ? net.signalStrength : 0);
    }

    function isPsk(net) {
        if (!net)
            return false;
        const s = net.security;
        return s === WifiSecurityType.WpaPsk || s === WifiSecurityType.Wpa2Psk || s === WifiSecurityType.Sae;
    }

    function isOpen(net) {
        if (!net)
            return false;
        const s = net.security;
        return s === WifiSecurityType.Open || s === WifiSecurityType.Owe;
    }

    function securityLabel(net) {
        if (!net)
            return "";
        if (pendingNet === net)
            return pendingSlow ? "Waiting for NetworkManager…" : (pendingKind === "disconnect" ? "Disconnecting…" : "Connecting…");
        if (net.connected)
            return "Connected";
        if (net.stateChanging)
            return net.state === ConnectionState.Disconnecting ? "Disconnecting…" : "Connecting…";
        if (net.known)
            return "Saved";
        if (net.security === WifiSecurityType.Owe)
            return "Encrypted · No password";
        if (isOpen(net))
            return "Open";
        if (isPsk(net))
            return "Secured";
        return "Needs extra login";
    }

    function refreshIdentity() {
        const devices = Networking.devices.values;
        let dev = null;
        for (let i = 0; i < devices.length; i++) {
            const d = devices[i];
            if (d && d.connected && d.name !== "lo") {
                dev = d;
                break;
            }
        }
        if (!dev) {
            iface = "";
            ssid = "";
            connected = false;
            wifi = false;
            return;
        }
        iface = dev.name;
        connected = true;
        wifi = dev.type === DeviceType.Wifi;
        let netName = "";
        const nets = dev.networks.values;
        for (let i = 0; i < nets.length; i++) {
            const n = nets[i];
            if (n && n.connected) {
                netName = n.name;
                break;
            }
        }
        ssid = netName;
    }

    function setRateConsumer(consumer, enabled) {
        const index = rateConsumers.indexOf(consumer);
        if (enabled && index < 0)
            rateConsumers = rateConsumers.concat([consumer]);
        else if (!enabled && index >= 0)
            rateConsumers = rateConsumers.filter(item => item !== consumer);
    }

    function resetRates() {
        _lastRx = -1;
        _lastTx = -1;
        _lastMs = 0;
        downBps = 0;
        upBps = 0;
    }

    function pollBytes() {
        if (!samplingRates || !iface.length) {
            resetRates();
            return;
        }
        rxFile.reload();
        txFile.reload();
        const rx = parseInt(rxFile.text(), 10);
        const tx = parseInt(txFile.text(), 10);
        const now = Date.now();
        if (!isNaN(rx) && !isNaN(tx) && _lastRx >= 0 && rx >= _lastRx && tx >= _lastTx && now > _lastMs) {
            const dt = (now - _lastMs) / 1000;
            downBps = Math.max(0, (rx - _lastRx) / dt);
            upBps = Math.max(0, (tx - _lastTx) / dt);
        } else {
            downBps = 0;
            upBps = 0;
        }
        if (!isNaN(rx))
            _lastRx = rx;
        if (!isNaN(tx))
            _lastTx = tx;
        _lastMs = now;
    }

    function setScanning(on) {
        const dev = on && open && usableWifi ? wifiDevice : null;
        if (scannerDevice && scannerDevice !== dev)
            scannerDevice.scannerEnabled = false;
        scannerDevice = dev;
        if (dev && !dev.scannerEnabled)
            dev.scannerEnabled = true;
    }

    function bumpList() {
        listGen++;
        const nets = savedNets.concat(otherNets);
        if (!selected || nets.indexOf(selected) === -1)
            selected = nets.length ? nets[0] : null;
        if (promptNet && nets.indexOf(promptNet) === -1)
            promptNet = null;
    }

    function toggle(win, item) {
        if (open) {
            close();
            return false;
        }
        Brightness.close();
        anchorWindow = win || null;
        anchorItem = item || null;
        open = true;
        promptNet = null;
        setScanning(true);
        bumpList();
        if (pendingNet)
            selected = pendingNet;
        else if (failNet && networkValues().indexOf(failNet) !== -1)
            selected = failNet;
        return true;
    }

    function close() {
        open = false;
        selected = null;
        promptNet = null;
        setScanning(false);
        anchorItem = null;
        anchorWindow = null;
    }

    function setWifiEnabled(on) {
        if (on && !Networking.wifiHardwareEnabled)
            return;
        Networking.wifiEnabled = on;
        setScanning(on);
    }

    function activate(net) {
        if (!net || !usableWifi || busy || net.stateChanging)
            return;
        selected = net;
        promptNet = null;
        failMessage = "";
        failNet = null;
        if (net.connected)
            return;
        if (retryNet === net && isPsk(net)) {
            promptNet = net;
            return;
        }
        if (net.known || isOpen(net)) {
            beginPending(net, "connect");
            net.connect();
            return;
        }
        if (isPsk(net)) {
            promptNet = net;
            return;
        }
        promptNet = null;
        failNet = net;
        failMessage = "This network needs advanced authentication. Open connection settings to configure it.";
    }

    function submitPsk(psk) {
        const net = promptNet;
        const secret = String(psk || "");
        if (!net || busy || !usableWifi || net.stateChanging || !isPsk(net))
            return false;
        const valid = net.security === WifiSecurityType.Sae ? secret.length > 0 : (/^[\x20-\x7e]{8,63}$/.test(secret) || /^[0-9a-fA-F]{64}$/.test(secret));
        if (!valid) {
            failNet = net;
            failMessage = net.security === WifiSecurityType.Sae ? "Enter a password." : "Use 8–63 ASCII characters or a 64-digit hexadecimal key.";
            return false;
        }
        beginPending(net, "connect");
        net.connectWithPsk(secret);
        return true;
    }

    function handleFail(net, reason) {
        if (!net || (net !== pendingNet && (busy || net !== failNet)))
            return;
        if (net === pendingNet) {
            pendingFailure = reason;
            // Reason and state arrive on separate D-Bus signals. Do not permit
            // retry while the backend still reports an activation in progress.
            if (net.state !== ConnectionState.Disconnected)
                return;
            finishPending();
        }
        const needsPassword = isPsk(net) && (reason === ConnectionFailReason.NoSecrets || reason === ConnectionFailReason.WifiAuthTimeout);
        failNet = net;
        retryNet = needsPassword ? net : null;
        if (reason === ConnectionFailReason.NoSecrets)
            failMessage = "Password required or not accepted. Try again.";
        else if (reason === ConnectionFailReason.WifiAuthTimeout)
            failMessage = "Authentication timed out. Check the password and try again.";
        else if (reason === ConnectionFailReason.WifiNetworkLost)
            failMessage = "Network lost. Move closer and try again.";
        else
            failMessage = "Could not complete the connection. Try again.";
        if (open) {
            selected = net;
            if (needsPassword)
                promptNet = net;
        }
    }

    function beginPending(net, kind) {
        pendingKind = kind;
        pendingDevice = net.device;
        pendingFailure = -1;
        pendingObserved = false;
        pendingSlow = false;
        pendingNet = net;
        promptNet = null;
        failNet = null;
        retryNet = null;
        failMessage = "";
        watchdog.interval = 15000;
        watchdog.restart();
    }

    function finishPending() {
        watchdog.stop();
        pendingNet = null;
        pendingKind = "";
        pendingDevice = null;
        pendingFailure = -1;
        pendingObserved = false;
        pendingSlow = false;
        refreshIdentity();
        bumpList();
    }

    function observePending() {
        const net = pendingNet;
        if (!net)
            return;
        if (pendingFailure >= 0 && net.state === ConnectionState.Disconnected) {
            handleFail(net, pendingFailure);
            return;
        }
        const done = pendingKind === "disconnect" ? net.state === ConnectionState.Disconnected : (net.connected && pendingFailure < 0);
        if (done) {
            failMessage = "";
            failNet = null;
            retryNet = null;
            finishPending();
        } else if (net.stateChanging && !pendingObserved) {
            pendingObserved = true;
            watchdog.interval = 90000;
            watchdog.restart();
        } else if (pendingObserved && net.state === ConnectionState.Disconnected) {
            handleFail(net, ConnectionFailReason.Unknown);
        }
    }

    function pendingTimeout() {
        observePending();
        if (!busy)
            return;
        pendingSlow = true;
        failNet = pendingNet;
        failMessage = "NetworkManager has not confirmed the result. Check connection settings, or turn Wi-Fi off and on before retrying.";
        // A timeout is not cancellation. Retain ownership until native state or
        // radio/device invalidation resolves it; never submit an overlapping join.
    }

    function disconnectNet(net) {
        if (!net || !net.connected || busy || net.stateChanging || !usableWifi)
            return;
        beginPending(net, "disconnect");
        net.disconnect();
    }

    function reconcileDevice() {
        setScanning(open);
        if (!usableWifi) {
            promptNet = null;
            selected = null;
        }
        if (busy && (!usableWifi || wifiDevices.indexOf(pendingDevice) === -1)) {
            finishPending();
            failNet = null;
            retryNet = null;
            failMessage = "Connection interrupted because Wi-Fi became unavailable.";
        }
        discovering = !!(open && usableWifi);
        if (discovering)
            discovery.restart();
        else
            discovery.stop();
        bumpList();
    }

    function forgetNet(net) {
        if (!net || busy || net.stateChanging)
            return;
        if (promptNet === net)
            promptNet = null;
        failMessage = "";
        failNet = null;
        retryNet = null;
        net.forget();
    }

    function selectMove(delta) {
        if (!open)
            return;
        const nets = savedNets.concat(otherNets);
        if (!nets.length)
            return;
        let i = nets.indexOf(selected);
        if (i < 0)
            i = delta > 0 ? -1 : 0;
        i = Math.max(0, Math.min(nets.length - 1, i + delta));
        selected = nets[i];
    }

    function activateSelected() {
        if (promptNet)
            return;
        if (selected)
            activate(selected);
    }

    function losePendingTarget() {
        pendingNet = null;
        watchdog.stop();
        pendingSlow = true;
        failNet = null;
        retryNet = null;
        failMessage = "The network disappeared before its result was confirmed. Check connection settings, or turn Wi-Fi off and on before retrying.";
        // Native activation can outlive its QObject. Preserve device ownership.
    }

    onIfaceChanged: resetRates()
    onSamplingRatesChanged: {
        resetRates();
        if (samplingRates)
            Qt.callLater(root.pollBytes);
    }
    onUsableWifiChanged: Qt.callLater(root.reconcileDevice)
    onWifiDeviceChanged: Qt.callLater(root.reconcileDevice)
    onWifiDevicesChanged: Qt.callLater(root.reconcileDevice)
    onOpenChanged: Qt.callLater(root.reconcileDevice)

    Connections {
        id: operationEvents
        target: root.pendingNet || root.failNet
        function onConnectedChanged() {
            Qt.callLater(root.observePending);
        }
        function onStateChanged() {
            Qt.callLater(root.observePending);
        }
        function onConnectionFailed(reason) {
            root.handleFail(operationEvents.target, reason);
        }
        function onDestroyed() {
            if (root.busy)
                root.losePendingTarget();
            else
                root.failNet = null;
        }
    }

    Timer {
        id: watchdog
        onTriggered: root.pendingTimeout()
    }

    Timer {
        id: discovery
        interval: 12000
        onTriggered: root.discovering = false
    }

    Instantiator {
        model: Networking.devices
        delegate: Connections {
            required property var modelData
            target: modelData
            function onConnectedChanged() {
                root.refreshIdentity();
            }
            function onNameChanged() {
                root.refreshIdentity();
            }
        }
        onObjectAdded: root.refreshIdentity()
        onObjectRemoved: root.refreshIdentity()
    }

    FileView {
        id: rxFile
        path: root.samplingRates && root.iface.length ? "/sys/class/net/" + root.iface + "/statistics/rx_bytes" : ""
        blockLoading: true
        printErrors: false
    }

    FileView {
        id: txFile
        path: root.samplingRates && root.iface.length ? "/sys/class/net/" + root.iface + "/statistics/tx_bytes" : ""
        blockLoading: true
        printErrors: false
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        onTriggered: {
            // Device events do not cover every connected-network identity change.
            root.refreshIdentity();
            if (root.samplingRates)
                root.pollBytes();
        }
    }

    IpcHandler {
        target: "network"

        function toggle(): bool {
            return root.toggle(null);
        }

        function close(): void {
            root.close();
        }

        function status(): string {
            return JSON.stringify({
                open: root.open,
                scanning: !!(root.scannerDevice && root.scannerDevice.scannerEnabled),
                networks: root.savedNets.length + root.otherNets.length,
                prompting: !!root.promptNet,
                pending: root.pendingKind,
                slow: root.pendingSlow
            });
        }

        function setWifiEnabled(on: bool): void {
            root.setWifiEnabled(on);
        }
    }

    Component.onCompleted: refreshIdentity()
    Component.onDestruction: setScanning(false)
}
