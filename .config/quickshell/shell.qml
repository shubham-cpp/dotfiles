//@ pragma UseQApplication
//@ pragma ShellId qs-shell
//@ pragma DataDir $BASE/qs-shell
//@ pragma StateDir $BASE/qs-shell
//@ pragma CacheDir $BASE/qs-shell
//@ pragma DropExpensiveFonts

import Quickshell
import QtQuick
import Quickshell.Io
import Quickshell.Wayland
import qs.Modules.bar
import qs.Services
import qs.Modules.emoji
import "Modules/lock" as LockMod
import "Modules/notifications" as Notifs
import "Modules/osd" as OsdMod
import "Modules/launcher" as LaunchMod
import "Modules/files" as FilesMod
import "Modules/clipboard" as ClipMod
import "Modules/calendar" as CalMod

ShellRoot {
    // v0.3.1 does not safely inherit a locked session on configuration reload.
    // Production changes are loaded explicitly through the guarded IPC below.
    Component.onCompleted: Quickshell.watchFiles = false
    // Singletons construct on first reference.
    readonly property bool _pinClock: Clock.ready
    readonly property int _pinWorkspaces: Workspaces.gen
    readonly property bool _pinAudio: Audio.ready
    readonly property bool _pinNetwork: Network.ready
    readonly property bool _pinBrightness: Brightness.present
    readonly property bool _pinNightLight: NightLight.ready
    readonly property bool _pinPower: Power.ready
    readonly property bool _pinSystemStats: SystemStats.ready
    readonly property bool _pinResources: Resources.ready
    readonly property bool _pinIdle: Idle.ready
    readonly property bool _pinNotifs: Notifications.ready
    readonly property bool _pinOsd: Osd.ready
    readonly property bool _pinLauncher: LauncherStats.ready
    readonly property bool _pinFiles: Files.ready
    readonly property bool _pinClipboard: Clipboard.ready
    readonly property bool _pinEmoji: Emoji.ready
    readonly property bool _pinAgenda: Agenda.ready
    readonly property bool _pinReminders: Reminders.ready
    readonly property bool _pinFootball: Football.ready
    readonly property bool _pinLock: Lock.ready
    readonly property bool _pinLogind: Logind.ready

    WlSessionLock {
        id: sessionLock
        locked: Lock.locked
        onSecureChanged: {
            Lock.setSecure(secure);
        }

        WlSessionLockSurface {
            LockMod.LockSurface {
                anchors.fill: parent
            }
        }
    }

    Bar {
        id: bar
    }
    Notifs.Toasts {}
    OsdMod.OsdPill {}

    LazyLoader {
        active: Notifications.centerOpen
        Notifs.Center {
            anchorWindow: bar.primaryWindow
        }
    }

    LazyLoader {
        active: LauncherStats.open
        LaunchMod.Launcher {}
    }

    LazyLoader {
        activeAsync: Files.open
        FilesMod.Picker {}
    }

    LazyLoader {
        active: Clipboard.open
        ClipMod.Picker {}
    }

    LazyLoader {
        activeAsync: Emoji.open
        Picker {}
    }

    LazyLoader {
        active: Agenda.open
        CalMod.Popup {
            anchorWindow: Agenda.anchorWindow || bar.primaryWindow
        }
    }

    LazyLoader {
        active: Power.open
        BatteryPopup {
            anchorWindow: Power.anchorWindow || bar.primaryWindow
        }
    }

    LazyLoader {
        active: Network.open
        NetworkPopup {
            id: networkPopup
            anchorWindow: Network.anchorWindow || bar.primaryWindow
            anchorItem: Network.anchorItem || bar.primaryNetworkAnchor
            Connections {
                target: bar
                function onNetworkAnchorMoved() {
                    Qt.callLater(networkPopup.updatePosition);
                }
            }
        }
    }

    LazyLoader {
        active: Brightness.open
        BrightnessPopup {
            id: brightnessPopup
            anchorWindow: Brightness.anchorWindow || bar.primaryWindow
            anchorItem: Brightness.anchorItem || bar.primaryBrightnessAnchor
            anchorControl: Brightness.anchorControl || bar.primaryBrightnessControl
            Connections {
                target: bar
                function onBrightnessAnchorMoved() {
                    Qt.callLater(brightnessPopup.updatePosition);
                }
            }
        }
    }

    LazyLoader {
        active: Audio.open
        VolumePopup {
            id: volumePopup
            anchorWindow: Audio.anchorWindow || bar.primaryWindow
            anchorItem: Audio.anchorItem || bar.primaryVolumeAnchor
            anchorControl: Audio.anchorControl || bar.primaryVolumeControl
            Connections {
                target: bar
                function onVolumeAnchorMoved() {
                    Qt.callLater(volumePopup.updatePosition);
                }
            }
        }
    }

    LazyLoader {
        active: Resources.open
        ResourcesPopup {
            id: resourcesPopup
            anchorWindow: Resources.anchorWindow || bar.primaryWindow
            anchorItem: Resources.anchorItem || bar.primarySystemAnchor
            Connections {
                target: bar
                function onSystemAnchorMoved() {
                    Qt.callLater(resourcesPopup.updatePosition);
                }
            }
        }
    }

    IpcHandler {
        target: "shell"

        function ping(): string {
            return "ok";
        }

        function reload(): bool {
            if (Lock.locked || Lock.unlockInProgress || !Logind.ready || Logind.preparingForSleep)
                return false;
            Quickshell.reload(false);
            return true;
        }
    }
}
