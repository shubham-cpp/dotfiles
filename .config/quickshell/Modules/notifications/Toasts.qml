pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Window
import qs.Common
import qs.Services

PanelWindow {
    id: win

    property Item replyInput: null

    visible: Notifications.toasts.length > 0
    onVisibleChanged: {
        if (!visible)
            replyInput = null;
    }
    screen: Quickshell.screens[0] || null
    anchors.top: true
    anchors.right: true
    margins.top: Tokens.barHeight + Tokens.toastMargin
    margins.right: Tokens.toastMargin
    implicitWidth: Tokens.toastWidth
    implicitHeight: list.implicitHeight
    exclusionMode: ExclusionMode.Ignore
    color: "transparent"

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell-toasts"
    // Mangowc can focus OnDemand surfaces as soon as they appear.
    WlrLayershell.keyboardFocus: replyInput ? WlrKeyboardFocus.OnDemand : WlrKeyboardFocus.None

    Column {
        id: list
        width: win.width
        spacing: 12
        Window.onActiveChanged: {
            if (!Window.active)
                win.replyInput = null;
        }

        Repeater {
            model: Notifications.toasts

            delegate: ToastCard {
                required property var modelData
                notification: modelData
                width: list.width
                onReplyHoverChanged: (input, hovered) => {
                    // Prepare before the click reaches the compositor; hovering does not activate it.
                    if (hovered)
                        win.replyInput = input;
                    else if (!list.Window.active && win.replyInput === input)
                        win.replyInput = null;
                }
            }
        }
    }
}
