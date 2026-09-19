pragma Singleton

import Quickshell
import QtQuick

Singleton {
    // Neutral overlays and coral selection, matching the existing Rofi theme.
    readonly property color bg: "#17171b"
    readonly property color bgAlt: "#111114"
    readonly property color surface: "#25252b"
    readonly property color overlay: "#85858f"
    readonly property color text: "#f2f2f4"
    readonly property color subtext: "#b4b4be"
    readonly property color accent: "#ff6363"
    readonly property color danger: "#ff7474"
    readonly property color warning: "#e8c278"
    readonly property color success: "#a8c99c"
    readonly property color border: "#3b3b43"
    readonly property color hover: "#2c2c33"
    readonly property color selection: "#34292d"
    readonly property color separator: "#2c2c32"

    // The bar keeps the cool, compact labels from the Waybar reference.
    readonly property color barBg: "#202027"
    readonly property color barModule: "#101013"
    readonly property color barHover: "#24242b"
    readonly property color barText: "#c5cee9"
    readonly property color barAccent: "#98becb"

    readonly property int radius: 12
    readonly property int radiusSm: 6
    readonly property int pad: 12
    readonly property int padSm: 8
    readonly property int fontSm: 12
    readonly property int fontMd: 14
    readonly property int fontLg: 16
    readonly property int barHeight: 44
    readonly property int barModuleHeight: 36
    readonly property int barGap: 4
    readonly property int rowHeight: 30
    readonly property int overlayGap: 6
    readonly property int calendarWidth: 520
    readonly property int calendarMaxHeight: 600
    readonly property int audioWidth: 480
    readonly property int audioMaxHeight: 520
    readonly property int brightnessWidth: 320
    readonly property int resourcesWidth: 760

    readonly property string fontFamily: "Fira Sans"
    readonly property string barFontFamily: "JetBrainsMono Nerd Font"
    readonly property string iconFontFamily: "Symbols Nerd Font"
    readonly property string emojiFontFamily: "Noto Color Emoji"
    readonly property int emojiWidth: 560
    readonly property int emojiCellSize: 48
    readonly property int emojiGlyphSize: 30

    readonly property int panelRadius: 20
    readonly property int launcherWidth: 760
    readonly property int launcherRowHeight: 52
    readonly property int searchHeight: 64
    readonly property int footerHeight: 40
    readonly property int clipboardWidth: 920
    readonly property int clipboardRowHeight: 68
    readonly property int previewGap: 12

    readonly property int toastCap: 4
    readonly property int toastWidth: 360
    readonly property int toastMargin: 16
    readonly property int notificationCenterWidth: 420
    readonly property int notificationCenterMaxHeight: 640
    readonly property int overlayDestroyMs: 600
    readonly property int osdHideMs: 1000

    readonly property int lockInputWidth: 360
    readonly property int lockInputHeight: 50
    readonly property color lockScrimStrong: "#ad111114"
    readonly property color lockScrimLight: "#5c111114"
}
