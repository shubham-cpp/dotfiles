import QtQuick
import qs.Common

// Presentation only. The preview and the secure surface share this component.
Rectangle {
    id: root

    property date dateTime: new Date()
    property string wallpaperSource: ""
    property string userName: ""
    property bool secure: false
    property bool busy: false
    property bool sleeping: false
    property string errorText: ""
    property bool batteryVisible: false
    property real batteryFraction: 0
    property string batteryText: ""
    property string batteryDetail: ""
    property color batteryColor: Tokens.text
    property bool batteryCharging: false
    property alias passwordText: password.text
    signal submitted

    readonly property bool compact: height < 650 || width < 700
    readonly property bool shortScreen: height < 480
    readonly property int edge: compact ? 24 : 48
    color: Tokens.bg

    function focusPassword(): void {
        password.forceActiveFocus();
    }

    Image {
        anchors.fill: parent
        source: root.wallpaperSource
        fillMode: Image.PreserveAspectCrop
        asynchronous: true
        sourceSize.width: Math.ceil(root.width)
        sourceSize.height: Math.ceil(root.height)
        cache: false
    }

    Rectangle {
        anchors.fill: parent
        gradient: Gradient {
            GradientStop {
                position: 0
                color: Tokens.lockScrimStrong
            }
            GradientStop {
                position: 0.48
                color: Tokens.lockScrimLight
            }
            GradientStop {
                position: 1
                color: Tokens.lockScrimStrong
            }
        }
    }

    Row {
        anchors.left: parent.left
        anchors.top: parent.top
        anchors.margins: root.edge
        spacing: 8
        Glyph {
            name: "lock"
            color: Tokens.text
            font.pixelSize: 14
        }
        Text {
            text: root.secure ? "Locked" : "Securing session…"
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 14
        }
    }

    Column {
        visible: root.batteryVisible
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: root.edge
        spacing: 6

        Row {
            anchors.right: parent.right
            spacing: 10
            BatteryIcon {
                iconHeight: 18
                fraction: root.batteryFraction
                charging: root.batteryCharging
                foreground: root.batteryColor
            }
            Text {
                text: root.batteryText
                color: root.batteryColor
                font.family: Tokens.fontFamily
                font.pixelSize: 14
            }
        }
        Text {
            anchors.right: parent.right
            visible: text !== ""
            text: root.batteryDetail
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 13
        }
    }

    Column {
        anchors.horizontalCenter: parent.horizontalCenter
        visible: root.height >= 300
        y: root.shortScreen ? root.edge + 30 : Math.max(root.edge + 65, root.height * 0.12)
        spacing: root.shortScreen ? 4 : root.compact ? 12 : 24

        Row {
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: 12
            Text {
                id: timeText
                text: (root.dateTime.getHours() % 12 || 12) + Qt.formatDateTime(root.dateTime, ":mm")
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.weight: Font.Light
                font.pixelSize: root.shortScreen ? 56 : Math.max(64, Math.min(176, root.width * 0.11, root.height * 0.2))
                renderType: Text.NativeRendering
            }
            Text {
                anchors.baseline: timeText.baseline
                text: Qt.formatDateTime(root.dateTime, "AP")
                color: Tokens.text
                font.family: Tokens.fontFamily
                font.pixelSize: root.compact ? 16 : 20
            }
        }
        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: Qt.formatDateTime(root.dateTime, "dddd, d MMMM")
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: root.compact ? 16 : 20
            renderType: Text.NativeRendering
        }
    }

    Column {
        id: authBlock
        width: Math.min(Tokens.lockInputWidth, root.width - root.edge * 2)
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: root.shortScreen ? 12 : root.compact ? 28 : Math.max(48, root.height * 0.07)
        spacing: root.shortScreen ? 10 : 16

        Text {
            width: parent.width
            horizontalAlignment: Text.AlignHCenter
            text: root.userName
            textFormat: Text.PlainText
            color: Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 22
            elide: Text.ElideRight
        }

        Rectangle {
            width: parent.width
            height: Tokens.lockInputHeight
            radius: Tokens.radiusSm
            color: Tokens.bgAlt
            border.width: 1
            border.color: root.errorText !== "" ? Tokens.danger : password.activeFocus ? Tokens.accent : Tokens.border

            TextInput {
                id: password
                objectName: "lockPassword"
                anchors.fill: parent
                anchors.leftMargin: 16
                anchors.rightMargin: 52
                verticalAlignment: Text.AlignVCenter
                echoMode: TextInput.Password
                passwordMaskDelay: 0
                maximumLength: 512
                enabled: root.secure && !root.busy && !root.sleeping
                inputMethodHints: Qt.ImhSensitiveData | Qt.ImhNoPredictiveText
                color: Tokens.text
                selectionColor: Tokens.selection
                font.family: Tokens.fontFamily
                font.pixelSize: 16
                clip: true
                Accessible.name: "Password"
                onAccepted: root.submitted()
            }
            Text {
                anchors.left: parent.left
                anchors.leftMargin: 16
                anchors.verticalCenter: parent.verticalCenter
                visible: password.text === ""
                text: root.busy ? "Checking…" : "Password"
                color: Tokens.subtext
                font.family: Tokens.fontFamily
                font.pixelSize: 16
            }
            MouseArea {
                anchors.right: parent.right
                width: 48
                height: parent.height
                enabled: password.enabled
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                Accessible.role: Accessible.Button
                Accessible.name: "Unlock"
                onClicked: root.submitted()
                Text {
                    anchors.centerIn: parent
                    text: "→"
                    color: parent.containsMouse ? Tokens.accent : Tokens.text
                    font.family: Tokens.fontFamily
                    font.pixelSize: 24
                }
            }
        }

        Text {
            width: parent.width
            height: 38
            horizontalAlignment: Text.AlignHCenter
            wrapMode: Text.WordWrap
            text: root.errorText !== "" ? root.errorText : root.busy ? "Checking password…" : root.sleeping ? "Preparing for sleep…" : "Enter to unlock"
            textFormat: Text.PlainText
            color: root.errorText !== "" ? Tokens.danger : Tokens.text
            font.family: Tokens.fontFamily
            font.pixelSize: 14
        }
    }
}
