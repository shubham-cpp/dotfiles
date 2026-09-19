import QtQuick
import qs.Common

Item {
    id: root

    property real fraction: 0
    property color foreground: Tokens.text
    property bool charging: false
    property int iconHeight: 12

    implicitWidth: row.implicitWidth
    implicitHeight: iconHeight
    width: implicitWidth
    height: implicitHeight

    readonly property int batteryWidth: Math.round(iconHeight * 1.7)
    readonly property int bodyWidth: Math.max(8, batteryWidth - 4)
    readonly property int bodyHeight: Math.max(6, iconHeight - 3)
    readonly property int nubWidth: Math.max(2, Math.round(batteryWidth * 0.1))
    readonly property int nubHeight: Math.max(3, Math.round(bodyHeight * 0.45))
    readonly property int inset: Math.max(2, Math.round(bodyHeight * 0.22))
    readonly property real fillWidth: {
        const inner = bodyWidth - 2 * inset;
        const t = Math.max(0, Math.min(1, fraction));
        if (t <= 0)
            return 0;
        return Math.max(2, inner * t);
    }

    Row {
        id: row
        spacing: 3
        height: root.height

        Item {
            width: root.batteryWidth
            height: parent.height

            Rectangle {
                id: body
                width: root.bodyWidth
                height: root.bodyHeight
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
                radius: Math.max(2, Math.round(bodyHeight / 5))
                color: "transparent"
                border.width: 1
                border.color: root.foreground
                antialiasing: true

                Rectangle {
                    x: root.inset
                    y: root.inset
                    width: root.fillWidth
                    height: parent.height - 2 * root.inset
                    radius: 1
                    color: root.foreground
                }
            }

            Rectangle {
                width: root.nubWidth
                height: root.nubHeight
                radius: 1
                anchors.left: body.right
                anchors.leftMargin: 1
                anchors.verticalCenter: body.verticalCenter
                color: root.foreground
            }
        }

        Glyph {
            visible: root.charging
            anchors.verticalCenter: parent.verticalCenter
            name: "bolt"
            color: root.foreground
            font.pixelSize: Math.max(8, root.bodyHeight)
        }
    }
}
