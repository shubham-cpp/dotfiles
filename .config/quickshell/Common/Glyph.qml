import QtQuick
import qs.Common

Text {
    property string name: ""

    text: Icons.glyphs[name] || ""
    color: Tokens.subtext
    font.family: Tokens.iconFontFamily
    font.pixelSize: 14
    textFormat: Text.PlainText
    renderType: Text.NativeRendering
    horizontalAlignment: Text.AlignHCenter
    verticalAlignment: Text.AlignVCenter
}
