import QtQuick
import QtTest
import qs.Modules.emoji
import qs.Common

TestCase {
    id: testCase
    name: "EmojiVariants"
    when: windowShown
    width: 560
    height: 500
    property var picker: null
    property var fixture: ({
        entries: {
            "base": { familyId: "base", text: "🤝", name: "handshake", tones: [] },
            "mixed": { familyId: "base", text: "🫱🏻‍🫲🏿", name: "handshake: light skin tone, dark skin tone", tones: [1, 5] },
            "reverse": { familyId: "base", text: "🫱🏿‍🫲🏻", name: "handshake: dark skin tone, light skin tone", tones: [5, 1] }
        },
        families: { "base": { id: "base", name: "handshake", slots: 2,
            variants: ["base", "mixed", "reverse"], tuples: { "": "base", "1,5": "mixed", "5,1": "reverse" } } }
    })
    Component { id: pickerComponent; VariantChooser {} }
    CatalogFixture { id: realCatalog }
    SignalSpy { id: applied; target: testCase.picker; signalName: "applied" }
    SignalSpy { id: dismissed; target: testCase.picker; signalName: "dismissed" }

    function init() {
        picker = createTemporaryObject(pickerComponent, testCase, { catalog: fixture, initialId: "mixed", width: 508, height: 400 });
        verify(picker !== null);
        applied.clear(); dismissed.clear();
    }
    function cleanup() { picker = null; }

    function test_initial_variant_and_pair_order() {
        compare(picker.pendingId, "mixed");
        compare(findChild(picker, "person1").currentIndex, 0);
        compare(findChild(picker, "person2").currentIndex, 4);
        findChild(picker, "person1").currentIndex = 4;
        findChild(picker, "person2").currentIndex = 0;
        picker.choosePair();
        compare(picker.pendingId, "reverse");
    }
    function test_real_catalog_uses_qt_code_point_iteration() {
        compare(realCatalog.catalog.order.length, 1914);
        compare(realCatalog.catalog.byText["👩🏽‍💻"], "1f469-1f3fd-200d-1f4bb");
        compare(realCatalog.catalog.byText["🫱🏻‍🫲🏿"], "1faf1-1f3fb-200d-1faf2-1f3ff");
    }
    function test_unavailable_pair_does_not_invent_sequence() {
        findChild(picker, "person1").currentIndex = 2;
        findChild(picker, "person2").currentIndex = 2;
        picker.choosePair();
        compare(picker.pendingId, "mixed");
    }
    function test_enter_applies_without_copying() {
        findChild(picker, "choices").forceActiveFocus();
        keyClick(Qt.Key_Return);
        compare(applied.count, 1);
        compare(applied.signalArguments[0][0], "mixed");
    }
    function test_escape_cancels_pending_change() {
        picker.pendingId = "reverse";
        findChild(picker, "choices").forceActiveFocus();
        keyClick(Qt.Key_Escape);
        compare(dismissed.count, 1);
        compare(applied.count, 0);
    }
    function test_keyboard_moves_between_exact_variants() {
        const grid = findChild(picker, "choices");
        grid.forceActiveFocus();
        keyClick(Qt.Key_Right);
        compare(picker.pendingId, "reverse");
    }
}
