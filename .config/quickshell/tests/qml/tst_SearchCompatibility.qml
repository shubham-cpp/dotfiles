import QtQuick
import QtTest
import qs.SearchFixtures

TestCase {
    name: "SearchQtCompatibility"
    when: windowShown
    SearchFixture { id: fixture }

    function test_fuzzy_data() {
        return fixture.fuzzyCases.map((row, index) => ({ tag: "case-" + index, index: index }));
    }

    function test_fuzzy(data) {
        const row = fixture.fuzzyCases[data.index];
        const actual = fixture.score(row.query, row.text);
        const context = "query=" + JSON.stringify(row.query) + " text=" + JSON.stringify(row.text) + " actual=" + JSON.stringify(actual);
        compare(!!actual, row.matched, context);
        if (actual)
            compare(actual.score, row.score, context);
    }

    function test_complete_catalog() {
        compare(fixture.catalog.order.length, 1914);
        compare(Object.keys(fixture.catalog.entries).length, 3944);
        compare(fixture.catalog.byText["👩🏽‍💻"], "1f469-1f3fd-200d-1f4bb");
    }

    function test_emoji_data() {
        return fixture.emojiCases.map((row, index) => ({ tag: "case-" + index, index: index }));
    }

    function test_emoji(data) {
        const row = fixture.emojiCases[data.index];
        const actual = fixture.search(row.query, row.category, row.preferences);
        const context = row.category + "/" + JSON.stringify(row.query);
        compare(actual.length, row.keys.length, context);
        for (let i = 0; i < actual.length; i++)
            compare(actual[i], row.keys[i], context + " at " + i);
    }
}
