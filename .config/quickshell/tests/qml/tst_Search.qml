import QtQuick
import QtTest
import qs.Services

TestCase {
    id: root
    name: "SearchTransportState"
    when: windowShown
    visible: true
    width: 100
    height: 100

    property var received: ({})
    property var publications: []
    property var failures: []

    Connections {
        target: Search
        function onResultsReady(profile, keys) {
            const next = Object.assign({}, root.received);
            next[profile] = keys;
            root.received = next;
            root.publications = root.publications.concat([{ profile: profile, keys: keys }]);
        }
        function onFailed(profile, message) {
            root.failures = root.failures.concat([{ profile: profile, message: message }]);
        }
    }

    function releaseAll() {
        Search.release("launcher");
        Search.release("clipboard");
        Search.release("emoji");
        Search.release("files");
        tryVerify(() => !Search.testWorker.running && !Search._stopping, 3000);
        tryVerify(() => Search.testWorker.processId <= 0, 3000);
        compare(Search._flight, null);
        compare(Object.keys(Search._profiles).length, 0);
    }

    function init() {
        releaseAll();
        received = ({});
        publications = [];
        failures = [];
    }

    function cleanup() {
        releaseAll();
    }

    function launcherRows() {
        return [
            { key: "firefox", id: "firefox.desktop", name: "Firefox", tie: 1 },
            { key: "foot", id: "foot.desktop", name: "Foot", tie: 0 },
            { key: "editor", id: "editor.desktop", name: "Editor", tie: 2 }
        ];
    }

    function openLauncher(query) {
        Search.setCatalog("launcher", launcherRows());
        Search.search("launcher", { query: query, now: 1000 });
    }

    function expectKeys(profile, keys) {
        tryVerify(() => root.received[profile] !== undefined
            && root.received[profile].join("|") === keys.join("|"), 6000);
        compare(failures.length, 0);
        verify(Search._profiles[profile] !== undefined);
        compare(Search._profiles[profile].accepted, Search._profiles[profile].desired);
    }

    function test_launcher_round_trip_and_final_release() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        verify(Search.testWorker.processId > 0);
        verify(Search._instance.length > 0);
        releaseAll();
        verify(!Search.testWatchdog.running);
    }

    function test_latest_query_retires_obsolete_flight() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const before = publications.length;
        // Send one request synchronously, then revise before the Qt event loop
        // can deliver its reply. The obsolete reply must free the flight slot.
        Search.search("launcher", { query: "foot", now: 1000 });
        Search.pump();
        verify(Search._flight !== null);
        compare(Search._flight.type, "search");
        const obsolete = Search._flight.desired;
        Search.search("launcher", { query: "edit", now: 1000 });
        verify(Search._profiles.launcher.desired !== obsolete);
        expectKeys("launcher", ["editor"]);
        compare(publications.length, before + 1);
        compare(Search._flight, null);
    }

    function test_simultaneous_clipboard_and_release() {
        openLauncher("fire");
        Search.setCatalog("clipboard", [
            { key: "pin-1", text: "alpha pinned", kind: "text", pinned: true },
            { key: "clip-1", text: "alpha history", kind: "text" },
            { key: "clip-2", text: "unrelated", kind: "text" }
        ]);
        Search.search("clipboard", { query: "alpha", filter: "all" });
        expectKeys("launcher", ["firefox"]);
        expectKeys("clipboard", ["pin-1", "clip-1"]);
        const instance = Search._instance;
        Search.release("clipboard");
        verify(Search._profiles.clipboard === undefined);
        Search.search("launcher", { query: "edit", now: 1000 });
        expectKeys("launcher", ["editor"]);
        compare(Search._instance, instance);
        verify(Search.testWorker.running);
        compare(Search._releases.length, 0);
    }

    function test_large_chunked_snapshot() {
        const rows = [];
        for (let i = 0; i < 1200; i++) {
            rows.push({ key: "large-" + i, id: "app-" + i,
                name: i === 1199 ? "UniqueLargeResult" : "Application " + i,
                comment: "metadata ".repeat(40), tie: i });
        }
        Search.setCatalog("launcher", rows);
        verify(Search._profiles.launcher.chunks.length > 10);
        Search.search("launcher", { query: "UniqueLargeResult", now: 1000 });
        expectKeys("launcher", ["large-1199"]);
        compare(Search._profiles.launcher.phase, "ready");
        compare(Search._profiles.launcher.chunk, Search._profiles.launcher.chunks.length);
    }

    function test_close_reopen_while_old_child_exits() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const instance = Search._instance;
        Search.release("launcher");
        openLauncher("edit");
        expectKeys("launcher", ["editor"]);
        verify(Search._instance !== instance);
        compare(Search._retries, 0);
        verify(!Search._stopping);
    }

    function test_unexpected_kill_reinstalls_latest_query() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const instance = Search._instance;
        Search.testWorker.signal(9);
        Search.search("launcher", { query: "edit", now: 1000 });
        expectKeys("launcher", ["editor"]);
        verify(Search._instance !== instance);
        compare(Search._retries, 1);
        verify(!Search._broken);
    }

    function test_malformed_version_retries_then_reports_failure() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const instance = Search._instance;
        Search.handleLine('{"v":999,"type":"ready","instance":"wrong"}');
        Search.search("launcher", { query: "edit", now: 1000 });
        expectKeys("launcher", ["editor"]);
        verify(Search._instance !== instance);
        Search.handleLine("not JSON");
        tryVerify(() => Search._broken, 3000);
        compare(failures.length, 1);
        compare(failures[0].profile, "launcher");
        verify(!Search.testWorker.running);
    }

    function test_watchdog_restarts_and_accepts_latest_query() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const instance = Search._instance;
        Search.testWatchdog.interval = 1;
        Search.testWatchdog.restart();
        tryVerify(() => Search._retries === 1, 3000);
        Search.search("launcher", { query: "edit", now: 1000 });
        expectKeys("launcher", ["editor"]);
        verify(Search._instance !== instance);
    }

    function test_null_envelope_restarts_without_throwing() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const instance = Search._instance;
        Search.handleLine("null");
        Search.search("launcher", { query: "edit", now: 1000 });
        expectKeys("launcher", ["editor"]);
        verify(Search._instance !== instance);
        compare(Search._retries, 1);
    }

    function test_malformed_results_data() {
        return [
            { tag: "null-key", keys: [null] },
            { tag: "boolean-key", keys: [false] },
            { tag: "number-key", keys: [0] },
            { tag: "empty-key", keys: [""] },
            { tag: "duplicate-keys", keys: ["firefox", "firefox"] },
            { tag: "too-many-keys", keys: Array.from({ length: 51 }, (_, index) => "key-" + index) },
            { tag: "null-list", keys: null },
            { tag: "boolean-list", keys: false },
            { tag: "number-list", keys: 0 },
            { tag: "empty-string-list", keys: "" }
        ];
    }

    function test_malformed_results(data) {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const before = publications.length;
        Search.search("launcher", { query: "edit", now: 1000 });
        Search.pump();
        const flight = Search._flight;
        verify(flight !== null);
        Search.handleLine(JSON.stringify({ v: 1, type: "results", instance: Search._instance,
            profile: flight.profile, epoch: flight.epoch, revision: flight.revision,
            request: flight.request, keys: data.keys }));
        compare(publications.length, before, "Malformed results became actionable");
        Search.search("launcher", { query: "foot", now: 1000 });
        expectKeys("launcher", ["foot"]);
        compare(Search._retries, 1);
    }

    function test_omitted_keys_is_a_valid_empty_result() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        Search.search("launcher", { query: "missing", now: 1000 });
        Search.pump();
        const flight = Search._flight;
        verify(flight !== null);
        Search.handleLine(JSON.stringify({ v: 1, type: "results", instance: Search._instance,
            profile: flight.profile, epoch: flight.epoch, revision: flight.revision,
            request: flight.request }));
        expectKeys("launcher", []);
        compare(Search._retries, 0);
    }

    function test_update_replay_changes_recovery_without_publication() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        const before = publications.length;
        const desired = Search._profiles.launcher.desired;
        const instance = Search._instance;
        const preferences = { schema: 1, tone: 3, overrides: { "family": "variant" }, recents: [] };
        Search.updateReplay("launcher", { query: "edit", preferences: preferences });
        compare(Search._profiles.launcher.query.preferences.tone, 3);
        compare(Search._profiles.launcher.desired, desired);
        compare(publications.length, before);
        compare(Search._flight, null);
        Search.testWorker.signal(9);
        expectKeys("launcher", ["editor"]);
        verify(Search._instance !== instance);
        compare(Search._profiles.launcher.query.preferences.overrides.family, "variant");
    }

    function test_files_empty_catalog_round_trip() {
        Search.setCatalog("files", []);
        Search.search("files", { query: "notes" });
        expectKeys("files", []);
        compare(Search._profiles.files.phase, "ready");
        compare(Search._profiles.files.chunks.length, 0);
        verify(Search.testWorker.processId > 0);
    }

    function test_update_replay_cannot_replace_pending_search() {
        openLauncher("fire");
        expectKeys("launcher", ["firefox"]);
        Search.search("launcher", { query: "foot", now: 1000 });
        Search.updateReplay("launcher", { query: "edit" });
        compare(Search._profiles.launcher.query.query, "foot");
        expectKeys("launcher", ["foot"]);
    }
}
