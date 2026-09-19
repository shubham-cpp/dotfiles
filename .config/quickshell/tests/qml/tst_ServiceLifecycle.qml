import QtQuick
import QtTest
import qs.Services

TestCase {
    name: "ServiceLifecycle"

    function init() {
        Notifications.imageJob = null;
        Notifications.testWorker.running = false;
        Notifications.imagesDirty = false;
        Notifications.history = [];
        LauncherStats.close();
        LauncherStats._statsReadOnly = false;
        LauncherStats._pinsReadOnly = false;
    }

    function imageJob() {
        Notifications.history = [{id: 1, imageKey: "img-test", imageSource: "file:///test/image.png", image: "", imageFailed: false}];
        Notifications.runImageWork();
        verify(Notifications.testWorker.running);
    }

    function test_image_failed_start_releases_job_and_marks_failure() {
        imageJob();
        Notifications.testWorker.failStart();
        compare(Notifications.imageJob, null);
        verify(Notifications.history[0].imageFailed);
        imageJob();
        verify(Notifications.testWorker.running);
    }

    function test_image_exit_waits_for_deferred_output_without_false_failure() {
        imageJob();
        Notifications.testWorker.finishExit(0);
        verify(Notifications.imageJob !== null);
        verify(!Notifications.history[0].imageFailed);
        compare(Notifications.imageJob.exitCode, 0);
        const destination = Notifications.imageCacheDir + "/img-test";
        Notifications.testWorker.finishOutput(JSON.stringify({path: destination, kept: ["img-test"]}));
        compare(Notifications.imageJob, null);
        compare(Notifications.history[0].image, destination);
    }

    function test_launcher_validates_persisted_state_under_qt() {
        LauncherStats.testStatsFile.content = '{"bad":null,"good":{"count":3,"last":1},"string":{"count":"4","last":1}}';
        LauncherStats.loadStats();
        compare(Object.keys(LauncherStats.stats).join(","), "good");
        LauncherStats.testPinsFile.content = '[null,"good","good"]';
        LauncherStats.loadPins();
        compare(LauncherStats.pins.length, 1);
        verify(LauncherStats.isPinned("good"));
        LauncherStats.testPinsFile.content = "null";
        LauncherStats.loadPins();
        verify(LauncherStats._pinsReadOnly);
        LauncherStats.persistPins();
        compare(LauncherStats.testPinsFile.content, "null");
    }

    function test_launcher_open_builds_one_catalog_and_query() {
        Search.catalogs = 0;
        Search.queries = 0;
        LauncherStats.query = "previous";
        LauncherStats.toggle();
        verify(LauncherStats.open);
        compare(LauncherStats.query, "");
        compare(Search.catalogs, 1);
        compare(Search.queries, 1);
    }
}
