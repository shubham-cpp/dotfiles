import QtQuick
import QtTest
import qs.Services

TestCase {
    name: "ClipboardLifecycle"

    function init() {
        Clipboard.close();
        Clipboard._listJob = null;
        Clipboard._decodeJob = null;
        Clipboard.testLister.running = false;
        Clipboard.testDecoder.running = false;
        Clipboard.testCopier.running = false;
        Clipboard.testPinner.running = false;
        Clipboard._pendingPin = null;
        Clipboard.copying = false;
        Clipboard._pinsReadOnly = false;
        Clipboard.pins = [];
    }

    function listing() {
        Clipboard.toggle();
        Clipboard.testLister.finish("1\thello", 0, true);
    }

    function test_close_releases_models_and_ignores_late_decoder() {
        listing();
        tryVerify(() => Clipboard.testDecoder.running);
        Clipboard.close();
        compare(Clipboard.items.length, 0);
        compare(Clipboard.results.length, 0);
        Clipboard.testDecoder.finish("hello", 0, false);
        compare(Clipboard.previewText, "");
        compare(Object.keys(Clipboard._decodedText).length, 0);
    }

    function test_reopen_waits_for_old_list_stream_before_starting_new_job() {
        Clipboard.toggle();
        const oldEpoch = Clipboard._epoch;
        Clipboard.close();
        Clipboard.toggle();
        verify(Clipboard._epoch > oldEpoch);
        Clipboard.testLister.finish("1\tstale", 0, false);
        compare(Clipboard.items.length, 0);
        tryVerify(() => Clipboard.testLister.running);
        Clipboard.testLister.finish("2\tfresh", 0, true);
        compare(Clipboard.items[0].id, "2");
    }

    function test_copy_failure_stays_open_and_success_closes() {
        listing();
        Clipboard.copySelected();
        verify(Clipboard.open);
        verify(Clipboard.copying);
        Clipboard.testCopier.finish("", 1, true);
        verify(Clipboard.open);
        verify(Clipboard.actionError.length > 0);
        Clipboard.copySelected();
        Clipboard.testCopier.finish("", 0, true);
        verify(!Clipboard.open);
        compare(Clipboard.results.length, 0);
    }

    function test_old_copy_completion_does_not_close_reopened_picker() {
        listing();
        Clipboard.copySelected();
        Clipboard.close();
        Clipboard.toggle();
        Clipboard.testCopier.finish("", 0, true);
        verify(Clipboard.open);
    }

    function test_corrupt_pin_file_is_preserved_and_read_only() {
        Clipboard.testPinsFile.content = '{"unexpected":"schema"}';
        Clipboard.loadPins();
        verify(Clipboard._pinsReadOnly);
        compare(Clipboard.pins.length, 0);
        Clipboard.persistPins();
        compare(Clipboard.testPinsFile.content, '{"unexpected":"schema"}');
    }

    function test_failed_list_start_releases_job_and_allows_reopen() {
        Clipboard.toggle();
        Clipboard.testLister.failStart();
        compare(Clipboard._listJob, null);
        verify(Clipboard.actionError.length > 0);
        Clipboard.close();
        listing();
        compare(Clipboard.results.length, 1);
    }

    function test_failed_decode_start_releases_job_and_shows_error() {
        listing();
        tryVerify(() => Clipboard.testDecoder.running);
        Clipboard.testDecoder.failStart();
        compare(Clipboard._decodeJob, null);
        verify(!Clipboard.previewLoading);
        compare(Clipboard.previewError, "Preview unavailable");
    }

    function test_failed_copy_and_pin_start_allow_retry() {
        listing();
        Clipboard.copySelected();
        Clipboard.testCopier.failStart();
        verify(!Clipboard.copying);
        verify(Clipboard.open);
        Clipboard.copySelected();
        verify(Clipboard.testCopier.running);
        Clipboard.testCopier.finish("", 1, true);

        Clipboard.togglePinSelected();
        Clipboard.testPinner.failStart();
        compare(Clipboard._pendingPin, null);
        compare(Clipboard.pins.length, 0);
        Clipboard.togglePinSelected();
        verify(Clipboard.testPinner.running);
        Clipboard.testPinner.finish("", 0, true);
        compare(Clipboard.pins.length, 1);
    }

    function test_exit_before_output_keeps_successful_preview() {
        listing();
        tryVerify(() => Clipboard.testDecoder.running);
        Clipboard.testDecoder.finish("decoded text", 0, false);
        compare(Clipboard.previewText, "decoded text");
        compare(Clipboard.previewError, "");
        compare(Clipboard._decodeJob, null);
    }
}
