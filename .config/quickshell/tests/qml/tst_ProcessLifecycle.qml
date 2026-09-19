import QtQuick
import QtTest
import qs.Services

TestCase {
    name: "ProcessLifecycle"

    function init() {
        Idle.enabled = false;
        if (Idle.testProcess.running) Idle.testProcess.finish();
        Workspaces.testRetry.stop();
        if (!Workspaces.testProcess.running) Workspaces.testProcess.running = true;
        Workspaces.testProcess.started();
    }

    function cleanup() {
        Idle.enabled = false;
        if (Idle.testProcess.running) Idle.testProcess.finish();
        Workspaces.testRetry.stop();
    }

    function test_idle_failed_start_clears_enabled_and_can_retry() {
        Idle.set(true);
        verify(Idle.enabled);
        Idle.testProcess.failStart();
        tryCompare(Idle, "enabled", false);
        const starts = Idle.testProcess.starts;
        Idle.set(true);
        compare(Idle.testProcess.starts, starts + 1);
        Idle.testProcess.started();
        verify(Idle.enabled);
    }

    function test_idle_unexpected_exit_clears_enabled() {
        Idle.set(true);
        Idle.testProcess.started();
        Idle.testProcess.finish();
        tryCompare(Idle, "enabled", false);
    }

    function test_idle_intentional_stop_stays_disabled() {
        Idle.set(true);
        Idle.testProcess.started();
        Idle.set(false);
        verify(Idle.testProcess.running, "termination remains pending until completion");
        Idle.testProcess.finish();
        wait(0);
        compare(Idle.enabled, false);
        compare(Idle.testProcess.running, false);
    }

    function test_idle_quick_off_on_preserves_queued_restart() {
        Idle.set(true);
        Idle.testProcess.started();
        const starts = Idle.testProcess.starts;
        Idle.set(false);
        Idle.set(true);
        compare(Idle.testProcess.starts, starts);
        Idle.testProcess.finish();
        compare(Idle.testProcess.starts, starts + 1);
        wait(0);
        verify(Idle.enabled);
        Idle.testProcess.failStart();
        tryCompare(Idle, "enabled", false);
    }

    function verifyDelayedRetry(failedStart) {
        const starts = Workspaces.testProcess.starts;
        if (failedStart) Workspaces.testProcess.failStart();
        else Workspaces.testProcess.finish();
        compare(Workspaces.testProcess.starts, starts);
        compare(Workspaces.testRetry.running, true);
        wait(50);
        compare(Workspaces.testProcess.starts, starts, "no immediate respawn");
        tryCompare(Workspaces.testProcess, "starts", starts + 1, 1500);
        Workspaces.testProcess.started();
        compare(Workspaces.testRetry.running, false);
    }

    function test_workspace_failed_start_retries_after_delay() {
        verifyDelayedRetry(true);
    }

    function test_workspace_exit_retries_after_delay() {
        verifyDelayedRetry(false);
    }
}
