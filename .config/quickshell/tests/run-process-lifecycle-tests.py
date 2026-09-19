#!/usr/bin/env python3
"""Test production process bindings offscreen with no external children or shell."""
import os
from pathlib import Path
import shlex
import subprocess

from qml_test_support import QmlTestEnvironment


root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-process-lifecycle-")
try:
    source = root / "tests/process-lifecycle-fixture.cpp"
    libexec = subprocess.check_output(["pkg-config", "--variable=libexecdir", "Qt6Core"], text=True).strip()
    moc = str(Path(libexec) / "moc")
    subprocess.run([moc, str(source), "-o", str(stage.path / "process-lifecycle-fixture.moc")], check=True)
    flags = shlex.split(subprocess.check_output(
        ["pkg-config", "--cflags", "--libs", "Qt6QuickTest", "Qt6Quick"], text=True))
    runner = stage.path / "process-lifecycle-tests"
    subprocess.run(["c++", "-std=c++17", "-fPIC", str(source), "-I", str(stage.path),
                    "-o", str(runner), *flags], check=True)
    stage.module("Quickshell", [
        ("Singleton", "import QtQuick\nItem {}", False),
        ("Quickshell", "pragma Singleton\nimport QtQuick\nQtObject { function execDetached(command) {} }", True)])
    stage.module("Quickshell.Io", [
        ("IpcHandler", 'import QtQuick\nItem { property string target }', False),
        ("SplitParser", 'import QtQuick\nQtObject { signal read(string data) }', False)])
    stage.module("Quickshell.WindowManager", [
        ("WindowManager", "pragma Singleton\nimport QtQuick\nQtObject { property var windowsets: [] }", True)])
    services = []
    for name, aliases in [
        ("Idle", "property alias testProcess: inhibitor"),
        ("Workspaces", "property alias testProcess: watcher\n    property alias testRetry: retry"),
    ]:
        contents = (root / "Services" / (name + ".qml")).read_text()
        contents = contents.replace("    id: root\n", "    id: root\n    " + aliases + "\n", 1)
        services.append((name, contents, True))
    stage.module("qs.Services", services)
    result = subprocess.run(
        [str(runner), "-import", str(stage.path), "-input", str(root / "tests/qml/tst_ProcessLifecycle.qml")],
        env={**os.environ, "QT_QPA_PLATFORM": "offscreen", "QT_QUICK_BACKEND": "software"}, timeout=60)
finally:
    stage.close()
raise SystemExit(result.returncode)
