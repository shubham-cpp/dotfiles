"""Temporary QML imports and an offscreen runner, without a second shell."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


class QmlTestEnvironment:
    def __init__(self, prefix):
        cache = Path.home() / ".cache"
        cache.mkdir(exist_ok=True)
        self.path = Path(tempfile.mkdtemp(prefix=prefix, dir=cache))

    def module(self, name, sources):
        destination = self.path.joinpath(*name.split("."))
        destination.mkdir(parents=True, exist_ok=True)
        declarations = ["module " + name]
        for type_name, source, singleton in sources:
            (destination / f"{type_name}.qml").write_text(source)
            declarations.append(f"{'singleton ' if singleton else ''}{type_name} 1.0 {type_name}.qml")
        (destination / "qmldir").write_text("\n".join(declarations) + "\n")

    def run(self, test_file):
        runner = shutil.which("qmltestrunner") or "/usr/lib/qt6/bin/qmltestrunner"
        return subprocess.run([runner, "-import", str(self.path), "-input", str(test_file)],
                              env={**os.environ, "QT_QPA_PLATFORM": "offscreen", "QT_QUICK_BACKEND": "software"},
                              timeout=60)

    def close(self):
        if shutil.which("gio"):
            result = subprocess.run(["gio", "trash", str(self.path)], check=False)
            if result.returncode:
                print(f"Temporary test imports retained at {self.path}")
        else:
            print(f"Temporary test imports retained at {self.path}")
