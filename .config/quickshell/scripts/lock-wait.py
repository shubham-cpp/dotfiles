#!/usr/bin/python3
"""Bounded readiness check for the single shell. Never infer lock from a hint."""
from pathlib import Path
import subprocess
import sys
import time


def main():
    command = ["qs", "-p", str(Path(__file__).resolve().parent.parent), "ipc", "call", "lock"]
    deadline = time.monotonic() + 5
    try:
        subprocess.run(command + ["activate"], check=True, timeout=2, stdout=subprocess.DEVNULL)
        while (remaining := deadline - time.monotonic()) > 0:
            try:
                result = subprocess.run(command + ["status"], check=True, capture_output=True,
                                        text=True, timeout=min(remaining, 0.5))
                if result.stdout.strip() == "secure":
                    return 0
            except subprocess.TimeoutExpired:
                pass
            time.sleep(min(0.1, max(0, deadline - time.monotonic())))
    except (OSError, subprocess.SubprocessError):
        pass
    print("qs-lock-wait: compositor lock was not confirmed", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
