#!/usr/bin/env python3
"""Bounded clipboard decoding and copying without shell pipelines."""
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import tempfile

MAX_BYTES = 5_000_000
CACHE_SLOTS = 16


def private_directory(path):
    directory = Path(path)
    directory.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = directory.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
        raise ValueError("Invalid clipboard directory")
    directory.chmod(0o700)
    return directory


def pin_path(directory, name):
    if not re.fullmatch(r"p_[0-9]+(?:_[0-9]+)?", name):
        raise ValueError("Invalid pin identifier")
    return private_directory(directory) / name


def read_pin(directory, name, output):
    descriptor = os.open(pin_path(directory, name), os.O_RDONLY | os.O_NOFOLLOW)
    with os.fdopen(descriptor, "rb") as source:
        info = os.fstat(source.fileno())
        if not stat.S_ISREG(info.st_mode) or info.st_size > MAX_BYTES:
            raise ValueError("Invalid pinned content")
        shutil.copyfileobj(source, output, 65536)


def decode(identifier, output):
    if not re.fullmatch(r"[0-9]+", identifier):
        raise ValueError("Invalid history identifier")
    with subprocess.Popen(["cliphist", "decode", identifier], stdout=subprocess.PIPE,
                          stderr=subprocess.DEVNULL) as child:
        size = 0
        try:
            while chunk := child.stdout.read(65536):
                size += len(chunk)
                if size > MAX_BYTES:
                    raise ValueError("Clipboard item too large")
                output.write(chunk)
            if child.wait() != 0:
                raise ValueError("Clipboard decode failed")
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()


def write_content(path, content, exclusive=False):
    flags = os.O_WRONLY | os.O_CREAT | os.O_NOFOLLOW
    flags |= os.O_EXCL if exclusive else os.O_TRUNC
    descriptor = os.open(path, flags, 0o600)
    with os.fdopen(descriptor, "wb") as target:
        shutil.copyfileobj(content, target, 65536)


def perform(action, source, identifier, pin_directory, destination):
    with tempfile.TemporaryFile() as content:
        if source == "pin":
            read_pin(pin_directory, identifier, content)
        elif source == "clip":
            decode(identifier, content)
        else:
            raise ValueError("Invalid content source")
        if content.tell() == 0:
            raise ValueError("Empty clipboard item")
        content.seek(0)
        if action == "copy":
            subprocess.run(["wl-copy"], stdin=content, stderr=subprocess.DEVNULL, check=True, timeout=10)
        elif action == "text":
            sys.stdout.buffer.write(content.read(8000))
        elif action == "pin":
            write_content(pin_path(pin_directory, destination), content, exclusive=True)
        elif action == "image":
            directory, slot = destination.rsplit("/", 1)
            if not slot.isdecimal() or not 0 <= int(slot) < CACHE_SLOTS:
                raise ValueError("Invalid image cache slot")
            path = private_directory(directory) / slot
            write_content(path, content)
            print(path)
        else:
            raise ValueError("Invalid clipboard action")


if __name__ == "__main__":
    os.umask(0o077)
    try:
        if len(sys.argv) != 6:
            raise ValueError("Invalid arguments")
        perform(*sys.argv[1:])
    except (OSError, ValueError, subprocess.SubprocessError):
        print("Clipboard operation failed", file=sys.stderr)
        sys.exit(1)
