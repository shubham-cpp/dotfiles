#!/usr/bin/python3
"""Copy owned local notification images, then bound the active history cache.

Runs once per queued copy/prune operation. Eviction uses the desktop Trash;
the byte budget covers this cache, not the user's separate Trash directory.
"""
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
from urllib.parse import unquote, urlsplit


MAX_FILES = 80
MAX_BYTES = 32 * 1024 * 1024
MAX_IMAGE_BYTES = 8 * 1024 * 1024
KEY = re.compile(r"img-[a-z0-9]+-[0-9]+\Z")


def trash(path):
    subprocess.run(["gio", "trash", str(path)], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def cached_files(directory):
    entries = []
    for path in directory.iterdir():
        info = path.lstat()
        if stat.S_ISREG(info.st_mode) or stat.S_ISLNK(info.st_mode):
            entries.append((path, info))
    return sorted(entries, key=lambda entry: entry[1].st_mtime_ns, reverse=True)


def prune(directory, keep, reserve=0):
    retained = []
    total = reserve
    for path, info in cached_files(directory):
        if (path.name not in keep or not stat.S_ISREG(info.st_mode)
                or len(retained) >= MAX_FILES - bool(reserve)
                or total + info.st_size > MAX_BYTES):
            trash(path)
            continue
        retained.append(path.name)
        total += info.st_size
    return retained


def source_file(source):
    url = urlsplit(source)
    if url.scheme != "file" or url.netloc not in ("", "localhost") or url.query or url.fragment:
        raise ValueError("Not a local image")
    path = unquote(url.path, errors="strict")
    if not path.startswith("/"):
        raise ValueError("Not an absolute image path")
    descriptor = os.open(path, os.O_RDONLY | os.O_NONBLOCK | os.O_NOFOLLOW)
    try:
        info = os.fstat(descriptor)
        if (not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid()
                or not 0 < info.st_size <= MAX_IMAGE_BYTES):
            raise ValueError("Image is not an owned, bounded regular file")
        return os.fdopen(descriptor, "rb")
    except BaseException:
        os.close(descriptor)
        raise


def copy_image(directory, key, source, keep):
    pending = directory / (".pending-" + key)
    destination = directory / key
    with source_file(source) as reader:
        # Reserve the full per-image budget before writing, including a growing source.
        prune(directory, keep - {key}, reserve=MAX_IMAGE_BYTES)
        try:
            with pending.open("xb") as writer:
                os.chmod(pending, 0o600)
                total = 0
                while chunk := reader.read(65536):
                    total += len(chunk)
                    if total > MAX_IMAGE_BYTES:
                        raise ValueError("Image grew beyond the limit")
                    writer.write(chunk)
                if not total:
                    raise ValueError("Image became empty")
            pending.replace(destination)
        finally:
            if pending.exists():
                trash(pending)
    return str(destination)


def update(directory, request):
    if not isinstance(request, dict) or not isinstance(request.get("keep"), list):
        raise ValueError("Invalid cache request")
    keep = {key for key in request["keep"][:MAX_FILES] if isinstance(key, str) and KEY.fullmatch(key)}
    key = request.get("key", "")
    source = request.get("source", "")
    if not isinstance(key, str) or not isinstance(source, str):
        raise ValueError("Invalid image request")
    directory.mkdir(mode=0o700, parents=True, exist_ok=True)
    info = directory.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid():
        raise ValueError("Invalid cache directory")
    os.chmod(directory, 0o700)
    path = ""
    try:
        if key and key in keep and KEY.fullmatch(key):
            path = copy_image(directory, key, source, keep)
    except (OSError, ValueError):
        # A missing or invalid sender image must not block cleanup of other entries.
        path = ""
    retained = prune(directory, keep)
    return {"path": path if key in retained else "", "kept": retained}


if __name__ == "__main__":
    try:
        result = update(Path(sys.argv[1]), json.loads(sys.argv[2]))
        print(json.dumps(result, separators=(",", ":")))
    except (IndexError, OSError, ValueError, subprocess.SubprocessError):
        print(json.dumps({"error": "Notification image cache unavailable"}))
        sys.exit(1)
