#!/usr/bin/env python3
"""Run fixed fuzzy and real-catalog Go comparisons in Qt's actual JS engine.

Uses offscreen qmltestrunner, with no Quickshell process or desktop actions.
The saved JavaScript references are deliberately independent of production ports.
"""
import json
from pathlib import Path
import shutil
import subprocess

from qml_test_support import QmlTestEnvironment


root = Path(__file__).resolve().parents[1]
stage = QmlTestEnvironment("qs-search-compat-")


def embedded(value):
    """Embed arbitrary fixture data as a safely quoted JSON.parse argument."""
    return "JSON.parse(" + json.dumps(json.dumps(value, separators=(",", ":"))) + ")"


try:
    binary = stage.path / "qs-search"
    subprocess.run(["mise", "exec", "--", "go", "build", "-o", str(binary), "./cmd/qs-search"],
                   cwd=root, check=True, timeout=60)
    catalog = json.loads((root / "data/emoji.json").read_text())
    fuzzy = json.loads((root / "tests/fixtures/search/fuzzy.json").read_text())
    preferences = {"schema": 1, "tone": 3, "overrides": {"1f44d": "1f44d-1f3ff"},
                   "recents": [{"id": "1f44d-1f3fb", "at": 100}, {"id": "1f600", "at": 99}]}
    queries = ["", "👍🏿", "thumbs up dark", "woman technologist", ":thumbsup:", "RED_HEART",
               "handshake light dark", "light bulb", "medium dark", "handshake medium light medium dark",
               "zzzzzzzzzzzzzzzzzzz", "tmbsup", "grnng", "😀", "flag india", "  flag\tindia  ",
               "person running", "family", "👩🏽‍💻", "İ", "ΟΣ", "💻", "birthday cake"]
    by_id = {entry["id"]: entry for entry in catalog["entries"]}
    for family in catalog["families"][::71]:
        name = by_id[family["id"]]["name"]
        queries.extend([name, "".join(letter for letter in name if letter not in "aeiou")])
    searches = [{"query": "", "category": category, "preferences": preferences}
                for category in ["all", "recent", *catalog["groups"]]]
    searches.extend({"query": query, "category": category, "preferences": preferences}
                    for query in queries for category in ["all", "recent"])
    envelope = {"v": 1, "profile": "emoji", "epoch": 1, "revision": 1}
    requests = [{**envelope, "type": "begin"}, {**envelope, "type": "commit"}]
    requests.extend({**envelope, "type": "search", "request": index + 1, **query}
                    for index, query in enumerate(searches))
    requests.append({**envelope, "type": "release"})
    result = subprocess.run([str(binary), "--emoji-catalog", str(root / "data/emoji.json")],
                            input="".join(json.dumps(message) + "\n" for message in requests),
                            capture_output=True, text=True, check=True, timeout=30)
    replies = [json.loads(line) for line in result.stdout.splitlines()]
    if len(replies) != len(requests) + 1 or replies[0].get("type") != "ready":
        raise ValueError("Go fixture generation did not complete its protocol")
    results = [reply for reply in replies if reply.get("type") == "results"]
    if len(results) != len(searches):
        raise ValueError("Go fixture generation returned incomplete emoji results")
    for index, (query, reply) in enumerate(zip(searches, results)):
        if reply.get("request") != index + 1 or reply.get("instance") != replies[0].get("instance"):
            raise ValueError("Go fixture response identity mismatch")
        query["keys"] = reply.get("keys", [])
    fixture = """import QtQuick
import "Fuzzy.js" as Fuzzy
import "EmojiCatalog.js" as Catalog
QtObject {
    readonly property var fuzzyCases: FUZZY
    readonly property var emojiCases: EMOJI
    readonly property var catalog: Catalog.index(CATALOG)
    function score(query, text) { return Fuzzy.scoreMultiTokenAND(query, text); }
    function search(query, category, preferences) { return Catalog.search(catalog, preferences, query, category); }
}
""".replace("FUZZY", embedded(fuzzy)).replace("EMOJI", embedded(searches)).replace("CATALOG", embedded(catalog))
    stage.module("qs.SearchFixtures", [("SearchFixture", fixture, False)])
    for name in ("Fuzzy.js", "EmojiCatalog.js"):
        shutil.copy2(root / "tests/reference" / name, stage.path / "qs/SearchFixtures" / name)
    print(f"Actual Qt JS compatibility: {len(fuzzy)} fixed fuzzy cases and {len(searches)} complete emoji result comparisons", flush=True)
    result = stage.run(root / "tests/qml/tst_SearchCompatibility.qml")
finally:
    stage.close()

raise SystemExit(result.returncode)
