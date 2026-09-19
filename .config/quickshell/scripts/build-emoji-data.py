#!/usr/bin/env python3
"""Build the offline Emoji 17.0 / CLDR 48 catalog. No runtime downloads."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import urllib.request
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
SOURCES = {
    "emoji-test.txt": "https://www.unicode.org/Public/17.0.0/emoji/emoji-test.txt",
    "annotations.xml": "https://raw.githubusercontent.com/unicode-org/cldr/release-48/common/annotations/en.xml",
    "derived.xml": "https://raw.githubusercontent.com/unicode-org/cldr/release-48/common/annotationsDerived/en.xml",
    "LICENSE.txt": "https://www.unicode.org/license.txt",
}
TONE = re.compile(r"(?:medium-light|medium-dark|light|medium|dark) skin tone")
ALIASES = {"thumbs up": ["+1", "thumbsup", "yes"], "thumbs down": ["-1", "thumbsdown", "no"],
           "red heart": ["heart", "love"], "face with tears of joy": ["joy", "lol", "laugh"],
           "rolling on the floor laughing": ["rofl", "lmao"], "party popper": ["tada"],
           "folded hands": ["pray", "thanks"], "fire": ["lit"], "pile of poo": ["poop", "shit"]}


def normalized(text):
    return re.sub(r"\s+", " ", re.sub(r"[_:\-]", " ", text.lower())).strip()


def family_name(name):
    # Names preserve semantic differences such as gender, hair and direction.
    name = TONE.sub("", name)
    name = re.sub(r":\s*,\s*", ": ", name)
    name = re.sub(r",\s*,", ",", name)
    name = re.sub(r"\s+", " ", name).strip(" ,:")
    # These neutral paired forms use a single legacy code point by default.
    return {"kiss: person, person": "kiss", "couple with heart: person, person": "couple with heart"}.get(name, name)


def read_annotations(inputs):
    annotations = {}
    for filename in ("annotations.xml", "derived.xml"):
        for node in ET.fromstring(inputs[filename]).iter("annotation"):
            key = node.attrib["cp"].replace("\ufe0f", "")
            annotations.setdefault(key, set()).update((node.text or "").split(" | "))
    return annotations


def build_entry(hit, annotations):
    codes = [int(code, 16) for code in hit[1].split()]
    text = "".join(map(chr, codes))
    name = hit[2]
    base_name = family_name(name)
    keywords = sorted(annotations.get(text.replace("\ufe0f", ""), set()))
    aliases = ALIASES.get(base_name, [])
    entry = {"id": "-".join(f"{code:x}" for code in codes), "text": text, "name": name,
             "tones": [code - 0x1F3FA for code in codes if 0x1F3FB <= code <= 0x1F3FF],
             "search": normalized(" ".join([name, *keywords, *aliases])),
             "aliases": [normalized(alias) for alias in aliases]}
    return base_name, entry


def build_entries(emoji_text, annotations):
    entries, groups, families = [], [], {}
    group = subgroup = ""
    for line in emoji_text.decode().splitlines():
        if line.startswith("# group: "):
            group = line[9:]
        elif line.startswith("# subgroup: "):
            subgroup = line[12:]
        hit = re.match(r"^([0-9A-F ]+)\s*; fully-qualified\s*# \S+ E[0-9.]+ (.+)$", line)
        if not hit:
            continue
        base_name, entry = build_entry(hit, annotations)
        family = families.setdefault(base_name, {"name": base_name, "group": group, "subgroup": subgroup, "variants": []})
        if group not in groups:
            groups.append(group)
        family["variants"].append(entry["id"])
        entries.append(entry)
    return entries, groups, families


def finalize_families(entries, families):
    by_id = {entry["id"]: entry for entry in entries}
    output_families = []
    for family in families.values():
        defaults = [key for key in family["variants"] if not by_id[key]["tones"]]
        if len(defaults) != 1:
            raise ValueError(f"Ambiguous family {family['name']}: {defaults}")
        family["id"] = defaults[0]
        family["slots"] = max(len(by_id[key]["tones"]) for key in family["variants"])
        for key in family["variants"]:
            by_id[key]["familyId"] = family["id"]
        output_families.append(family)
    assert len(by_id) == len(entries)
    assert {key for family in output_families for key in family["variants"]} == set(by_id)
    return output_families


def build(inputs):
    annotations = read_annotations(inputs)
    entries, groups, families = build_entries(inputs["emoji-test.txt"], annotations)
    output_families = finalize_families(entries, families)
    return {"schema": 1, "unicode": "17.0", "cldr": "48", "groups": groups,
            "families": output_families, "entries": entries}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache", type=Path, default=Path.home()/".cache/qs-emoji-sources")
    parser.add_argument("--check", action="store_true", help="verify sources against manifest and generated output")
    args = parser.parse_args()
    args.cache.mkdir(parents=True, exist_ok=True)
    inputs = {}
    for name, url in SOURCES.items():
        cached = args.cache/name
        if not cached.exists():
            with urllib.request.urlopen(url, timeout=30) as response:
                cached.write_bytes(response.read())
        inputs[name] = cached.read_bytes()
    manifest = {"unicode": "17.0", "cldr": "48", "sources": [
        {"file": name, "url": url, "sha256": hashlib.sha256(inputs[name]).hexdigest()}
        for name, url in SOURCES.items()]}
    data = build(inputs)
    outputs = {"emoji.json": json.dumps(data, ensure_ascii=False, separators=(",", ":"))+"\n",
               "emoji-sources.json": json.dumps(manifest, indent=2)+"\n",
               "emoji-LICENSE.txt": inputs["LICENSE.txt"].decode()}
    (ROOT/"data").mkdir(exist_ok=True)
    for name, content in outputs.items():
        destination = ROOT/"data"/name
        if args.check:
            if destination.read_text() != content:
                raise SystemExit(f"Mismatch: {destination}")
        else:
            destination.write_text(content)
    print(f"{len(data['entries'])} sequences, {len(data['families'])} families; {len(outputs['emoji.json'].encode())} bytes")


if __name__ == "__main__":
    main()
