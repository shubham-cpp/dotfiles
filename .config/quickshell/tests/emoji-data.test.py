import importlib.util
from pathlib import Path
import unittest
import sys

sys.dont_write_bytecode = True

spec = importlib.util.spec_from_file_location("emoji_build", Path(__file__).parents[1]/"scripts/build-emoji-data.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class EmojiGeneration(unittest.TestCase):
    def test_exceptional_family_names(self):
        for name, expected in [
            ("kiss: person, person, light skin tone, dark skin tone", "kiss"),
            ("couple with heart: person, person, medium skin tone, light skin tone", "couple with heart"),
            ("person: medium skin tone, beard", "person: beard"),
            ("woman running facing right: dark skin tone", "woman running facing right"),
        ]:
            self.assertEqual(module.family_name(name), expected)

    def test_incomplete_family_fails(self):
        with self.assertRaisesRegex(ValueError, "Ambiguous"):
            module.build({"annotations.xml": b"<ldml/>", "derived.xml": b"<ldml/>",
                          "emoji-test.txt": "# group: People\n# subgroup: hands\n1F44D 1F3FD ; fully-qualified # 👍🏽 E1.0 thumbs up: medium skin tone".encode()})


if __name__ == "__main__":
    unittest.main()
