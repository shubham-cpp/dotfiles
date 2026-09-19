"""Cache tests use fixture files and move evictions into a fixture trash folder."""
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch


spec = importlib.util.spec_from_file_location("notification_images", Path(__file__).parents[1] / "scripts/notification-images.py")
images = importlib.util.module_from_spec(spec)
spec.loader.exec_module(images)


class ImageCache(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="qs-notification-images-test-"))
        self.directory = self.root / "cache"
        self.discarded = self.root / "trash"
        self.discarded.mkdir()
        self.sequence = 0
        self.trash = patch.object(images, "trash", self.move_to_fixture_trash)
        self.trash.start()
        self.addCleanup(self.trash.stop)

    def move_to_fixture_trash(self, path):
        self.sequence += 1
        path.rename(self.discarded / str(self.sequence))

    def image(self, name="source.png", content=b"fixture image"):
        path = self.root / name
        path.write_bytes(content)
        return path

    def update(self, key="img-test-1", source=None, keep=None):
        return images.update(self.directory, {"key": key, "source": source.as_uri() if source else "",
                                            "keep": keep if keep is not None else [key]})

    def test_first_copy_creates_directory_and_publishes_complete_private_file(self):
        source = self.image("source with spaces.png")
        result = self.update(source=source)
        self.assertEqual(Path(result["path"]).read_bytes(), source.read_bytes())
        self.assertEqual(Path(result["path"]).stat().st_mode & 0o777, 0o600)
        self.assertEqual(result["kept"], ["img-test-1"])
        self.assertFalse(list(self.directory.glob(".pending-*")))

    def test_missing_source_and_invalid_paths_publish_nothing(self):
        self.assertEqual(self.update(source=self.root / "missing")["path"], "")
        for source in ("https://example.invalid/image.png", "file://remote/tmp/image.png", "file:relative.png"):
            result = images.update(self.directory, {"key": "img-test-1", "source": source, "keep": ["img-test-1"]})
            self.assertEqual(result["path"], "")
        result = self.update(key="../outside", source=self.image())
        self.assertEqual(result["path"], "")
        self.assertFalse((self.root / "outside").exists())

    def test_symlink_fifo_and_oversized_sources_are_rejected(self):
        source = self.image()
        link = self.root / "link"
        link.symlink_to(source)
        fifo = self.root / "fifo"
        os.mkfifo(fifo)
        with patch.object(images, "MAX_IMAGE_BYTES", 4):
            for candidate in (source, link, fifo):
                self.assertEqual(self.update(source=candidate)["path"], "")

    def test_unowned_files_are_rejected(self):
        with patch.object(images.os, "getuid", return_value=os.getuid() + 1):
            with self.assertRaises(ValueError):
                images.source_file(self.image().as_uri())

    def test_byte_and_count_budgets_evict_oldest_files(self):
        source = self.image(content=b"12345678")
        keep = []
        with patch.object(images, "MAX_FILES", 2), patch.object(images, "MAX_BYTES", 16), patch.object(images, "MAX_IMAGE_BYTES", 8):
            for i in range(1, 5):
                key = f"img-test-{i}"
                keep = [key] + keep
                result = self.update(key=key, source=source, keep=keep)
                files = list(self.directory.iterdir())
                self.assertLessEqual(len(files), 2)
                self.assertLessEqual(sum(path.stat().st_size for path in files), 16)
                self.assertIn(key, result["kept"])
        self.assertGreater(len(list(self.discarded.iterdir())), 0)

    def test_clear_prunes_copies_and_legacy_files_without_touching_sender(self):
        source = self.image()
        self.update(source=source)
        (self.directory / "legacy-name").write_bytes(b"legacy")
        result = self.update(key="", keep=[])
        self.assertEqual(result, {"path": "", "kept": []})
        self.assertEqual(list(self.directory.iterdir()), [])
        self.assertTrue(source.exists())

    def test_failed_atomic_publication_never_returns_a_partial_path(self):
        source = self.image()
        with patch.object(Path, "replace", side_effect=OSError("fixture failure")):
            result = self.update(source=source)
        self.assertEqual(result["path"], "")
        self.assertEqual(list(self.directory.iterdir()), [])


if __name__ == "__main__":
    unittest.main()
