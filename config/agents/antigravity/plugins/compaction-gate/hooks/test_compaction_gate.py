"""Run: python3 -m unittest discover -s config/agents/antigravity/plugins/compaction-gate/hooks"""

import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import compaction_gate as g

CFG = {"enabled": True, "mode": "gate", "threshold": 500_000, "warnThreshold": 400_000}


class ConfigTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.path = Path(self.dir.name) / "compaction.json"
        env = mock.patch.dict(os.environ, {}, clear=False)
        env.start()
        self.addCleanup(env.stop)
        for key in (
            "AGY_COMPACTION_THRESHOLD",
            "AGY_COMPACTION_MODE",
            "AGY_COMPACTION_CONFIG",
        ):
            os.environ.pop(key, None)

    def tearDown(self):
        self.dir.cleanup()

    def write(self, text):
        self.path.write_text(text)

    def test_missing_file_uses_defaults(self):
        self.assertEqual(
            g.read_config(self.path), ({**CFG, "warnThreshold": None}, None)
        )

    def test_update_refuses_to_clobber_corrupt_file(self):
        self.write('{"mode": "warn", "note": "keep"')
        with self.assertRaises(ValueError):
            g.update_config(self.path, threshold="600k")
        self.assertEqual(self.path.read_text(), '{"mode": "warn", "note": "keep"')

    def test_update_validates_before_writing(self):
        self.write('{"threshold": "700k"}')
        with self.assertRaises(ValueError):
            g.update_config(self.path, mode="loud")
        self.assertEqual(self.path.read_text(), '{"threshold": "700k"}')

    def test_half_invalid_file_falls_back_entirely(self):
        self.write('{"mode": "warn", "threshold": "bogus"}')
        cfg, warning = g.read_config(self.path)
        self.assertEqual(cfg["mode"], "gate")
        self.assertIn("using defaults", warning)

    def test_env_threshold_does_not_reenable_disabled_gate(self):
        self.write('{"enabled": false}')
        with mock.patch.dict(os.environ, {"AGY_COMPACTION_THRESHOLD": "100k"}):
            cfg, _ = g.read_config(self.path)
        self.assertEqual((cfg["enabled"], cfg["threshold"]), (False, 100_000))

    def test_off_keeps_threshold_so_on_restores_it(self):
        g.run_command(["750k"], self.path)
        g.run_command(["off"], self.path)
        self.assertFalse(g.read_config(self.path)[0]["enabled"])
        g.run_command(["on"], self.path)
        self.assertEqual(g.read_config(self.path)[0]["threshold"], 750_000)

    def test_commands_write_expected_file(self):
        self.write('{"extra": 1}')
        for words in (["set", "600k"], ["warn"], ["warn-at", "400k"]):
            g.run_command(words, self.path)
        saved = json.loads(self.path.read_text())
        self.assertEqual(
            saved,
            {
                "enabled": True,
                "mode": "warn",
                "threshold": "600k",
                "warnThreshold": "400k",
                "extra": 1,
            },
        )

    def test_bad_arity_is_usage_error(self):
        for words in (["set"], ["mode"], ["on", "now"], ["set", "1k", "2k"]):
            with self.assertRaises(ValueError, msg=words):
                g.run_command(words, self.path)
        self.assertFalse(self.path.exists())


class ChatCommandTest(unittest.TestCase):
    def test_parse(self):
        self.assertEqual(
            g.parse_chat_command(
                "<USER_REQUEST>\n!compaction  warn-at 400k\n</USER_REQUEST>"
            ),
            ["warn-at", "400k"],
        )
        self.assertEqual(g.parse_chat_command("!COMPACTION"), [])
        self.assertIsNone(g.parse_chat_command("!compactionfoo"))
        self.assertIsNone(g.parse_chat_command("please run !compaction"))


class VerdictTest(unittest.TestCase):
    def test_rules(self):
        cases = [
            (CFG, 100, 0, None),
            (CFG, 450_000, 0, "note"),
            (CFG, 450_000, 1, None),
            (CFG, 500_000, 3, "block"),
            ({**CFG, "mode": "warn"}, 600_000, 0, "note"),
            ({**CFG, "mode": "warn"}, 600_000, 1, None),
            ({**CFG, "enabled": False}, 900_000, 0, None),
        ]
        for cfg, tokens, inv, kind in cases:
            result = g.verdict(cfg, tokens, inv, "cfg")
            self.assertEqual(result and result[0], kind, (cfg, tokens, inv))


class HookTest(unittest.TestCase):
    def test_blocks_over_threshold_and_runs_chat_commands(self):
        with tempfile.TemporaryDirectory() as d:
            transcript = Path(d) / "t.jsonl"
            transcript.write_text(
                json.dumps({"type": "PLANNER_RESPONSE", "input_tokens": 600_000}) + "\n"
            )
            with mock.patch.dict(os.environ, {"AGY_COMPACTION_CONFIG": f"{d}/c.json"}):
                payload = {"transcriptPath": str(transcript), "invocationNum": 2}
                self.assertEqual(
                    g.hook_response(payload)["terminationBehavior"], "terminate"
                )

                transcript.write_text(
                    transcript.read_text() + json.dumps({"type": "USER_INPUT"}) + "\n"
                )
                payload = {
                    **payload,
                    "invocationNum": 0,
                    "lastUserInput": "!compaction 750k",
                }
                msg = g.hook_response(payload)["injectSteps"][0]["errorMessage"][
                    "userMessage"
                ]
                self.assertIn("threshold=750,000", msg)
                del payload["lastUserInput"]
                self.assertEqual(g.hook_response(payload), {})


if __name__ == "__main__":
    unittest.main()
