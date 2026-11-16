"""Offline unit tests (no network, no key). Run: python -m unittest -v test_unit"""
import os
import unittest
from unittest import mock

import pandas as pd

import clinical_trial_data_agent as m
from clinical_trial_data_agent import ClinicalTrialDataAgent as Agent

P = Agent.parse
DF = pd.DataFrame({
    "USUBJID": ["S1", "S1", "S2", "S3", "S4"],
    "AESEV": ["MILD", "MODERATE", "MODERATE", "SEVERE", "MILD"],
    "AETERM": ["HEADACHE", "NAUSEA", "Headache", "SINUS TACHYCARDIA", "RASH"],
    "AESOC": ["NERVOUS SYSTEM DISORDERS", "GASTROINTESTINAL DISORDERS", "NERVOUS SYSTEM DISORDERS",
              "CARDIAC DISORDERS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"],
})


class Parse(unittest.TestCase):
    def test_plain(self):
        self.assertEqual(P('{"target_column":"AESEV","filter_value":"severe"}'),
                         {"target_column": "AESEV", "filter_value": "severe"})

    def test_code_fence(self):
        r = P('```json\n{"target_column":"AETERM","filter_value":"HEADACHE"}\n```')
        self.assertEqual(r["target_column"], "AETERM")

    def test_bad_column(self):
        with self.assertRaises(ValueError):
            P('{"target_column":"AGE","filter_value":"5"}')

    def test_not_json(self):
        with self.assertRaises(ValueError):  # JSONDecodeError is a ValueError
            P("sorry, I cannot help")

    def test_null_and_empty_value(self):
        for v in ("null", '""', '"  "', "5"):
            with self.assertRaises(ValueError):
                P('{"target_column":"AESEV","filter_value":%s}' % v)


class Execute(unittest.TestCase):
    def run_q(self, col, val):
        return Agent(DF, None).execute({"target_column": col, "filter_value": val})

    def test_severity_exact_unique_subjects(self):
        r = self.run_q("AESEV", "moderate")
        self.assertEqual((r["n_subjects"], r["subject_ids"]), (2, ["S1", "S2"]))

    def test_term_case_insensitive_and_dedup(self):
        r = self.run_q("AETERM", "headache")
        self.assertEqual(r["subject_ids"], ["S1", "S2"])

    def test_soc_partial(self):
        self.assertEqual(self.run_q("AESOC", "cardiac")["subject_ids"], ["S3"])

    def test_no_match(self):
        r = self.run_q("AETERM", "ZZZ")
        self.assertEqual((r["n_subjects"], r["subject_ids"]), (0, []))

    def test_special_characters_not_regex(self):
        self.assertEqual(self.run_q("AETERM", "(")["n_subjects"], 0)


class Flow(unittest.TestCase):
    def test_prompt_has_schema_and_question(self):
        p = Agent(DF, None).build_prompt("Any rash?")
        for w in ("AESEV", "AETERM", "AESOC", "Any rash?"):
            self.assertIn(w, p)

    def test_ask_uses_llm_reply(self):
        seen = []
        def fake(prompt):
            seen.append(prompt)
            return '{"target_column":"AESOC","filter_value":"SKIN"}'
        r = Agent(DF, fake).ask("skin problems?")
        self.assertEqual(r["subject_ids"], ["S4"])
        self.assertEqual(len(seen), 1)

    def test_bad_llm_reply_raises(self):
        with self.assertRaises(ValueError):
            Agent(DF, lambda p: "nope").ask("x")


class RealData(unittest.TestCase):
    def test_csv_found_relative_to_file(self):
        ae = m.load_ae()
        self.assertEqual((len(ae), ae.USUBJID.nunique()), (1191, 225))

    def test_mock_routes_three_example_types(self):
        ae = m.load_ae()
        a = Agent(ae, m.MockLLM(ae))
        self.assertEqual(a.ask("moderate severity")["target_column"], "AESEV")
        self.assertEqual(a.ask("Headache")["target_column"], "AETERM")
        self.assertEqual(a.ask("Cardiac")["target_column"], "AESOC")


class NoAccidentalLive(unittest.TestCase):
    def test_key_in_env_does_not_enable_live(self):
        import subprocess, sys
        env = dict(os.environ, OPENAI_API_KEY="sk-fake-not-used")
        out = subprocess.run([sys.executable, os.path.join(m.HERE, "test_clinical_data_agent.py")],
                             capture_output=True, text=True, env=env, cwd="/").stdout
        self.assertIn("Mode: MOCK LLM", out)
        self.assertNotIn("LIVE", out)


if __name__ == "__main__":
    unittest.main()
