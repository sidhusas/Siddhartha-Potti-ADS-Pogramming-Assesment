"""Test script: runs 3 example queries and prints the results, with an independent recount.

  python test_clinical_data_agent.py          # default: offline MockLLM, no API, no cost
  python test_clinical_data_agent.py --live   # deliberate opt-in: real OpenAI calls (paid)

An OPENAI_API_KEY in the environment does NOT switch on live mode by itself.
"""
import argparse

from clinical_trial_data_agent import ClinicalTrialDataAgent, MockLLM, OpenAILLM, load_ae

ap = argparse.ArgumentParser()
ap.add_argument("--live", action="store_true", help="call the real OpenAI model (costs money)")
args = ap.parse_args()

ae = load_ae()
if args.live:
    llm, mode = OpenAILLM(), "LIVE OpenAI model"
else:
    llm, mode = MockLLM(ae), "MOCK LLM (offline, no model used)"
print("Mode:", mode)
agent = ClinicalTrialDataAgent(ae, llm)

queries = [
    "Give me the subjects who had Adverse events of Moderate severity.",
    "Which patients had a Headache?",
    "List subjects with Cardiac adverse events.",
]
expected = {  # recomputed independently, straight from the data
    queries[0]: ae.loc[ae.AESEV == "MODERATE", "USUBJID"].nunique(),
    queries[1]: ae.loc[ae.AETERM.str.upper() == "HEADACHE", "USUBJID"].nunique(),
    queries[2]: ae.loc[ae.AESOC == "CARDIAC DISORDERS", "USUBJID"].nunique(),
}
failed = 0
for q in queries:
    r = agent.ask(q)
    ok = r["n_subjects"] == expected[q]
    failed += not ok
    print(f"\nQ: {q}\n  column={r['target_column']}  value={r['filter_value']}")
    print(f"  unique subjects: {r['n_subjects']} (independent recount: {expected[q]}) {'OK' if ok else 'MISMATCH'}")
    print("  first 5 IDs:", r["subject_ids"][:5])
print("\nRESULT:", "all 3 queries matched the recount" if not failed else f"{failed} MISMATCH")
