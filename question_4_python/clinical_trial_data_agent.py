"""Question 4: GenAI Clinical Data Assistant (LLM + Pandas).

Flow:  question --> Prompt (schema + question) --> LLM --> JSON --> Parse/validate --> Pandas filter --> result

The LLM only chooses WHICH column to filter (target_column) and WHAT value to
search for (filter_value). No keyword rules map "severity" -> AESEV in the agent
itself; the LLM does that from the schema description.

Two LLM back ends share the exact same flow:
  * OpenAILLM : live model through LangChain (paid; only used when the caller asks for it explicitly)
  * MockLLM   : offline stand-in that returns the same JSON shape (no API, no cost)
The mock is only for running without a key. It is NOT a language model.
"""
import json
import os
import re

import pandas as pd

# ---------------------------------------------------------------- 1. Schema
AE_SCHEMA = {
    "AESEV": "Severity/intensity of the adverse event. Values: MILD, MODERATE, SEVERE.",
    "AETERM": "Reported term: the specific adverse event or condition, e.g. HEADACHE, PRURITUS, NAUSEA.",
    "AESOC": "System organ class: the body system, e.g. CARDIAC DISORDERS, SKIN AND SUBCUTANEOUS TISSUE DISORDERS.",
}
ALLOWED_COLUMNS = list(AE_SCHEMA)

SCHEMA_TEXT = "\n".join(f"- {col}: {desc}" for col, desc in AE_SCHEMA.items())

PROMPT_TEMPLATE = """You translate a clinical safety reviewer's question about an adverse event (AE) dataset into a filter.

Dataset columns you may filter on:
{schema}

Pick the single best column for the question and the value to search for.
Rules:
- Questions about severity or intensity use AESEV, with the value MILD, MODERATE or SEVERE.
- Questions about a specific condition or event use AETERM, with the condition name.
- Questions about a body system or organ area use AESOC, with the body system word(s).
- Use upper case for filter_value.
Reply with ONLY a JSON object, no other text, in exactly this form:
{{"target_column": "<AESEV|AETERM|AESOC>", "filter_value": "<value>"}}

Question: {question}
"""


# ------------------------------------------------------------ 2. LLM back ends
class OpenAILLM:
    """Live model via LangChain. Reads OPENAI_API_KEY from the environment only."""

    def __init__(self, model="gpt-4o-mini"):
        from langchain_openai import ChatOpenAI  # imported here so offline use needs no package

        if not os.environ.get("OPENAI_API_KEY"):
            # Ask in the terminal without echoing; the key is kept in memory only.
            import getpass

            os.environ["OPENAI_API_KEY"] = getpass.getpass("OpenAI API key (hidden, not saved): ")
        self.llm = ChatOpenAI(model=model, temperature=0)

    def __call__(self, prompt: str) -> str:
        return self.llm.invoke(prompt).content


class MockLLM:
    """Offline stand-in for the LLM. Returns JSON text like a real model would."""

    SEVERITY = {"MILD", "MODERATE", "SEVERE"}

    def __init__(self, ae: pd.DataFrame):
        self.terms = sorted(ae["AETERM"].dropna().unique(), key=len, reverse=True)
        self.socs = list(ae["AESOC"].dropna().unique())

    def __call__(self, prompt: str) -> str:
        q = prompt.split("Question:", 1)[1].strip().upper()
        words = set(re.findall(r"[A-Z]+", q))
        out = None
        if words & self.SEVERITY:
            out = ("AESEV", sorted(words & self.SEVERITY)[0])
        if out is None:
            for t in self.terms:
                if re.search(r"\b" + re.escape(t) + r"\b", q):
                    out = ("AETERM", t)
                    break
        if out is None:
            soc_words = {w for s in self.socs for w in re.findall(r"[A-Z]+", s)}
            stop = {"AND", "THE", "OF", "SYSTEM", "DISORDERS", "CONDITIONS"}
            hit = [w for w in re.findall(r"[A-Z]+", q) if w in soc_words and w not in stop and len(w) > 3]
            if hit:
                out = ("AESOC", hit[0])
        if out is None:
            return json.dumps({"target_column": None, "filter_value": None})
        return json.dumps({"target_column": out[0], "filter_value": out[1]})


# -------------------------------------------------------------- 3. The agent
class ClinicalTrialDataAgent:
    def __init__(self, ae: pd.DataFrame, llm):
        self.ae = ae
        self.llm = llm  # any callable: prompt text -> reply text

    # Prompt
    def build_prompt(self, question: str) -> str:
        return PROMPT_TEMPLATE.format(schema=SCHEMA_TEXT, question=question)

    # Parse
    @staticmethod
    def parse(reply: str) -> dict:
        text = reply.strip()
        text = re.sub(r"^```(?:json)?|```$", "", text, flags=re.MULTILINE).strip()  # strip code fences
        data = json.loads(text)
        col, val = data.get("target_column"), data.get("filter_value")
        if col not in ALLOWED_COLUMNS:
            raise ValueError(f"LLM chose an unsupported column: {col!r}")
        if not isinstance(val, str) or not val.strip():
            raise ValueError(f"LLM gave no usable filter_value: {val!r}")
        return {"target_column": col, "filter_value": val.strip()}

    # Execute
    def execute(self, parsed: dict) -> dict:
        col, val = parsed["target_column"], parsed["filter_value"].upper()
        series = self.ae[col].astype(str).str.upper()
        if col == "AESEV":  # fixed vocabulary: exact match
            mask = series == val
        else:  # free text: case-insensitive contains, so "Cardiac" finds "CARDIAC DISORDERS"
            mask = series.str.contains(re.escape(val), na=False)
        ids = sorted(self.ae.loc[mask, "USUBJID"].dropna().unique())
        return {"target_column": col, "filter_value": parsed["filter_value"],
                "n_subjects": len(ids), "subject_ids": ids}

    def ask(self, question: str) -> dict:
        reply = self.llm(self.build_prompt(question))
        return self.execute(self.parse(reply))


HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_CSV = os.path.join(HERE, "adae.csv")  # found next to this file, whatever the working directory


def load_ae(path=DEFAULT_CSV) -> pd.DataFrame:
    return pd.read_csv(path, dtype=str, keep_default_na=False, na_values=[""])
