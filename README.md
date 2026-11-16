# Siddhartha-Potti-ADS-Pogramming-Assesment
Siddhartha Potti ADS Pogramming Assesment
**question_1**
# SDTM DS domain in R
Original assessment Question 1.
## Start here
1. Unzip this folder on your computer. Keep the `question_1` folder intact.
2. Open RStudio or a Posit Cloud R project. Upload the folder if using Posit Cloud.
3. Set the R working directory to the folder containing this README (not the inner `question_1` folder).
4. In the R Console, install the packages once:
#r
install.packages(c("sdtm.oak", "pharmaverseraw", "pharmaversesdtm", "dplyr"))
5. Run the program:
#r
source("question_1/01_create_ds_domain.R")
If R says it cannot open the file, check `getwd()` and use RStudio's Session > Set Working Directory > Choose Directory to select the extracted folder containing this README. Do not paste the entire program into the Console line by line.


**question_2_adam**
# ADAM ADSL domain in R
## Start here
1. Unzip this folder on your computer. Keep the `question_2_adam` folder intact.
2. Open RStudio or a Posit Cloud R project. Upload the folder if using Posit Cloud.
3. Set the R working directory to the folder containing this README (not the inner `question_2_adam` folder).
4. In the R Console, install the packages once:
#r
install.packages(c("admiral", "pharmaversesdtm", "dplyr", "lubridate", "stringr"))
5. Run the program:
#r
source("question_2_adam/create_adsl.R")
If R says it cannot open the file, check `getwd()` and use RStudio's Session > Set Working Directory > Choose Directory to select the extracted folder containing this README. Do not paste the entire program into the Console line by line.

**question_3_tlg**
# TLG: AE Summary Table and Visualization data 
## Start here
1. Unzip this folder on your computer. Keep the `question_3_tlg` folder intact.
2. Open RStudio or a Posit Cloud R project. Upload the folder if using Posit Cloud.
3. Set the R working directory to the folder containing this README (not the inner `question_3_tlg` folder).
4. In the R Console, install the packages once:
#r
install.packages(c("pharmaverseadam", "gtsummary", "gt", "dplyr","ggplot2"))
5. Run the program:
#r
source("question_3_tlg/create_ae_summary_table.R")
source("question_3_tlg/create_visualizations.R")
If R says it cannot open the file, check `getwd()` and use RStudio's Session > Set Working Directory > Choose Directory to select the extracted folder containing this README. Do not paste the entire program into the Console line by line.

**question_4_python**
# Question 4 - GenAI Clinical Data Assistant
Files (keep them together in one folder, e.g. question_4_python)
- clinical_trial_data_agent.py - schema, prompt, LLM back ends, ClinicalTrialDataAgent (prompt -> LLM -> JSON parse -> Pandas filter).
- test_clinical_data_agent.py - the 3 example queries, printed with an independent recount.
- test_unit.py - 16 offline unit tests (parser, validation, filter, flow, no accidental live mode).
- adae.csv - pharmaversesdtm::ae exported from R (1191 rows, 225 subjects). The code finds it next to the script.
- requirements.txt
How it works
The LLM sees the column descriptions (AESEV, AETERM, AESOC) and returns JSON {"target_column", "filter_value"}. The code checks the column is allowed, filters the dataframe (AESEV exact match; AETERM and AESOC case-insensitive contains) and returns the unique USUBJID count and ID list.
Run (offline, free, default). In the Posit Cloud Terminal:
    cd question_4_python        # use the real path of the folder holding these files
    pip install -r requirements.txt
    python test_clinical_data_agent.py
    python -m unittest -v test_unit
It prints "Mode: MOCK LLM". An OPENAI_API_KEY in your environment does NOT turn live mode on.
Live model (optional, paid by your OpenAI account, only if you choose):
    python test_clinical_data_agent.py --live
With no OPENAI_API_KEY set, it asks for the key with a hidden prompt and keeps it in memory only. Do not put the key in code, the repo, chat or a file in the repo.
Scope
Tested offline only: the mock run matched an independent recount on 3 queries, and 16 unit tests pass. The live OpenAI path has never been run, so its answers are unverified. The mock is a keyword stand-in, not a language model. Not yet run in Posit Cloud.
