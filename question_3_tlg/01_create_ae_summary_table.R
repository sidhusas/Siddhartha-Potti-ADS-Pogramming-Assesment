# =============================================================================
# Question 3, task 1: TEAE summary table with {gtsummary}
# Script  : question_3_tlg/01_create_ae_summary_table.R
#
# Inputs  : pharmaverseadam::adae (events) and pharmaverseadam::adsl (denominators)
# Output  : ae_summary_table.html, ae_summary_table.csv, 01_create_ae_summary_table.log
#           (written to the current working directory, or ADSL_TLG_OUTPUT_DIR if set)
#
# Specification (assessment, Question 3, task 1):
#   - treatment-emergent AEs only: ADAE.TRTEMFL == "Y"
#   - rows: AESOC and AETERM  (shown nested: each AESOC with its AETERMs under it;
#     AETERM is the reported term as collected, not the coded preferred term)
#   - columns: treatment groups (ACTARM) plus a total column with all subjects
#   - cell values: count (n) and percentage (%)
#   - sorted by descending frequency
#
# =============================================================================

# ---- Settings you can change ---------------------------------------------------
# The document asks for "a total column with all subjects" and does not mention a
# safety restriction, so the default (FALSE) uses EVERY subject in ADSL: 306 subjects,
# including the 52 "Screen Failure" subjects (they have no TEAEs, so their column
# shows 0). Set to TRUE for an optional safety-population version (SAFFL == "Y",
# 254 subjects, Screen Failure column dropped). This is NOT specified by the document.
use_safety_population <- FALSE

# ---- Packages --------------------------------------------------------------------
required_pkgs <- c("pharmaverseadam", "gtsummary", "gt", "dplyr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(gtsummary)
  library(gt)
  library(dplyr)
})

# ---- Table builder (also used by test_ae_summary_table.R) ----------------------------
# adae: adverse events; adsl: one row per subject (the denominator).
build_ae_table <- function(adae, adsl, use_safety_population = TRUE) {
  if (use_safety_population) adsl <- adsl %>% filter(SAFFL == "Y")
  
  # Make ACTARM a factor with every ADSL group as a level, so a group without any
  # TEAE (here "Screen Failure") still gets its own column showing 0.
  arm_levels <- sort(unique(adsl$ACTARM))
  adsl <- adsl %>% mutate(ACTARM = factor(ACTARM, levels = arm_levels))
  
  # Treatment-emergent events of subjects in the denominator population only.
  teae <- adae %>%
    filter(TRTEMFL == "Y", USUBJID %in% adsl$USUBJID) %>%
    select(USUBJID, ACTARM, AESOC, AETERM) %>%
    mutate(ACTARM = factor(ACTARM, levels = arm_levels))
  
  # tbl_hierarchical counts each SUBJECT once per SOC / term (not each event) and
  # uses ADSL for the denominators, so subjects without any TEAE are still counted.
  tbl <- tbl_hierarchical(
    data         = teae,
    variables    = c(AESOC, AETERM),
    by           = ACTARM,
    denominator  = adsl,
    id           = USUBJID,
    overall_row  = TRUE,
    label        = list(..ard_hierarchical_overall.. = "Treatment Emergent AEs")
  ) %>%
    add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") %>%
    sort_hierarchical(sort = "descending")   # most frequent first, at each level
  
  tbl
}

# ---- Log file and error / warning tracking ---------------------------------------
# Skipped when the test script sources this file to reuse build_ae_table().
if (!isTRUE(getOption("tlg.test_mode"))) {
  out_dir <- Sys.getenv("ADSL_TLG_OUTPUT_DIR", unset = getwd())
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  log_con <- file(file.path(out_dir, "01_create_ae_summary_table.log"), open = "wt")
  sink(log_con, split = TRUE)
  sink(log_con, type = "message")
  n_warn <- 0L
  n_err  <- 0L
  log_line <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", sep = "")
  
  run_table <- function() {
    log_line("Start AE summary table | ", R.version.string,
             " | gtsummary ", as.character(packageVersion("gtsummary")),
             " | pharmaverseadam ", as.character(packageVersion("pharmaverseadam")))
    adae <- pharmaverseadam::adae
    adsl <- pharmaverseadam::adsl
    log_line("ADAE records: ", nrow(adae), " | ADSL subjects: ", nrow(adsl),
             " | safety population only: ", use_safety_population)
    
    tbl <- build_ae_table(adae, adsl, use_safety_population)
    
    # Save the HTML table
    html_file <- file.path(out_dir, "ae_summary_table.html")
    tbl %>%
      as_gt() %>%
      tab_header(title = "Summary of Treatment-Emergent Adverse Events",
                 subtitle = "Number of subjects (%) by System Organ Class (AESOC) and Reported Term (AETERM)") %>%
      tab_source_note("TEAE = treatment-emergent adverse event (ADAE.TRTEMFL = 'Y'). A subject is counted once per row. Percentages use all subjects of each ACTARM group in ADSL as the denominator (Screen Failure subjects included unless use_safety_population is TRUE). Sorted by descending frequency.") %>%
      gtsave(html_file)
    log_line("Saved ", html_file)
    
    # Save a plain CSV copy of the same table for checking
    csv_df <- as.data.frame(as_tibble(tbl, col_labels = FALSE))
    write.csv(csv_df, file.path(out_dir, "ae_summary_table.csv"), row.names = FALSE, na = "")
    log_line("Saved ae_summary_table.csv (", nrow(csv_df), " rows)")
    cat("\nFirst rows of the table:\n")
    print(head(csv_df, 12))
    invisible(tbl)
  }
  
  result <- withCallingHandlers(
    tryCatch({ run_table(); TRUE },
             error = function(e) { n_err <<- n_err + 1L; log_line("ERROR: ", conditionMessage(e)); FALSE }),
    warning = function(w) { n_warn <<- n_warn + 1L; log_line("WARNING: ", conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  if (isTRUE(result) && n_warn == 0L && n_err == 0L) {
    log_line("SUCCESS: zero warnings and zero errors in this run")
  } else {
    log_line("FAILED or completed with issues: warnings = ", n_warn, ", errors = ", n_err)
  }
  sink(type = "message"); sink(); close(log_con)
}
