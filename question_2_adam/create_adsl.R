# =============================================================================
# Question 2: ADaM ADSL dataset creation with {admiral}
# Script  : question_2_adam/create_adsl.R
#
# Inputs  : pharmaversesdtm::dm, vs, ex, ds, ae  (public example SDTM data)
# Output  : adsl_output/adsl.rds, adsl_output/adsl.csv, adsl_output/create_adsl.log
#
# Derived variables (per the assessment specification)
#   AGEGR9 / AGEGR9N  age group "<18", "18 - 50", ">50" and 1, 2, 3
#   TRTSDTM / TRTSTMF treatment start datetime from first valid-dose exposure,
#                     missing time imputed (hours/minutes -> 00, seconds not flagged)
#   ITTFL             "Y" if DM.ARM is populated, else "N"
#   LSTAVLDT          last known alive date from VS, AE, DS and exposure (TRTEDTM)
# Supporting variables: TRTEDTM / TRTETMF (needed for LSTAVLDT source 4)

# =============================================================================

# ---- 0. Packages --------------------------------------------------------------
required_pkgs <- c("admiral", "pharmaversesdtm", "dplyr", "lubridate", "stringr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

suppressPackageStartupMessages({
  library(admiral)
  library(pharmaversesdtm)
  library(dplyr)
  library(lubridate)
  library(stringr)
})

# build_adsl() holds all derivation logic. It takes the five SDTM datasets as
# arguments so the same code can be run on the public data (below) and on small
# hand-made test data (test_adsl.R).
build_adsl <- function(dm, vs, ex, ds, ae) {
  # SDTM character columns may hold "" for missing; treat them as NA
  convert_blanks_to_na <- function(x) admiral::convert_blanks_to_na(x)
  dm <- convert_blanks_to_na(dm)
  vs <- convert_blanks_to_na(vs)
  ex <- convert_blanks_to_na(ex)
  ds <- convert_blanks_to_na(ds)
  ae <- convert_blanks_to_na(ae)
  
  # DM is the basis of ADSL. DM has one record per subject; drop DOMAIN as in the
  # admiral ADSL template.
  adsl <- dm %>% select(-DOMAIN)
  
  # ---- 3. AGEGR9 / AGEGR9N ------------------------------------------------------
  # Based on DM.AGE: <18 -> 1, 18 - 50 -> 2, >50 -> 3. Missing AGE stays missing.
  adsl <- adsl %>%
    mutate(
      AGEGR9 = case_when(
        is.na(AGE)  ~ NA_character_,
        AGE < 18    ~ "<18",
        AGE <= 50   ~ "18 - 50",
        TRUE        ~ ">50"
      ),
      AGEGR9N = case_when(
        AGEGR9 == "<18"     ~ 1,
        AGEGR9 == "18 - 50" ~ 2,
        AGEGR9 == ">50"     ~ 3,
        TRUE                ~ NA_real_
      )
    )
  
  # ---- 4. Exposure: valid dose, TRTSDTM/TRTSTMF and TRTEDTM/TRTETMF -------------
  # Valid dose: EXDOSE > 0, or EXDOSE == 0 and EXTRT contains "PLACEBO".
  # Only records with a complete date part in EXSTDTC (10 characters, yyyy-mm-dd)
  # are used (the complete-date check is done by the imputation function: a
  # partial date such as "2014-01" gives NA and is dropped by the filter).
  ex_valid <- ex %>%
    filter(
      (EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) &
        !is.na(EXSTDTC) & nchar(EXSTDTC) >= 10
    ) %>%
    # convert EXSTDTC / EXENDTC to datetimes. highest_imputation = "h" imputes
    # missing time only (never a missing date). Missing time -> 00:00:00 for the
    # start. admiral flags imputed seconds as "S"; the specification says not to
    # populate the flag when only seconds are missing, so "S" is set to NA below.
    derive_vars_dtm(
      dtc = EXSTDTC,
      new_vars_prefix = "EXST",
      highest_imputation = "h",
      time_imputation = "first",
      ignore_seconds_flag = FALSE
    ) %>%
    # last exposure: missing end time imputed to 23:59:59 ("last") since the end
    # of treatment is the last moment of that day
    derive_vars_dtm(
      dtc = EXENDTC,
      new_vars_prefix = "EXEN",
      highest_imputation = "h",
      time_imputation = "last",
      ignore_seconds_flag = FALSE
    )
  
  ex_valid <- ex_valid %>%
    mutate(
      EXSTTMF = if_else(EXSTTMF == "S", NA_character_, EXSTTMF),
      EXENTMF = if_else(EXENTMF == "S", NA_character_, EXENTMF)
    )
  
  # First valid exposure per subject -> TRTSDTM, TRTSTMF
  adsl <- adsl %>%
    derive_vars_merged(
      dataset_add = ex_valid,
      filter_add  = !is.na(EXSTDTM),
      new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
      order       = exprs(EXSTDTM, EXSEQ),
      mode        = "first",
      by_vars     = exprs(STUDYID, USUBJID)
    ) %>%
    # Last valid exposure per subject -> TRTEDTM, TRTETMF (used for LSTAVLDT)
    derive_vars_merged(
      dataset_add = ex_valid,
      filter_add  = !is.na(EXENDTM),
      new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
      order       = exprs(EXENDTM, EXSEQ),
      mode        = "last",
      by_vars     = exprs(STUDYID, USUBJID)
    ) %>%
    derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))   # TRTSDT, TRTEDT
  
  # ---- 5. ITTFL -----------------------------------------------------------------
  # "Y" if DM.ARM is populated, else "N" (literal specification).
  adsl <- adsl %>%
    mutate(ITTFL = if_else(!is.na(ARM), "Y", "N"))
  
  # ---- 6. LSTAVLDT --------------------------------------------------------------
  # Max of: (1) VS date with a valid result, (2) complete AE onset date,
  #         (3) complete disposition date, (4) date of last valid-dose exposure.
  # date_imputation is not set, so partial dates (e.g. "2014-03") become NA and
  # are ignored: only COMPLETE dates count.
  adsl <- adsl %>%
    derive_vars_extreme_event(
      by_vars = exprs(STUDYID, USUBJID),
      events = list(
        event(
          dataset_name = "vs",
          order = exprs(VSDTC, VSSEQ),
          condition = !is.na(convert_dtc_to_dt(VSDTC)) &
            !(is.na(VSSTRESN) & is.na(VSSTRESC)),
          set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(VSDTC), SEQ = VSSEQ)
        ),
        event(
          dataset_name = "ae",
          order = exprs(AESTDTC, AESEQ),
          condition = !is.na(convert_dtc_to_dt(AESTDTC)),
          set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(AESTDTC), SEQ = AESEQ)
        ),
        event(
          dataset_name = "ds",
          order = exprs(DSSTDTC, DSSEQ),
          condition = !is.na(convert_dtc_to_dt(DSSTDTC)),
          set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(DSSTDTC), SEQ = DSSEQ)
        ),
        event(
          dataset_name = "adsl",
          condition = !is.na(TRTEDT),
          set_values_to = exprs(LSTAVLDT = TRTEDT, SEQ = 0)
        )
      ),
      source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
      tmp_event_nr_var = event_nr,
      order = exprs(LSTAVLDT, SEQ, event_nr),
      mode = "last",
      new_vars = exprs(LSTAVLDT)
    )
  
  adsl
}

# ---- 7-9. Run on the public data, check, summarise, save -----------------------
run_adsl <- function() {
  log_line("Start ADSL creation")
  log_line(R.version.string, " | admiral ", as.character(packageVersion("admiral")),
           " | pharmaversesdtm ", as.character(packageVersion("pharmaversesdtm"))) 
  dm <- pharmaversesdtm::dm
  log_line("DM subjects: ", nrow(dm))
  adsl <- build_adsl(dm, pharmaversesdtm::vs, pharmaversesdtm::ex,
                     pharmaversesdtm::ds, pharmaversesdtm::ae)
  
  # ---- 7. Final checks (these stop the script if something is wrong) ------------
  stopifnot(
    nrow(adsl) == nrow(dm),                                   # one row per subject
    !anyDuplicated(adsl$USUBJID),
    all(adsl$AGEGR9N %in% c(1, 2, 3)),
    all(adsl$ITTFL %in% c("Y", "N")),
    all(is.na(adsl$TRTSTMF) | adsl$TRTSTMF %in% c("H", "M")),
    all(is.na(adsl$TRTSDTM) | is.na(adsl$TRTEDTM) | adsl$TRTSDTM <= adsl$TRTEDTM)
  )
  log_line("Checks passed")
  
  # ---- 8. Summary for the log ---------------------------------------------------
  cat("\nAGEGR9 counts:\n");  print(table(adsl$AGEGR9, adsl$AGEGR9N, useNA = "ifany"))
  cat("\nITTFL counts:\n");   print(table(adsl$ITTFL, adsl$ARM, useNA = "ifany"))
  cat("\nTRTSTMF counts:\n"); print(table(adsl$TRTSTMF, useNA = "ifany"))
  cat("\nMissing TRTSDTM:", sum(is.na(adsl$TRTSDTM)), " Missing LSTAVLDT:",
      sum(is.na(adsl$LSTAVLDT)), "\n")
  cat("\nTRTSDTM / LSTAVLDT first rows:\n")
  print(as.data.frame(head(adsl[, c("USUBJID", "AGE", "AGEGR9", "AGEGR9N", "TRTSDTM",
                                    "TRTSTMF", "TRTEDTM", "ITTFL", "LSTAVLDT")], 10)))
  
  # ---- 9. Save ------------------------------------------------------------------
  saveRDS(adsl, file.path(out_dir, "adsl.rds"))
  write.csv(adsl, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")
  log_line("Saved adsl.rds and adsl.csv to ", out_dir, " (", nrow(adsl), " rows, ",
           ncol(adsl), " columns)")
  invisible(adsl)
}

# ---- 1. Log file and error/warning tracking ----------------------------------
# Skipped when test_adsl.R sources this file to reuse build_adsl().
if (!isTRUE(getOption("adsl.test_mode"))) {
  # ---- 1. Log file and error/warning tracking ----------------------------------
  out_dir <- Sys.getenv("ADSL_OUTPUT_DIR", unset = file.path(getwd(), "adsl_output"))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(out_dir, "create_adsl.log")
  
  log_con <- file(log_file, open = "wt")
  sink(log_con, split = TRUE)                    # copy Console output to the log
  sink(log_con, type = "message")                # and messages/warnings too
  n_warn <- 0L
  n_err  <- 0L
  
  log_line <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", sep = "")
  
  result <- withCallingHandlers(
    tryCatch(
      { run_adsl(); TRUE },
      error = function(e) {
        n_err <<- n_err + 1L
        log_line("ERROR: ", conditionMessage(e))
        FALSE
      }
    ),
    warning = function(w) {
      n_warn <<- n_warn + 1L
      log_line("WARNING: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (isTRUE(result) && n_warn == 0L && n_err == 0L) {
    log_line("SUCCESS: zero warnings and zero errors in this run")
  } else {
    log_line("FAILED or completed with issues: warnings = ", n_warn, ", errors = ", n_err)
  }
  sink(type = "message")
  sink()
  close(log_con)
}
