# SDTM DS domain - original assessment Question 1
# install.packages(c("sdtm.oak", "pharmaverseraw", "pharmaversesdtm", "dplyr"))
# source("01_create_ds_domain.R")  
# Mapping specification: linked Subject_Disposition_aCRF.pdf in the assessment.
# Raw DS is the input. Public DM/SV are reference lookups, not prebuilt DS.
# DSSTDY requires DM.RFSTDTC; VISITNUM requires the visit schedule in SV.
# Actual ds_raw date field: IT.DSSTDAT (the aCRF calls it IT.DSSDAT).
# Output: ds in your R session; ds.csv/ds.rds/run log in ./ds_output.
# Output files are overwritten when this script is rerun.
# Tested example: 850 rows, 306 subjects; 52 intentionally missing DSSTDY.
# See README.md for study-specific assumptions and differences from the aCRF.

required_packages <- c("sdtm.oak", "pharmaverseraw", "pharmaversesdtm", "dplyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages first: ", paste(missing_packages, collapse = ", "),
       call. = FALSE)
}

# Starting dataset requested by the user. It is not overwritten by derivations.
raw <- pharmaverseraw::ds_raw

# The ten study CT rows provided in the assessment, stored locally in code.
make_study_ct <- function() {
  data.frame(
    codelist_code = rep("C66727", 10),
    term_code = c("C41331", "C25250", "C28554", "C48226", "C48227", "C48250",
                  "C142185", "C49628", "C49632", "C49634"),
    term_value = c("ADVERSE EVENT", "COMPLETED", "DEATH", "LACK OF EFFICACY",
                   "LOST TO FOLLOW-UP", "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
                   "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR", "WITHDRAWAL BY SUBJECT"),
    collected_value = c("Adverse Event", "Complete", "Dead", "Lack of Efficacy",
                        "Lost To Follow-Up", "Physician Decision", "Protocol Violation",
                        "Trial Screen Failure", "Study Terminated By Sponsor", "Withdrawal by Subject"),
    term_preferred_term = c("AE", "Completed", "Died", NA, NA, NA, "Violation",
                            "Failure to Meet Inclusion/Exclusion Criteria", NA, "Dropout"),
    term_synonyms = c("ADVERSE EVENT", "COMPLETE", "Death", NA, NA, NA, NA, NA, NA,
                      "Discontinued Participation"),
    stringsAsFactors = FALSE
  )
}

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}
require_columns <- function(x, cols, name) {
  if (!is.data.frame(x)) stop(name, " must be a data frame.", call. = FALSE)
  missing <- setdiff(cols, names(x))
  if (length(missing)) stop(name, " missing columns: ", paste(missing, collapse = ", "),
                            call. = FALSE)
}
# Incomplete ISO dates are retained in DSSTDTC but never imputed for DSSTDY.
complete_date <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|T)", x)
  out <- rep(as.Date(NA), length(x))
  out[ok] <- as.Date(substr(x[ok], 1, 10), format = "%Y-%m-%d")
  out
}

derive_ds <- function(raw, dm, visits, study_ct = make_study_ct()) {
  require_columns(raw, c("STUDY", "PATNUM", "INSTANCE", "IT.DSTERM", "IT.DSDECOD",
                         "OTHERSP", "DSDTCOL", "DSTMCOL", "IT.DSSTDAT"), "raw")
  require_columns(dm, c("STUDYID", "USUBJID", "SITEID", "SUBJID", "RFSTDTC"), "dm")
  require_columns(visits, c("VISIT", "VISITNUM"), "visits")
  require_columns(study_ct, names(make_study_ct()), "study_ct")
  if (!nrow(raw)) stop("raw has no records.", call. = FALSE)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  for (col in names(raw)) if (is.character(raw[[col]])) raw[[col]] <- clean_text(raw[[col]])
  if (anyNA(raw[c("STUDY", "PATNUM", "INSTANCE")])) {
    stop("Missing study, patient or visit identifier.", call. = FALSE)
  }
  has_time <- !is.na(raw$DSTMCOL)
  if (any(has_time & !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", raw$DSTMCOL))) {
    stop("Invalid collection time: expected HH:MM or missing.", call. = FALSE)
  }
  raw <- sdtm.oak::generate_oak_id_vars(raw, pat_var = "PATNUM", raw_src = "ds_raw")
  
  # Public example PATNUM is SITEID-SUBJID. Resolve to DM, do not guess USUBJID.
  key <- dm[c("STUDYID", "USUBJID", "SITEID", "SUBJID", "RFSTDTC")]
  key$STUDY <- as.character(key$STUDYID)
  key$PATNUM <- paste(key$SITEID, key$SUBJID, sep = "-")
  if (anyDuplicated(key[c("STUDY", "PATNUM")]) ||
      anyDuplicated(key[c("STUDYID", "USUBJID")] )) {
    stop("DM keys are not unique.", call. = FALSE)
  }
  raw <- dplyr::left_join(raw, key[c("STUDY", "PATNUM", "USUBJID", "RFSTDTC")],
                          by = c("STUDY", "PATNUM"))
  if (anyNA(raw$USUBJID)) stop("Raw patient not found in DM.", call. = FALSE)
  
  # aCRF overrides take precedence, even if a reason was also collected.
  raw$other_event <- !is.na(raw$OTHERSP)
  raw$reason <- toupper(raw$IT.DSDECOD)
  raw$term <- toupper(ifelse(raw$other_event, raw$OTHERSP, raw$IT.DSTERM))
  raw$category <- ifelse(raw$other_event, "OTHER EVENT",
                         ifelse(raw$reason == "RANDOMIZED", "PROTOCOL MILESTONE",
                                "DISPOSITION EVENT"))
  if (anyNA(raw$term) || anyNA(raw$category)) {
    stop("Missing reported term or disposition reason.", call. = FALSE)
  }
  
  # Standardize CT case for matching. oak uses standard terms and collected/synonym
  # aliases. Explicit validation prevents its unmatched-value fallback being silent.
  ct <- study_ct[study_ct$codelist_code == "C66727", , drop = FALSE]
  if (!nrow(ct)) stop("No C66727 disposition CT supplied.", call. = FALSE)
  for (col in c("term_value", "collected_value", "term_synonyms")) {
    ct[[col]] <- toupper(clean_text(ct[[col]]))
  }
  aliases <- unique(c(ct$term_value, ct$collected_value,
                      trimws(unlist(strsplit(stats::na.omit(ct$term_synonyms), ";", fixed = TRUE)))))
  is_disposition <- !raw$other_event & !is.na(raw$reason) & raw$reason != "RANDOMIZED"
  bad <- unique(raw$reason[is_disposition & !raw$reason %in% aliases])
  if (length(bad)) stop("Unmapped disposition reason: ", paste(bad, collapse = ", "), call. = FALSE)
  
  ds <- sdtm.oak::assign_no_ct(raw_dat = raw, raw_var = "term", tgt_var = "DSTERM")
  if (any(is_disposition)) {
    ds <- sdtm.oak::assign_ct(tgt_dat = ds, raw_dat = raw[is_disposition, ],
                              raw_var = "reason", tgt_var = "DSDECOD",
                              ct_spec = ct, ct_clst = "C66727")
  } else ds$DSDECOD <- NA_character_
  # RANDOMIZED is a milestone, not a C66727 disposition term.
  raw$override_decod <- ifelse(raw$other_event, toupper(raw$OTHERSP),
                               ifelse(raw$reason == "RANDOMIZED", "RANDOMIZED", NA_character_))
  ds <- sdtm.oak::assign_no_ct(tgt_dat = ds, raw_dat = raw,
                               raw_var = "override_decod", tgt_var = "DSDECOD")
  ds <- sdtm.oak::assign_no_ct(tgt_dat = ds, raw_dat = raw,
                               raw_var = "category", tgt_var = "DSCAT")
  ds <- sdtm.oak::assign_datetime(tgt_dat = ds, tgt_var = "DSSTDTC", raw_dat = raw,
                                  raw_var = "IT.DSSTDAT", raw_fmt = "m-d-y")
  ds <- sdtm.oak::assign_datetime(tgt_dat = ds, tgt_var = "DSDTC", raw_dat = raw,
                                  raw_var = c("DSDTCOL", "DSTMCOL"),
                                  raw_fmt = c("m-d-y", "H:M"))
  for (col in c("DSSTDTC", "DSDTC")) {
    date_problems <- sdtm.oak::problems(ds[[col]])
    if (!is.null(date_problems) && NROW(date_problems) > 0L) {
      stop("Date parsing problem in ", col, call. = FALSE)
    }
    ds[[col]] <- as.character(ds[[col]])
    # oak parses components, so also reject impossible complete calendar dates.
    has_full_date <- !is.na(ds[[col]]) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", ds[[col]])
    if (any(has_full_date & is.na(complete_date(ds[[col]])))) {
      stop("Invalid calendar date in ", col, call. = FALSE)
    }
  }
  # Attach raw context through oak_id_vars, never by row position.
  context <- raw[c(sdtm.oak::oak_id_vars(), "STUDY", "USUBJID", "INSTANCE")]
  ds <- dplyr::left_join(ds, context, by = sdtm.oak::oak_id_vars())
  ds$STUDYID <- ds$STUDY
  ds$DOMAIN <- "DS"
  ds$VISIT <- toupper(ds$INSTANCE)
  vm <- unique(as.data.frame(visits[c("VISIT", "VISITNUM")]))
  vm$VISIT <- toupper(clean_text(vm$VISIT))
  if (!is.numeric(vm$VISITNUM) || anyNA(vm) || anyDuplicated(vm$VISIT)) {
    stop("Visit lookup is missing, ambiguous or nonnumeric.", call. = FALSE)
  }
  ds <- dplyr::left_join(ds, vm, by = "VISIT")
  if (anyNA(ds$VISITNUM)) stop("Visit not found in supplied visit lookup.", call. = FALSE)
  
  # SDTM study days are relative to RFSTDTC; there is no day zero.
  # Protect against partial dates: pass Date vectors, not partial ISO strings.
  day_input <- ds[c("STUDYID", "USUBJID", "oak_id", "DSSTDTC")]
  day_input$DSSTDTC <- complete_date(day_input$DSSTDTC)
  day_dm <- dm[c("USUBJID", "RFSTDTC")]
  if (anyDuplicated(day_dm$USUBJID)) stop("USUBJID is not unique in DM.", call. = FALSE)
  day_dm$RFSTDTC <- complete_date(day_dm$RFSTDTC)
  day_output <- sdtm.oak::derive_study_day(day_input, day_dm,
                                           tgdt = "DSSTDTC", refdt = "RFSTDTC", study_day_var = "DSSTDY")
  ds <- dplyr::left_join(ds, day_output[c("oak_id", "DSSTDY")], by = "oak_id")
  ds <- sdtm.oak::derive_seq(ds, tgt_var = "DSSEQ",
                             rec_vars = c("STUDYID", "USUBJID", "DSSTDTC", "DSCAT", "DSDECOD", "oak_id"),
                             sbj_vars = c("STUDYID", "USUBJID"))
  ds <- as.data.frame(ds[c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD",
                           "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY")])
  stopifnot(nrow(ds) == nrow(raw), !anyNA(ds[c("STUDYID", "USUBJID", "DSTERM", "DSDECOD")]),
            !anyDuplicated(ds[c("STUDYID", "USUBJID", "DSSEQ")]),
            all(is.na(ds$DSSTDY) | ds$DSSTDY != 0L))
  labels <- c("Study Identifier", "Domain Abbreviation", "Unique Subject Identifier",
              "Sequence Number", "Reported Term for the Disposition Event", "Standardized Disposition Term",
              "Category for Disposition Event", "Visit Number", "Visit Name", "Date/Time of Collection",
              "Start Date/Time of Disposition Event", "Study Day of Start of Disposition Event")
  for (i in seq_along(ds)) attr(ds[[i]], "label") <- labels[i]
  ds
}

run_ds <- function(raw, output_dir = Sys.getenv("DS_OUTPUT_DIR", "ds_output"))
  {
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create output directory: ", output_dir, call. = FALSE)
  }
  log_file <- file.path(output_dir, "create_ds_domain.log")
  sink(log_file, split = TRUE)
  on.exit(sink(), add = TRUE)
  cat("SDTM DS creation from public pharmaverseraw example data\n")
  cat("Run time (UTC):", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")
  tryCatch(withCallingHandlers({
    ds <- derive_ds(raw, pharmaversesdtm::dm,
                    pharmaversesdtm::sv[c("VISIT", "VISITNUM")])
    utils::write.csv(ds, file.path(output_dir, "ds.csv"), row.names = FALSE, na = "")
    saveRDS(ds, file.path(output_dir, "ds.rds"))
    cat("PASS: Created", nrow(ds), "records for", length(unique(ds$USUBJID)), "subjects.\n")
    cat("PASS: Required columns, row count, identifiers, CT coverage, visits, keys and no day zero.\n")
    cat("Missing DSSTDY (no complete event/reference date):", sum(is.na(ds$DSSTDY)), "\n")
    print(table(ds$DSCAT)); print(utils::head(ds, 6)); print(sessionInfo())
    cat("SUCCESS: zero warnings and zero errors in this run.\n")
    invisible(ds)
  }, warning = function(w) stop("Warning treated as error: ", conditionMessage(w), call. = FALSE)),
  error = function(e) {
    cat("FAILED:", conditionMessage(e), "\n")
    stop(conditionMessage(e), call. = FALSE)
  })
}
if (Sys.getenv("DS_SKIP_MAIN") != "1") {
  # Keep both objects available for inspection, e.g. View(raw) and View(ds).
  ds <- run_ds(raw = raw)
}

