# =============================================================================
# Question 3, task 2: AE visualizations with {ggplot2}
# Script  : question_3_tlg/02_create_visualizations.R
#
# Input   : pharmaverseadam::adae (and pharmaverseadam::adsl only if you choose
#           the ADSL denominator below)
# Output  : ae_severity_by_treatment.png      (Plot 1)
#           top10_adverse_events.png          (Plot 2)
#           02_create_visualizations.log      (run log)
#           written to the current working directory, or ADSL_TLG_OUTPUT_DIR if set
#
# Specification (assessment, Question 3, task 2):
#   Plot 1: AE severity distribution by treatment (bar chart or heatmap).
#           Severity is ADAE.AESEV; treatment is ACTARM.
#   Plot 2: Top 10 most frequent AEs (ADAE.AETERM) with 95% CI for incidence rates.
#   Output format: PNG for both.
#
# =============================================================================

# ---- Settings you can change -----------------------------------------------------
# The document does not restrict the records, so by default ALL ADAE records are used
# (this reproduces the sample figures in the document: bar totals about 301 / 436 / 454
# and "n = 225 subjects" in the Plot 2 subtitle). Set TRUE for treatment-emergent
# records only (TRTEMFL == "Y"); that version is NOT asked for by the document.
teae_only <- FALSE

# Denominator for the Plot 2 percentages. The document's sample shows "n = 225 subjects",
# which is the number of subjects who have at least one record in ADAE, so the default is
# "adae". Use "adsl" to divide by every subject in ADSL (306) instead, which is the
# incidence among all subjects.
denominator <- "adae"

# ---- Packages ----------------------------------------------------------------------
required_pkgs <- c("pharmaverseadam", "ggplot2", "dplyr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(ggplot2)
  library(dplyr)
})

# ---- Data preparation (also used by test_visualizations.R) ---------------------------
# Records used in both plots.
select_records <- function(adae, teae_only = FALSE) {
  if (teae_only) adae <- adae %>% filter(TRTEMFL == "Y")
  adae
}

# Plot 1 data: number of AE records per treatment arm and severity.
prep_severity_data <- function(adae, teae_only = FALSE) {
  select_records(adae, teae_only) %>%
    filter(!is.na(ACTARM), !is.na(AESEV)) %>%
    mutate(AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))) %>%
    count(ACTARM, AESEV, name = "n_ae", .drop = FALSE)
}

# Plot 2 data: top-N AETERM by number of subjects, with Clopper-Pearson 95% CI.
# A subject is counted once per term. Percent = subjects with the term / denominator.
prep_top_ae_data <- function(adae, adsl, top_n = 10, teae_only = FALSE,
                             denominator = c("adae", "adsl")) {
  denominator <- match.arg(denominator)
  recs <- select_records(adae, teae_only)
  n_total <- if (denominator == "adae") dplyr::n_distinct(recs$USUBJID) else dplyr::n_distinct(adsl$USUBJID)
  
  counts <- recs %>%
    filter(!is.na(AETERM)) %>%
    group_by(AETERM) %>%
    summarise(n_subj = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
    arrange(desc(n_subj), AETERM) %>%          # ties broken alphabetically
    slice_head(n = top_n)
  
  ci <- t(vapply(counts$n_subj, function(x) {
    stats::binom.test(x, n_total, conf.level = 0.95)$conf.int   # Clopper-Pearson exact
  }, numeric(2)))
  counts %>%
    mutate(n_total = n_total,
           pct = 100 * n_subj / n_total,
           lower = 100 * ci[, 1],
           upper = 100 * ci[, 2])
}

# ---- Plot builders ------------------------------------------------------------------
make_plot1 <- function(sev) {
  ggplot(sev, aes(x = ACTARM, y = n_ae, fill = AESEV)) +
    geom_col() +                                   # stacked bars of AE counts
    labs(title = "AE severity distribution by treatment",
         x = "Treatment Arm", y = "Count of AEs", fill = "Severity/Intensity")
}

make_plot2 <- function(top) {
  top <- top %>% mutate(AETERM = factor(AETERM, levels = rev(AETERM)))  # most frequent on top
  ggplot(top, aes(x = pct, y = AETERM)) +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, orientation = "y") +
    geom_point(size = 2.5) +
    labs(title = "Top 10 Most Frequent Adverse Events",
         subtitle = sprintf("n = %d subjects; 95%% Clopper-Pearson CIs", top$n_total[1]),
         x = "Percentage of Patients (%)", y = NULL) +
    scale_x_continuous(labels = function(x) paste0(x, "%"))
}

# ---- Log file, run, save ----------------------------------------------------------------
# Skipped when test_visualizations.R sources this file to reuse the functions above.
if (!isTRUE(getOption("viz.test_mode"))) {
  out_dir <- Sys.getenv("ADSL_TLG_OUTPUT_DIR", unset = getwd())
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  log_con <- file(file.path(out_dir, "02_create_visualizations.log"), open = "wt")
  sink(log_con, split = TRUE)
  sink(log_con, type = "message")
  n_warn <- 0L
  n_err  <- 0L
  log_line <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", sep = "")
  
  run_plots <- function() {
    log_line("Start AE visualizations | ", R.version.string, " | ggplot2 ",
             as.character(packageVersion("ggplot2")), " | pharmaverseadam ",
             as.character(packageVersion("pharmaverseadam")))
    adae <- pharmaverseadam::adae
    adsl <- pharmaverseadam::adsl
    log_line("ADAE records: ", nrow(adae), " | subjects in ADAE: ", dplyr::n_distinct(adae$USUBJID),
             " | ADSL subjects: ", nrow(adsl), " | teae_only: ", teae_only,
             " | Plot 2 denominator: ", denominator)
    
    # Plot 1
    sev <- prep_severity_data(adae, teae_only)
    cat("\nPlot 1 data (AE records by arm and severity):\n"); print(as.data.frame(sev))
    p1 <- make_plot1(sev)
    f1 <- file.path(out_dir, "ae_severity_by_treatment.png")
    ggsave(f1, p1, width = 7, height = 5, dpi = 150)
    log_line("Saved ", f1)
    
    # Plot 2
    top <- prep_top_ae_data(adae, adsl, top_n = 10, teae_only = teae_only, denominator = denominator)
    cat("\nPlot 2 data (top 10 AETERM, subjects, % and 95% Clopper-Pearson CI):\n")
    print(as.data.frame(top %>% mutate(across(c(pct, lower, upper), ~ round(.x, 2)))))
    p2 <- make_plot2(top)
    f2 <- file.path(out_dir, "top10_adverse_events.png")
    ggsave(f2, p2, width = 8, height = 5, dpi = 150)
    log_line("Saved ", f2)
    stopifnot(file.exists(f1), file.exists(f2), file.size(f1) > 0, file.size(f2) > 0)
    invisible(list(p1 = p1, p2 = p2))
  }
  
  result <- withCallingHandlers(
    tryCatch({ run_plots(); TRUE },
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
