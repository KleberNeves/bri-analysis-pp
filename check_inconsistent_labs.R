#!/usr/bin/env Rscript

## Script: check_inconsistent_labs.R
## Purpose: Detect lab assignment inconsistencies between Replication Assessment outputs
## Compares replication-level effect sizes (replication_es) between two inclusion sets
## Default comparison: primary vs all_exps_lab_units within the latest output date folder

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(openxlsx)
})

args <- commandArgs(trailingOnly = TRUE)

# Resolve base output folder (default: latest YYYY-MM-DD under ./output)
resolve_results_path <- function() {
  if (length(args) >= 1 && nzchar(args[[1]])) {
    return(args[[1]])
  }
  out_dirs <- list.dirs("output", recursive = FALSE, full.names = FALSE)
  date_dirs <- out_dirs[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", out_dirs)]
  if (length(date_dirs) == 0) stop("No dated output folders found under ./output")
  date_dirs[order(date_dirs)][[length(date_dirs)]]
}

results_path <- resolve_results_path()

# Methods to check (default: c("t","z","knha"))
methods <- if (length(args) >= 2 && nzchar(args[[2]])) strsplit(args[[2]], ",")[[1]] else c("t","z","knha")
methods <- trimws(methods)

# Inclusion sets to compare (source vs target)
src_analysis <- if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else "primary"
tgt_analysis <- if (length(args) >= 4 && nzchar(args[[4]])) args[[4]] else "all_exps_lab_units"

# Numeric tolerance for equality of replication_es across analyses
tol <- if (length(args) >= 5 && nzchar(args[[5]])) as.numeric(args[[5]]) else 1e-9

base_dir <- file.path("output", results_path)

read_rep_table <- function(analysis, method) {
  dirp <- file.path(base_dir, paste0(analysis, " ", method))
  fp <- file.path(dirp, "Replication Assessment by Replication.tsv")
  if (!file.exists(fp)) return(NULL)
  suppressMessages(readr::read_tsv(fp, show_col_types = FALSE))
}

compare_one <- function(method) {
  src_df <- read_rep_table(src_analysis, method)
  tgt_df <- read_rep_table(tgt_analysis, method)
  if (is.null(src_df) || is.null(tgt_df)) {
    return(list(method = method, inconsistencies = tibble(), note = sprintf("Missing files for method %s", method)))
  }

  common <- full_join(
    src_df %>% select(LAB, EXP, replication_es) %>% mutate(present_src = TRUE),
    tgt_df %>% select(LAB, EXP, replication_es) %>% mutate(present_tgt = TRUE),
    by = c("LAB","EXP"),
    suffix = c(".src",".tgt")
  ) %>% mutate(
    diff_replication_es = abs(replication_es.src - replication_es.tgt),
    present_src = ifelse(is.na(present_src), FALSE, present_src),
    present_tgt = ifelse(is.na(present_tgt), FALSE, present_tgt),
    inconsistent = case_when(
      present_src & present_tgt ~ (is.finite(diff_replication_es) & diff_replication_es > tol),
      xor(present_src, present_tgt) ~ TRUE,
      TRUE ~ FALSE
    )
  )

  inc <- common %>% filter(inconsistent) %>% arrange(EXP, LAB)
  list(method = method, inconsistencies = inc, note = NA_character_)
}

res <- map(methods, compare_one)

# Collate inconsistencies
inc_all <- map_dfr(res, function(x) {
  if (nrow(x$inconsistencies) == 0) return(tibble())
  mutate(x$inconsistencies, method = x$method, .before = 1)
})

out_file <- file.path(base_dir, sprintf("lab_assignment_inconsistencies_%s_vs_%s.tsv", src_analysis, tgt_analysis))

  if (nrow(inc_all) == 0) {
    message(sprintf("No inconsistencies found comparing %s vs %s in %s (methods: %s)", src_analysis, tgt_analysis, results_path, paste(methods, collapse = ", ")))
  } else {
  # Keep only replication_es columns for each table
  inc_out <- inc_all %>%
    transmute(
      method, EXP, LAB,
      !!paste0("replication_es_", src_analysis) := replication_es.src,
      !!paste0("replication_es_", tgt_analysis) := replication_es.tgt
    )
  readr::write_tsv(inc_out, out_file)
  message(sprintf("Found %d lab-level inconsistencies across %d experiments.", nrow(inc_out), dplyr::n_distinct(inc_out$EXP)))
  message(sprintf("Report written to: %s", out_file))

  # Print a brief per-experiment summary
  summary_tbl <- inc_out %>% group_by(method, EXP) %>% summarise(n_labs = n(), .groups = "drop")
  print(summary_tbl, n = 200)

  # Also print per-inconsistency replication_es from each table for quick inspection
  inc_print <- inc_out %>%
    mutate(across(starts_with("replication_es_"), ~round(.x, 6))) %>%
    arrange(method, EXP, LAB)
  message("\nDetails (replication_es in each table):")
  print(inc_print, n = 200)

  # Export to XLSX as well (summary and details)
  out_xlsx <- file.path(base_dir, sprintf("lab_assignment_inconsistencies_%s_vs_%s.xlsx", src_analysis, tgt_analysis))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "summary")
  openxlsx::writeData(wb, "summary", summary_tbl)
  openxlsx::addWorksheet(wb, "details")
  openxlsx::writeData(wb, "details", inc_out)
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  message(sprintf("XLSX written to: %s", out_xlsx))
  }

invisible(TRUE)
