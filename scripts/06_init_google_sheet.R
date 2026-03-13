#!/usr/bin/env Rscript

# 06_init_google_sheet.R
#
# One-time (or occasional) script to initialize / validate the Google Sheet
# schema used by scripts/review_dashboard.R for review decisions.
#
# Usage (from project root):
#   Rscript scripts/06_init_google_sheet.R
#      or
#   source("scripts/06_init_google_sheet.R")

library(googlesheets4)

# --- Configuration -------------------------------------------------------------

# Must match SHEET_ID in scripts/review_dashboard.R
sheet_id <- "1Sol8ZKsB4QAaROC5rHifZbSZo_xZD9LYDo2It8C5fsw"

# Prefer environment variable for service account path, fall back to .secrets/
root <- getwd()
sa_path <- Sys.getenv(
  "GOOGLE_SERVICE_ACCOUNT_JSON",
  unset = file.path(root, ".secrets", "service_account.json")
)

if (!file.exists(sa_path)) {
  stop(
    "Service account key not found at ", sa_path, ".\n",
    "Set GOOGLE_SERVICE_ACCOUNT_JSON or create .secrets/service_account.json ",
    "before running this script."
  )
}

# --- Authenticate --------------------------------------------------------------

gs4_auth(path = sa_path)
cat("Authenticated with service account.\n\n")

# --- Check current sheet state -------------------------------------------------

sheet_meta <- gs4_get(sheet_id)
cat("Sheet name:", sheet_meta$name, "\n")
cat("Existing tabs:", paste(sheet_meta$sheets$name, collapse = ", "), "\n\n")

existing <- tryCatch(
  read_sheet(sheet_id, sheet = 1),
  error = function(e) {
    cat("No existing data found on first sheet (or could not read).\n")
    tibble::tibble()
  }
)

expected_cols <- c("article_id", "coder_id", "headline", "is_relevant", "timestamp")

if (nrow(existing) > 0) {
  cat("Sheet already contains", nrow(existing), "rows of data.\n")
  cat("Existing columns:", paste(names(existing), collapse = ", "), "\n\n")

  missing <- setdiff(expected_cols, names(existing))
  extra   <- setdiff(names(existing), expected_cols)

  if (length(missing) == 0 && length(extra) == 0) {
    cat("Schema looks correct. No changes made.\n")
  } else {
    if (length(missing) > 0) {
      cat("WARNING: Missing expected columns:", paste(missing, collapse = ", "), "\n")
    }
    if (length(extra) > 0) {
      cat("Note: Extra columns present:", paste(extra, collapse = ", "), "\n")
    }
    cat("No automatic overwrite performed. Please adjust the sheet manually if needed.\n")
  }
} else {
  # --- Initialize header row ---------------------------------------------------

  header <- tibble::tibble(
    article_id  = integer(0),
    coder_id    = character(0),
    headline    = character(0),
    is_relevant = character(0),
    timestamp   = character(0)
  )

  write_sheet(header, ss = sheet_id, sheet = 1)

  cat("Initialized sheet with columns:\n")
  cat("  - article_id  (integer): Unique article identifier\n")
  cat("  - coder_id    (string):  Coder's name/identifier\n")
  cat("  - headline    (string):  Article headline for readability\n")
  cat("  - is_relevant (string):  YES or NO\n")
  cat("  - timestamp   (string):  ISO 8601 timestamp of last response\n")
  cat("\nDone. Verify at:\n")
  cat("  https://docs.google.com/spreadsheets/d/", sheet_id, "/edit\n", sep = "")
}

