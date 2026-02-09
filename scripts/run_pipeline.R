# run_pipeline.R
# Master orchestrator: runs the full CHEV pipeline in sequence.
#
# Usage: source("scripts/run_pipeline.R")
#   or:  Rscript scripts/run_pipeline.R [--from STEP]
#
# Options:
#   --from STEP   Resume from a specific step (1-5). Skips earlier steps if
#                 their output files already exist.

source(file.path("scripts", "00_setup.R"))

# --- Parse arguments ----------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
start_from <- 1L

if ("--from" %in% args) {
  idx <- which(args == "--from")
  if (idx < length(args)) {
    start_from <- as.integer(args[idx + 1])
  }
}

# --- Pipeline steps -----------------------------------------------------------

steps <- list(
  list(
    step = 1L,
    name = "Parse & clean DOCX files",
    script = file.path("scripts", "01_parse.R"),
    output = file.path(paths$data, "01_articles.json")
  ),
  list(
    step = 2L,
    name = "Classify relevant articles",
    script = file.path("scripts", "02_classify.R"),
    output = file.path(paths$data, "02_articles_filtered.json")
  ),
  list(
    step = 3L,
    name = "Extract damage event details",
    script = file.path("scripts", "03_extract.R"),
    output = file.path(paths$data, "03_article_damage_events_raw.json")
  ),
  list(
    step = 4L,
    name = "Geocode locations",
    script = file.path("scripts", "05_geocode.R"),
    output = file.path(paths$data, "03_article_damage_events.json")
  ),
  list(
    step = 5L,
    name = "Deduplicate to unique events",
    script = file.path("scripts", "04_deduplicate.R"),
    output = file.path(paths$data, "04_damage_events.json")
  )
)

# --- Execute ------------------------------------------------------------------

cli::cli_h1("CHEV Pipeline")
cli::cli_alert_info("Starting from step {start_from}")

for (s in steps) {
  if (s$step < start_from) {
    if (file.exists(s$output)) {
      cli::cli_alert_success("Step {s$step}: {s$name} — skipped (output exists)")
      next
    } else {
      cli::cli_abort(c(
        "x" = "Cannot skip step {s$step}: output {.file {s$output}} not found.",
        "i" = "Run from an earlier step or produce the missing file."
      ))
    }
  }

  cli::cli_h2("Step {s$step}: {s$name}")
  start_time <- Sys.time()

  source(s$script, local = new.env(parent = globalenv()))

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
  cli::cli_alert_success("Step {s$step} complete ({elapsed} min)")

  # Row count check
  if (file.exists(s$output)) {
    data <- jsonlite::fromJSON(s$output)
    n_rows <- if (is.data.frame(data)) nrow(data) else length(data)
    cli::cli_alert_info("Output: {.file {basename(s$output)}} — {n_rows} rows")
  }
}

cli::cli_h1("Pipeline complete")
