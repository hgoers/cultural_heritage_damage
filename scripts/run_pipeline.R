# run_pipeline.R
# Master orchestrator: runs the full CHEV pipeline in sequence.
#
# Usage:
#   Rscript scripts/run_pipeline.R               # Run full pipeline
#   Rscript scripts/run_pipeline.R --from 3       # Resume from step 3
#   Rscript scripts/run_pipeline.R --only 2       # Run only step 2
#   Rscript scripts/run_pipeline.R --skip-review   # Skip human review pauses
#   TEST_N=100 Rscript scripts/run_pipeline.R     # Test mode (100-article sample)
#
# Pipeline steps:
#   1  Parse & clean DOCX files          → data/01_articles.json
#   2  Classify relevant articles        → data/02_articles_filtered.json
#   3  Extract damage event details      → data/03_article_damage_events.json
#  3b  Geocode locations                 → updates 03_article_damage_events.json
#   4  Deduplicate to unique events      → data/04_damage_events.json
#
# Human review checkpoints pause after Steps 2 and 4 unless --skip-review is set.
# The review dashboard (scripts/review_dashboard.R) can be launched separately.

source(file.path("scripts", "00_setup.R"))

# ==============================================================================
# Parse arguments
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
start_from   <- 1L
only_step    <- NULL
skip_review  <- FALSE

if ("--from" %in% args) {
  idx <- which(args == "--from")
  if (idx < length(args)) start_from <- as.integer(args[idx + 1])
}

if ("--only" %in% args) {
  idx <- which(args == "--only")
  if (idx < length(args)) only_step <- as.integer(args[idx + 1])
}

if ("--skip-review" %in% args) skip_review <- TRUE

# ==============================================================================
# Pipeline definition
# ==============================================================================
#
# Each step declares:
#   step     Numeric ID (supports decimals for sub-steps: 3.5 = 3b)
#   label    Short label for display (used with --from / --only)
#   name     Human-readable description
#   script   Path to R script
#   output   Primary output file (used for skip checks and row counts)
#   depends  Files that must exist before this step runs

steps <- list(
  list(
    step    = 1L,
    label   = "1",
    name    = "Parse & clean DOCX files",
    script  = file.path("scripts", "01_parse.R"),
    output  = file.path(paths$data, "01_articles.json"),
    depends = list.files(paths$data_raw, pattern = "\\.DOCX$|\\.docx$", full.names = TRUE)
  ),
  list(
    step    = 2L,
    label   = "2",
    name    = "Classify relevant articles",
    script  = file.path("scripts", "02_classify.R"),
    output  = file.path(paths$data, "02_articles_filtered.json"),
    depends = file.path(paths$data, "01_articles.json")
  ),
  list(
    step    = 3L,
    label   = "3",
    name    = "Extract damage event details",
    script  = file.path("scripts", "03_extract.R"),
    output  = file.path(paths$data, "03_article_damage_events.json"),
    depends = file.path(paths$data, "02_articles_filtered.json")
  ),
  list(
    step    = 3L,
    label   = "3b",
    name    = "Geocode damage event locations",
    script  = file.path("scripts", "05_geocode.R"),
    output  = file.path(paths$data, "05_geocode_cache.json"),
    depends = file.path(paths$data, "03_article_damage_events.json")
  ),
  list(
    step    = 4L,
    label   = "4",
    name    = "Deduplicate to unique events",
    script  = file.path("scripts", "04_deduplicate.R"),
    output  = file.path(paths$data, "04_damage_events.json"),
    depends = file.path(paths$data, "03_article_damage_events.json")
  )
)

# ==============================================================================
# Helpers
# ==============================================================================

count_rows <- function(json_path) {
  if (!file.exists(json_path) || file.size(json_path) == 0) return(NA_integer_)
  data <- tryCatch(jsonlite::fromJSON(json_path), error = function(e) NULL)
  if (is.null(data)) return(NA_integer_)
  if (is.data.frame(data)) nrow(data) else length(data)
}

check_dependencies <- function(dep_paths) {
  missing <- dep_paths[!file.exists(dep_paths)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "x" = "Missing dependency file(s):",
      set_names(missing, rep("*", length(missing)))
    ))
  }
  invisible(TRUE)
}

prompt_review <- function(step_label, review_file) {
  if (skip_review) {
    cli::cli_alert_info("Skipping human review checkpoint (--skip-review)")
    return(invisible(NULL))
  }

  if (!file.exists(review_file) || file.size(review_file) == 0) {
    cli::cli_alert_success("No items flagged for review after step {step_label}")
    return(invisible(NULL))
  }

  n <- count_rows(review_file)
  cli::cli_rule()
  cli::cli_alert_warning(
    "HUMAN REVIEW CHECKPOINT after step {step_label}: {n} item(s) in {.file {basename(review_file)}}"
  )

  if (interactive()) {
    cli::cli_alert_info(
      "Review the flagged items, then press ENTER to continue or type 'stop' to halt."
    )
    response <- readline(prompt = "Continue? [ENTER/stop] ")
    if (tolower(trimws(response)) == "stop") {
      cli::cli_alert_danger("Pipeline halted by user at review checkpoint.")
      quit(save = "no", status = 0)
    }
  } else {
    cli::cli_alert_info(
      "Non-interactive mode: review {.file {review_file}} and re-run with {.code --from {as.integer(step_label) + 1}}"
    )
  }
}

# ==============================================================================
# Determine which steps to run
# ==============================================================================

if (!is.null(only_step)) {
  run_steps <- keep(steps, \(s) s$step == only_step)
  if (length(run_steps) == 0) {
    cli::cli_abort("No steps match --only {only_step}. Valid steps: 1, 2, 3, 4.")
  }
} else {
  run_steps <- keep(steps, \(s) s$step >= start_from)
}

# ==============================================================================
# Execute pipeline
# ==============================================================================

pipeline_start <- Sys.time()

cli::cli_h1("CHEV Pipeline")

test_n <- Sys.getenv("TEST_N", unset = "")
if (test_n != "") {
  cli::cli_alert_warning("TEST MODE: processing {test_n} articles only (TEST_N={test_n})")
}

cli::cli_alert_info("Steps to run: {paste(sapply(run_steps, \\(s) s$label), collapse = ' -> ')}")

# Track row counts for sanity checks
row_counts <- list()

for (s in run_steps) {
  cli::cli_h2("Step {s$label}: {s$name}")

  # Check dependencies
  if (length(s$depends) > 0) {
    tryCatch(
      check_dependencies(s$depends),
      error = function(e) {
        cli::cli_alert_danger(conditionMessage(e))
        cli::cli_abort("Cannot run step {s$label} — missing inputs.")
      }
    )
  }

  # Run the step
  step_start <- Sys.time()

  tryCatch({
    source(s$script, local = new.env(parent = globalenv()))
  }, error = function(e) {
    cli::cli_alert_danger("Step {s$label} FAILED: {conditionMessage(e)}")
    cli::cli_abort("Pipeline halted at step {s$label}.")
  })

  elapsed <- round(difftime(Sys.time(), step_start, units = "mins"), 1)
  cli::cli_alert_success("Step {s$label} complete ({elapsed} min)")

  # Output row count
  if (file.exists(s$output)) {
    n <- count_rows(s$output)
    if (!is.na(n)) {
      row_counts[[s$label]] <- n
      cli::cli_alert_info("Output: {.file {basename(s$output)}} — {n} rows")
    }
  }

  # Human review checkpoints
  if (s$label == "2") {
    prompt_review("2", file.path(paths$data, "02_review_needed.json"))
  }
  if (s$label == "4") {
    prompt_review("4", file.path(paths$data, "04_review_needed.json"))
  }
}

# ==============================================================================
# Post-pipeline sanity checks
# ==============================================================================

cli::cli_h2("Sanity checks")

# Row count expectations: Step 1 > Step 2 >= 1; Step 3 >= Step 2; Step 4 <= Step 3
checks_passed <- 0L
checks_total  <- 0L

run_check <- function(desc, condition) {
  checks_total <<- checks_total + 1L
  if (isTRUE(condition)) {
    cli::cli_alert_success(desc)
    checks_passed <<- checks_passed + 1L
  } else {
    cli::cli_alert_danger(desc)
  }
}

if (!is.null(row_counts[["1"]]) && !is.null(row_counts[["2"]])) {
  run_check(
    "Step 1 ({row_counts[['1']]} articles) > Step 2 ({row_counts[['2']]} filtered)",
    row_counts[["1"]] > row_counts[["2"]]
  )
}

if (!is.null(row_counts[["2"]]) && !is.null(row_counts[["3"]])) {
  run_check(
    "Step 3 ({row_counts[['3']]} records) >= Step 2 ({row_counts[['2']]} articles)",
    row_counts[["3"]] >= row_counts[["2"]]
  )
}

if (!is.null(row_counts[["3"]]) && !is.null(row_counts[["4"]])) {
  run_check(
    "Step 4 ({row_counts[['4']]} events) <= Step 3 ({row_counts[['3']]} records)",
    row_counts[["4"]] <= row_counts[["3"]]
  )
}

# Schema validation on final outputs
final_outputs <- list(
  list(
    file = file.path(paths$data, "01_articles.json"),
    schema = schemas$newspaper,
    name = "01_articles.json"
  ),
  list(
    file = file.path(paths$data, "03_article_damage_events.json"),
    schema = schemas$newspaper_damage_event,
    name = "03_article_damage_events.json"
  ),
  list(
    file = file.path(paths$data, "04_damage_events.json"),
    schema = schemas$damage_event,
    name = "04_damage_events.json"
  )
)

for (fo in final_outputs) {
  if (!file.exists(fo$file)) next

  checks_total <- checks_total + 1L
  data <- fromJSON(fo$file)
  records <- if (is.data.frame(data)) {
    split(data, seq_len(nrow(data)))
  } else {
    data
  }

  n_valid <- 0L
  for (rec in records) {
    rec_json <- toJSON(rec, auto_unbox = TRUE, na = "null", pretty = FALSE)
    valid <- tryCatch(
      jsonvalidate::json_validate(rec_json, fo$schema, verbose = FALSE, engine = "ajv"),
      error = function(e) FALSE
    )
    if (isTRUE(valid)) n_valid <- n_valid + 1L
  }

  all_valid <- n_valid == length(records)
  if (all_valid) {
    cli::cli_alert_success("{fo$name}: all {length(records)} records pass schema validation")
    checks_passed <- checks_passed + 1L
  } else {
    cli::cli_alert_danger(
      "{fo$name}: {n_valid}/{length(records)} records pass schema validation"
    )
  }
}

if (checks_total > 0) {
  cli::cli_alert_info("Checks passed: {checks_passed}/{checks_total}")
}

# ==============================================================================
# Summary
# ==============================================================================

pipeline_elapsed <- round(difftime(Sys.time(), pipeline_start, units = "mins"), 1)

cli::cli_h1("Pipeline complete ({pipeline_elapsed} min)")

cli::cli_rule("Output files")

output_files <- list.files(paths$data, pattern = "\\.json$", full.names = TRUE)
for (f in sort(output_files)) {
  n <- count_rows(f)
  size_kb <- round(file.size(f) / 1024, 1)
  label <- if (!is.na(n)) paste0(n, " rows, ", size_kb, " KB") else paste0(size_kb, " KB")
  cli::cli_alert_info("{.file {basename(f)}} — {label}")
}
