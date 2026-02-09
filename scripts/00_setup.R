# 00_setup.R
# Project setup: install/load packages, set constants, configure API access
# Source this script at the start of every pipeline step.

# --- Package management -------------------------------------------------------

required_packages <- c(

  # Data wrangling
  "tidyverse",
  "jsonlite",
  "jsonvalidate",


  # DOCX parsing
  "officer",


  # LLM integration
  "ellmer",

  # Geocoding
  "tidygeocoder",

  # String matching (deduplication)

  "stringdist",

  # CLI logging
  "cli"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    cli::cli_alert_info("Installing missing packages: {.pkg {missing}}")
    install.packages(missing)
  }
}

install_if_missing(required_packages)

# Load core packages
library(tidyverse)
library(jsonlite)
library(jsonvalidate)
library(cli)

# --- API configuration --------------------------------------------------------

# Anthropic API key should be set in .Renviron as ANTHROPIC_API_KEY
if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
  cli::cli_warn(c(
    "!" = "{.envvar ANTHROPIC_API_KEY} is not set.",
    "i" = "Add {.code ANTHROPIC_API_KEY=sk-...} to {.file .Renviron} and restart R."
  ))
}

# --- Project paths ------------------------------------------------------------

project_root <- here::here()

paths <- list(
  data_raw   = file.path(project_root, "data-raw"),
  data       = file.path(project_root, "data"),
  scripts    = file.path(project_root, "scripts"),
  schemas    = file.path(project_root, "schemas")
)

# Ensure output directory exists
if (!dir.exists(paths$data)) dir.create(paths$data, recursive = TRUE)

# --- Schema paths -------------------------------------------------------------

schemas <- list(
  newspaper             = file.path(paths$schemas, "schema_newspaper.json"),
  newspaper_damage_event = file.path(paths$schemas, "schema_newspaper_damage_event.json"),
  damage_event          = file.path(paths$schemas, "schema_damage_event.json")
)

# --- ID prefixes & formatting -------------------------------------------------

id_prefixes <- list(
  article = "ART",
  record  = "REC",
  event   = "EVT"
)

#' Generate zero-padded IDs
#'
#' @param prefix Character. One of "ART", "REC", "EVT".
#' @param n Integer. Number of IDs to generate.
#' @param start Integer. Starting number (default 1).
#' @return Character vector of IDs like "ART_0001", "ART_0002", ...
generate_ids <- function(prefix, n, start = 1L) {
  sprintf("%s_%04d", prefix, seq(from = start, length.out = n))
}

# --- Damage instance definition -----------------------------------------------
# Embedded verbatim for use in LLM system prompts (Steps 2, 3, 4).

damage_definition <- paste0(
  'An instance of heritage site damage is defined as: ',
  '"evidence of activity by an actor that damages a culturally significant ',
  'location over a specific temporal duration."\n\n',
  'a. Evidence: The occurrence or product of fighting, or the occurrence or ',
  'product of a destructive act (e.g., church hit by shelling; mosque set ablaze).\n',
  'b. Actor: A state or a non-state formally or informally organized group.\n',
  'c. Damage: Actors in the event are using manufactured weapons or are engaged ',
  'in actions directed against human life or property.\n',
  'd. Culturally significant location: Any site of cultural interest to the local, ',
  'national, or international community. Examples include libraries and archives, ',
  'archaeological sites, cemeteries, cultural centers (opera houses, movie houses, ',
  'film centers, etc.), historic structures in current use (e.g., historic markets), ',
  'monuments, statues, museums, religious sites, and shrines.\n',
  'e. Specific temporal duration: The start and end date of an event. The smallest ',
  'possible temporal unit to which an instance of damage can be related is a ',
  'calendar day (24 hours) starting at midnight.'
)

# --- Validation helper --------------------------------------------------------

#' Validate a JSON file or string against a schema
#'
#' @param data Either a file path to JSON or a JSON string.
#' @param schema_path Path to the JSON schema file.
#' @param verbose Logical. Print validation errors if TRUE.
#' @return Logical. TRUE if valid.
validate_against_schema <- function(data, schema_path, verbose = TRUE) {
  result <- jsonvalidate::json_validate(
    json = data,
    schema = schema_path,
    verbose = verbose,
    engine = "ajv"
  )

  if (result) {
    cli::cli_alert_success("Validation passed against {.file {basename(schema_path)}}")
  } else {
    cli::cli_alert_danger("Validation failed against {.file {basename(schema_path)}}")
    if (verbose) {
      errors <- attr(result, "errors")
      if (!is.null(errors)) print(errors)
    }
  }

  result
}

# --- Coding date --------------------------------------------------------------

coding_date <- format(Sys.Date(), "%Y-%m-%d")

# --- Log setup complete -------------------------------------------------------

cli::cli_alert_success("Setup complete. Project root: {.path {project_root}}")
