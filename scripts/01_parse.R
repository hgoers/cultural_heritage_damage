# 01_parse.R
# Step 1: Parse & clean all LexisNexis DOCX files into structured article data.
#
# Input:  38 DOCX files in data-raw/
# Output: data/01_articles.json (conforming to schema_newspaper.json)

source(file.path("scripts", "00_setup.R"))

library(officer)
library(stringdist)

# --- Custom LexisNexis DOCX parser --------------------------------------------

# lnt_read() misses the first article in each file because its heading style
# ("heading 1") differs from subsequent articles ("Heading 1_N"). We parse
# using officer::read_docx() and split on all heading-1 styles.

parse_lexisnexis_docx <- function(file_path) {
  doc <- read_docx(file_path)
  content <- docx_summary(doc)

  # Identify heading rows (article titles) — match any "heading 1" variant
  heading_mask <- grepl("^[Hh]eading\\s*1", content$style_name)
  heading_indices <- which(heading_mask)

  if (length(heading_indices) == 0) {
    cli::cli_alert_warning("No articles found in {.file {basename(file_path)}}")
    return(tibble(
      Source_File = character(),
      Headline    = character(),
      Newspaper   = character(),
      Date        = as.Date(character()),
      Article     = character()
    ))
  }

  # Define article boundaries: each article runs from its heading to the row

  # before the next heading (or end of file)
  articles <- vector("list", length(heading_indices))

  for (k in seq_along(heading_indices)) {
    start_idx <- heading_indices[k]
    end_idx <- if (k < length(heading_indices)) {
      heading_indices[k + 1] - 1
    } else {
      nrow(content)
    }

    block <- content[start_idx:end_idx, ]
    texts <- block$text

    # Headline is the heading row itself
    headline <- str_trim(texts[1])

    # The lines after the heading, before "Body", contain metadata:
    #   Line 1 after heading: Newspaper name
    #   Then (variable order): secondary source, date, copyright, edition,
    #   section, length, byline — followed by "Body"
    # After "Body": article text paragraphs
    # After article: "Load-Date: ...", "End of Document"
    #
    # The date is NOT always the 2nd non-empty line: some articles (e.g. BBC
    # Monitoring) insert extra lines between newspaper and date. We search for
    # the first metadata line matching a LexisNexis date pattern.

    # Find "Body" marker and "End of Document" / "Load-Date" marker
    body_idx <- which(str_trim(texts) == "Body")
    end_doc_idx <- which(grepl("^\\s*(End of Document|Load-Date:)", texts))

    # Extract metadata lines (between heading and "Body")
    meta_end <- if (length(body_idx) > 0) body_idx[1] - 1 else min(length(texts), 15)
    meta_texts <- texts[2:min(length(texts), meta_end)]
    meta_texts_trimmed <- str_trim(meta_texts)
    non_empty <- which(meta_texts_trimmed != "")

    newspaper <- if (length(non_empty) >= 1) meta_texts_trimmed[non_empty[1]] else NA_character_

    # Regex to detect LexisNexis date lines. Typical formats:
    #   "January 13, 2022 Thursday"
    #   "June 1, 2022 Wednesday 9:33 AM GMT"
    #   "19 January 2022"
    #   "2022-01-19"
    date_pattern <- paste0(
      "^(",
      "(?:January|February|March|April|May|June|July|August|",
      "September|October|November|December)\\s+\\d{1,2},?\\s+\\d{4}",
      "|",
      "\\d{1,2}\\s+(?:January|February|March|April|May|June|July|August|",
      "September|October|November|December)\\s+\\d{4}",
      "|",
      "\\d{4}-\\d{2}-\\d{2}",
      ")"
    )

    # Find the first metadata line that matches a date pattern, then extract
    # just the date portion (stripping weekday names, times, timezones) so
    # lubridate can parse it reliably.
    date_raw <- NA_character_
    for (ml in meta_texts_trimmed[non_empty]) {
      if (grepl(date_pattern, ml, perl = TRUE)) {
        # Extract only the "Month DD, YYYY" / "DD Month YYYY" / "YYYY-MM-DD"
        # portion, discarding trailing weekday/time info that confuses lubridate
        date_raw <- str_extract(ml, date_pattern)
        break
      }
    }

    # Parse the date (compact orders — lubridate handles separators)
    date_parsed <- tryCatch({
      lubridate::parse_date_time(
        date_raw,
        orders = c("Bdy", "dBy", "ymd"),
        quiet = TRUE
      ) |> as.Date()
    }, error = function(e) NA_Date_)

    # Extract article body text (between "Body" and "Load-Date"/"End of Document")
    if (length(body_idx) > 0 && length(end_doc_idx) > 0) {
      body_start <- body_idx[1] + 1
      body_end <- min(end_doc_idx) - 1
      if (body_start <= body_end) {
        body_text <- paste(str_trim(texts[body_start:body_end]), collapse = "\n")
      } else {
        body_text <- NA_character_
      }
    } else if (length(body_idx) > 0) {
      # No end marker found — take everything after Body
      body_text <- paste(str_trim(texts[(body_idx[1] + 1):length(texts)]), collapse = "\n")
    } else {
      # No Body marker — take everything after metadata
      body_text <- paste(str_trim(texts[min(6, length(texts)):length(texts)]), collapse = "\n")
    }

    # Clean up body text
    body_text <- str_trim(body_text)
    if (body_text == "" || is.na(body_text)) body_text <- NA_character_

    articles[[k]] <- tibble(
      Source_File = basename(file_path),
      Headline    = headline,
      Newspaper   = newspaper,
      Date        = date_parsed,
      Article     = body_text
    )
  }

  bind_rows(articles)
}

# --- 1. Read all DOCX files ---------------------------------------------------

cli::cli_h2("Step 1: Parse & clean DOCX files")

docx_files <- list.files(
  paths$data_raw,
  pattern = "\\.docx$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Exclude Office temp/lock files (prefixed with ~$)
docx_files <- docx_files[!grepl("^~\\$", basename(docx_files))]

cli::cli_alert_info("Found {length(docx_files)} DOCX files in {.path {paths$data_raw}}")

# Parse each DOCX with our custom parser
articles_list <- map(docx_files, \(f) {
  cli::cli_alert_info("Parsing {.file {basename(f)}} ...")
  parse_lexisnexis_docx(f)
})

articles_df <- bind_rows(articles_list)
cli::cli_alert_success("Parsed {nrow(articles_df)} articles from {length(docx_files)} files")

# --- 2. Detect and remove near-duplicates -------------------------------------

# lnt_similarity() is too memory-intensive for 8000+ articles.
# Instead, use a two-pass approach:
#   Pass 1: Remove exact duplicates (same headline + date + source)
#   Pass 2: Flag near-duplicate headlines on the same date using string distance

cli::cli_alert_info("Checking for duplicate articles...")

n_before <- nrow(articles_df)

# Pass 1: Exact duplicates (headline + date + source)
articles_df <- articles_df |>
  distinct(Headline, Date, Newspaper, .keep_all = TRUE)

n_exact_dups <- n_before - nrow(articles_df)
if (n_exact_dups > 0) {
  cli::cli_alert_warning("Removed {n_exact_dups} exact duplicate articles (same headline + date + source)")
}

# Pass 2: Near-duplicate headlines on the same date (from split-month files)
# Compare headlines within each date using Jaro-Winkler similarity

near_dup_indices <- c()

articles_by_date <- articles_df |>
  mutate(.row_id = row_number()) |>
  group_by(Date) |>
  group_split()

for (date_group in articles_by_date) {
  if (nrow(date_group) < 2) next

  headlines <- date_group$Headline
  n <- length(headlines)

  # Pairwise Jaro-Winkler distance (0 = identical, 1 = completely different)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      sim <- 1 - stringdist(headlines[i], headlines[j], method = "jw")
      if (sim > 0.95) {
        # Mark the later one as duplicate
        near_dup_indices <- c(near_dup_indices, date_group$.row_id[j])
      }
    }
  }
}

near_dup_indices <- unique(near_dup_indices)

if (length(near_dup_indices) > 0) {
  cli::cli_alert_warning("Removing {length(near_dup_indices)} near-duplicate articles (similar headlines on same date)")
  articles_df <- articles_df |>
    filter(!row_number() %in% near_dup_indices)
}

cli::cli_alert_info("After deduplication: {nrow(articles_df)} articles remain (removed {n_before - nrow(articles_df)} total)")

# --- 3. Map fields to schema -------------------------------------------------

# Handle articles with NA dates: attempt to infer from source filename (MM-YYYY)
articles_df <- articles_df |>
  mutate(
    Date = if_else(
      is.na(Date),
      # Extract month-year from Source_File, use 1st of month as fallback
      as.Date(paste0(
        str_extract(Source_File, "\\d{2}-\\d{4}") |>
          str_replace("(\\d{2})-(\\d{4})", "\\2-\\1"),
        "-01"
      )),
      Date
    )
  )

n_still_na <- sum(is.na(articles_df$Date))
if (n_still_na > 0) {
  cli::cli_alert_warning("{n_still_na} articles still have NA dates after filename inference — dropping")
  articles_df <- articles_df |> filter(!is.na(Date))
} else {
  cli::cli_alert_success("All NA dates resolved from source filenames")
}

articles_clean <- articles_df |>
  # Sort by date then source for consistent ID assignment
  arrange(Date, Newspaper) |>
  mutate(
    article_id       = generate_ids(id_prefixes$article, n()),
    article_date     = format(as.Date(Date), "%Y-%m-%d"),
    article_source   = str_trim(Newspaper),
    article_headline = str_trim(Headline),
    article_body     = str_trim(Article)
  ) |>
  # Select only schema fields
  select(
    article_id,
    article_date,
    article_source,
    article_headline,
    article_body
  )

# Handle any NA bodies (set to null for JSON)
articles_clean <- articles_clean |>
  mutate(
    article_body = if_else(
      is.na(article_body) | article_body == "",
      NA_character_,
      article_body
    )
  )

cli::cli_alert_info("Mapped {nrow(articles_clean)} articles to schema fields")

# --- 4. Summary statistics ----------------------------------------------------

cli::cli_h3("Summary")
cli::cli_alert_info("Total articles: {nrow(articles_clean)}")
cli::cli_alert_info("Date range: {min(articles_clean$article_date, na.rm = TRUE)} to {max(articles_clean$article_date, na.rm = TRUE)}")
cli::cli_alert_info("Unique sources: {n_distinct(articles_clean$article_source)}")

# Top sources
top_sources <- articles_clean |>
  count(article_source, sort = TRUE) |>
  head(10)

cli::cli_alert_info("Top 10 sources:")
for (i in seq_len(nrow(top_sources))) {
  cli::cli_alert("  {top_sources$article_source[i]}: {top_sources$n[i]}")
}

# Articles per year
per_year <- articles_clean |>
  mutate(year = str_sub(article_date, 1, 4)) |>
  count(year)

cli::cli_alert_info("Articles per year:")
for (i in seq_len(nrow(per_year))) {
  cli::cli_alert("  {per_year$year[i]}: {per_year$n[i]}")
}

# --- 5. Validate against schema -----------------------------------------------

cli::cli_h3("Validation")

# Convert to JSON for validation (array of objects)
articles_json <- toJSON(articles_clean, pretty = TRUE, auto_unbox = TRUE, na = "null")

# Validate each record individually against the schema
# (json_validate expects a single object, so validate a sample)
sample_record <- toJSON(
  articles_clean |> slice(1) |> as.list(),
  auto_unbox = TRUE,
  na = "null"
)

validation_result <- validate_against_schema(sample_record, schemas$newspaper)

if (!validation_result) {
  cli::cli_alert_danger("Schema validation failed. Check schema and data alignment.")
} else {
  # Validate a few more records as a spot check
  spot_check_indices <- c(1, ceiling(nrow(articles_clean) / 2), nrow(articles_clean))
  all_valid <- TRUE

  for (idx in spot_check_indices) {
    record_json <- toJSON(
      articles_clean |> slice(idx) |> as.list(),
      auto_unbox = TRUE,
      na = "null"
    )
    if (!json_validate(record_json, schemas$newspaper, engine = "ajv")) {
      cli::cli_alert_danger("Record {idx} failed validation")
      all_valid <- FALSE
    }
  }

  if (all_valid) {
    cli::cli_alert_success("Spot-check validation passed for {length(spot_check_indices)} records")
  }
}

# --- 6. Write output ----------------------------------------------------------

output_path <- file.path(paths$data, "01_articles.json")

write(articles_json, output_path)

cli::cli_alert_success("Wrote {nrow(articles_clean)} articles to {.file {output_path}}")

