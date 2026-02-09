# 02b_retry_failures.R
# Retry classification for articles that failed in the initial run.
# Reads 02_classification_log.json, re-submits failures, and merges results
# back into all Step 2 output files.
#
# Can be run multiple times — each run retries whatever NAs remain.
# Set MAX_RETRIES to control how many retry passes to attempt per invocation.

source(file.path("scripts", "00_setup.R"))
library(ellmer)

# --- Configuration ------------------------------------------------------------

classify_model <- "claude-3-5-haiku-20241022"
MAX_RETRIES <- 3
RETRY_DELAY <- 5 # seconds between retry passes

# --- 1. Load existing classification log --------------------------------------

cli::cli_h2("Step 2b: Retry failed classifications")

log_path <- file.path(paths$data, "02_classification_log.json")
stopifnot("Classification log not found — run 02_classify.R first" = file.exists(log_path))

classification_log <- fromJSON(log_path)
n_total <- nrow(classification_log)

failed_ids <- classification_log |>
  filter(is.na(reports_damage)) |>
  pull(article_id)

cli::cli_alert_info("Loaded classification log: {n_total} articles, {length(failed_ids)} failures to retry")

if (length(failed_ids) == 0) {
  cli::cli_alert_success("No failures to retry — all articles have classifications")
  quit(save = "no", status = 0)
}

# --- 2. Load full article data for failed IDs ---------------------------------

articles_all <- fromJSON(file.path(paths$data, "01_articles.json"))
articles_retry <- articles_all |>
  filter(article_id %in% failed_ids)

cli::cli_alert_info("Loaded {nrow(articles_retry)} articles to retry")

# --- 3. Set up classifier (same prompt as 02_classify.R) ----------------------

site_types_ref <- paste(
  "1 = place of worship; 2 = library; 3 = museum; 4 = archive;",
  "5 = monument; 6 = statue; 7 = sculpture; 8 = place of burial;",
  "9 = archaeological site; 10 = garden; 11 = opera/theatre;",
  "12 = house of culture; 13 = gallery; 14 = zoo;",
  "15 = monastery/seminary/convent; 16 = memorial;",
  "17 = art/music school; 18 = other"
)

system_prompt <- paste0(
  "You are a research assistant classifying newspaper articles for a dataset ",
  "on damage to cultural heritage sites in Ukraine during the Russia-Ukraine war ",
  "(2022-present).\n\n",
  "## Definition of a damage instance\n\n",
  damage_definition, "\n\n",
  "## Site types considered cultural heritage\n\n",
  site_types_ref, "\n\n",
  "## Your task\n\n",
  "For each article, determine whether it reports **at least one instance** of ",
  "PHYSICAL damage to a PHYSICAL cultural heritage site in Ukraine caused by the ",
  "Russia-Ukraine war.\n\n",
  "An article is RELEVANT if it describes specific physical damage (past or ",
  "ongoing) to a named or identifiable physical cultural heritage site in Ukraine. ",
  "The damage must be caused by military action or deliberate destruction related ",
  "to the conflict. The site must be a tangible, physical location (e.g., a ",
  "building, monument, archaeological site, cemetery) — not a cultural practice, ",
  "art form, tradition, or abstract concept.\n\n",
  "An article is NOT RELEVANT if it:\n",
  "- Only discusses heritage sites without reporting physical damage\n",
  "- Reports damage outside Ukraine\n",
  "- Reports damage unrelated to the conflict (e.g., natural disasters, accidents)\n",
  "- Only mentions heritage protection efforts, policy, or funding without ",
  "describing a specific damage event\n",
  "- Is about cultural events, exhibitions, or commemorations with no damage reported\n",
  "- Reports only civilian infrastructure damage (hospitals, schools, residential ",
  "buildings) without a cultural heritage component\n",
  "- Discusses damage to cultural life, artistic practice, cultural identity, or ",
  "intangible cultural heritage rather than damage to a physical site\n",
  "- Only provides aggregate statistics of damaged sites (e.g., 'over 1,000 sites ",
  "damaged') without naming or identifying any specific site\n\n",
  "Be inclusive: if the article mentions physical damage to a heritage site even ",
  "briefly among other topics, classify it as relevant."
)

chat <- chat_anthropic(
  system_prompt = system_prompt,
  model = classify_model
)

classification_type <- type_object(
  reports_damage = type_boolean(
    "TRUE if the article reports at least one instance of damage to a cultural heritage site in Ukraine from the Russia-Ukraine war. FALSE otherwise."
  ),
  confidence = type_enum(
    "Your confidence level in this classification.",
    values = c("high", "medium", "low")
  ),
  reasoning = type_string(
    "Brief explanation (1-3 sentences) of why the article is or is not relevant. If relevant, mention the heritage site(s) and type of damage. If not relevant, explain why it does not meet the criteria."
  )
)

# --- 4. Retry loop ------------------------------------------------------------

remaining <- articles_retry

for (attempt in seq_len(MAX_RETRIES)) {
  n_remaining <- nrow(remaining)
  if (n_remaining == 0) break

  cli::cli_h3("Retry attempt {attempt}/{MAX_RETRIES}: {n_remaining} articles")

  if (attempt > 1) {
    cli::cli_alert_info("Waiting {RETRY_DELAY}s before retry...")
    Sys.sleep(RETRY_DELAY)
  }

  prompts <- as.list(paste0(
    "Article ID: ", remaining$article_id, "\n",
    "Headline: ", remaining$article_headline, "\n\n",
    "Article text (may be truncated):\n",
    str_sub(remaining$article_body, 1, 2000)
  ))

  retry_results <- parallel_chat_structured(
    chat = chat,
    prompts = prompts,
    type = classification_type,
    convert = FALSE,
    max_active = 5,  # lower concurrency for retries
    rpm = 300,
    on_error = "continue"
  )

  retry_tbl <- tibble(
    article_id = remaining$article_id,
    reports_damage = map_lgl(retry_results, \(x) tryCatch(x$reports_damage, error = \(e) NA)),
    confidence = map_chr(retry_results, \(x) tryCatch(x$confidence, error = \(e) NA_character_)),
    reasoning = map_chr(retry_results, \(x) tryCatch(x$reasoning, error = \(e) NA_character_))
  )

  n_succeeded <- sum(!is.na(retry_tbl$reports_damage))
  n_still_failed <- sum(is.na(retry_tbl$reports_damage))
  cli::cli_alert_info("Attempt {attempt}: {n_succeeded} succeeded, {n_still_failed} still failed")

  # Merge successful retries into classification_log
  succeeded <- retry_tbl |> filter(!is.na(reports_damage))

  if (nrow(succeeded) > 0) {
    for (i in seq_len(nrow(succeeded))) {
      idx <- which(classification_log$article_id == succeeded$article_id[i])
      classification_log$reports_damage[idx] <- succeeded$reports_damage[i]
      classification_log$confidence[idx] <- succeeded$confidence[i]
      classification_log$reasoning[idx] <- succeeded$reasoning[i]
    }
  }

  # Update remaining to only those still failed
  still_failed_ids <- retry_tbl |>
    filter(is.na(reports_damage)) |>
    pull(article_id)

  remaining <- remaining |> filter(article_id %in% still_failed_ids)
}

# --- 5. Summary ---------------------------------------------------------------

cli::cli_h3("Retry Summary")

n_originally_failed <- length(failed_ids)
n_still_na <- sum(is.na(classification_log$reports_damage))
n_recovered <- n_originally_failed - n_still_na

cli::cli_alert_success("Recovered {n_recovered} / {n_originally_failed} previously failed articles")
if (n_still_na > 0) {
  cli::cli_alert_warning("{n_still_na} articles still failed after {MAX_RETRIES} retry attempts")
}

n_relevant <- sum(classification_log$reports_damage == TRUE, na.rm = TRUE)
n_irrelevant <- sum(classification_log$reports_damage == FALSE, na.rm = TRUE)

cli::cli_alert_info("Updated totals — Relevant: {n_relevant}, Not relevant: {n_irrelevant}, Failed: {n_still_na}")

# --- 6. Update all output files -----------------------------------------------

cli::cli_h3("Updating output files")

# 6a. Classification log
write(
  toJSON(classification_log, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  log_path
)
cli::cli_alert_success("Updated {.file {log_path}}")

# 6b. Filtered articles (relevant only)
relevant_ids <- classification_log |>
  filter(reports_damage == TRUE) |>
  pull(article_id)

articles_filtered <- articles_all |>
  filter(article_id %in% relevant_ids)

filtered_path <- file.path(paths$data, "02_articles_filtered.json")
write(
  toJSON(articles_filtered, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  filtered_path
)
cli::cli_alert_success("Updated {.file {filtered_path}} — {nrow(articles_filtered)} relevant articles")

# 6c. Review needed (medium/low confidence + remaining failures)
review_needed <- classification_log |>
  filter(
    (reports_damage == TRUE & confidence %in% c("medium", "low")) |
    is.na(reports_damage)
  )

review_path <- file.path(paths$data, "02_review_needed.json")
if (nrow(review_needed) > 0) {
  write(
    toJSON(review_needed, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    review_path
  )
  cli::cli_alert_success("Updated {.file {review_path}} — {nrow(review_needed)} cases")
} else {
  # Remove review file if no cases remain
  if (file.exists(review_path)) file.remove(review_path)
  cli::cli_alert_success("No borderline/failed cases remain — removed review file")
}

cli::cli_alert_success("All Step 2 outputs updated")
