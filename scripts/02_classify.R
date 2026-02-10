# 02_classify.R
# Step 2: Classify articles as relevant (reporting cultural heritage damage) or not.
#
# Input:  data/01_articles.json
# Output: data/02_articles_filtered.json (same schema, filtered)
#         data/02_classification_log.json (all articles with classification metadata)
#         data/02_review_needed.json (borderline cases for human review)
#
# Set TEST_N in the environment to run on a random subset (e.g., TEST_N=20).

source(file.path("scripts", "00_setup.R"))
library(ellmer)

# --- Configuration ------------------------------------------------------------

# Model: Haiku 3.5 — sufficient for binary classification, cost-effective
classify_model <- "claude-3-5-haiku-20241022"

# Allow running on a subset for testing (set TEST_N env var or edit here)
test_n <- as.integer(Sys.getenv("TEST_N", unset = NA))

# --- 1. Load articles ---------------------------------------------------------

cli::cli_h2("Step 2: Classify articles for cultural heritage damage relevance")

articles <- fromJSON(file.path(paths$data, "01_articles.json"))
cli::cli_alert_info("Loaded {nrow(articles)} articles from Step 1")

# Subsample if TEST_N is set
if (!is.na(test_n) && test_n < nrow(articles)) {
  set.seed(789)
  sample_idx <- sample(nrow(articles), test_n)
  articles <- articles[sample_idx, ]
  cli::cli_alert_warning("TEST MODE: Processing {nrow(articles)} randomly sampled articles")
}

# --- 2. Build system prompt ---------------------------------------------------

# The 18 site types from the schema, for context
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
  "An article is RELEVANT if and only if it describes specific, already-occurred ",
  "physical damage to a named or identifiable physical cultural heritage site in ",
  "Ukraine. The damage must be caused by military action or deliberate destruction ",
  "related to the conflict. The site must be a tangible, physical location (e.g., ",
  "a building, monument, archaeological site, cemetery) — not a cultural practice, ",
  "art form, tradition, or abstract concept.\n\n",
  "Even if the details of the destruction are brief (e.g., a single sentence), ",
  "the article is relevant as long as it identifies a specific site and states ",
  "that physical damage occurred to it.\n\n",
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
  "- Only describes the THREAT or RISK of future damage to heritage sites (e.g., ",
  "curators evacuating art for fear of attack, sites being placed on endangered ",
  "lists, protective measures being taken) without reporting that damage has ",
  "already occurred\n",
  "- Only provides broad, non-specific references to heritage damage across ",
  "Ukraine (e.g., 'hundreds of cultural sites have been damaged', 'museums across ",
  "Ukraine are being destroyed') without naming or identifying at least one specific ",
  "site that was damaged\n",
  "- Cites aggregate damage figures from external sources (e.g., 'UNESCO reports ",
  "230 cultural sites damaged', 'an estimated 210,000 buildings destroyed') without ",
  "reporting on a specific, named damage event at an identifiable site. Statistics ",
  "alone — even authoritative ones — do not constitute a report of a specific ",
  "damage instance\n",
  "- Reports destruction NEAR a cultural heritage site but does not state that ",
  "the site itself was damaged (e.g., a missile hit near a cathedral, fighting ",
  "occurred close to a museum)\n",
  "- Mentions damage to a town, city, or region that contains cultural heritage ",
  "sites but does not specifically report damage to a heritage site itself (e.g., ",
  "'Odesa was bombed' without mentioning damage to a specific heritage site in Odesa)\n",
  "- Is primarily an art exhibition review, artist profile, architecture ",
  "criticism, or cultural commentary piece that only references heritage damage ",
  "as incidental background context. The article must be *reporting on* a damage ",
  "event, not merely referencing one in passing\n",
  "- Describes intentional government decisions to modify, remove, relocate, or ",
  "rename monuments or heritage sites (e.g., decommunization, de-Russification ",
  "policy). Only damage caused by military action or deliberate destruction by an ",
  "adversary qualifies\n",
  "- Mentions damage to a heritage site but does not provide enough information ",
  "to identify which specific site was damaged (e.g., 'a museum was destroyed' ",
  "without naming it, or a garbled transcript referencing an unnamed institution). ",
  "The site must be identifiable by name, description, or location\n\n",
  "KEY PRINCIPLE: The article must report that a specific, identifiable cultural ",
  "heritage site has already suffered physical damage. Threats, risks, proximity, ",
  "aggregate statistics, regional damage, exhibition reviews, government-initiated ",
  "monument changes, and unnamed sites are not sufficient."
)

# --- 3. Set up ellmer chat and type -------------------------------------------

chat <- chat_anthropic(
  system_prompt = system_prompt,
  model = classify_model
)

# Structured output type for classification
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

# --- 4. Build prompts ---------------------------------------------------------

# Use headline + truncated body (first 2000 chars) to keep costs down
prompts <- as.list(paste0(
  "Article ID: ", articles$article_id, "\n",
  "Headline: ", articles$article_headline, "\n\n",
  "Article text (may be truncated):\n",
  str_sub(articles$article_body, 1, 2000)
))

cli::cli_alert_info("Submitting {length(prompts)} articles to Claude ({classify_model}) for classification...")

# --- 5. Run parallel classification -------------------------------------------

results <- parallel_chat_structured(
  chat = chat,
  prompts = prompts,
  type = classification_type,
  convert = FALSE,
  include_cost = TRUE,
  max_active = 10,
  rpm = 500,
  on_error = "continue"
)

# Convert results safely — parallel_chat_structured with convert=FALSE returns
# a list of raw results. We extract fields individually to avoid subscript
# errors on malformed responses.
results_tbl <- tibble(
  reports_damage = map_lgl(results, \(x) tryCatch(x$reports_damage, error = \(e) NA)),
  confidence = map_chr(results, \(x) tryCatch(x$confidence, error = \(e) NA_character_)),
  reasoning = map_chr(results, \(x) tryCatch(x$reasoning, error = \(e) NA_character_))
)

cli::cli_alert_success("Classification complete")

# --- 6. Combine results with article metadata ---------------------------------

# results_tbl is a tibble with reports_damage, confidence, reasoning columns
classification_log <- bind_cols(
  articles |> select(article_id, article_date, article_source, article_headline),
  results_tbl
)

# Report any errors
if (".error" %in% names(classification_log)) {
  n_errors <- sum(!is.na(classification_log$.error))
  if (n_errors > 0) {
    cli::cli_alert_danger("{n_errors} articles failed classification — check classification log")
  }
}

# --- 7. Summary statistics ----------------------------------------------------

cli::cli_h3("Classification Summary")

n_relevant <- sum(classification_log$reports_damage == TRUE, na.rm = TRUE)
n_irrelevant <- sum(classification_log$reports_damage == FALSE, na.rm = TRUE)
n_na <- sum(is.na(classification_log$reports_damage))

cli::cli_alert_info("Relevant: {n_relevant} ({round(n_relevant/nrow(classification_log)*100, 1)}%)")
cli::cli_alert_info("Not relevant: {n_irrelevant} ({round(n_irrelevant/nrow(classification_log)*100, 1)}%)")
if (n_na > 0) cli::cli_alert_warning("Failed/NA: {n_na}")

if ("cost" %in% names(classification_log)) {
  total_cost <- sum(classification_log$cost, na.rm = TRUE)
  cli::cli_alert_info("Total API cost: ${round(total_cost, 4)}")

  # Project cost for full dataset if in test mode
  if (!is.na(test_n)) {
    full_n <- nrow(fromJSON(file.path(paths$data, "01_articles.json")))
    projected <- total_cost * (full_n / nrow(articles))
    cli::cli_alert_info("Projected cost for all {full_n} articles: ${round(projected, 2)}")
  }
}

# Confidence breakdown
conf_counts <- classification_log |>
  count(reports_damage, confidence) |>
  arrange(desc(reports_damage), confidence)

cli::cli_alert_info("Confidence breakdown:")
for (i in seq_len(nrow(conf_counts))) {
  cli::cli_alert("  damage={conf_counts$reports_damage[i]}, confidence={conf_counts$confidence[i]}: {conf_counts$n[i]}")
}

# --- 8. Write outputs ---------------------------------------------------------

# 8a. Full classification log (all articles with classification metadata)
log_path <- file.path(paths$data, "02_classification_log.json")
write(
  toJSON(classification_log, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  log_path
)
cli::cli_alert_success("Wrote classification log for {nrow(classification_log)} articles to {.file {log_path}}")

# 8b. Borderline cases for human review
review_needed <- classification_log |>
  filter(
    reports_damage == TRUE & confidence %in% c("medium", "low") |
    is.na(reports_damage)
  )

if (nrow(review_needed) > 0) {
  review_path <- file.path(paths$data, "02_review_needed.json")
  write(
    toJSON(review_needed, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    review_path
  )
  cli::cli_alert_warning("{nrow(review_needed)} borderline/failed cases written to {.file {review_path}} for human review")
} else {
  cli::cli_alert_success("No borderline cases requiring review")
}

# 8c. Filtered articles (relevant only) — full article data for downstream steps
relevant_ids <- classification_log |>
  filter(reports_damage == TRUE) |>
  pull(article_id)

articles_all <- fromJSON(file.path(paths$data, "01_articles.json"))
articles_filtered <- articles_all |>
  filter(article_id %in% relevant_ids)

filtered_path <- file.path(paths$data, "02_articles_filtered.json")
write(
  toJSON(articles_filtered, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  filtered_path
)
cli::cli_alert_success("Wrote {nrow(articles_filtered)} relevant articles to {.file {filtered_path}}")

# --- 9. Print sample of relevant articles for verification --------------------

cli::cli_h3("Sample relevant articles")
relevant_sample <- classification_log |>
  filter(reports_damage == TRUE) |>
  head(10)

for (i in seq_len(nrow(relevant_sample))) {
  cli::cli_alert_info("{relevant_sample$article_id[i]} | {relevant_sample$article_date[i]} | {str_trunc(relevant_sample$article_headline[i], 70)}")
  cli::cli_alert("  Confidence: {relevant_sample$confidence[i]} | {str_trunc(relevant_sample$reasoning[i], 100)}")
}

# Print sample of irrelevant articles too
cli::cli_h3("Sample irrelevant articles")
irrelevant_sample <- classification_log |>
  filter(reports_damage == FALSE) |>
  head(5)

for (i in seq_len(nrow(irrelevant_sample))) {
  cli::cli_alert_info("{irrelevant_sample$article_id[i]} | {str_trunc(irrelevant_sample$article_headline[i], 70)}")
  cli::cli_alert("  {str_trunc(irrelevant_sample$reasoning[i], 120)}")
}
