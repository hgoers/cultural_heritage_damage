# 03_extract.R
# Step 3: Extract structured damage event details from each relevant article.
#
# Input:  data/02_articles_filtered.json
# Output: data/03_article_damage_events.json (conforming to schema_newspaper_damage_event.json)
#         data/03_extraction_log.json (per-article extraction metadata)
#
# Geocoding is deferred to 05_geocode.R — this step extracts a location_text
# field that the geocoder will resolve to coordinates.

source(file.path("scripts", "00_setup.R"))
library(ellmer)

# --- Configuration ------------------------------------------------------------

extract_model <- "claude-sonnet-4-20250514"

# --- 1. Load filtered articles ------------------------------------------------

cli::cli_h2("Step 3: Extract damage event details from relevant articles")

articles <- fromJSON(file.path(paths$data, "02_articles_filtered.json"))
cli::cli_alert_info("Loaded {nrow(articles)} relevant articles from Step 2")

if (nrow(articles) == 0) {
  cli::cli_alert_warning("No relevant articles to process. Exiting Step 3.")
  quit(save = "no", status = 0)
}

# --- 2. Build system prompt ---------------------------------------------------

site_types_codebook <- paste(
  "Site type codes:",
  "1 = place of worship (sub-types: 1=mosque, 2=church, 3=synagogue, 4=cathedral, 5=temple, 6=chapel)",
  "2 = library",
  "3 = museum",
  "4 = archive",
  "5 = monument",
  "6 = statue",
  "7 = sculpture",
  "8 = place of burial (sub-types: 1=shrine, 2=mausoleum, 3=tomb, 4=grave)",
  "9 = archaeological site",
  "10 = garden",
  "11 = opera/theatre",
  "12 = house of culture",
  "13 = gallery",
  "14 = zoo",
  "15 = monastery/seminary/convent",
  "16 = memorial",
  "17 = art/music school",
  "18 = other (you MUST provide other_site_type description)",
  sep = "\n"
)

damage_types_codebook <- paste(
  "Damage type codes:",
  "1 = fire",
  "2 = explosion (bomb, explosive detonation)",
  "3 = shelling (artillery fire, bullet)",
  "4 = airstrike",
  "5 = rocket fire",
  "6 = looting/stealing",
  "7 = deface/desecrate/vandalize",
  "8 = occupied/barricaded",
  "9 = other (you MUST provide other_damage_type description)",
  "If multiple damage types apply, separate with pipe: e.g., '3|4' for shelling + airstrike.",
  sep = "\n"
)

temp_prec_codebook <- paste(
  "Temporal precision codes (temp_prec):",
  "0 = not applicable (summary event spanning extended period)",
  "1 = exact day known",
  "2 = time period 2-6 days",
  "3 = week only",
  "4 = month only",
  "5 = year only",
  sep = "\n"
)

geo_prec_codebook <- paste(
  "Geographic precision codes (geo_prec) — based on info available in article:",
  "1 = exact site identified (named building, monument, etc.)",
  "2 = village/city level (site named but precise location within city unclear)",
  "3 = second-order admin division (district/raion)",
  "4 = first-order admin division (oblast/region)",
  "5 = section of country",
  "6 = whole country",
  sep = "\n"
)

verification_codebook <- paste(
  "Verification quality codes:",
  "1 = minimal sourcing (single unnamed source, social media)",
  "2 = moderate sourcing (named official or reporter on scene)",
  "3 = strong sourcing (multiple independent sources)",
  "4 = investigative/in-depth verification (detailed investigation, satellite imagery, etc.)",
  sep = "\n"
)

system_prompt <- paste0(
  "You are a research assistant extracting structured data about damage to ",
  "cultural heritage sites in Ukraine during the Russia-Ukraine war (2022-present) ",
  "from newspaper articles.\n\n",
  "## Definition of a damage instance\n\n",
  damage_definition, "\n\n",
  "## Codebooks\n\n",
  site_types_codebook, "\n\n",
  damage_types_codebook, "\n\n",
  temp_prec_codebook, "\n\n",
  geo_prec_codebook, "\n\n",
  verification_codebook, "\n\n",
  "## Your task\n\n",
  "For each article, extract ALL instances of damage to cultural heritage sites ",
  "reported in the article. A single article may describe damage to MULTIPLE sites ",
  "or multiple damage events at the same site — return one record per distinct ",
  "damage event.\n\n",
  "For each damage event, extract:\n",
  "- site_name: The name of the heritage site as stated in the article\n",
  "- site_type: Integer code from the codebook above\n",
  "- place_of_worship: Integer sub-type if site_type=1, otherwise null\n",
  "- place_of_burial: Integer sub-type if site_type=8, otherwise null\n",
  "- other_site_type: Description if site_type=18, otherwise null\n",
  "- location_text: The city/town/village and region where the site is located, ",
  "as stated in the article (e.g., 'Mariupol, Donetsk region' or 'Odesa')\n",
  "- country: Always 'Ukraine' unless the article explicitly states otherwise\n",
  "- year: The calendar year the damage occurred (may differ from article date)\n",
  "- event_date_start: Start date in YYYY-MM-DD format, or null if unknown\n",
  "- event_date_end: End date in YYYY-MM-DD format, or null if unknown. ",
  "Use the same date as event_date_start if the event occurred on a single day\n",
  "- temp_prec: Temporal precision code\n",
  "- geo_prec: Geographic precision code\n",
  "- initiator: The party that caused the damage, as named in the article\n",
  "- group_at_site: Any group present at the site during the event, or null\n",
  "- damage_type: String code(s) from the codebook, pipe-delimited if multiple\n",
  "- other_damage_type: Description if damage_type includes '9', otherwise null\n",
  "- site_dmg_smry: Brief summary of the damage (e.g., 'partially destroyed by shelling')\n",
  "- site_dmg_full: The full sentence(s) from the article describing the damage — ",
  "copy the exact text from the article\n",
  "- article_excerpt: The broader passage (1-3 sentences) providing context for ",
  "the damage event — copy exact text\n",
  "- reported_source: Who provided the information (e.g., 'local official', ",
  "'UNESCO', 'eyewitness'), or null if not stated\n",
  "- verification_quality: Integer code from the codebook\n",
  "- notes: Any additional observations, or null\n\n",
  "## Important rules\n\n",
  "- Extract ONLY events where physical damage has already occurred to a specific, ",
  "named cultural heritage site. Do NOT extract threats, risks, protection efforts, ",
  "or aggregate statistics.\n",
  "- If the article reports the same event at the same site multiple times (e.g., ",
  "in a headline and the body), return only ONE record for that event.\n",
  "- If the article reports damage to the same site on different occasions, return ",
  "SEPARATE records.\n",
  "- For event dates: if the article says 'last week' or 'on Monday', calculate ",
  "the actual date from the article publication date provided.\n",
  "- Copy text exactly from the article for site_dmg_full and article_excerpt. ",
  "Do not paraphrase.\n",
  "- If you cannot determine a field, use null rather than guessing.\n",
  "- If the article was classified as relevant but you find no extractable damage ",
  "events upon close reading, return an empty array."
)

# --- 3. Define ellmer structured output type ----------------------------------

damage_event_type <- type_object(
  site_name = type_string("Name of the cultural heritage site as mentioned in the article"),
  site_type = type_integer("Site type code (1-18) from the codebook"),
  place_of_worship = type_enum(
    "Sub-type code if site_type=1 (place of worship), otherwise null",
    values = c("1", "2", "3", "4", "5", "6", "null")
  ),
  place_of_burial = type_enum(
    "Sub-type code if site_type=8 (place of burial), otherwise null",
    values = c("1", "2", "3", "4", "null")
  ),
  other_site_type = type_string(
    "Description if site_type=18 (other), otherwise null. Can be null."
  ),
  location_text = type_string(
    "City/town and region where the site is located, as stated in the article"
  ),
  country = type_string("Country where the site is located, typically 'Ukraine'"),
  year = type_integer("Calendar year the damage occurred"),
  event_date_start = type_string(
    "Start date of the damage event in YYYY-MM-DD format, or 'null' if unknown"
  ),
  event_date_end = type_string(
    "End date of the damage event in YYYY-MM-DD format, or 'null' if unknown"
  ),
  temp_prec = type_integer("Temporal precision code (0-5) from the codebook"),
  geo_prec = type_integer("Geographic precision code (1-6) from the codebook"),
  initiator = type_string("The party that caused the damage, or 'null' if unknown"),
  group_at_site = type_string(
    "Group present at the site during the event, or 'null' if unknown/not mentioned"
  ),
  damage_type = type_string(
    "Damage type code(s), pipe-delimited if multiple (e.g., '3' or '3|4'), or 'null'"
  ),
  other_damage_type = type_string(
    "Description if damage_type includes '9', otherwise 'null'"
  ),
  site_dmg_smry = type_string("Brief summary of damage (e.g., 'partially destroyed by shelling')"),
  site_dmg_full = type_string("Full sentence(s) from article describing damage — exact text"),
  article_excerpt = type_string("Broader passage (1-3 sentences) providing context — exact text"),
  reported_source = type_string("Source cited for the information, or 'null' if not stated"),
  verification_quality = type_integer("Verification quality code (1-4) from the codebook"),
  notes = type_string("Additional observations, or 'null'")
)

# Wrap in an array — each article may yield 0 or more events
extraction_type <- type_array(
  "Array of damage events extracted from this article. Return an empty array if no extractable events found.",
  items = damage_event_type
)

# --- 4. Build prompts ---------------------------------------------------------

# Include full article body for extraction (more detail needed than classification)
prompts <- as.list(paste0(
  "Article ID: ", articles$article_id, "\n",
  "Publication date: ", articles$article_date, "\n",
  "Source: ", articles$article_source, "\n",
  "Headline: ", articles$article_headline, "\n\n",
  "Full article text:\n",
  articles$article_body
))

cli::cli_alert_info(
  "Submitting {length(prompts)} articles to Claude ({extract_model}) for extraction..."
)

# --- 5. Run parallel extraction -----------------------------------------------

chat <- chat_anthropic(
  system_prompt = system_prompt,
  model = extract_model
)

results <- parallel_chat_structured(
  chat = chat,
  prompts = prompts,
  type = extraction_type,
  convert = FALSE,
  include_cost = TRUE,
  max_active = 5,
  rpm = 200,
  on_error = "continue"
)

cli::cli_alert_success("Extraction complete")

# --- 6. Process results into flat table ---------------------------------------

# Each result is a list of damage events (or NULL/error)
all_events <- list()
extraction_log <- tibble(
  article_id = articles$article_id,
  n_events = integer(nrow(articles)),
  status = character(nrow(articles))
)

for (i in seq_along(results)) {
  res <- results[[i]]
  aid <- articles$article_id[i]
  adate <- articles$article_date[i]
  asource <- articles$article_source[i]
  aheadline <- articles$article_headline[i]

  # Handle errors/NULLs
  events <- tryCatch({
    if (is.null(res) || length(res) == 0) {
      list()
    } else {
      # res should be a list of event objects
      res
    }
  }, error = function(e) {
    list()
  })

  extraction_log$n_events[i] <- length(events)
  extraction_log$status[i] <- if (length(events) > 0) "ok" else if (is.null(res)) "error" else "no_events"

  for (evt in events) {
    # Clean up "null" strings to actual NULLs
    clean_field <- function(val, as_type = "string") {
      if (is.null(val) || identical(val, "null") || identical(val, "NULL")) {
        return(NA)
      }
      if (as_type == "integer") {
        return(as.integer(val))
      }
      as.character(val)
    }

    event_row <- tibble(
      article_id       = aid,
      article_date     = adate,
      article_source   = asource,
      article_headline = aheadline,
      site_name        = clean_field(evt$site_name),
      site_type        = clean_field(evt$site_type, "integer"),
      place_of_worship = clean_field(evt$place_of_worship, "integer"),
      place_of_burial  = clean_field(evt$place_of_burial, "integer"),
      other_site_type  = clean_field(evt$other_site_type),
      location_text    = clean_field(evt$location_text),
      country          = {
        val <- clean_field(evt$country)
        if (is.na(val)) "Ukraine" else val
      },
      year             = clean_field(evt$year, "integer"),
      event_date_start = clean_field(evt$event_date_start),
      event_date_end   = clean_field(evt$event_date_end),
      temp_prec        = clean_field(evt$temp_prec, "integer"),
      geo_prec         = clean_field(evt$geo_prec, "integer"),
      initiator        = clean_field(evt$initiator),
      group_at_site    = clean_field(evt$group_at_site),
      damage_type      = clean_field(evt$damage_type),
      other_damage_type = clean_field(evt$other_damage_type),
      site_dmg_smry    = clean_field(evt$site_dmg_smry),
      site_dmg_full    = clean_field(evt$site_dmg_full),
      article_excerpt  = clean_field(evt$article_excerpt),
      reported_source  = clean_field(evt$reported_source),
      verification_quality = clean_field(evt$verification_quality, "integer"),
      notes            = clean_field(evt$notes),
      coding_date      = coding_date
    )

    all_events <- c(all_events, list(event_row))
  }
}

# Combine into a single tibble
if (length(all_events) > 0) {
  events_df <- bind_rows(all_events)
} else {
  cli::cli_alert_warning("No damage events extracted from any article.")
  events_df <- tibble()
}

# --- 7. Generate record IDs --------------------------------------------------

if (nrow(events_df) > 0) {
  events_df <- events_df |>
    mutate(record_id = generate_ids("REC", n())) |>
    select(record_id, everything())
}

# --- 8. Summary statistics ----------------------------------------------------

cli::cli_h3("Extraction Summary")
cli::cli_alert_info("Total damage events extracted: {nrow(events_df)}")
cli::cli_alert_info("Articles with events: {sum(extraction_log$n_events > 0)}")
cli::cli_alert_info("Articles with no events: {sum(extraction_log$n_events == 0)}")
cli::cli_alert_info("Articles with errors: {sum(extraction_log$status == 'error')}")

if (nrow(events_df) > 0) {
  cli::cli_alert_info("Events per article: min={min(extraction_log$n_events[extraction_log$n_events > 0])}, max={max(extraction_log$n_events)}, median={median(extraction_log$n_events[extraction_log$n_events > 0])}")

  # Site type breakdown
  site_type_labels <- c(
    "1" = "place of worship", "2" = "library", "3" = "museum", "4" = "archive",
    "5" = "monument", "6" = "statue", "7" = "sculpture", "8" = "place of burial",
    "9" = "archaeological site", "10" = "garden", "11" = "opera/theatre",
    "12" = "house of culture", "13" = "gallery", "14" = "zoo",
    "15" = "monastery/seminary/convent", "16" = "memorial",
    "17" = "art/music school", "18" = "other"
  )

  cli::cli_alert_info("Site type breakdown:")
  st_counts <- events_df |> count(site_type) |> arrange(desc(n))
  for (j in seq_len(nrow(st_counts))) {
    label <- site_type_labels[as.character(st_counts$site_type[j])]
    cli::cli_alert("  {label} ({st_counts$site_type[j]}): {st_counts$n[j]}")
  }
}

# --- 9. Print sample events --------------------------------------------------

if (nrow(events_df) > 0) {
  cli::cli_h3("Sample extracted events")
  sample_events <- events_df |> head(10)
  for (j in seq_len(nrow(sample_events))) {
    e <- sample_events[j, ]
    cli::cli_alert_info(
      "{e$record_id} | {e$site_name} ({e$location_text}) | {e$site_dmg_smry}"
    )
    cli::cli_alert(
      "  Article: {e$article_id} | Type: {e$site_type} | Damage: {e$damage_type} | Year: {e$year}"
    )
  }
}

# --- 10. Write outputs --------------------------------------------------------

# 10a. Main output: article-damage events
output_path <- file.path(paths$data, "03_article_damage_events.json")

if (nrow(events_df) > 0) {
  # Convert to list-of-lists for JSON, handling NAs as nulls
  events_json <- toJSON(events_df, pretty = TRUE, auto_unbox = TRUE, na = "null")
  write(events_json, output_path)
  cli::cli_alert_success("Wrote {nrow(events_df)} damage events to {.file {output_path}}")
} else {
  write("[]", output_path)
  cli::cli_alert_warning("Wrote empty array to {.file {output_path}}")
}

# 10b. Extraction log
log_path <- file.path(paths$data, "03_extraction_log.json")
write(
  toJSON(extraction_log, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  log_path
)
cli::cli_alert_success("Wrote extraction log to {.file {log_path}}")

# 10c. Validate against schema (skip coordinates — those come from Step 5)
cli::cli_h3("Schema validation")
cli::cli_alert_info("Note: event_latitude/event_longitude will be null until Step 5 (geocoding)")

if (nrow(events_df) > 0) {
  # Validate each record individually
  n_valid <- 0
  n_invalid <- 0

  for (j in seq_len(nrow(events_df))) {
    record_json <- toJSON(
      as.list(events_df[j, ]),
      pretty = FALSE,
      auto_unbox = TRUE,
      na = "null"
    )
    valid <- tryCatch({
      jsonvalidate::json_validate(
        json = record_json,
        schema = schemas$newspaper_damage_event,
        verbose = FALSE,
        engine = "ajv"
      )
    }, error = function(e) FALSE)

    if (valid) n_valid <- n_valid + 1 else n_invalid <- n_invalid + 1
  }

  cli::cli_alert_info("Schema validation: {n_valid} valid, {n_invalid} invalid out of {nrow(events_df)} records")
  if (n_invalid > 0) {
    cli::cli_alert_warning("Run detailed validation to inspect invalid records")
  }
}
