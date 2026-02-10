# 04_deduplicate.R
# Step 4: Identify unique damage events by merging duplicate reports.
#
# Multiple newspaper articles may report on the same physical damage event.
# This script clusters article-damage records that describe the same event,
# uses Claude to adjudicate ambiguous clusters, and merges them into unique
# damage events conforming to schema_damage_event.json.
#
# Input:  data/03_article_damage_events.json (geocoded)
# Output: data/04_damage_events.json (conforming to schema_damage_event.json)
#         data/04_dedup_log.json (clustering decisions for auditability)
#         data/04_review_needed.json (events with conflicts for human review)

source(file.path("scripts", "00_setup.R"))
library(stringdist)
library(ellmer)

cli::cli_h2("Step 4: Identify unique damage events")

# ==============================================================================
# 1. Load article-damage-event records
# ==============================================================================

records <- fromJSON(
  file.path(paths$data, "03_article_damage_events.json")
) |> as_tibble()

cli::cli_alert_info("Loaded {nrow(records)} article-damage-event records")

if (nrow(records) == 0) {
  cli::cli_alert_warning("No records to deduplicate. Exiting.")
  quit(save = "no", status = 0)
}

# ==============================================================================
# 2. Normalise site names for fuzzy matching
# ==============================================================================

normalise_site_name <- function(name) {
  name |>
    tolower() |>
    str_remove_all("^the\\s+") |>
    str_remove_all("\\s+(museum|of|in|at|the|and|for|national|city|local|history)") |>
    str_replace_all("[^a-z0-9]", "") |>
    str_trim()
}

records <- records |>
  mutate(
    site_name_norm = normalise_site_name(site_name),
    location_norm  = tolower(str_trim(location_text))
  )

# ==============================================================================
# 3. Identify candidate duplicate clusters
# ==============================================================================
#
# Strategy: two records are candidate duplicates if:
#   (a) Same year, AND
#   (b) site_name Jaro-Winkler similarity >= 0.75, OR
#       same location_text AND site_type match
#
# We build a pairwise similarity matrix and use single-linkage clustering.

n <- nrow(records)

if (n == 1) {
  # Only one record — skip clustering
  records$cluster_id <- 1L
} else {
  # Compute pairwise Jaro-Winkler similarity on normalised site names
  sim_matrix <- stringdistmatrix(
    records$site_name_norm,
    records$site_name_norm,
    method = "jw"
  )
  # Convert distance to similarity
  sim_matrix <- 1 - as.matrix(sim_matrix)

  # Build adjacency: pair (i,j) are candidates if criteria met
  cluster_id <- rep(NA_integer_, n)
  next_cluster <- 1L

  # Union-Find helpers
  parent <- seq_len(n)

  find_root <- function(i) {
    while (parent[i] != i) {
      parent[i] <<- parent[parent[i]]
      i <- parent[i]
    }
    i
  }

  union_nodes <- function(i, j) {
    ri <- find_root(i)
    rj <- find_root(j)
    if (ri != rj) parent[ri] <<- rj
  }

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      # Must be same year
      if (records$year[i] != records$year[j]) next

      # Check name similarity
      name_sim <- sim_matrix[i, j]

      # Check location match
      same_location <- records$location_norm[i] == records$location_norm[j]
      same_site_type <- records$site_type[i] == records$site_type[j]

      is_candidate <- (name_sim >= 0.75) ||
        (same_location && same_site_type)

      if (is_candidate) {
        union_nodes(i, j)
      }
    }
  }

  # Extract clusters
  records$cluster_id <- vapply(seq_len(n), find_root, integer(1))
  # Re-number clusters sequentially
  cluster_map <- tibble(
    old = unique(records$cluster_id)
  ) |> mutate(new = row_number())
  records$cluster_id <- cluster_map$new[match(records$cluster_id, cluster_map$old)]
}

n_clusters <- n_distinct(records$cluster_id)
multi_clusters <- records |>
  count(cluster_id) |>
  filter(n > 1)

cli::cli_alert_info(
  "{n_clusters} initial clusters ({nrow(multi_clusters)} with multiple records)"
)

# Print cluster composition
for (cid in sort(unique(records$cluster_id))) {
  members <- records |> filter(cluster_id == cid)
  if (nrow(members) > 1) {
    cli::cli_alert_info("Cluster {cid} ({nrow(members)} records):")
    for (k in seq_len(nrow(members))) {
      cli::cli_alert(
        "  {members$record_id[k]} | {members$site_name[k]} | {members$location_text[k]} | {members$article_id[k]}"
      )
    }
  }
}

# ==============================================================================
# 4. Claude adjudication for multi-record clusters
# ==============================================================================
#
# For clusters with >1 record, ask Claude whether they truly describe the
# same physical damage event.

dedup_log <- list()

if (nrow(multi_clusters) > 0) {
  cli::cli_h3("Claude adjudication for {nrow(multi_clusters)} multi-record clusters")

  adjudication_model <- "claude-sonnet-4-20250514"

  system_prompt <- paste0(
    "You are a research assistant helping deduplicate records of damage to ",
    "cultural heritage sites during the Russia-Ukraine war.\n\n",
    "DEFINITION OF A DAMAGE EVENT:\n", damage_definition, "\n\n",
    "Your task: given a cluster of article-damage records, determine whether ",
    "they describe the SAME physical damage event at the SAME site.\n\n",
    "Two records describe the same event if:\n",
    "- They refer to the same physical site (even with different name spellings)\n",
    "- The damage occurred at approximately the same time\n",
    "- The type of damage is consistent\n\n",
    "Two records describe DIFFERENT events if:\n",
    "- They refer to different physical sites (e.g., a monument vs a museum even if in the same city)\n",
    "- The damage happened at clearly different times\n",
    "- The damage types are fundamentally different (e.g., shelling vs looting at different times)\n\n",
    "IMPORTANT: A single article may report on multiple distinct damage events ",
    "(e.g., a monument damaged AND a museum looted in the same city). These are ",
    "SEPARATE events even if from the same article.\n\n",
    "Return your decision as a JSON object."
  )

  for (cid in multi_clusters$cluster_id) {
    members <- records |> filter(cluster_id == cid)

    # Build description of each record for Claude
    record_descriptions <- map_chr(seq_len(nrow(members)), function(k) {
      r <- members[k, ]
      paste0(
        "Record ", r$record_id, ":\n",
        "  Site name: ", r$site_name, "\n",
        "  Site type: ", r$site_type, "\n",
        "  Location: ", r$location_text, "\n",
        "  Year: ", r$year, "\n",
        "  Date start: ", if (is.na(r$event_date_start)) "unknown" else r$event_date_start, "\n",
        "  Date end: ", if (is.na(r$event_date_end)) "unknown" else r$event_date_end, "\n",
        "  Damage type: ", if (is.na(r$damage_type)) "unknown" else r$damage_type, "\n",
        "  Damage summary: ", if (is.na(r$site_dmg_smry)) "unknown" else r$site_dmg_smry, "\n",
        "  Full description: ", if (is.na(r$site_dmg_full)) "unknown" else r$site_dmg_full, "\n",
        "  From article: ", r$article_headline, " (", r$article_source, ", ", r$article_date, ")"
      )
    })

    user_prompt <- paste0(
      "Here are ", nrow(members), " article-damage records that may describe ",
      "the same damage event. Analyse whether they refer to the SAME physical ",
      "damage event:\n\n",
      paste(record_descriptions, collapse = "\n\n---\n\n"),
      "\n\nDo ALL of these records describe the same physical damage event at the same site?"
    )

    cli::cli_alert("Adjudicating cluster {cid} ({nrow(members)} records)...")

    result <- tryCatch({
      chat <- chat_anthropic(
        model = adjudication_model,
        system_prompt = system_prompt
      )

      response <- chat$chat_structured(
        user_prompt,
        type = type_object(
          same_event = type_boolean(
            "TRUE if ALL records describe the same physical damage event at the same site"
          ),
          reasoning = type_string(
            "Brief explanation of why these are/aren't the same event"
          ),
          sub_groups = type_array(
            "If same_event is FALSE, group record IDs that DO belong together. Each sub-array is a group of record_ids that share the same event. Singleton groups are fine.",
            items = type_array(items = type_string())
          )
        )
      )

      response
    }, error = function(e) {
      cli::cli_alert_danger("  Adjudication failed: {e$message}")
      list(same_event = FALSE, reasoning = paste("ERROR:", e$message), sub_groups = list())
    })

    log_entry <- list(
      cluster_id = cid,
      record_ids = members$record_id,
      same_event = result$same_event,
      reasoning = result$reasoning,
      sub_groups = result$sub_groups
    )
    dedup_log <- c(dedup_log, list(log_entry))

    if (isTRUE(result$same_event)) {
      cli::cli_alert_success(
        "  Cluster {cid}: SAME EVENT — {result$reasoning}"
      )
      # Keep all members in the same cluster (already are)
    } else {
      cli::cli_alert_warning(
        "  Cluster {cid}: DIFFERENT EVENTS — {result$reasoning}"
      )

      # Split cluster based on sub_groups
      if (length(result$sub_groups) > 0) {
        # Assign new cluster IDs for sub-groups
        max_cluster <- max(records$cluster_id)
        for (sg_idx in seq_along(result$sub_groups)) {
          sg_ids <- unlist(result$sub_groups[[sg_idx]])
          if (sg_idx == 1) {
            # Keep original cluster_id for first sub-group
            next
          }
          new_cid <- max_cluster + sg_idx - 1
          records$cluster_id[records$record_id %in% sg_ids] <- new_cid
        }

        # Handle records not mentioned in any sub_group — each becomes its own cluster
        all_sg_ids <- unlist(result$sub_groups)
        orphans <- members$record_id[!members$record_id %in% all_sg_ids]
        if (length(orphans) > 0) {
          max_cluster <- max(records$cluster_id)
          for (oi in seq_along(orphans)) {
            records$cluster_id[records$record_id == orphans[oi]] <- max_cluster + oi
          }
        }
      } else {
        # No sub_groups provided — split each record into its own cluster
        max_cluster <- max(records$cluster_id)
        for (k in seq_len(nrow(members))) {
          records$cluster_id[records$record_id == members$record_id[k]] <- max_cluster + k
        }
      }
    }
  }
}

# Save dedup log
write(
  toJSON(dedup_log, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  file.path(paths$data, "04_dedup_log.json")
)

# Re-number clusters sequentially after any splits
cluster_map <- tibble(old = unique(records$cluster_id)) |>
  arrange(old) |>
  mutate(new = row_number())
records$cluster_id <- cluster_map$new[match(records$cluster_id, cluster_map$old)]

n_events <- n_distinct(records$cluster_id)
cli::cli_alert_info("Final: {n_events} unique damage events from {nrow(records)} records")

# ==============================================================================
# 5. Merge clusters into unique damage events
# ==============================================================================

cli::cli_h3("Merging clusters into unique damage events")

# Helper: pick best (most precise) value — lowest non-NA code
best_precision <- function(values) {
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_integer_)
  min(values)
}

# Helper: union of pipe-delimited codes
union_pipe_codes <- function(values) {
  if (is.list(values)) values <- unlist(values)
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_character_)
  codes <- unique(unlist(str_split(values, "\\|")))
  codes <- sort(codes)
  paste(codes, collapse = "|")
}

# Helper: pick the value from the "best" (most detailed) record
# "Best" = longest site_dmg_full, then most precise geo_prec
pick_best_record <- function(cluster_df) {
  cluster_df |>
    mutate(
      detail_score = nchar(replace(site_dmg_full, is.na(site_dmg_full), "")) +
                     nchar(replace(site_dmg_smry, is.na(site_dmg_smry), ""))
    ) |>
    arrange(desc(detail_score), geo_prec) |>
    slice(1)
}

# Helper: derive verification_level from num_articles and max verification_quality
derive_verification_level <- function(num_articles, max_vq) {
  if (is.na(max_vq)) max_vq <- 1L

  if (num_articles >= 3 && max_vq >= 3) return(4L)    # extensively verified
  if (num_articles >= 2 || max_vq >= 3)  return(3L)    # multiple independent sources
  if (max_vq >= 2)                       return(2L)    # single high-quality article
  1L                                                    # minimal verification
}

# Helper: detect conflicting reports across articles in a cluster
detect_conflicts <- function(cluster_df) {
  conflicts <- character()

  # Conflicting initiators
  init_vals <- cluster_df$initiator
  if (is.list(init_vals)) init_vals <- unlist(init_vals)
  initiators <- unique(init_vals[!is.na(init_vals)])
  if (length(initiators) > 1) {
    conflicts <- c(conflicts, paste0("Initiator: ", paste(initiators, collapse = " vs ")))
  }

  # Conflicting damage types
  dt_vals <- cluster_df$damage_type
  if (is.list(dt_vals)) dt_vals <- unlist(dt_vals)
  dtypes <- unique(dt_vals[!is.na(dt_vals)])
  if (length(dtypes) > 1) {
    conflicts <- c(conflicts, paste0("Damage type: ", paste(dtypes, collapse = " vs ")))
  }

  # Conflicting dates (non-overlapping)
  date_vals <- cluster_df$event_date_start
  if (is.list(date_vals)) date_vals <- unlist(date_vals)
  starts <- as.Date(date_vals[!is.na(date_vals)])
  if (length(starts) > 1) {
    if (max(starts) - min(starts) > 30) {
      conflicts <- c(conflicts, paste0(
        "Event dates span > 30 days: ",
        min(starts), " to ", max(starts)
      ))
    }
  }

  list(
    has_conflicts = length(conflicts) > 0,
    notes = if (length(conflicts) > 0) paste(conflicts, collapse = "; ") else NA_character_
  )
}

# --- Safe extraction from tibble rows (handles list-columns from fromJSON) ---
# fromJSON() stores JSON null as NULL inside list-columns; is.na() fails on these.
# This helper safely extracts a scalar, returning the appropriate NA type for nulls.
safe_scalar <- function(x, na_val = NA_character_) {
  # Unwrap single-element lists (from list-columns)
  if (is.list(x)) x <- x[[1]]
  # Handle NULL, length-0, and NA
  if (is.null(x) || length(x) == 0) return(na_val)
  if (is.na(x)) return(na_val)
  x
}

safe_int <- function(x) safe_scalar(x, NA_integer_)
safe_chr <- function(x) safe_scalar(x, NA_character_)
safe_num <- function(x) safe_scalar(x, NA_real_)

# Build event records
events_list <- list()

for (cid in sort(unique(records$cluster_id))) {
  cluster <- records |> filter(cluster_id == cid)
  best <- pick_best_record(cluster)

  # --- Article-level sub-records ---
  articles_arr <- map(seq_len(nrow(cluster)), function(k) {
    r <- cluster[k, ]
    list(
      article_id   = safe_chr(r$article_id),
      article_date = safe_chr(r$article_date),
      article_source = safe_chr(r$article_source),
      article_headline = safe_chr(r$article_headline),
      article_excerpt  = safe_chr(r$article_excerpt),
      article_type     = NA_character_,
      verification_quality = safe_int(r$verification_quality),
      reported_source  = safe_chr(r$reported_source),
      site_dmg_full    = safe_chr(r$site_dmg_full),
      initiator        = safe_chr(r$initiator),
      group_at_site    = safe_chr(r$group_at_site),
      damage_type      = safe_chr(r$damage_type),
      event_date_start = safe_chr(r$event_date_start),
      event_date_end   = safe_chr(r$event_date_end),
      notes            = safe_chr(r$notes)
    )
  })

  # --- Unique article IDs (a cluster may have multiple records from same article) ---
  unique_article_ids <- unique(cluster$article_id)
  num_articles <- length(unique_article_ids)

  # --- Date computations ---
  event_starts <- as.Date(na.omit(cluster$event_date_start))
  event_ends   <- as.Date(na.omit(cluster$event_date_end))
  article_dates <- as.Date(cluster$article_date)

  event_date_start <- if (length(event_starts) > 0) {
    format(min(event_starts), "%Y-%m-%d")
  } else {
    # Default: start of conflict (2022-02-24) per schema
    "2022-02-24"
  }

  event_date_end <- if (length(event_ends) > 0) {
    format(max(event_ends), "%Y-%m-%d")
  } else if (length(event_starts) > 0) {
    # If we have starts but no ends, use latest start as end
    format(max(event_starts), "%Y-%m-%d")
  } else {
    # Default: earliest article date
    format(min(article_dates), "%Y-%m-%d")
  }

  earliest_article_date <- format(min(article_dates), "%Y-%m-%d")
  latest_article_date   <- format(max(article_dates), "%Y-%m-%d")

  # Year from event dates (prefer event_date_start)
  event_year <- if (length(event_starts) > 0) {
    as.integer(format(min(event_starts), "%Y"))
  } else {
    as.integer(best$year)
  }

  # --- Precision fields: pick most precise ---
  best_temp_prec <- best_precision(cluster$temp_prec)
  best_geo_prec  <- best_precision(cluster$geo_prec)

  # --- Coordinates: from most precise geo_prec ---
  coord_row <- cluster |>
    filter(!is.na(event_latitude)) |>
    arrange(geo_prec) |>
    slice(1)

  event_lat <- if (nrow(coord_row) > 0) coord_row$event_latitude else NA_real_
  event_lon <- if (nrow(coord_row) > 0) coord_row$event_longitude else NA_real_

  # --- Union of damage types ---
  merged_damage_type <- union_pipe_codes(cluster$damage_type)

  # --- Union of initiators ---
  init_vals <- cluster$initiator
  if (is.list(init_vals)) init_vals <- unlist(init_vals)
  initiators <- unique(init_vals[!is.na(init_vals)])
  merged_initiator <- if (length(initiators) == 0) {
    NA_character_
  } else {
    paste(initiators, collapse = "; ")
  }

  # --- Groups at site ---
  grp_vals <- cluster$group_at_site
  if (is.list(grp_vals)) grp_vals <- unlist(grp_vals)
  groups <- unique(grp_vals[!is.na(grp_vals)])
  merged_group_at_site <- if (length(groups) == 0) {
    NA_character_
  } else {
    paste(groups, collapse = "; ")
  }

  # --- Damage descriptions: use best record ---
  merged_smry <- safe_chr(best$site_dmg_smry)
  merged_full <- if (nrow(cluster) == 1) {
    safe_chr(best$site_dmg_full)
  } else {
    # Combine all full descriptions for multi-article events
    all_fulls <- cluster$site_dmg_full
    # Handle list-column: unlist and remove NAs/NULLs
    if (is.list(all_fulls)) all_fulls <- unlist(all_fulls)
    all_fulls <- all_fulls[!is.na(all_fulls)]
    if (length(all_fulls) == 0) NA_character_
    else if (length(unique(all_fulls)) == 1) all_fulls[1]
    else paste(unique(all_fulls), collapse = " | ")
  }

  # --- Sources ---
  unique_sources <- unique(cluster$article_source)
  primary_source <- cluster |>
    arrange(article_date) |>
    pull(article_source) |>
    first()

  # --- Verification ---
  vq_vals <- cluster$verification_quality
  if (is.list(vq_vals)) vq_vals <- unlist(vq_vals)
  vq_vals <- vq_vals[!is.na(vq_vals)]
  max_vq <- if (length(vq_vals) > 0) max(vq_vals) else NA_integer_
  verification_level <- derive_verification_level(num_articles, max_vq)

  # --- Conflicts ---
  conflict_info <- detect_conflicts(cluster)

  # --- Build event record ---
  # Use NA (not NULL) for nullable fields — toJSON(..., na = "null") converts them.
  # Use I() for article_sources_list to prevent auto_unbox from collapsing 1-element arrays.
  event <- list(
    event_id         = NA_character_,   # assigned after sorting
    event_date_start = event_date_start,
    event_date_end   = event_date_end,
    year             = event_year,
    temp_prec        = best_temp_prec,
    event_latitude   = if (is.na(event_lat)) NA_real_ else event_lat,
    event_longitude  = if (is.na(event_lon)) NA_real_ else event_lon,
    geo_prec         = best_geo_prec,
    site_name        = safe_chr(best$site_name),
    site_type        = safe_int(best$site_type),
    place_of_worship = safe_int(best$place_of_worship),
    place_of_burial  = safe_int(best$place_of_burial),
    other_site_type  = safe_chr(best$other_site_type),
    initiator        = if (is.na(merged_initiator)) NA_character_ else merged_initiator,
    group_at_site    = if (is.na(merged_group_at_site)) NA_character_ else merged_group_at_site,
    damage_type      = if (is.na(merged_damage_type)) NA_character_ else merged_damage_type,
    other_damage_type = safe_chr(best$other_damage_type),
    site_dmg_smry    = if (is.na(merged_smry)) NA_character_ else merged_smry,
    site_dmg_full    = if (is.na(merged_full)) NA_character_ else merged_full,
    country          = safe_chr(best$country),
    num_articles        = num_articles,
    articles            = articles_arr,
    earliest_article_date = earliest_article_date,
    latest_article_date   = latest_article_date,
    primary_article_source = primary_source,
    article_sources_list   = I(unique_sources),
    verification_level      = verification_level,
    conflicting_reports     = conflict_info$has_conflicts,
    conflict_notes          = if (is.na(conflict_info$notes)) NA_character_ else conflict_info$notes,
    event_notes             = NA_character_,
    coding_date             = coding_date
  )

  events_list <- c(events_list, list(event))
}

# Sort events by date then assign EVT IDs
events_list <- events_list[order(
  sapply(events_list, \(e) e$event_date_start %||% "9999-99-99"),
  sapply(events_list, \(e) e$site_name)
)]

evt_ids <- generate_ids(id_prefixes$event, length(events_list))
for (i in seq_along(events_list)) {
  events_list[[i]]$event_id <- evt_ids[i]
}

cli::cli_alert_success("Created {length(events_list)} unique damage events")

# ==============================================================================
# 6. Write output
# ==============================================================================

output_path <- file.path(paths$data, "04_damage_events.json")
write(
  toJSON(events_list, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  output_path
)
cli::cli_alert_success("Wrote {length(events_list)} events to {.file {output_path}}")

# ==============================================================================
# 7. Schema validation
# ==============================================================================

cli::cli_h3("Schema validation")

n_valid <- 0
n_invalid <- 0
validation_errors <- list()

for (i in seq_along(events_list)) {
  event_json <- toJSON(
    events_list[[i]],
    pretty = FALSE,
    auto_unbox = TRUE,
    na = "null"
  )
  valid <- tryCatch({
    jsonvalidate::json_validate(
      json = event_json,
      schema = schemas$damage_event,
      verbose = TRUE,
      engine = "ajv"
    )
  }, error = function(e) {
    cli::cli_alert_danger("Validation error for {events_list[[i]]$event_id}: {e$message}")
    FALSE
  })

  if (isTRUE(valid)) {
    n_valid <- n_valid + 1
  } else {
    n_invalid <- n_invalid + 1
    errs <- attr(valid, "errors")
    validation_errors <- c(validation_errors, list(list(
      event_id = events_list[[i]]$event_id,
      errors = if (!is.null(errs)) as.list(errs) else "unknown"
    )))
    cli::cli_alert_danger(
      "  {events_list[[i]]$event_id} ({events_list[[i]]$site_name}): INVALID"
    )
    if (!is.null(errs)) print(errs)
  }
}

cli::cli_alert_info(
  "Schema validation: {n_valid} valid, {n_invalid} invalid out of {length(events_list)} events"
)

# ==============================================================================
# 8. Flag events for human review
# ==============================================================================

review_needed <- keep(events_list, function(evt) {
  isTRUE(evt$conflicting_reports) ||
  (!is.null(evt$verification_level) && evt$verification_level <= 1)
})

if (length(review_needed) > 0) {
  review_path <- file.path(paths$data, "04_review_needed.json")
  write(
    toJSON(review_needed, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    review_path
  )
  cli::cli_alert_warning(
    "{length(review_needed)} events flagged for human review → {.file {review_path}}"
  )
} else {
  cli::cli_alert_success("No events flagged for human review")
}

# ==============================================================================
# 9. Summary
# ==============================================================================

cli::cli_h3("Summary")

cli::cli_alert_info("Input records: {nrow(records)}")
cli::cli_alert_info("Unique events: {length(events_list)}")
cli::cli_alert_info("Events with multiple articles: {sum(sapply(events_list, \\(e) e$num_articles > 1))}")
cli::cli_alert_info("Events with conflicts: {sum(sapply(events_list, \\(e) isTRUE(e$conflicting_reports)))}")
cli::cli_alert_info("Schema valid: {n_valid}/{length(events_list)}")

# Print event table
cli::cli_h3("Event inventory")
for (evt in events_list) {
  coords <- if (!is.null(evt$event_latitude)) {
    paste0("(", round(evt$event_latitude, 2), ", ", round(evt$event_longitude, 2), ")")
  } else {
    "no coords"
  }
  cli::cli_alert_info(
    "{evt$event_id} | {evt$site_name} | {evt$year} | {evt$num_articles} article(s) | {coords}"
  )
}
