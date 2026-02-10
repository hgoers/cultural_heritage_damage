# 05_geocode.R
# Geocode damage event locations using OpenStreetMap/Nominatim.
# Called after Step 3 (extract) and reusable for Step 4 (deduplicate).
#
# Input:  data/03_article_damage_events.json
# Output: data/03_article_damage_events.json (updated in-place with coordinates)
#         data/05_geocode_cache.json (cached geocoding results for reuse)
#         data/05_geocode_failed.json (failed lookups for manual review)

source(file.path("scripts", "00_setup.R"))
library(tidygeocoder)

cli::cli_h2("Step 3b: Geocode damage event locations")

# --- 1. Load article-damage-event records -------------------------------------

events <- fromJSON(file.path(paths$data, "03_article_damage_events.json")) |>
  as_tibble()
cli::cli_alert_info("Loaded {nrow(events)} damage event records")

if (nrow(events) == 0) {
  cli::cli_alert_warning("No events to geocode. Exiting.")
  quit(save = "no", status = 0)
}

# --- 2. Collect unique locations to geocode -----------------------------------

# De-duplicate by site_name + location_text to minimise API calls
unique_locs <- events |>
  distinct(site_name, location_text, country) |>
  mutate(loc_key = paste(site_name, location_text, country, sep = " | "))

cli::cli_alert_info("{nrow(unique_locs)} unique site-location combinations to geocode")

# --- 3. Load cache if it exists -----------------------------------------------

cache_path <- file.path(paths$data, "05_geocode_cache.json")

if (file.exists(cache_path) && file.size(cache_path) > 0) {
  cache <- fromJSON(cache_path) |> as_tibble()
  cli::cli_alert_info("Loaded geocode cache with {nrow(cache)} entries")
} else {
  cache <- tibble(
    loc_key = character(),
    query = character(),
    lat = numeric(),
    long = numeric(),
    osm_type = character(),
    display_name = character(),
    geo_prec_from_osm = integer()
  )
  cli::cli_alert_info("No geocode cache found — starting fresh")
}

# Identify which locations still need geocoding
unique_locs <- unique_locs |>
  mutate(cached = loc_key %in% cache$loc_key)

to_geocode <- unique_locs |> filter(!cached)
cli::cli_alert_info(
  "{nrow(to_geocode)} locations to geocode ({sum(unique_locs$cached)} cached)"
)

# --- 4. Build geocoding queries -----------------------------------------------

# Strategy: try specific query first (site_name + location_text + Ukraine)
# On failure, fall back to location_text + Ukraine only

build_query <- function(site_name, location_text, country = "Ukraine") {
  # Clean up vague location text
  loc <- location_text
  loc <- str_remove(loc, "(?i)^near\\s+")
  loc <- str_remove(loc, "(?i),?\\s*(western|eastern|southern|northern)\\s+ukraine$")
  loc <- str_remove(loc, "(?i)\\s*ukraine$")
  loc <- str_trim(loc)

  # Build full query
  paste0(site_name, ", ", loc, ", Ukraine")
}

build_fallback_query <- function(location_text, country = "Ukraine") {
  loc <- location_text
  loc <- str_remove(loc, "(?i)^near\\s+")
  loc <- str_remove(loc, "(?i),?\\s*(western|eastern|southern|northern)\\s+ukraine$")
  loc <- str_trim(loc)

  if (!str_detect(loc, "(?i)ukraine")) {
    paste0(loc, ", Ukraine")
  } else {
    loc
  }
}

# --- 5. Map OSM type to geo_prec code ----------------------------------------

# geo_prec: 1=exact site, 2=village/city, 3=ADM2 (district), 4=ADM1 (oblast),
#           5=section of country, 6=whole country
osm_type_to_geo_prec <- function(osm_type, osm_class = NA_character_) {
  if (is.na(osm_type)) return(NA_integer_)

  type_lower <- tolower(osm_type)
  class_lower <- tolower(osm_class %||% "")

  # Exact site matches
  if (type_lower %in% c(
    "building", "place_of_worship", "museum", "monument", "memorial",
    "artwork", "attraction", "theatre", "library", "archive", "cemetery",
    "archaeological_site", "castle", "ruins", "church", "cathedral",
    "chapel", "monastery", "synagogue", "mosque", "temple", "zoo",
    "university", "school", "college", "yes", "house"
  )) {
    return(1L)
  }

  # Village/city level
  if (type_lower %in% c(
    "city", "town", "village", "hamlet", "suburb", "neighbourhood",
    "residential", "quarter", "borough", "municipality"
  )) {
    return(2L)
  }

  # District/raion level
  if (type_lower %in% c("county", "district", "administrative")) {
    if (class_lower %in% c("boundary")) return(3L)
    return(3L)
  }

  # Oblast/region level
  if (type_lower %in% c("state", "region", "province")) {
    return(4L)
  }

  # Country level
  if (type_lower %in% c("country")) {
    return(6L)
  }

  # Default: city-level for unrecognized types
  2L
}

# --- 6. Geocode with Nominatim ------------------------------------------------

geocode_one <- function(query) {
  # tidygeocoder returns a tibble; we use the free-form query approach
  result <- tryCatch({
    tibble(query = query) |>
      geocode(
        address = query,
        method = "osm",
        full_results = TRUE,
        limit = 1,
        quiet = TRUE
      )
  }, error = function(e) {
    cli::cli_alert_danger("Geocoding error for '{query}': {e$message}")
    tibble(
      query = query,
      lat = NA_real_,
      long = NA_real_,
      osm_type = NA_character_,
      display_name = NA_character_
    )
  })

  # Be polite to Nominatim — 1 request per second

  Sys.sleep(1.1)

  result
}

if (nrow(to_geocode) > 0) {
  cli::cli_alert_info("Geocoding {nrow(to_geocode)} locations via Nominatim (OSM)...")
  cli::cli_alert_info("This will take ~{nrow(to_geocode) * 2} seconds due to rate limiting")

  new_cache_rows <- list()

  for (i in seq_len(nrow(to_geocode))) {
    row <- to_geocode[i, ]
    query <- build_query(row$site_name, row$location_text, row$country)

    cli::cli_alert("  [{i}/{nrow(to_geocode)}] {row$site_name} -> {query}")

    # Attempt 1: full query (site name + location)
    result <- geocode_one(query)

    succeeded <- !is.na(result$lat[1])

    # Attempt 2: fallback to location only
    if (!succeeded) {
      fallback_q <- build_fallback_query(row$location_text, row$country)
      cli::cli_alert_warning("    Retrying with fallback: {fallback_q}")
      result <- geocode_one(fallback_q)
      query <- fallback_q
      succeeded <- !is.na(result$lat[1])
    }

    # Attempt 3: just the site name + Ukraine
    if (!succeeded && row$site_name != row$location_text) {
      simple_q <- paste0(row$site_name, ", Ukraine")
      cli::cli_alert_warning("    Retrying with simple: {simple_q}")
      result <- geocode_one(simple_q)
      query <- simple_q
      succeeded <- !is.na(result$lat[1])
    }

    # Extract OSM type for geo_prec mapping
    osm_type_val <- if ("type" %in% names(result)) result$type[1] else NA_character_
    osm_class_val <- if ("class" %in% names(result)) result$class[1] else NA_character_
    display_name_val <- if ("display_name" %in% names(result)) result$display_name[1] else NA_character_

    geo_prec_val <- osm_type_to_geo_prec(osm_type_val, osm_class_val)

    cache_row <- tibble(
      loc_key = row$loc_key,
      query = query,
      lat = result$lat[1],
      long = result$long[1],
      osm_type = osm_type_val %||% NA_character_,
      display_name = display_name_val %||% NA_character_,
      geo_prec_from_osm = geo_prec_val %||% NA_integer_
    )

    new_cache_rows <- c(new_cache_rows, list(cache_row))

    if (succeeded) {
      cli::cli_alert_success(
        "    -> ({round(result$lat[1], 4)}, {round(result$long[1], 4)}) [type={osm_type_val}, prec={geo_prec_val}]"
      )
    } else {
      cli::cli_alert_danger("    -> FAILED all attempts")
    }
  }

  # Merge new results into cache
  new_cache <- bind_rows(new_cache_rows)
  cache <- bind_rows(cache, new_cache)

  # Save updated cache
  write(
    toJSON(cache, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    cache_path
  )
  cli::cli_alert_success("Updated geocode cache ({nrow(cache)} total entries)")
}

# --- 7. Join coordinates back to events ---------------------------------------

# Build lookup from cache
geo_lookup <- cache |>
  select(loc_key, lat, long, geo_prec_from_osm)

# Join via loc_key
events <- events |>
  mutate(loc_key = paste(site_name, location_text, country, sep = " | ")) |>
  left_join(geo_lookup, by = "loc_key")

# Update event fields
events <- events |>
  mutate(
    event_latitude = lat,
    event_longitude = long,
    # Update geo_prec only if OSM gave us a more precise answer
    # (lower number = more precise; keep existing if OSM is less precise)
    geo_prec = case_when(
      !is.na(geo_prec_from_osm) & (is.na(geo_prec) | geo_prec_from_osm < geo_prec) ~ geo_prec_from_osm,
      TRUE ~ geo_prec
    )
  ) |>
  select(-loc_key, -lat, -long, -geo_prec_from_osm)

# --- 8. Validate coordinates are in Ukraine -----------------------------------

ukraine_bbox <- list(
  lat_min = 44.2, lat_max = 52.4,
  lon_min = 22.1, lon_max = 40.2
)

out_of_bounds <- events |>
  filter(
    !is.na(event_latitude) &
    (event_latitude < ukraine_bbox$lat_min | event_latitude > ukraine_bbox$lat_max |
     event_longitude < ukraine_bbox$lon_min | event_longitude > ukraine_bbox$lon_max)
  )

if (nrow(out_of_bounds) > 0) {
  cli::cli_alert_warning(
    "{nrow(out_of_bounds)} events have coordinates outside Ukraine's bounding box:"
  )
  for (j in seq_len(nrow(out_of_bounds))) {
    r <- out_of_bounds[j, ]
    cli::cli_alert_danger(
      "  {r$record_id} | {r$site_name} | ({r$event_latitude}, {r$event_longitude})"
    )
  }
  # Null out bad coordinates
  events <- events |>
    mutate(
      event_latitude = if_else(
        !is.na(event_latitude) &
        (event_latitude < ukraine_bbox$lat_min | event_latitude > ukraine_bbox$lat_max |
         event_longitude < ukraine_bbox$lon_min | event_longitude > ukraine_bbox$lon_max),
        NA_real_, event_latitude
      ),
      event_longitude = if_else(
        is.na(event_latitude), NA_real_, event_longitude
      )
    )
}

# --- 9. Summary ---------------------------------------------------------------

cli::cli_h3("Geocoding Summary")

n_geocoded <- sum(!is.na(events$event_latitude))
n_failed <- sum(is.na(events$event_latitude))

cli::cli_alert_info("Geocoded: {n_geocoded} / {nrow(events)} events ({round(n_geocoded/nrow(events)*100, 1)}%)")
cli::cli_alert_info("Failed: {n_failed}")

if (n_failed > 0) {
  failed_sites <- events |>
    filter(is.na(event_latitude)) |>
    distinct(site_name, location_text)
  cli::cli_alert_warning("Sites without coordinates:")
  for (j in seq_len(nrow(failed_sites))) {
    cli::cli_alert_danger("  {failed_sites$site_name[j]} ({failed_sites$location_text[j]})")
  }

  # Write failed lookups for manual review
  failed_path <- file.path(paths$data, "05_geocode_failed.json")
  write(
    toJSON(failed_sites, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    failed_path
  )
  cli::cli_alert_info("Wrote failed lookups to {.file {failed_path}}")
}

# Print sample geocoded events
cli::cli_h3("Sample geocoded events")
geocoded_sample <- events |>
  filter(!is.na(event_latitude)) |>
  head(10)

for (j in seq_len(nrow(geocoded_sample))) {
  e <- geocoded_sample[j, ]
  cli::cli_alert_info(
    "{e$record_id} | {e$site_name} | ({round(e$event_latitude, 4)}, {round(e$event_longitude, 4)}) | geo_prec={e$geo_prec}"
  )
}

# --- 10. Write updated events -------------------------------------------------

output_path <- file.path(paths$data, "03_article_damage_events.json")
write(
  toJSON(events, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  output_path
)
cli::cli_alert_success("Wrote {nrow(events)} geocoded events to {.file {output_path}}")

# Schema validation
cli::cli_h3("Schema validation")
n_valid <- 0
n_invalid <- 0

for (j in seq_len(nrow(events))) {
  record_json <- toJSON(
    as.list(events[j, ]),
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

cli::cli_alert_info("Schema validation: {n_valid} valid, {n_invalid} invalid out of {nrow(events)} records")
# 9. Validate against schema_newspaper_damage_event.json
# 10. Write to data/03_article_damage_events.json
