# 05_geocode.R
# Geocode damage event locations using OpenStreetMap/Nominatim.
# Called after Step 3 (extract) and shared with Step 4 (deduplicate).
#
# Input:  data/03_article_damage_events_raw.json (pre-geocoding)
# Output: data/03_article_damage_events.json (with coordinates)
#         data/05_geocode_cache.json (cached geocoding results)
#         data/05_geocode_failed.json (failed lookups for manual review)

source(file.path("scripts", "00_setup.R"))

# TODO: Implement geocoding
# 1. Load article-damage-event records from Step 3
# 2. Collect unique (site_name, location_text, country) tuples
# 3. Check cache for previously geocoded locations
# 4. geocode(method = "osm") with country = "UA" constraint
# 5. Assign geo_prec from Nominatim response type
# 6. Retry failed lookups with simplified place names
# 7. Join coordinates back to records
# 8. Update cache, write failed lookups
# 9. Validate against schema_newspaper_damage_event.json
# 10. Write to data/03_article_damage_events.json
