# 04_deduplicate.R
# Step 4: Identify unique damage events by merging duplicate reports.
#
# Input:  data/03_article_damage_events.json (geocoded)
# Output: data/04_damage_events.json (conforming to schema_damage_event.json)
#         data/04_review_needed.json (events with conflicts for human review)

source(file.path("scripts", "00_setup.R"))

# TODO: Implement Step 4
# 1. Load article-damage-event records from Step 3
# 2. Group by candidate match key (site_name_normalized, year, proximity)
# 3. Use Claude to adjudicate ambiguous clusters
# 4. Merge fields per schema_damage_event.json
# 5. Generate event_id as EVT_XXXX
# 6. Compute derived fields (num_articles, earliest/latest dates, sources list, etc.)
# 7. Flag conflicting_reports, set verification_level
# 8. Validate against schema_damage_event.json
# 9. Write to data/04_damage_events.json
