# 03_extract.R
# Step 3: Extract structured damage event details from each relevant article.
#
# Input:  data/02_articles_filtered.json
# Output: data/03_article_damage_events.json (conforming to schema_newspaper_damage_event.json)
#         Before geocoding — coordinates added by 05_geocode.R

source(file.path("scripts", "00_setup.R"))

# TODO: Implement Step 3
# 1. Load filtered articles from Step 2
# 2. Define ellmer type_object() matching schema_newspaper_damage_event.json
# 3. For each article, chat_structured() -> array of damage events
# 4. Carry forward article metadata (article_id, date, source, headline)
# 5. Generate record_id as REC_XXXX
# 6. Set coding_date
# 7. Write to data/03_article_damage_events_raw.json (pre-geocoding)
