# 02_classify.R
# Step 2: Classify articles as relevant (reporting cultural heritage damage) or not.
#
# Input:  data/01_articles.json
# Output: data/02_articles_filtered.json (same schema, filtered)
#         data/02_classification_log.json (all articles with classification metadata)
#         data/02_review_needed.json (borderline cases for human review)

source(file.path("scripts", "00_setup.R"))

# TODO: Implement Step 2
# 1. Load articles from Step 1
# 2. Create chat_anthropic() with damage_definition in system prompt
# 3. For each article, chat_structured() -> reports_damage, confidence, reasoning
# 4. Batch with parallel_chat_structured()
# 5. Filter to reports_damage == TRUE
# 6. Export borderline cases for human review
# 7. Write filtered articles to data/02_articles_filtered.json
