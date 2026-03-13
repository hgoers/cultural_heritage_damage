# build_review_articles.R
# Creates 02_review_articles.json from existing 02_review_needed.json and 01_articles.json.
# No LLM API calls — use when you already have classification results.

library(jsonlite)
library(dplyr)

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
data_dir <- file.path(root, "data")

review_path <- file.path(data_dir, "02_review_needed.json")
articles_path <- file.path(data_dir, "01_articles.json")
out_path <- file.path(data_dir, "02_review_articles.json")

if (!file.exists(review_path)) {
  stop("02_review_needed.json not found at ", review_path)
}
if (!file.exists(articles_path)) {
  stop("01_articles.json not found at ", articles_path)
}

review_needed <- fromJSON(review_path)
articles <- fromJSON(articles_path)

review_articles <- review_needed |>
  left_join(
    articles |> select(article_id, article_body),
    by = "article_id"
  )

write(
  toJSON(review_articles, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  out_path
)

message("Wrote ", nrow(review_articles), " articles to ", out_path)
