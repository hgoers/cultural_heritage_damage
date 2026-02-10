# review_dashboard.R
# Shiny dashboard for reviewing articles flagged as edge cases in Step 2.
# Displays article metadata, body text, AI reasoning, and human coder assessments.
# User decisions are saved and update 02_articles_filtered.json.
#
# Launch: Rscript scripts/review_dashboard.R
#   or:   shiny::runApp("scripts/review_dashboard.R")

library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# --- Data paths ---------------------------------------------------------------

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
data_dir <- file.path(root, "data")
human_dir <- file.path(root, "human-coding")

review_path     <- file.path(data_dir, "02_review_needed.json")
articles_path   <- file.path(data_dir, "01_articles.json")
filtered_path   <- file.path(data_dir, "02_articles_filtered.json")
log_path        <- file.path(data_dir, "02_classification_log.json")
decisions_path  <- file.path(data_dir, "02_review_decisions.json")

# --- Load & prepare data (once at startup) ------------------------------------

review_raw <- fromJSON(review_path)
articles_all <- fromJSON(articles_path)
human_raw <- read_csv(
  file.path(human_dir, "article_relevance.csv"),
  col_types = cols(
    article_id = col_character(),
    article_header = col_character(),
    evidence_of_attack = col_integer(),
    summary_of_attack = col_character(),
    coder = col_character(),
    ldi = col_character()
  )
)

# Join article body onto review data
review_data <- review_raw |>
  left_join(
    articles_all |> select(article_id, article_body),
    by = "article_id"
  )

# Summarise human coding per headline
human_summary <- human_raw |>
  group_by(article_header) |>
  summarise(
    human_count = n(),
    n_relevant = sum(evidence_of_attack == 1, na.rm = TRUE),
    n_irrelevant = sum(evidence_of_attack == 0, na.rm = TRUE),
    n_flagged = sum(evidence_of_attack == -1, na.rm = TRUE),
    coders = paste(unique(coder), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    human_assessment = case_when(
      n_relevant > 0 & n_irrelevant == 0 & n_flagged == 0 ~ "Relevant",
      n_irrelevant > 0 & n_relevant == 0 & n_flagged == 0 ~ "Not relevant",
      n_flagged > 0 & n_relevant == 0 & n_irrelevant == 0 ~ "Flagged for review",
      n_relevant > 0 & n_irrelevant > 0 ~ "Disagreement",
      n_relevant > 0 & n_flagged > 0 ~ "Disagreement",
      n_irrelevant > 0 & n_flagged > 0 ~ "Disagreement",
      TRUE ~ "Mixed"
    )
  )

# Join human summary onto review data
review_data <- review_data |>
  left_join(human_summary, by = c("article_headline" = "article_header"))

# Fill missing human data
review_data <- review_data |>
  mutate(
    human_count = replace_na(human_count, 0L),
    human_assessment = replace_na(human_assessment, "No human review")
  )

# Sort: NAs first (failures), then low confidence, then medium
review_data <- review_data |>
  mutate(
    sort_key = case_when(
      is.na(reports_damage) ~ 1L,
      confidence == "low" ~ 2L,
      confidence == "medium" ~ 3L,
      TRUE ~ 4L
    )
  ) |>
  arrange(sort_key, article_id)

n_articles <- nrow(review_data)

# --- Load any prior decisions -------------------------------------------------

load_decisions <- function() {
  if (file.exists(decisions_path) && file.size(decisions_path) > 0) {
    fromJSON(decisions_path) |> as_tibble()
  } else {
    tibble(article_id = character(), decision = character())
  }
}

save_decisions <- function(decisions_df) {
  write(
    toJSON(decisions_df, pretty = TRUE, auto_unbox = TRUE),
    decisions_path
  )
}

update_filtered_articles <- function(decisions_df) {
  # Start from the classification log to get all relevant IDs
  class_log <- fromJSON(log_path)

  # IDs that are relevant from the original classification (non-review articles)
  review_ids <- review_data$article_id
  auto_relevant <- class_log |>
    filter(reports_damage == TRUE, !article_id %in% review_ids) |>
    pull(article_id)

  # IDs from review decisions marked relevant
  decided_relevant <- decisions_df |>
    filter(decision == "relevant") |>
    pull(article_id)

  # Review articles with no decision yet — keep original classification
  undecided_ids <- setdiff(review_ids, decisions_df$article_id)
  undecided_relevant <- review_data |>
    filter(article_id %in% undecided_ids, reports_damage == TRUE) |>
    pull(article_id)

  # Combine all relevant IDs
  all_relevant <- unique(c(auto_relevant, decided_relevant, undecided_relevant))

  # Write filtered articles
  filtered <- articles_all |> filter(article_id %in% all_relevant)
  write(
    toJSON(filtered, pretty = TRUE, auto_unbox = TRUE, na = "null"),
    filtered_path
  )

  nrow(filtered)
}

# --- UI -----------------------------------------------------------------------

human_badge <- function(assessment) {
  cls <- switch(assessment,
    "Relevant"           = "bg-success",
    "Not relevant"       = "bg-danger",
    "Flagged for review" = "bg-warning text-dark",
    "Disagreement"       = "bg-info",
    "No human review"    = "bg-secondary",
    "bg-secondary"
  )
  tags$span(class = paste("badge", cls), assessment)
}

confidence_badge <- function(conf) {
  if (is.na(conf)) return(tags$span(class = "badge bg-dark", "FAILED"))
  cls <- switch(conf,
    "high"   = "bg-success",
    "medium" = "bg-warning text-dark",
    "low"    = "bg-danger",
    "bg-secondary"
  )
  tags$span(class = paste("badge", cls), conf)
}

ai_badge <- function(reports_damage) {
  if (is.na(reports_damage)) return(tags$span(class = "badge bg-dark", "API failure"))
  if (reports_damage) {
    tags$span(class = "badge bg-success", "Relevant")
  } else {
    tags$span(class = "badge bg-danger", "Not relevant")
  }
}

ui <- page_sidebar(
  title = "Article Relevance Review",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,
    h5("Navigation"),
    p(class = "text-muted small", textOutput("progress_text")),

    # Filter controls
    selectInput("filter_status", "Filter by review status",
      choices = c("All", "Unreviewed", "Relevant", "Not relevant"),
      selected = "All"
    ),
    selectInput("filter_human", "Filter by human assessment",
      choices = c("All", "Relevant", "Not relevant", "Flagged for review",
                  "Disagreement", "No human review"),
      selected = "All"
    ),
    selectInput("filter_confidence", "Filter by AI confidence",
      choices = c("All", "high", "medium", "low", "FAILED"),
      selected = "All"
    ),

    hr(),

    # Article selector
    uiOutput("article_selector_ui"),

    hr(),

    # Navigation buttons
    layout_columns(
      col_widths = c(6, 6),
      actionButton("prev_btn", "\u25c0 Prev", class = "btn-outline-primary btn-sm w-100"),
      actionButton("next_btn", "Next \u25b6", class = "btn-outline-primary btn-sm w-100")
    ),

    hr(),

    # Decision buttons
    h5("Your Decision"),
    layout_columns(
      col_widths = c(6, 6),
      actionButton("mark_relevant", "\u2713 Relevant",
        class = "btn-success btn-sm w-100"),
      actionButton("mark_irrelevant", "\u2717 Not Relevant",
        class = "btn-danger btn-sm w-100")
    ),
    actionButton("clear_decision", "Clear Decision",
      class = "btn-outline-secondary btn-sm w-100 mt-2"),

    hr(),
    actionButton("save_btn", "Save & Update Files",
      class = "btn-primary w-100",
      icon = icon("floppy-disk"))
  ),

  # Main content
  layout_columns(
    col_widths = c(12),

    # Header card with metadata
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        textOutput("article_id_text"),
        uiOutput("decision_badge")
      ),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          # Left: headline + metadata
          div(
            h4(textOutput("headline_text")),
            layout_columns(
              col_widths = c(4, 4, 4),
              div(tags$strong("Date: "), textOutput("date_text", inline = TRUE)),
              div(tags$strong("Source: "), textOutput("source_text", inline = TRUE)),
              div(tags$strong("AI Confidence: "), uiOutput("confidence_ui", inline = TRUE))
            )
          ),
          # Right: assessments
          div(
            class = "text-end",
            div(class = "mb-2", tags$strong("AI: "), uiOutput("ai_badge_ui", inline = TRUE)),
            div(class = "mb-2", tags$strong("Human: "), uiOutput("human_badge_ui", inline = TRUE)),
            div(class = "small text-muted", textOutput("human_detail_text"))
          )
        )
      )
    ),

    # AI reasoning
    card(
      card_header("AI Reasoning"),
      card_body(
        class = "bg-light",
        textOutput("reasoning_text")
      )
    ),

    # Article body
    card(
      card_header("Article Body"),
      card_body(
        style = "max-height: 500px; overflow-y: auto;",
        uiOutput("body_html")
      )
    )
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive values
  decisions <- reactiveVal(load_decisions())
  current_idx <- reactiveVal(1L)

  # Filtered article indices
  filtered_indices <- reactive({
    decs <- decisions()
    idx <- seq_len(n_articles)

    # Filter by review status
    if (input$filter_status != "All") {
      reviewed_ids <- decs$article_id
      if (input$filter_status == "Unreviewed") {
        idx <- idx[!review_data$article_id[idx] %in% reviewed_ids]
      } else if (input$filter_status == "Relevant") {
        rel_ids <- decs |> filter(decision == "relevant") |> pull(article_id)
        idx <- idx[review_data$article_id[idx] %in% rel_ids]
      } else if (input$filter_status == "Not relevant") {
        irr_ids <- decs |> filter(decision == "not_relevant") |> pull(article_id)
        idx <- idx[review_data$article_id[idx] %in% irr_ids]
      }
    }

    # Filter by human assessment
    if (input$filter_human != "All") {
      idx <- idx[review_data$human_assessment[idx] == input$filter_human]
    }

    # Filter by confidence
    if (input$filter_confidence != "All") {
      if (input$filter_confidence == "FAILED") {
        idx <- idx[is.na(review_data$confidence[idx])]
      } else {
        idx <- idx[!is.na(review_data$confidence[idx]) &
                   review_data$confidence[idx] == input$filter_confidence]
      }
    }

    idx
  })

  # Keep current_idx within bounds when filters change
  observeEvent(filtered_indices(), {
    fi <- filtered_indices()
    if (length(fi) == 0) return()
    cur <- current_idx()
    if (!cur %in% fi) {
      current_idx(fi[1])
    }
  })

  # Current article data
  current_article <- reactive({
    review_data[current_idx(), ]
  })

  # Current decision
  current_decision <- reactive({
    decs <- decisions()
    aid <- current_article()$article_id
    match <- decs |> filter(article_id == aid)
    if (nrow(match) > 0) match$decision[1] else NA_character_
  })

  # --- Article selector dropdown ---
  output$article_selector_ui <- renderUI({
    fi <- filtered_indices()
    if (length(fi) == 0) {
      return(p(class = "text-muted", "No articles match filters"))
    }
    decs <- decisions()
    choices <- fi
    labels <- map_chr(fi, \(i) {
      aid <- review_data$article_id[i]
      headline <- str_trunc(review_data$article_headline[i], 35)
      dec_mark <- if (aid %in% decs$article_id) "\u2713 " else "  "
      paste0(dec_mark, aid, " | ", headline)
    })
    names(choices) <- labels

    selectInput("article_select", "Select article",
      choices = choices,
      selected = current_idx()
    )
  })

  observeEvent(input$article_select, {
    req(input$article_select)
    new_idx <- as.integer(input$article_select)
    if (!is.na(new_idx) && new_idx != current_idx()) {
      current_idx(new_idx)
    }
  })

  # --- Progress ---
  output$progress_text <- renderText({
    decs <- decisions()
    fi <- filtered_indices()
    n_decided <- sum(review_data$article_id[fi] %in% decs$article_id)
    paste0("Showing ", length(fi), " of ", n_articles, " articles | ",
           nrow(decs), " decided total")
  })

  # --- Navigation ---
  observeEvent(input$prev_btn, {
    fi <- filtered_indices()
    if (length(fi) == 0) return()
    cur_pos <- match(current_idx(), fi)
    if (!is.na(cur_pos) && cur_pos > 1) {
      current_idx(fi[cur_pos - 1])
    }
  })

  observeEvent(input$next_btn, {
    fi <- filtered_indices()
    if (length(fi) == 0) return()
    cur_pos <- match(current_idx(), fi)
    if (!is.na(cur_pos) && cur_pos < length(fi)) {
      current_idx(fi[cur_pos + 1])
    }
  })

  # --- Decision buttons ---
  persist_decisions <- function(decs) {
    save_decisions(decs)
    n_filtered <- update_filtered_articles(decs)
    showNotification(
      paste0("Saved. ", n_filtered, " articles in filtered set."),
      type = "message", duration = 2
    )
  }

  make_decision <- function(decision_value) {
    decs <- decisions()
    aid <- current_article()$article_id
    decs <- decs |> filter(article_id != aid)
    decs <- bind_rows(decs, tibble(article_id = aid, decision = decision_value))
    decisions(decs)
    persist_decisions(decs)

    # Auto-advance to next unreviewed
    fi <- filtered_indices()
    cur_pos <- match(current_idx(), fi)
    if (!is.na(cur_pos) && cur_pos < length(fi)) {
      current_idx(fi[cur_pos + 1])
    }
  }

  observeEvent(input$mark_relevant, make_decision("relevant"))
  observeEvent(input$mark_irrelevant, make_decision("not_relevant"))

  observeEvent(input$clear_decision, {
    decs <- decisions()
    aid <- current_article()$article_id
    decs <- decs |> filter(article_id != aid)
    decisions(decs)
    persist_decisions(decs)
  })

  # --- Save ---
  observeEvent(input$save_btn, {
    decs <- decisions()
    save_decisions(decs)
    n_filtered <- update_filtered_articles(decs)
    showNotification(
      paste0("Saved ", nrow(decs), " decisions. ",
             "02_articles_filtered.json now has ", n_filtered, " articles."),
      type = "message",
      duration = 5
    )
  })

  # --- Display outputs ---
  output$article_id_text <- renderText({
    art <- current_article()
    fi <- filtered_indices()
    pos <- match(current_idx(), fi)
    paste0(art$article_id, "  (", pos, " / ", length(fi), ")")
  })

  output$headline_text <- renderText(current_article()$article_headline)
  output$date_text <- renderText(as.character(current_article()$article_date))
  output$source_text <- renderText(current_article()$article_source)

  output$confidence_ui <- renderUI({
    confidence_badge(current_article()$confidence)
  })

  output$ai_badge_ui <- renderUI({
    ai_badge(current_article()$reports_damage)
  })

  output$human_badge_ui <- renderUI({
    human_badge(current_article()$human_assessment)
  })

  output$human_detail_text <- renderText({
    art <- current_article()
    if (art$human_count == 0) {
      "Not reviewed by human coders"
    } else {
      parts <- c()
      if (!is.na(art$n_relevant) && art$n_relevant > 0)
        parts <- c(parts, paste0(art$n_relevant, " relevant"))
      if (!is.na(art$n_irrelevant) && art$n_irrelevant > 0)
        parts <- c(parts, paste0(art$n_irrelevant, " not relevant"))
      if (!is.na(art$n_flagged) && art$n_flagged > 0)
        parts <- c(parts, paste0(art$n_flagged, " flagged"))
      paste0(art$human_count, " coder(s): ", paste(parts, collapse = ", "),
             " | ", art$coders)
    }
  })

  output$decision_badge <- renderUI({
    dec <- current_decision()
    if (is.na(dec)) {
      tags$span(class = "badge bg-secondary fs-6", "Unreviewed")
    } else if (dec == "relevant") {
      tags$span(class = "badge bg-success fs-6", "\u2713 Relevant")
    } else {
      tags$span(class = "badge bg-danger fs-6", "\u2717 Not Relevant")
    }
  })

  output$reasoning_text <- renderText({
    r <- current_article()$reasoning
    if (is.na(r)) "Classification failed — no reasoning available" else r
  })

  output$body_html <- renderUI({
    body <- current_article()$article_body
    if (is.na(body) || is.null(body)) {
      p(class = "text-muted", "Article body not available")
    } else {
      # Convert newlines to <br> for display
      paras <- str_split(body, "\n+")[[1]]
      paras <- paras[nchar(trimws(paras)) > 0]
      tags$div(
        lapply(paras, \(p_text) tags$p(p_text))
      )
    }
  })
}

# --- Launch -------------------------------------------------------------------
shiny::runApp(shinyApp(ui, server), launch.browser = TRUE)