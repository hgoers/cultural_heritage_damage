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

review_path         <- file.path(data_dir, "02_review_needed.json")
review_articles_path <- file.path(data_dir, "02_review_articles.json")
articles_path       <- file.path(data_dir, "01_articles.json")
filtered_path   <- file.path(data_dir, "02_articles_filtered.json")
log_path        <- file.path(data_dir, "02_classification_log.json")

# --- Google Sheets configuration ----------------------------------------------

SHEET_ID <- "1Sol8ZKsB4QAaROC5rHifZbSZo_xZD9LYDo2It8C5fsw"
SA_PATH <- Sys.getenv(
  "GOOGLE_SERVICE_ACCOUNT_JSON",
  unset = file.path(root, ".secrets", "service_account.json")
)

# --- Configuration ------------------------------------------------------------

CODERS <- c("Deniz Cil", "Harriet Goers")

auth_google_sheets <- function() {
  if (isTRUE(getOption("review_dashboard_gs4_authed", FALSE))) return(invisible(TRUE))
  if (!file.exists(SA_PATH)) {
    stop(
      "Service account key not found at ", SA_PATH, ". ",
      "Set GOOGLE_SERVICE_ACCOUNT_JSON or add .secrets/service_account.json."
    )
  }
  googlesheets4::gs4_auth(path = SA_PATH)
  options(review_dashboard_gs4_authed = TRUE)
  invisible(TRUE)
}

null_to_empty <- function(x) {
  if (is.null(x)) "" else as.character(x)
}

# --- Lazy data loading (deferred for shinyapps.io startup timeout) ------------
# Heavy work runs in server(), not at script parse time.

load_app_data <- function() {
  # Prefer slim file (02_review_articles.json) for fast startup - no 01_articles load
  if (file.exists(review_articles_path)) {
    review_raw <- fromJSON(review_articles_path)
  } else {
    articles_all <- fromJSON(articles_path)
    review_raw <- fromJSON(review_path) |>
      left_join(
        articles_all |> select(article_id, article_body),
        by = "article_id"
      )
  }
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

  review_data <- review_raw
  if (!"article_body" %in% names(review_raw)) {
    if (!exists("articles_all")) {
      articles_all <- fromJSON(articles_path)
    }
    review_data <- review_data |>
      left_join(
        articles_all |> select(article_id, article_body),
        by = "article_id"
      )
  }
  review_data <- review_data |>
    left_join(human_summary, by = c("article_headline" = "article_header")) |>
    mutate(
      human_count = replace_na(human_count, 0L),
      human_assessment = replace_na(human_assessment, "No human review")
    ) |>
    mutate(
      sort_key = case_when(
        is.na(reports_damage) ~ 1L,
        confidence == "low" ~ 2L,
        confidence == "medium" ~ 3L,
        TRUE ~ 4L
      )
    ) |>
    arrange(sort_key, article_id) |>
    mutate(
      review_reason = case_when(
        is.na(reports_damage) ~ "Classification failed",
        confidence == "low" ~ "The LLM has low confidence in its assessment",
        human_assessment == "Disagreement" ~ "The humans disagree with each other",
        (human_assessment == "Relevant" & reports_damage == FALSE) |
          (human_assessment == "Not relevant" & reports_damage == TRUE) ~
          "The LLM and the humans disagree",
        reports_damage == TRUE & confidence == "medium" ~
          "The LLM has medium confidence (no human review available)",
        TRUE ~ "Flagged for review"
      )
    )

  list(
    review_data = review_data,
    n_articles = nrow(review_data)
  )
}

# --- Decision helpers (Google Sheets + local output update) -------------------

empty_decisions <- function() {
  tibble(article_id = character(), decision = character())
}

sheet_to_decision <- function(value) {
  v <- toupper(trimws(as.character(value)))
  case_when(
    v %in% c("YES", "RELEVANT", "TRUE", "1") ~ "relevant",
    v %in% c("NO", "NOT_RELEVANT", "NOT RELEVANT", "FALSE", "0") ~ "not_relevant",
    TRUE ~ NA_character_
  )
}

decision_to_sheet <- function(value) {
  if (identical(value, "relevant")) "YES" else "NO"
}

load_coder_decisions <- function(coder_id) {
  auth_google_sheets()
  sheet_data <- googlesheets4::read_sheet(SHEET_ID, sheet = 1, col_types = "ccccc")

  # Normalize column names: trim, lowercase, collapse spaces to underscores
  names(sheet_data) <- str_trim(names(sheet_data))
  normalized <- tolower(names(sheet_data)) |>
    str_replace_all("\\s+", "_")
  required <- c("article_id", "coder_id", "headline", "is_relevant", "timestamp")
  for (col in required) {
    idx <- which(normalized == col)
    if (length(idx) == 0) {
      stop(
        "Google Sheet is missing required column '", col, "'. ",
        "Found columns: ", paste(names(sheet_data), collapse = ", ")
      )
    }
    names(sheet_data)[idx] <- col
  }
  sheet_data <- sheet_data |>
    mutate(
      article_id = as.character(article_id),
      coder_id = as.character(coder_id),
      is_relevant = as.character(is_relevant)
    )

  coder_rows <- which(sheet_data$coder_id == coder_id)
  row_map <- set_names(
    as.list(coder_rows + 1L), # +1 for header row
    sheet_data$article_id[coder_rows]
  )

  active_decisions <- sheet_data |>
    filter(coder_id == !!coder_id) |>
    mutate(decision = sheet_to_decision(is_relevant)) |>
    filter(!is.na(decision)) |>
    transmute(article_id, decision) |>
    distinct(article_id, .keep_all = TRUE)

  list(
    decisions = active_decisions,
    row_map = row_map
  )
}

update_filtered_articles <- function(decisions_df, review_data) {
  articles_all <- fromJSON(articles_path)
  class_log <- fromJSON(log_path)
  review_ids <- review_data$article_id
  auto_relevant <- class_log |>
    filter(reports_damage == TRUE, !article_id %in% review_ids) |>
    pull(article_id)
  decided_relevant <- decisions_df |>
    filter(decision == "relevant") |>
    pull(article_id)
  undecided_ids <- setdiff(review_ids, decisions_df$article_id)
  undecided_relevant <- review_data |>
    filter(article_id %in% undecided_ids, reports_damage == TRUE) |>
    pull(article_id)
  all_relevant <- unique(c(auto_relevant, decided_relevant, undecided_relevant))
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
  tags$head(
    tags$script(HTML(
      "function setCookie(name, value, days) {
         var expires = '';
         if (days) {
           var date = new Date();
           date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
           expires = '; expires=' + date.toUTCString();
         }
         document.cookie = name + '=' + (value || '') + expires + '; path=/; SameSite=Lax';
       }
       function getCookie(name) {
         var nameEQ = name + '=';
         var ca = document.cookie.split(';');
         for (var i = 0; i < ca.length; i++) {
           var c = ca[i].trim();
           if (c.indexOf(nameEQ) === 0) return c.substring(nameEQ.length);
         }
         return null;
       }
       $(document).on('shiny:connected', function() {
         var savedCoder = getCookie('coder_id');
         Shiny.setInputValue('cookie_coder_id', savedCoder ? savedCoder : '', {priority: 'event'});
       });
       Shiny.addCustomMessageHandler('save_coder_cookie', function(value) {
         setCookie('coder_id', value, 365);
       });

       // Keyboard shortcuts (Y/N, arrows, J), disabled while typing
       $(document).on('keydown', function(e) {
         if ($(e.target).is('input, textarea, select')) return;

         if (e.key === 'y' || e.key === 'Y') {
           $('#mark_relevant').click();
         } else if (e.key === 'n' || e.key === 'N') {
           $('#mark_irrelevant').click();
         } else if (e.key === 'ArrowLeft') {
           $('#prev_btn').click();
         } else if (e.key === 'ArrowRight') {
           $('#next_btn').click();
         } else if (e.key === 'j' || e.key === 'J') {
           $('#jump_uncoded_btn').click();
         }
       });"
    ))
  ),

  sidebar = sidebar(
    width = 320,
    h5("Coder"),
    selectInput("coder_id", "Your ID", choices = CODERS, selected = CODERS[1]),
    textOutput("coder_status_text"),
    hr(),
    h5("Navigation"),
    p(class = "fw-bold mt-0", textOutput("article_position_text")),

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

    # Navigation buttons
    layout_columns(
      col_widths = c(6, 6),
      actionButton("prev_btn", "\u25c0 Prev", class = "btn-outline-primary btn-sm w-100"),
      actionButton("next_btn", "Next \u25b6", class = "btn-outline-primary btn-sm w-100")
    ),

    actionButton(
      "jump_uncoded_btn", "Next Unreviewed",
      class = "btn-primary btn-sm w-100 mt-2"
    ),

    hr(),

    h5("Progress"),
    uiOutput("progress_display"),

    hr(),
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

    h5("Shortcuts"),
    tags$ul(
      class = "list-unstyled small text-muted",
      tags$li(tags$kbd("Y"), " \u2014 Relevant"),
      tags$li(tags$kbd("N"), " \u2014 Not relevant"),
      tags$li(tags$kbd("\u2190"), " / ", tags$kbd("\u2192"), " \u2014 Prev / Next"),
      tags$li(tags$kbd("J"), " \u2014 Next unreviewed")
    )
  ),

  # Main content
  layout_columns(
    col_widths = c(12),

    # Top row: review reason + AI reasoning (left) + assessments (right)
    layout_columns(
      col_widths = c(8, 4),

      # Review reason + AI reasoning
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          uiOutput("review_reason_ui"),
          uiOutput("decision_badge")
        ),
        card_body(
          div(
            class = "bg-light rounded p-3",
            tags$strong(class = "d-block mb-1 text-muted small", "AI Reasoning"),
            textOutput("reasoning_text")
          )
        )
      ),

      # Assessment card
      card(
        card_header("Assessments"),
        card_body(
          class = "p-2",
          div(
            class = "d-flex justify-content-between align-items-center py-1 border-bottom",
            tags$strong(class = "small", "AI Confidence"),
            uiOutput("confidence_ui")
          ),
          div(
            class = "d-flex justify-content-between align-items-center py-1 border-bottom",
            tags$strong(class = "small", "AI Assessment"),
            uiOutput("ai_badge_ui")
          ),
          div(
            class = "d-flex justify-content-between align-items-center py-1",
            tags$strong(class = "small", "Human Assessment"),
            div(
              class = "text-end",
              uiOutput("human_badge_ui"),
              div(class = "small text-muted", textOutput("human_detail_text"))
            )
          )
        )
      )
    ),

    # Article
    card(
      card_header(h4(class = "mb-0", textOutput("headline_text", inline = TRUE))),
      card_body(
        div(
          class = "mb-3",
          layout_columns(
            col_widths = c(4, 4, 4),
            div(tags$strong("Date: "), textOutput("date_text", inline = TRUE)),
            div(tags$strong("Source: "), textOutput("source_text", inline = TRUE)),
            div(tags$strong("ID: "), textOutput("article_id_text", inline = TRUE))
          )
        ),
        div(
          style = "max-height: 500px; overflow-y: auto;",
          uiOutput("body_html")
        )
      )
    )
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # Load data once per worker without promises/futures.
  app_data <- reactiveVal(NULL)
  observe({
    if (is.null(app_data())) {
      app_data(load_app_data())
    }
  })

  review_data <- reactive({ req(app_data()); app_data()$review_data })
  n_articles <- reactive({ req(app_data()); nrow(review_data()) })

  decisions <- reactiveVal(empty_decisions())
  coder_active <- reactiveVal(FALSE)
  active_coder <- reactiveVal("")
  sheet_row_map <- reactiveVal(list())
  current_idx <- reactiveVal(1L)

  jump_to_first_unreviewed <- function() {
    rd <- review_data()
    decs <- decisions()
    unreviewed <- which(!rd$article_id %in% decs$article_id)
    if (length(unreviewed) > 0) {
      current_idx(unreviewed[1])
    } else {
      current_idx(1L)
    }
  }

  observeEvent(input$cookie_coder_id, {
    cid <- str_squish(null_to_empty(input$cookie_coder_id))
    if (cid %in% CODERS) {
      updateSelectInput(session, "coder_id", selected = cid)
    }
  }, once = TRUE)

  observeEvent(input$coder_id, {
    cid <- str_squish(null_to_empty(input$coder_id))
    if (nchar(cid) == 0 || !(cid %in% CODERS)) return()

    showNotification("Loading your decisions from Google Sheets...", id = "load_coder", duration = NULL)
    tryCatch({
      loaded <- load_coder_decisions(cid)
      decisions(loaded$decisions)
      sheet_row_map(loaded$row_map)
      active_coder(cid)
      coder_active(TRUE)
      session$sendCustomMessage("save_coder_cookie", cid)
      if (!is.null(app_data())) {
        jump_to_first_unreviewed()
      }

      removeNotification("load_coder")
      showNotification(
        paste0("Loaded coder '", cid, "' (", nrow(loaded$decisions), " saved decisions)."),
        type = "message",
        duration = 2
      )
    }, error = function(e) {
      removeNotification("load_coder")
      coder_active(FALSE)
      showNotification(
        paste("Could not load decisions from Google Sheets:", e$message),
        type = "error",
        duration = 8
      )
    })
  }, ignoreInit = FALSE)

  # When app_data loads and coder is already active, jump to first unreviewed
  observe({
    req(app_data(), coder_active())
    jump_to_first_unreviewed()
  })

  output$coder_status_text <- renderText({
    if (coder_active()) {
      paste("Signed in as", active_coder())
    } else {
      "Not connected"
    }
  })

  filtered_indices <- reactive({
    rd <- review_data()
    decs <- decisions()
    idx <- seq_len(n_articles())

    if (input$filter_status != "All") {
      reviewed_ids <- decs$article_id
      if (input$filter_status == "Unreviewed") {
        idx <- idx[!rd$article_id[idx] %in% reviewed_ids]
      } else if (input$filter_status == "Relevant") {
        rel_ids <- decs |> filter(decision == "relevant") |> pull(article_id)
        idx <- idx[rd$article_id[idx] %in% rel_ids]
      } else if (input$filter_status == "Not relevant") {
        irr_ids <- decs |> filter(decision == "not_relevant") |> pull(article_id)
        idx <- idx[rd$article_id[idx] %in% irr_ids]
      }
    }

    if (input$filter_human != "All") {
      idx <- idx[rd$human_assessment[idx] == input$filter_human]
    }

    if (input$filter_confidence != "All") {
      if (input$filter_confidence == "FAILED") {
        idx <- idx[is.na(rd$confidence[idx])]
      } else {
        idx <- idx[!is.na(rd$confidence[idx]) & rd$confidence[idx] == input$filter_confidence]
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

  current_article <- reactive({
    review_data()[current_idx(), ]
  })

  # Current decision
  current_decision <- reactive({
    decs <- decisions()
    aid <- as.character(current_article()$article_id)
    match <- decs |> filter(article_id == aid)
    if (nrow(match) > 0) match$decision[1] else NA_character_
  })

  output$article_selector_ui <- renderUI({
    rd <- review_data()
    fi <- filtered_indices()
    if (length(fi) == 0) {
      return(p(class = "text-muted", "No articles match filters"))
    }
    decs <- decisions()
    choices <- fi
    labels <- map_chr(fi, \(i) {
      aid <- rd$article_id[i]
      headline <- str_trunc(rd$article_headline[i], 35)
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

  output$progress_text <- renderText({
    fi <- filtered_indices()
    paste0("Showing ", length(fi), " of ", n_articles(), " articles")
  })

  output$progress_display <- renderUI({
    rd <- review_data()
    decs <- decisions()
    n_decided <- nrow(decs)
    n_art <- n_articles()
    pct <- round(100 * n_decided / n_art, 1)

    bar_color <- if (pct < 25) {
      "danger"
    } else if (pct < 75) {
      "warning"
    } else {
      "success"
    }

    if (n_decided > 0) {
      # Join decisions onto review_data to compare against LLM and human assessments
      decs_detail <- decs |>
        mutate(article_id = as.character(article_id)) |>
        left_join(
          rd |>
            transmute(
              article_id = as.character(article_id),
              reports_damage,
              human_assessment
            ),
          by = "article_id"
        ) |>
        mutate(
          llm_label = case_when(
            is.na(reports_damage) ~ NA_character_,
            reports_damage ~ "relevant",
            !reports_damage ~ "not_relevant"
          ),
          human_label = case_when(
            human_assessment == "Relevant" ~ "relevant",
            human_assessment == "Not relevant" ~ "not_relevant",
            TRUE ~ NA_character_
          ),
          agrees_llm = !is.na(llm_label) & decision == llm_label,
          agrees_human = !is.na(human_label) & decision == human_label
        )

      n_agree_llm <- sum(decs_detail$agrees_llm, na.rm = TRUE)
      n_agree_human <- sum(decs_detail$agrees_human, na.rm = TRUE)
    } else {
      n_agree_llm <- 0L
      n_agree_human <- 0L
    }

    tagList(
      div(
        class = "progress mb-1",
        style = "height: 10px;",
        div(
          class = paste0("progress-bar bg-", bar_color),
          role = "progressbar",
          style = paste0("width: ", pct, "%;"),
          `aria-valuenow` = n_decided,
          `aria-valuemin` = "0",
          `aria-valuemax` = n_art
        )
      ),
      div(
        class = "small text-muted",
        span(class = "text-primary", paste0(n_agree_llm, " agree with LLM")),
        " \u00b7 ",
        span(class = "text-success", paste0(n_agree_human, " agree with humans")),
        " \u00b7 ",
        span(paste0(n_art - n_decided, " remaining"))
      )
    )
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

  jump_to_next_unreviewed <- function() {
    decs <- decisions()
    decided_ids <- decs$article_id
    fi <- filtered_indices()
    if (length(fi) == 0) return()

    cur_pos <- match(current_idx(), fi)
    if (is.na(cur_pos)) cur_pos <- 0L

    order <- c(seq(cur_pos + 1L, length(fi)), seq_len(cur_pos))

    for (i in order) {
      idx <- fi[i]
      aid <- as.character(review_data()$article_id[idx])
      if (!aid %in% decided_ids) {
        current_idx(idx)
        return(invisible(TRUE))
      }
    }

    showNotification(
      "All articles in the current filter have decisions.",
      type = "message",
      duration = 3
    )
    invisible(FALSE)
  }

  observeEvent(input$jump_uncoded_btn, {
    jump_to_next_unreviewed()
  })

  # --- Decision buttons ---
  persist_decision_to_sheet <- function(article_row, decision_value = NULL, clear = FALSE) {
    req(coder_active())

    aid <- as.character(article_row$article_id)
    ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    sheet_value <- if (clear) "" else decision_to_sheet(decision_value)

    new_row <- tibble(
      article_id = aid,
      coder_id = active_coder(),
      headline = as.character(article_row$article_headline),
      is_relevant = sheet_value,
      timestamp = ts
    )

    row_map <- sheet_row_map()

    if (!is.null(row_map[[aid]])) {
      googlesheets4::range_write(
        ss = SHEET_ID,
        data = new_row,
        sheet = 1,
        range = paste0("A", row_map[[aid]]),
        col_names = FALSE
      )
    } else {
      googlesheets4::sheet_append(ss = SHEET_ID, data = new_row, sheet = 1)
      sheet_data <- googlesheets4::read_sheet(SHEET_ID, sheet = 1, col_types = "ccccc")
      row_map[[aid]] <- nrow(sheet_data) + 1L
      sheet_row_map(row_map)
    }
  }

  make_decision <- function(decision_value) {
    req(coder_active())
    decs <- decisions()
    aid <- as.character(current_article()$article_id)
    decs <- decs |> filter(article_id != aid)
    decs <- bind_rows(decs, tibble(article_id = aid, decision = decision_value))
    decisions(decs)

    tryCatch({
      persist_decision_to_sheet(current_article(), decision_value = decision_value, clear = FALSE)
      n_filtered <- update_filtered_articles(decs, review_data())
      showNotification(
        paste0("Saved to Google Sheets. ", n_filtered, " articles in filtered set."),
        type = "message",
        duration = 2
      )
    }, error = function(e) {
      showNotification(
        paste("Error saving to Google Sheets:", e$message),
        type = "error",
        duration = 8
      )
    })

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
    req(coder_active())
    decs <- decisions()
    aid <- as.character(current_article()$article_id)
    decs <- decs |> filter(article_id != aid)
    decisions(decs)
    tryCatch({
      persist_decision_to_sheet(current_article(), clear = TRUE)
      n_filtered <- update_filtered_articles(decs, review_data())
      showNotification(
        paste0("Cleared in Google Sheets. ", n_filtered, " articles in filtered set."),
        type = "message",
        duration = 2
      )
    }, error = function(e) {
      showNotification(
        paste("Error clearing in Google Sheets:", e$message),
        type = "error",
        duration = 8
      )
    })
  })

  # --- Display outputs ---
  output$article_id_text <- renderText(current_article()$article_id)

  output$review_reason_ui <- renderUI({
    reason <- current_article()$review_reason
    cls <- switch(reason,
      "Classification failed" = "bg-dark text-white",
      "The LLM has low confidence in its assessment" = "text-white",
      "The humans disagree with each other" = "text-white",
      "The LLM and the humans disagree" = "text-white",
      "The LLM has medium confidence (no human review available)" = "text-dark",
      "text-dark"
    )
    bg_style <- switch(reason,
      "Classification failed" = "",
      "The LLM has low confidence in its assessment" = "background-color: #6f42c1;",
      "The humans disagree with each other" = "background-color: #d63384;",
      "The LLM and the humans disagree" = "background-color: #e35d00;",
      "The LLM has medium confidence (no human review available)" = "background-color: #ffc870;",
      "background-color: #adb5bd;"
    )
    tags$span(
      class = paste("badge fs-6", cls),
      style = bg_style,
      icon("triangle-exclamation"),
      reason
    )
  })

  output$article_position_text <- renderText({
    fi <- filtered_indices()
    pos <- match(current_idx(), fi)
    decs <- decisions()

    paste0("Article ", pos, " of ", length(fi), " | ",
           nrow(decs), " decided total")
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
      paste0(art$human_count, " coder(s): ", paste(parts, collapse = ", "))
    }
  })

  render_decision_badge <- function() {
    dec <- current_decision()
    if (is.na(dec)) {
      tags$span(class = "badge bg-secondary fs-6", "Unreviewed")
    } else if (dec == "relevant") {
      tags$span(class = "badge bg-success fs-6", "\u2713 Relevant")
    } else {
      tags$span(class = "badge bg-danger fs-6", "\u2717 Not Relevant")
    }
  }

  output$decision_badge <- renderUI(render_decision_badge())

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
shinyApp(ui, server)