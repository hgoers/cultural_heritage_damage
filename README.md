# Cultural Heritage Event Dataset (CHEV) Pipeline

An R pipeline that constructs a structured dataset of reported damage to cultural heritage sites in Ukraine during the Russia-Ukraine war (2022–present) from LexisNexis newspaper archives.

## Overview

The pipeline processes 39 LexisNexis DOCX exports (monthly batches from January 2022 through December 2024) through five automated stages: parsing, classification, extraction, geocoding, and deduplication. The final output is a JSON dataset of unique damage events conforming to a predefined schema, where each event aggregates evidence from one or more newspaper articles.

### Definition of a damage instance

All classification and extraction steps apply this definition consistently:

> **"Evidence of activity by an actor that damages a culturally significant location over a specific temporal duration."**

- **Evidence**: The occurrence or product of fighting, or a destructive act (e.g., church hit by shelling; mosque set ablaze).
- **Actor**: A state or non-state formally or informally organized group.
- **Damage**: Actors using manufactured weapons or engaged in actions directed against human life or property.
- **Culturally significant location**: Any site of cultural interest — libraries, archives, archaeological sites, cemeteries, cultural centres, historic structures, monuments, statues, museums, religious sites, shrines, etc.
- **Specific temporal duration**: The start and end date of an event. The smallest temporal unit is a calendar day.

## Pipeline stages

### Step 0: Project setup (`scripts/00_setup.R`)

Shared configuration sourced by all pipeline scripts:

- **Packages**: `tidyverse`, `jsonlite`, `jsonvalidate`, `officer`, `ellmer`, `tidygeocoder`, `stringdist`, `cli`
- **API access**: Anthropic API key loaded from `.Renviron` (not committed to version control)
- **ID schemes**: `ART_XXXX` (articles), `REC_XXXX` (article-damage records), `EVT_XXXX` (unique events)
- **Schemas**: Three JSON schemas in `schemas/` define the structure at each stage
- **Damage definition**: The definition above is stored as a constant and injected into all LLM system prompts

### Step 1: Parse & clean DOCX files (`scripts/01_parse.R`)

**Input**: 39 DOCX files in `data-raw/` (monthly LexisNexis exports, named `MM-YYYY.DOCX`; some months split into parts, e.g., `03-2022(1).DOCX`, `03-2022(2).DOCX`)

**Output**: `data/01_articles.json` — array of article objects conforming to `schemas/schema_newspaper.json`

**Process**:

1. **Custom DOCX parser** using `officer::read_docx()`. The standard `LexisNexisTools::lnt_read()` function was tested but found to miss the first article in each file because its heading style (`heading 1`) differs from subsequent articles (`Heading 1_N`). The custom parser splits document content on all heading-1 style variants.
2. **Metadata extraction**: For each article block, the parser extracts the headline (heading text), newspaper name (first body paragraph), publication date (parsed from the second body paragraph, with fallback to the filename `MM-YYYY` pattern), and article body (remaining paragraphs joined).
3. **Three-pass deduplication**:
   - **Pass 1 — Exact duplicates**: Removes articles with identical headline + date + source.
   - **Pass 2 — Near-duplicate headlines**: Within each date, compares headlines using Jaro-Winkler string similarity (threshold > 0.95). Removes the later occurrence.
   - **Pass 3 — Syndicated/cross-published articles**: Within each date, compares the first 300 lowercased characters of the article body. Keeps only the first article per body-text cluster, catching wire stories republished by multiple outlets under different headlines.
4. **Field mapping**: LexisNexis fields are mapped to schema fields (`Headline` → `article_headline`, `Date` → `article_date` in `YYYY-MM-DD`, `Newspaper` → `article_source`, body text → `article_body`).
5. **ID generation**: Articles are ordered by date then source, and assigned sequential `ART_XXXX` IDs.
6. **Validation**: Each record is validated against `schema_newspaper.json` using `jsonvalidate`.

**Output size**: ~6,600 unique articles after deduplication.

### Step 2: Classify relevant articles (`scripts/02_classify.R`)

**Input**: `data/01_articles.json`

**Output**:
- `data/02_articles_filtered.json` — articles classified as relevant (same schema as Step 1)
- `data/02_classification_log.json` — all articles with classification metadata (for auditability)
- `data/02_review_needed.json` — borderline cases for human review

**Model**: **Claude 3.5 Haiku** (`claude-3-5-haiku-20241022`) via `ellmer::chat_anthropic()` — selected for cost-efficiency on a binary classification task.

**Process**:

1. A detailed system prompt defines relevance criteria based on the damage definition, lists the 18 cultural heritage site types from the schema, and enumerates explicit exclusion rules. The prompt was iteratively refined through multiple rounds of human review (see below).
2. For each article, the LLM receives the headline and first ~2,000 characters of the body, and returns a structured response:
   - `reports_damage`: boolean
   - `confidence`: "high", "medium", or "low"
   - `reasoning`: free-text explanation
3. Articles are processed in parallel using `ellmer::parallel_chat_structured()`.
4. Articles where `reports_damage == TRUE` are written to the filtered output.
5. Borderline cases (`confidence == "medium"` or `"low"` with `reports_damage == TRUE`) are flagged for human review.

**Prompt refinement**: The classification prompt was refined through four rounds of iteration:

1. **Initial prompt**: Basic relevance classification.
2. **Added exclusions** for damage to intangible heritage (art forms, cultural practices) and proximity-only damage (damage near but not to a heritage site).
3. **Added exclusions** for threat/risk-only articles, broad non-specific references, and regional damage without specific sites named.
4. **Human review via Shiny dashboard** (`scripts/review_dashboard.R`): 46 borderline decisions were manually reviewed, and patterns identified led to four additional exclusions — aggregate statistics, exhibition/artist profiles, government monument modifications (decommunization), and unnamed sites.

**Retry mechanism** (`scripts/02b_retry_failures.R`): Any articles that fail classification due to API errors are retried up to 3 times with a 5-second delay between attempts.

**Test mode**: Setting `TEST_N=100` samples 100 articles for rapid iteration (seed = 789 for reproducibility).

### Step 3: Extract damage event details (`scripts/03_extract.R`)

**Input**: `data/02_articles_filtered.json`

**Output**:
- `data/03_article_damage_events.json` — array of article-damage-event pairs conforming to `schemas/schema_newspaper_damage_event.json`
- `data/03_extraction_log.json` — per-article extraction metadata

**Model**: **Claude Sonnet 4** (`claude-sonnet-4-20250514`) via `ellmer::chat_anthropic()` — selected for its stronger reasoning capability needed for nuanced structured extraction.

**Process**:

1. The system prompt includes the full codebook for all coded fields:
   - **Site type** (18 categories with sub-types for places of worship and burial)
   - **Damage type** (9 codes, pipe-delimited for multiple types, e.g., `"3|4"` for shelling + airstrike)
   - **Temporal precision** (`temp_prec`): 0 = not applicable, 1 = exact day, 2 = 2–6 days, 3 = week, 4 = month, 5 = year
   - **Geographic precision** (`geo_prec`): 1 = exact site, 2 = city, 3 = district, 4 = oblast, 5 = section, 6 = country
   - **Verification quality**: 1 = minimal, 2 = moderate, 3 = strong, 4 = investigative
2. For each article, the LLM extracts an **array** of damage events (one article may report damage to multiple sites). Each event record includes:
   - Site identification (`site_name`, `site_type`, sub-types)
   - Location (`location_text` for geocoding, `country`)
   - Temporal data (`year`, `event_date_start`, `event_date_end`, `temp_prec`)
   - Damage details (`damage_type`, `initiator`, `group_at_site`, `site_dmg_smry`, `site_dmg_full`)
   - Source quality (`reported_source`, `verification_quality`, `article_excerpt`)
3. Article metadata (`article_id`, `article_date`, `article_source`, `article_headline`) is carried forward onto each extracted record.
4. Records are assigned sequential `REC_XXXX` IDs and validated per-record against the schema.

**Output structure**: The unit of observation is an **article–damage event pair** — a single article reporting damage to three sites produces three records, all sharing the same `article_id`.

### Step 3b: Geocode locations (`scripts/05_geocode.R`)

**Input**: `data/03_article_damage_events.json`

**Output**:
- `data/03_article_damage_events.json` — updated in place with `event_latitude`, `event_longitude`, and refined `geo_prec`
- `data/05_geocode_cache.json` — cached geocoding results (reused on re-runs)
- `data/05_geocode_failed.json` — failed lookups for manual review

**Geocoding service**: **OpenStreetMap Nominatim** via `tidygeocoder::geocode(method = "osm")`.

**Process**:

1. Unique `(site_name, location_text, country)` tuples are collected from the extraction output.
2. Previously geocoded locations are loaded from the cache to avoid duplicate API calls.
3. For each new location, up to three geocoding attempts are made:
   - **Attempt 1**: Full query — `"{site_name}, {location}, Ukraine"`
   - **Attempt 2**: Fallback — `"{location}, Ukraine"` (drops the site name)
   - **Attempt 3**: Simple — `"{site_name}, Ukraine"` (drops the location context)
4. Nominatim's response `type` field is mapped to a `geo_prec` code (e.g., `building` → 1, `city` → 2, `administrative` → 3). The `geo_prec` is only updated if the OSM result is more precise than the LLM's original estimate.
5. **Ukraine bounding box validation**: Coordinates outside ~44.2–52.4°N, ~22.1–40.2°E are nullified.
6. Rate limiting: 1 request per 1.1 seconds to comply with Nominatim usage policy.

### Step 4: Deduplicate to unique events (`scripts/04_deduplicate.R`)

**Input**: `data/03_article_damage_events.json` (geocoded)

**Output**:
- `data/04_damage_events.json` — array of unique damage events conforming to `schemas/schema_damage_event.json`
- `data/04_dedup_log.json` — clustering and adjudication decisions
- `data/04_review_needed.json` — events with conflicting reports or low verification for human review

**Models**: **Claude Sonnet 4** (`claude-sonnet-4-20250514`) for adjudicating ambiguous clusters.

**Process**:

1. **Site name normalisation**: Names are lowercased and common filler words stripped (`museum`, `of`, `national`, `the`, etc.) to improve fuzzy matching.
2. **Candidate clustering** using union-find: Two records are candidate duplicates if they share the same `year` AND either (a) Jaro-Winkler similarity on normalised names ≥ 0.75, or (b) same `location_text` and same `site_type`.
3. **LLM adjudication**: For clusters with multiple records, Claude is presented with all records' details (site name, type, location, dates, damage description, source article) and asked to determine whether they describe the same physical damage event. The LLM returns:
   - `same_event`: boolean
   - `reasoning`: explanation
   - `sub_groups`: if not the same event, which records belong together
4. **Field merging** for each unique event:
   - `event_date_start` / `event_date_end` → earliest start, latest end across articles (default: conflict start `2022-02-24` if no date known)
   - `temp_prec`, `geo_prec` → best (lowest = most precise) across articles
   - Coordinates → from the record with the most precise `geo_prec`
   - `damage_type` → union of all codes (pipe-delimited)
   - `initiator`, `group_at_site` → concatenated across articles (semicolon-separated)
   - `site_name`, `site_type`, sub-types → from the most detailed record
   - `site_dmg_smry`, `site_dmg_full` → from the most detailed record (or concatenated if multiple articles differ)
   - `articles` → nested array preserving all per-article details
   - Derived fields: `num_articles`, `earliest_article_date`, `latest_article_date`, `primary_article_source`, `article_sources_list`
5. **Verification level** derived from article count and max verification quality:
   - Level 1: single article, minimal verification
   - Level 2: single high-quality article
   - Level 3: multiple articles or single strongly-sourced article
   - Level 4: 3+ articles with strong sourcing
6. **Conflict detection**: Events are flagged with `conflicting_reports = TRUE` if articles disagree on initiator, damage type, or report event dates spanning > 30 days.
7. Events are sorted by date and assigned sequential `EVT_XXXX` IDs.

## Orchestration (`scripts/run_pipeline.R`)

A master script runs all steps in sequence with logging, dependency checks, and validation:

```bash
# Full pipeline
Rscript scripts/run_pipeline.R

# Resume from step 3
Rscript scripts/run_pipeline.R --from 3

# Run only step 2 (and sub-steps)
Rscript scripts/run_pipeline.R --only 2

# Skip interactive review pauses
Rscript scripts/run_pipeline.R --skip-review

# Test mode (100-article sample)
TEST_N=100 Rscript scripts/run_pipeline.R
```

**Human review checkpoints** pause the pipeline after Steps 2b and 4 (in interactive mode) so flagged items can be reviewed. In non-interactive mode the pipeline prints a resume command and exits.

**Post-pipeline sanity checks**:
- Row count expectations: Step 1 > Step 2; Step 3 ≥ Step 2; Step 4 ≤ Step 3
- Per-record schema validation of all final output files

## Project structure

```
cultural_heritage_damage/
├── data-raw/                       # 39 LexisNexis DOCX exports (not committed)
│   ├── 01-2022.DOCX
│   ├── ...
│   └── 12-2024.DOCX
├── data/                           # Pipeline outputs
│   ├── 01_articles.json            # Parsed articles (~6,600)
│   ├── 02_articles_filtered.json   # Relevant articles
│   ├── 02_classification_log.json  # All classification decisions
│   ├── 02_review_needed.json       # Borderline articles for review
│   ├── 03_article_damage_events.json  # Article-damage pairs (geocoded)
│   ├── 03_extraction_log.json      # Extraction metadata
│   ├── 04_damage_events.json       # Final unique damage events
│   ├── 04_dedup_log.json           # Deduplication decisions
│   ├── 04_review_needed.json       # Events with conflicts
│   ├── 05_geocode_cache.json       # Geocoding cache
│   └── 05_geocode_failed.json      # Failed geocoding lookups
├── schemas/
│   ├── schema_newspaper.json                # Article schema
│   ├── schema_newspaper_damage_event.json   # Article-damage-event pair schema
│   └── schema_damage_event.json             # Unique damage event schema
├── scripts/
│   ├── 00_setup.R                  # Shared config and utilities
│   ├── 01_parse.R                  # Step 1: DOCX parsing
│   ├── 02_classify.R               # Step 2: LLM classification
│   ├── 02b_retry_failures.R        # Step 2b: Retry failed classifications
│   ├── 03_extract.R                # Step 3: LLM extraction
│   ├── 04_deduplicate.R            # Step 4: Deduplication and merging
│   ├── 05_geocode.R                # Step 3b: Geocoding
│   ├── review_dashboard.R          # Shiny app for reviewing borderline cases
│   └── run_pipeline.R              # Orchestrator
├── .Renviron                       # API key (not committed)
├── .gitignore
└── README.md
```

## Models used

| Step | Model | Model ID | Purpose |
|------|-------|----------|---------|
| 2 (Classify) | Claude 3.5 Haiku | `claude-3-5-haiku-20241022` | Binary relevance classification |
| 2b (Retry) | Claude 3.5 Haiku | `claude-3-5-haiku-20241022` | Retry failed classifications |
| 3 (Extract) | Claude Sonnet 4 | `claude-sonnet-4-20250514` | Structured damage event extraction |
| 4 (Deduplicate) | Claude Sonnet 4 | `claude-sonnet-4-20250514` | Adjudicating duplicate clusters |

All models are accessed via the `ellmer` R package (`ellmer::chat_anthropic()`) using structured output (`$chat_structured()` and `parallel_chat_structured()`). The API key must be set as `ANTHROPIC_API_KEY` in `.Renviron`.

## Schemas

Three JSON schemas (JSON Schema draft 2020-12) define the data structure at each pipeline stage:

- **`schema_newspaper.json`**: Article-level records with `article_id`, `article_date`, `article_source`, `article_headline`, `article_body`.
- **`schema_newspaper_damage_event.json`**: Article–damage event pairs. Unit of observation is one damage event as reported by one article. Includes full codebooks for site type (18 codes), damage type (9 codes), temporal precision (6 codes), geographic precision (7 codes), and verification quality (4 codes).
- **`schema_damage_event.json`**: Unique damage events. Merges multiple article reports into a single record with nested `articles` array, derived fields (`num_articles`, `verification_level`, `conflicting_reports`), and synthesised descriptions.

## Requirements

- **R** ≥ 4.1 with packages: `tidyverse`, `jsonlite`, `jsonvalidate`, `officer`, `ellmer`, `tidygeocoder`, `stringdist`, `cli`, `shiny`, `bslib`
- **Anthropic API key** with access to Claude 3.5 Haiku and Claude Sonnet 4
- **Internet access** for Nominatim geocoding (Step 3b)

## Reproducibility notes

- Classification uses `set.seed(789)` when `TEST_N` is set for reproducible sub-sampling.
- Geocoding results are cached in `data/05_geocode_cache.json`; re-runs skip previously resolved locations.
- All LLM calls use structured output to enforce schema-compatible responses, but LLM responses are non-deterministic. Re-running Steps 2–4 may produce slightly different results.
- The deduplication log (`data/04_dedup_log.json`) records Claude's reasoning for each cluster adjudication for full auditability.
