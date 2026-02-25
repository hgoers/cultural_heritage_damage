# Cultural Heritage Event Dataset (CHEV) Pipeline

CHEV is a research pipeline that turns unstructured newspaper archives into a structured, auditable dataset of reported damage to cultural heritage sites in Ukraine during the Russia-Ukraine war (2022-present).

The repository is built for two goals at once:
- Produce a high-quality event dataset that can support empirical analysis.
- Preserve traceability from each final event back to the exact source articles and coding decisions.

## Quickstart in 5 commands

```bash
cd /path/to/cultural_heritage_damage
printf "ANTHROPIC_API_KEY=sk-ant-...\n" >> .Renviron
mkdir -p data-raw data
# Place LexisNexis .DOCX files into data-raw/
Rscript scripts/run_pipeline.R
```

After the run, check `data/04_damage_events.json` for final event-level output.

## Why this project matters

Large-scale conflict reporting often captures cultural loss in narrative form, scattered across thousands of articles. CHEV addresses that gap by combining deterministic preprocessing with structured LLM coding and schema validation.

Key contributions:
- A full end-to-end pipeline from raw LexisNexis DOCX exports to deduplicated event-level JSON.
- A transparent coding framework for defining and identifying heritage damage events.
- Multi-stage quality controls, including schema validation, logs, confidence flags, and human-review checkpoints.
- Event construction that preserves uncertainty and disagreement across sources instead of flattening it away.

## What makes CHEV technically complex

This is not a simple scraper or one-shot LLM extraction. The pipeline handles multiple difficult tasks:
- Parsing messy DOCX exports with inconsistent heading styles and metadata formatting.
- Multi-pass deduplication at the article level (exact matches, fuzzy headline matches, syndicated text detection).
- High-volume classification and structured extraction with model prompts tied to a formal damage definition and codebook.
- Geocoding noisy location text with caching, fallback queries, and geographic sanity checks.
- Event-level deduplication that merges partially conflicting reports while preserving source provenance.

## Conceptual approach

The project uses one stable definition across all coding steps:

> Evidence of activity by an actor that damages a culturally significant location over a specific temporal duration.

Operationally, CHEV treats data production as a staged reduction problem:
1. Parse and clean all articles.
2. Classify relevance to heritage damage.
3. Extract structured damage-event records from relevant articles.
4. Geocode extracted locations.
5. Deduplicate article-level records into unique events.

Each stage produces machine-readable outputs and logs to support replication, diagnostics, and auditability.

## Pipeline overview

### Step 0: Shared setup (`scripts/00_setup.R`)
- Installs/loads required packages.
- Configures project paths and schema locations.
- Reads `ANTHROPIC_API_KEY` from `.Renviron`.
- Defines helper utilities and shared constants.

### Step 1: Parse LexisNexis DOCX files (`scripts/01_parse.R`)
Input: `data-raw/*.DOCX`
Output: `data/01_articles.json`
- Extracts article metadata and body text.
- Standardizes fields and dates.
- Applies three deduplication passes.
- Assigns `ART_XXXX` IDs.
- Validates against `schemas/schema_newspaper.json`.

### Step 2: Classify relevant articles (`scripts/02_classify.R`)
Input: `data/01_articles.json`
Outputs:
- `data/02_articles_filtered.json`
- `data/02_classification_log.json`
- `data/02_review_needed.json`

- Uses Claude 3.5 Haiku for binary relevance classification.
- Returns structured decisions with confidence and rationale.
- Flags borderline cases for review.

### Step 3: Extract article-level damage events (`scripts/03_extract.R`)
Input: `data/02_articles_filtered.json`
Outputs:
- `data/03_article_damage_events.json`
- `data/03_extraction_log.json`

- Uses Claude Sonnet 4 to extract one or more event records per article.
- Applies a detailed codebook (site type, damage type, temporal/geographic precision, verification quality).
- Assigns `REC_XXXX` IDs and validates schema compliance.

### Step 3b: Geocode extracted locations (`scripts/05_geocode.R`)
Input: `data/03_article_damage_events.json`
Outputs:
- Updated `data/03_article_damage_events.json`
- `data/05_geocode_cache.json`
- `data/05_geocode_failed.json`

- Geocodes via OpenStreetMap Nominatim (`tidygeocoder`).
- Uses fallback query strategies and request throttling.
- Updates `geo_prec` only when geocoding improves location precision.

### Step 4: Deduplicate to unique events (`scripts/04_deduplicate.R`)
Input: `data/03_article_damage_events.json`
Outputs:
- `data/04_damage_events.json`
- `data/04_dedup_log.json`
- `data/04_review_needed.json`

- Clusters candidate duplicates with fuzzy matching + metadata rules.
- Uses Claude Sonnet 4 for ambiguous cluster adjudication.
- Merges records into event-level observations with `EVT_XXXX` IDs.
- Preserves article-level provenance in a nested `articles` array.

## Project structure

```text
cultural_heritage_damage/
|-- data/                           # Pipeline outputs and logs
|-- data-raw/                       # LexisNexis DOCX inputs (not committed)
|-- human-coding/                   # Manual review artifacts
|-- schemas/                        # JSON schemas for each stage
|-- scripts/                        # Pipeline scripts and dashboard
|-- README.md
```

Core schemas:
- `schemas/schema_newspaper.json`
- `schemas/schema_newspaper_damage_event.json`
- `schemas/schema_damage_event.json`

## Replication guide (for first-time users)

### 1. Prerequisites
- R 4.1+
- Internet access (Anthropic API + OSM geocoding)
- Anthropic API key with access to `claude-3-5-haiku-20241022` and `claude-sonnet-4-20250514`

### 2. Place required input data
Put LexisNexis monthly DOCX exports into:
- `data-raw/`

Expected naming pattern:
- `MM-YYYY.DOCX`
- Split-month variants are supported (for example `03-2022(1).DOCX`, `03-2022(2).DOCX`).

### 3. Configure credentials
Create or edit `.Renviron` in the repository root and add:

```bash
ANTHROPIC_API_KEY=sk-ant-...
```

Restart R/session after editing `.Renviron`.

### 4. Run the full pipeline

```bash
Rscript scripts/run_pipeline.R
```

Useful run modes:

```bash
Rscript scripts/run_pipeline.R --from 3
Rscript scripts/run_pipeline.R --only 2
Rscript scripts/run_pipeline.R --skip-review
TEST_N=100 Rscript scripts/run_pipeline.R
```

### 5. Review checkpoints
By default, the orchestrator pauses after key stages when review files exist:
- `data/02_review_needed.json`
- `data/04_review_needed.json`

Use `--skip-review` for uninterrupted runs.

### 6. Validate outputs
Primary expected outputs:
- `data/01_articles.json`
- `data/02_articles_filtered.json`
- `data/03_article_damage_events.json`
- `data/04_damage_events.json`

Diagnostic/audit outputs:
- `data/02_classification_log.json`
- `data/03_extraction_log.json`
- `data/04_dedup_log.json`
- `data/05_geocode_cache.json`
- `data/05_geocode_failed.json`

## Reproducibility and auditability

CHEV includes explicit reproducibility features:
- JSON-schema validation at each major stage.
- Deterministic ID generation (`ART_`, `REC_`, `EVT_`).
- Cached geocoding to stabilize reruns and reduce API variability.
- Structured model outputs (rather than free-form text extraction).
- Full logs for classification, extraction, and deduplication decisions.

Important caveat:
- LLM-based stages are probabilistic. Re-running Steps 2-4 may produce small differences even with the same inputs.

## Practical notes

- The most time- and cost-intensive stages are classification, extraction, and deduplication (API calls).
- Geocoding is rate-limited by design to respect Nominatim usage requirements.
- If you are iterating on prompts or code, use `TEST_N=100` before full-scale runs.

## Citation and use

If you use this repository to build analysis or derivative data products, cite both:
- The CHEV pipeline repository.
- The original source corpus (LexisNexis exports) and model/geocoding services used.
