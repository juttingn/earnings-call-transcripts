# Earnings Call Transcript Scraper

An R-based web scraper that collects earnings call transcripts from
[investing.com/news/transcripts](https://www.investing.com/news/transcripts)
and structures them into a tidy dataset for text analysis.

Earnings calls provide a rich source of firm-level information typically occurring every quarter. Following a
presentation of the company's earnings, external analysts can ask questions in a Q&A session,
on both backward- and forward-looking matters they would like to get more details on. The transcripts can be key insights into the most pressing issues companies are facing, and have been used to create indices of firm-level exposure to different topics over time.

This project directly relates to my Master Thesis, which you can find [here](https://juttingn.github.io/personal-site/thesis.pdf), where I used company earning calls to estimate the geoeconomic risk exposure companies have been facing over time, and looked at how said exposure affected firm's investment response to Russia's invasion of Ukraine.

This scraper is a first step to deepen the analysis of my Master Thesis by recovering the full transcripts of the earnings calls, which were previously not accesible to me, and were instead accessed through the NL Analytics platform. The main disadvantage of this platform is that it only allows you to run a dictionary-based keyword query on the transcripts. By gaining full access to the transcripts, I pkan to use more sophisticated Large Language Model classification techniques in my analysis to further disentangle between the different types of geoeconomic risk companies are facing.

---

## Table of Contents

- [Overview](#overview)
- [Requirements](#requirements)
- [Installation](#installation)
- [Scripts](#scripts)
  - [Interactive scraper](#1-interactive-scraper-recommended)
  - [Quick test scraper](#2-quick-test-scraper)
  - [Full bulk scraper](#3-full-bulk-scraper)
- [Output format](#output-format)
- [Project structure](#project-structure)
- [How it works](#how-it-works)
- [Notes](#notes)

---

## Overview

investing.com serves its transcript pages behind Cloudflare, so plain HTTP
requests are blocked. This project drives a real Google Chrome browser via the
[chromote](https://github.com/rstudio/chromote) package, waits for Cloudflare
to resolve, and then parses the rendered HTML.

The result is a data frame with **one row per speaker turn**, making the
dataset immediately ready for NLP pipelines, sentiment analysis, or topic
modelling.

---

## Requirements

| Dependency | Version | Notes |
|---|---|---|
| R | ≥ 4.1 | |
| Google Chrome | any recent | path must match `CHROME_PATH` in the scripts |
| chromote | CRAN | headless Chrome bridge |
| rvest | CRAN | HTML parsing |
| dplyr | CRAN | data manipulation |
| stringr | CRAN | string helpers |
| lubridate | CRAN | date arithmetic |
| jsonlite | CRAN | JSON-LD parsing |

---

## Installation

```r
# Install all required packages in one step
install.packages(c("chromote", "rvest", "dplyr", "stringr", "lubridate", "jsonlite"))
```

If Google Chrome is not installed at the default path
(`/Applications/Google Chrome.app/…` on macOS), update the `CHROME_PATH`
constant near the top of whichever script you run.

---

## Scripts

### 1. Interactive scraper (recommended)

**`scrape_earnings_calls_interactive.R`**

Prompts you for three parameters (transcript count, date range, and optional
company filter) then scrapes in two phases: a fast listing-page scan followed
by parallel article scraping across multiple Chrome workers.

> **Important:** this script must be run from a **Terminal** (macOS Terminal,
> iTerm2, etc.), not from RStudio's Source button. The interactive prompts rely
> on terminal stdin and will not work when sourced inside an IDE.

#### Step 1 — open a Terminal and navigate to the project folder

```bash
cd /path/to/earnings-call-transcripts
```

#### Step 2 — confirm you are using the correct Rscript

```bash
which Rscript
```

The path should point to your main R installation (e.g. `/usr/local/bin/Rscript`).
If it resolves to an Anaconda or conda-managed path instead, call Rscript by
its full path in the next step (e.g. `/usr/local/bin/Rscript`).

#### Step 3 — run the script

```bash
Rscript scrape_earnings_calls_interactive.R
```

Or, if you need to specify the full path to Rscript:

```bash
/usr/local/bin/Rscript scrape_earnings_calls_interactive.R
```

The script will print a banner and prompt you for three inputs:

```
╔══════════════════════════════════════════════════════════════════╗
║      Earnings Call Transcript Scraper — investing.com            ║
╚══════════════════════════════════════════════════════════════════╝

  Note: investing.com transcripts are available from Q1 2025 onward.

  How many transcripts to collect? [default: 50] > 100

  Date range (e.g. Q1 2025 - Q4 2025). Earliest available: Q1 2025.
  Press Enter to collect the most recent transcripts > Q1 2025 - Q4 2025

  Company name filter — use the name exactly as it appears in article
  titles on investing.com (e.g. 'Apple', 'JPMorgan', 'Tesla').
  Press Enter to collect transcripts from all companies > Apple

  Will collect up to 100 transcripts published between 2025-01-01 and 2025-12-31 for company matching "Apple".
  Output will be saved to: earning_call_transcripts.rds

  Start scraping? [Y/n] >
```

#### Running from RStudio (non-interactive alternative)

If you prefer to run from RStudio, open the script and set your parameters
directly in the `USER CONFIGURATION` block near the top — the prompts will be
skipped automatically:

```r
N_TRANSCRIPTS_CONFIG  <- 100                   # max transcripts to collect
DATE_RANGE_CONFIG     <- "Q1 2025 - Q4 2025"  # or NA for most-recent
COMPANY_FILTER_CONFIG <- "Apple"               # or NA for all companies
```

Then source the file as usual.

**Date range format:** `Q1 YYYY - Q4 YYYY`

- `Q1` = January – March
- `Q2` = April – June
- `Q3` = July – September
- `Q4` = October – December

Because investing.com lists transcripts in reverse-chronological order, the
scraper stops automatically once it has encountered several consecutive articles
older than the requested start date. This makes date-range queries efficient
without requiring a full scan of all pages.

---

### 2. Quick test scraper

**`scrape_earnings_calls.R`**

A self-contained script for quickly testing that everything works.
Edit `N_TRANSCRIPTS` at the top to control how many articles are fetched
(default: 50). No checkpointing — just runs straight through and saves one RDS.

```
Rscript scrape_earnings_calls.R
```

---

### 3. Full bulk scraper

**`scrape_earnings_calls_full.R`**

Designed to collect the entire archive (~10,000 transcripts across ~286
listing pages). Key features:

- **Checkpointing** — each transcript is immediately saved to
  `transcripts_raw/<id>.rds`; the current listing-page number is written to
  `checkpoint_page.txt`. Killing the process and re-running picks up exactly
  where it left off.
- **Memory management** — Chrome is restarted every 150 articles.
- **Periodic combines** — the final `earning_call_transcripts.rds` is rebuilt
  every 200 new articles and again at the end.

```bash
# Run / resume
Rscript scrape_earnings_calls_full.R

# Only rebuild the combined RDS from already-downloaded raw files
Rscript scrape_earnings_calls_full.R --combine
```

---

## Output format

All scripts produce the same RDS schema:

| Column | Type | Description |
|---|---|---|
| `url` | character | Full article URL |
| `article_title` | character | H1 headline of the article |
| `company_name` | character | Company name parsed from the transcript heading |
| `ticker` | character | Stock ticker symbol (e.g. `AAPL`) |
| `quarter` | character | Fiscal quarter (e.g. `Q3`) |
| `call_year` | integer | Fiscal year (e.g. `2024`) |
| `call_date` | Date | Publication date from JSON-LD structured data |
| `speaker` | character | Speaker's name |
| `speaker_role` | character | Speaker's title or role (e.g. `"Chief Executive Officer"`) |
| `speaker_company` | character | Company or firm the speaker represents (e.g. `"JP Morgan"`); `NA` if not stated |
| `text` | character | Verbatim speech for that turn |

One transcript typically produces 40–90 rows (one per speaker intervention).

**Reading the output in R:**

```r
df <- readRDS("earning_call_transcripts.rds")

# Quick summary
dplyr::count(df, ticker, quarter, call_year) |> head(10)
```

---

## Project structure

```
.
├── scrape_earnings_calls_interactive.R  # Interactive scraper (prompts user)
├── scrape_earnings_calls.R              # Quick test / small-scale scraper
├── scrape_earnings_calls_full.R         # Full bulk scraper with checkpointing
└── README.md
```

Files generated at runtime (excluded from version control):

```
earning_call_transcripts.rds   # combined output dataset
transcripts_raw/               # one .rds per transcript (full scraper only)
checkpoint_page.txt            # listing-page resume pointer
scrape_errors.txt              # error log
```

---

## How it works

1. **Browser launch** — `chromote` opens a headless Google Chrome instance with
   a desktop user-agent string to appear as a regular browser visit.

2. **Cloudflare bypass** — after each navigation the script polls
   `document.title` until the Cloudflare challenge page is gone (typically
   8–10 seconds).

3. **Listing pages** — investing.com paginates its transcript index at
   `…/news/transcripts`, `…/news/transcripts/2`, etc. The scraper iterates
   these pages to discover article URLs.

4. **Article parsing** — for each article the scraper:
   - Extracts the publication date from the page's
     [JSON-LD](https://schema.org/) structured data block.
   - Locates the `<h2>` heading that matches
     `"Full transcript - Company (TICKER) QN YYYY:"` to derive company name,
     ticker, quarter, and year.
   - Collects all `<p>` elements after that heading; each paragraph with a
     `<b>` child is a speaker turn (`<b>Name, Role</b><span>: text</span>`).

5. **Output** — rows are assembled into a tidy tibble and saved as an RDS file.

---

## Notes

- **Rate limiting** — the scripts include polite delays between requests
  (`WAIT_LIST`, `WAIT_ARTICLE`, `DELAY_BETWEEN`). Do not reduce these
  aggressively; investing.com may temporarily block the IP.
- **Resumability** — the full scraper is safe to kill at any time. The
  interactive and test scripts are single-run and do not checkpoint.
- **NA values** — `ticker` and `company_name` may be `NA` for transcripts
  whose headings deviate from the standard format.
- **`speaker_role`** and **`speaker_company`** are split on the first comma in
  the speaker line (e.g., `"Jane Doe, Chief Executive Officer, Acme Corp"` →
  role `"Chief Executive Officer"`, company `"Acme Corp"`). `speaker_company`
  is `NA` for speakers identified only by role (e.g., `"Operator"`).
