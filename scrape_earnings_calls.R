# scrape_earnings_calls.R
# Scrapes earning call transcripts from investing.com/news/transcripts
# Uses chromote (headless Chrome) to bypass Cloudflare protection
#
# HTML structure discovered on 2026-02-27:
#   - Article body: #article div
#   - Transcript begins after <h2> containing "Full transcript - Company (TICKER) Qn YEAR:"
#   - Each speaker: <p><b>Speaker Name</b><span>: text</span></p>
#   - Date: JSON-LD <script type="application/ld+json"> → datePublished
#
# Output: one row per speaker intervention with columns:
#   url, article_title, company_name, ticker, quarter, call_year, call_date,
#   speaker, speaker_role, text

suppressMessages({
  library(chromote)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
})

# ── Configuration ─────────────────────────────────────────────────────────────
CHROME_PATH   <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
BASE_URL      <- "https://www.investing.com/news/transcripts"
N_TRANSCRIPTS <- 5
OUTPUT_FILE   <- "earning_call_transcripts.rds"
WAIT_LIST     <- 10   # seconds after navigating to listing page
WAIT_ARTICLE  <- 8    # seconds after navigating to article page

Sys.setenv(CHROMOTE_CHROME = CHROME_PATH)

# ── Browser helpers ───────────────────────────────────────────────────────────
navigate_wait <- function(b, url, wait) {
  b$Page$navigate(url, wait_ = FALSE)
  Sys.sleep(wait)

  for (attempt in 1:8) {
    title <- tryCatch(
      b$Runtime$evaluate("document.title")$result$value,
      error = function(e) ""
    )
    if (!grepl("just a moment|cloudflare|challenge", title, ignore.case = TRUE)) {
      message(sprintf("    [OK] '%s'", str_trunc(title, 70)))
      return(invisible(title))
    }
    message(sprintf("    [wait %d] Cloudflare still active...", attempt))
    Sys.sleep(5)
  }
  warning("Cloudflare challenge may not have resolved for: ", url)
}

get_html <- function(b) {
  tryCatch(
    b$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
    error = function(e) NA_character_
  )
}

# ── Metadata helpers ──────────────────────────────────────────────────────────

# Extract date from JSON-LD structured data embedded in the page
extract_date_jsonld <- function(page) {
  scripts <- page %>%
    html_elements('script[type="application/ld+json"]') %>%
    html_text()

  for (s in scripts) {
    data <- tryCatch(fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(data)) next

    for (field in c("datePublished", "dateCreated", "dateModified")) {
      val <- data[[field]]
      if (!is.null(val) && nchar(val) >= 10) {
        d <- suppressWarnings(as.Date(str_extract(val, "^\\d{4}-\\d{2}-\\d{2}")))
        if (!is.na(d)) return(d)
      }
    }
  }
  NA_Date_
}

# Parse "Full transcript - Company Name LP (TICKER) Q4 2025:" heading
parse_transcript_heading <- function(heading_text) {
  # Remove "Full transcript - " prefix (case-insensitive)
  body <- str_remove(heading_text, "(?i)^Full transcript\\s*[-–—]\\s*")
  body <- str_remove(body, ":\\s*$")   # remove trailing colon

  # Ticker in parentheses (upper-case letters)
  ticker <- str_extract(body, "\\(([A-Z]{1,6})\\)", group = 1)

  # Quarter: Q1–Q4
  quarter <- str_extract(body, "\\b(Q[1-4])\\b")

  # Year: four digit year
  call_year <- as.integer(str_extract(body, "\\b(20\\d{2})\\b"))

  # Company: everything before the first "(" or "Q\d" or four-digit year
  company_name <- body %>%
    str_remove("\\s*\\(.*")    %>%          # remove from first "("
    str_remove("\\s+Q[1-4].*") %>%          # remove from "Q1/Q2/Q3/Q4"
    str_remove("\\s+20\\d{2}.*") %>%        # remove from year
    str_trim()

  list(
    company_name = if (nchar(company_name) > 0) company_name else NA_character_,
    ticker       = ticker,
    quarter      = quarter,
    call_year    = call_year
  )
}

# ── Core parser ───────────────────────────────────────────────────────────────
parse_transcript_page <- function(b, url) {
  message(sprintf("\n  ─ %s", url))
  navigate_wait(b, url, WAIT_ARTICLE)

  raw_html <- get_html(b)
  if (is.na(raw_html) || nchar(raw_html) < 500) {
    warning("Empty page for: ", url); return(NULL)
  }

  page <- tryCatch(read_html(raw_html), error = function(e) {
    warning("HTML parse error: ", e$message); NULL
  })
  if (is.null(page)) return(NULL)

  # ── Metadata ─────────────────────────────────────────────────
  article_title <- page %>%
    html_element("h1") %>%
    html_text(trim = TRUE)
  message(sprintf("    title : %s", str_trunc(article_title, 80)))

  # Skip anything that is not an earnings call transcript
  if (!grepl("^Earnings call transcript:", article_title, ignore.case = TRUE)) {
    message("    SKIP  not an earnings call transcript")
    return(NULL)
  }

  call_date <- extract_date_jsonld(page)
  message(sprintf("    date  : %s", call_date))

  # ── Locate the article body ───────────────────────────────────
  article_div <- page %>% html_element("#article")
  if (is.na(article_div)) {
    # Fallback to largest div with substantial text
    divs <- page %>% html_elements("div")
    lens <- sapply(divs, function(d) nchar(html_text(d, trim = TRUE)))
    article_div <- divs[[which.max(lens)]]
  }

  # ── Find the "Full transcript" heading ────────────────────────
  # The h2 text is like: "Full transcript - Company LP (TICKER) Q4 2025:"
  headings <- article_div %>%
    html_elements("h2") %>%
    { data.frame(
        text  = html_text(., trim = TRUE),
        stringsAsFactors = FALSE
    )}

  transcript_h2_idx <- which(grepl("^Full transcript", headings$text, ignore.case = TRUE))[1]

  if (!is.na(transcript_h2_idx)) {
    transcript_heading_text <- headings$text[transcript_h2_idx]
    meta <- parse_transcript_heading(transcript_heading_text)
    message(sprintf("    company: %s  ticker: %s  %s %s",
                    meta$company_name, meta$ticker, meta$quarter, meta$call_year))
  } else {
    # Fall back to h1 parsing
    meta <- list(
      company_name = article_title %>%
        str_remove("(?i)^Earnings call transcript:\\s*") %>%
        str_remove("(?i)\\s+(Q[1-4]|FY|Annual|Fourth|Third|Second|First).*$") %>%
        str_trim(),
      ticker    = str_extract(article_title, "\\(([A-Z]{1,6})\\)", group = 1),
      quarter   = str_extract(article_title, "\\b(Q[1-4])\\b"),
      call_year = as.integer(str_extract(article_title, "\\b(20\\d{2})\\b"))
    )
    message(sprintf("    [no h2] company: %s", meta$company_name))
  }

  # ── Extract speaker paragraphs from transcript section ────────
  # We grab ALL <p> nodes inside #article.
  # Each speaker paragraph has structure: <p><b>Name</b><span>: text</span></p>
  # We only process paragraphs that appear AFTER the "Full transcript" h2.

  all_children <- article_div %>% html_elements("h2, p")

  # Find index of the "Full transcript" h2
  if (!is.na(transcript_h2_idx)) {
    all_texts <- html_text(all_children, trim = TRUE)
    start_idx <- which(grepl("^Full transcript", all_texts, ignore.case = TRUE))[1]
    if (!is.na(start_idx) && start_idx < length(all_children)) {
      transcript_nodes <- all_children[(start_idx + 1):length(all_children)]
    } else {
      transcript_nodes <- all_children
    }
  } else {
    transcript_nodes <- all_children
  }

  # Filter to only <p> nodes (skip any h2 sub-headings inside transcript)
  p_nodes <- transcript_nodes[html_name(transcript_nodes) == "p"]

  message(sprintf("    transcript <p> nodes: %d", length(p_nodes)))

  # For each <p>, check if it has a <b> child (= speaker name)
  rows <- lapply(p_nodes, function(p_node) {
    b_node    <- tryCatch(html_element(p_node, "b"), error = function(e) NULL)
    span_node <- tryCatch(html_element(p_node, "span"), error = function(e) NULL)

    speaker_raw <- if (!is.null(b_node) && !is.na(b_node))
      html_text(b_node, trim = TRUE) else NA_character_

    if (is.na(speaker_raw) || nchar(speaker_raw) == 0) return(NULL)

    # Text: prefer <span> content; fall back to full <p> minus speaker header
    if (!is.null(span_node) && !is.na(span_node)) {
      speech <- html_text(span_node, trim = TRUE) %>%
        str_remove("^:\\s*")   # remove leading ": " that investing.com includes
    } else {
      speech <- html_text(p_node, trim = TRUE) %>%
        str_remove(fixed(speaker_raw)) %>%
        str_remove("^:\\s*") %>%
        str_squish()
    }

    # Parse speaker name, role, and company
    # Formats seen:  "Operator"
    #                "Jane Doe, CFO"
    #                "Jane Doe, Chief Executive Officer, Acme Corp"
    #                "Jane Doe (CFO)"
    role_paren  <- str_extract(speaker_raw, "\\(([^)]+)\\)", group = 1)
    comma_parts <- str_split(speaker_raw, ",\\s*")[[1]]

    speaker_name    <- if (!is.na(role_paren))
      str_trim(str_remove(speaker_raw, "\\s*\\(.*"))
    else
      comma_parts[1]
    speaker_role    <- if (!is.na(role_paren))
      role_paren
    else if (length(comma_parts) >= 2L) comma_parts[2] else NA_character_
    speaker_company <- if (!is.na(role_paren))
      NA_character_
    else if (length(comma_parts) >= 3L)
      paste(comma_parts[3:length(comma_parts)], collapse = ", ") else NA_character_

    tibble(
      speaker         = speaker_name,
      speaker_role    = speaker_role,
      speaker_company = speaker_company,
      text            = str_squish(speech)
    )
  })

  speaker_df <- bind_rows(Filter(Negate(is.null), rows))

  if (nrow(speaker_df) == 0) {
    # Last resort: return full text as single row
    full_text <- paste(html_text(p_nodes, trim = TRUE), collapse = "\n")
    speaker_df <- tibble(speaker = NA_character_, speaker_role = NA_character_,
                         speaker_company = NA_character_, text = str_squish(full_text))
  }

  message(sprintf("    speaker rows: %d", nrow(speaker_df)))

  speaker_df %>%
    mutate(
      url           = url,
      article_title = article_title,
      company_name  = meta$company_name,
      ticker        = meta$ticker,
      quarter       = meta$quarter,
      call_year     = meta$call_year,
      call_date     = call_date,
      .before       = 1
    )
}

# ── Launch browser ────────────────────────────────────────────────────────────
message("\n══ Starting Chrome ════════════════════════════════════════════════════")
b <- ChromoteSession$new()
b$Network$setUserAgentOverride(
  userAgent = paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/121.0.0.0 Safari/537.36"
  )
)

# ── Get transcript listing ────────────────────────────────────────────────────
message(sprintf("\n── Listing page: %s", BASE_URL))
navigate_wait(b, BASE_URL, WAIT_LIST)

html_listing <- get_html(b)
page_listing <- read_html(html_listing)

all_anchors <- page_listing %>% html_elements("a[href]")
all_hrefs   <- html_attr(all_anchors, "href")

is_trans      <- grepl("/news/transcripts/[^/]+-\\d+$", all_hrefs)
trans_hrefs   <- all_hrefs[is_trans]
trans_anchors <- all_anchors[is_trans]
dedup_idx     <- !duplicated(trans_hrefs)
trans_hrefs   <- trans_hrefs[dedup_idx]
trans_anchors <- trans_anchors[dedup_idx]

transcript_links  <- ifelse(startsWith(trans_hrefs, "http"), trans_hrefs,
                             paste0("https://www.investing.com", trans_hrefs))
transcript_titles <- html_text(trans_anchors, trim = TRUE)

if (length(transcript_links) == 0) {
  # Fallback: broader search (titles unavailable in this path)
  fallback_hrefs   <- all_hrefs %>%
    grep("transcript", ., ignore.case = TRUE, value = TRUE) %>%
    grep("^/|^http", ., value = TRUE) %>%
    unique()
  transcript_links  <- ifelse(startsWith(fallback_hrefs, "http"), fallback_hrefs,
                               paste0("https://www.investing.com", fallback_hrefs))
  transcript_titles <- rep("", length(transcript_links))
}

message(sprintf("Found %d transcript links", length(transcript_links)))
if (length(transcript_links) == 0)
  stop("No transcript links found. Page structure may have changed.")

# Pre-filter: keep only earnings call transcripts (or those with no title yet)
is_earnings_call  <- nchar(transcript_titles) == 0L |
                     grepl("^Earnings call transcript:", transcript_titles, ignore.case = TRUE)
transcript_links  <- transcript_links[is_earnings_call]
message(sprintf("After title pre-filter: %d earnings call links", length(transcript_links)))

links_to_scrape <- head(transcript_links, N_TRANSCRIPTS)

# ── Scrape loop ───────────────────────────────────────────────────────────────
message(sprintf("\n══ Scraping %d transcripts ════════════════════════════════════════════",
                length(links_to_scrape)))

results <- vector("list", length(links_to_scrape))

for (i in seq_along(links_to_scrape)) {
  message(sprintf("\n[%d/%d]", i, length(links_to_scrape)))
  results[[i]] <- tryCatch(
    parse_transcript_page(b, links_to_scrape[i]),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  Sys.sleep(2)
}

# ── Combine & save ────────────────────────────────────────────────────────────
message("\n── Combining results ─────────────────────────────────────────────────")
final_df <- bind_rows(Filter(Negate(is.null), results))

message(sprintf("Total speaker-intervention rows : %d", nrow(final_df)))
message(sprintf("Distinct transcripts captured   : %d", n_distinct(final_df$url)))

print(
  final_df %>%
    distinct(url, .keep_all = TRUE) %>%
    select(company_name, ticker, quarter, call_year, call_date, n_rows = url) %>%
    left_join(
      count(final_df, url) %>% rename(n_rows = n),
      by = c(n_rows = "url")
    ) %>%
    select(-n_rows)
)

saveRDS(final_df, OUTPUT_FILE)
message(sprintf("\nSaved → %s", OUTPUT_FILE))

b$close()
message("Done.")

# ── Load RDS file for text mining (second assignement) ────────────────────────────────────────────────────────────

earning_calls <- readRDS("~/Documents/Claude projects/Earning call transcripts/earning_call_transcripts.rds")
