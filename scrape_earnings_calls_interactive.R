# =============================================================================
# scrape_earnings_calls_interactive.R
#
# An interactive scraper for earnings call transcripts published on
# investing.com/news/transcripts.
#
# On start-up the script prompts the user for:
#   (1) Maximum number of transcripts to collect (default: 50)
#   (2) Optional date range expressed as "Q1 YYYY - Q4 YYYY"
#
# The scraper uses a real Chromium browser (via the {chromote} package) to
# bypass Cloudflare protection, which blocks plain HTTP requests.
#
# Output
#   An RDS file (default: earning_call_transcripts.rds) containing a
#   data frame with one row per speaker turn.
#   Columns: url, article_title, company_name, ticker, quarter, call_year,
#            call_date, speaker, speaker_role, text
#
# Requirements
#   R >= 4.1, packages: chromote, rvest, dplyr, stringr, lubridate, jsonlite
#   Google Chrome installed at CHROME_PATH (see Configuration block below)
#
# Usage
#   Rscript scrape_earnings_calls_interactive.R
# =============================================================================

suppressMessages({
  library(chromote)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
})

# =============================================================================
# ── USER CONFIGURATION ────────────────────────────────────────────────────────
#
# If you are running this script non-interactively (e.g. via RStudio's Source
# button, or Rscript from a terminal that does not support stdin), set your
# parameters here and the interactive prompts will be skipped automatically.
#
# Leave a value as NA to be prompted for it at run time instead.
# =============================================================================

N_TRANSCRIPTS_CONFIG <- NA          # e.g. 100  — max transcripts to collect
DATE_RANGE_CONFIG    <- NA          # e.g. "Q1 2023 - Q4 2024"  or NA for most-recent

# ── System configuration (edit paths here if needed) ─────────────────────────

CHROME_PATH         <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
BASE_URL            <- "https://www.investing.com/news/transcripts"
OUTPUT_FILE         <- "earning_call_transcripts.rds"

# Timing (seconds) — lower values may trigger Cloudflare; raise if you see errors
WAIT_LIST           <- 10    # wait after navigating to a listing page
WAIT_ARTICLE        <- 8     # wait after navigating to an article page
DELAY_BETWEEN       <- 2     # polite pause between consecutive articles
DELAY_BETWEEN_PAGES <- 3     # polite pause between consecutive listing pages

# Memory management
RESTART_CHROME_EVERY <- 100  # restart Chrome every N articles to prevent leaks

# Date-range stopping heuristic
# investing.com lists transcripts newest-first; once we see this many
# consecutive articles dated before the requested start date, we stop.
PAST_START_STREAK_LIMIT <- 5

Sys.setenv(CHROMOTE_CHROME = CHROME_PATH)

# =============================================================================
# ── Utility helpers ───────────────────────────────────────────────────────────
# =============================================================================

#' Read one line from the user, compatible with both interactive R and Rscript.
read_line <- function(prompt_text) {
  cat(prompt_text)
  if (interactive()) readline() else readLines("stdin", n = 1L, warn = FALSE)
}

#' Parse a "Q1 2020" string into a list(quarter, year).
parse_quarter_year <- function(s) {
  m <- regmatches(s, regexpr("Q([1-4])\\s+(20\\d{2})", s, perl = TRUE))
  if (length(m) == 0 || nchar(m) == 0)
    stop("Cannot parse '", trimws(s), "' — expected format: Q1 2020")
  parts <- regmatches(m, regexec("Q([1-4])\\s+(20\\d{2})", m, perl = TRUE))[[1]]
  list(quarter = as.integer(parts[2]), year = as.integer(parts[3]))
}

#' Return the first day of a fiscal quarter.
quarter_to_start_date <- function(q, year) {
  as.Date(paste0(year, "-", sprintf("%02d", (q - 1L) * 3L + 1L), "-01"))
}

#' Return the last day of a fiscal quarter.
quarter_to_end_date <- function(q, year) {
  ceiling_date(
    as.Date(paste0(year, "-", sprintf("%02d", q * 3L), "-01")),
    unit = "month"
  ) - days(1L)
}

# =============================================================================
# ── Interactive prompts ───────────────────────────────────────────────────────
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║      Earnings Call Transcript Scraper — investing.com            ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

# ── Prompt 1: number of transcripts ───────────────────────────────────────────

if (!is.na(N_TRANSCRIPTS_CONFIG)) {
  N_TRANSCRIPTS <- as.integer(N_TRANSCRIPTS_CONFIG)
  cat(sprintf("  Transcripts to collect : %d  (from USER CONFIGURATION block)\n",
              N_TRANSCRIPTS))
} else {
  n_raw <- trimws(read_line("  How many transcripts to collect? [default: 50] > "))
  N_TRANSCRIPTS <- if (nchar(n_raw) == 0L) 50L else suppressWarnings(as.integer(n_raw))
}

if (is.na(N_TRANSCRIPTS) || N_TRANSCRIPTS <= 0L)
  stop("Invalid input: please enter a positive whole number.")

# ── Prompt 2: optional date range ─────────────────────────────────────────────

if (!is.na(DATE_RANGE_CONFIG)) {
  range_raw <- trimws(as.character(DATE_RANGE_CONFIG))
  cat(sprintf("  Date range             : %s  (from USER CONFIGURATION block)\n",
              range_raw))
} else {
  cat("\n")
  cat("  Date range (e.g. Q1 2020 - Q4 2024).\n")
  range_raw <- trimws(read_line(
    "  Press Enter to collect the most recent transcripts > "
  ))
}

use_date_filter <- nchar(range_raw) > 0L

if (use_date_filter) {
  parts <- strsplit(range_raw, "\\s*[-\u2013\u2014]\\s*")[[1]]

  if (length(parts) != 2L)
    stop("Expected 'Q1 YYYY - Q4 YYYY', got: '", range_raw, "'")

  start_qy <- tryCatch(
    parse_quarter_year(parts[1]),
    error = function(e) stop("Bad start date — ", e$message)
  )
  end_qy <- tryCatch(
    parse_quarter_year(parts[2]),
    error = function(e) stop("Bad end date — ", e$message)
  )

  DATE_START <- quarter_to_start_date(start_qy$quarter, start_qy$year)
  DATE_END   <- quarter_to_end_date(end_qy$quarter,   end_qy$year)

  if (DATE_START > DATE_END)
    stop("Start date (", DATE_START, ") is after end date (", DATE_END, ").")

  cat(sprintf("\n  Date filter: %s  to  %s\n", DATE_START, DATE_END))

} else {
  DATE_START <- as.Date("1900-01-01")  # no lower bound
  DATE_END   <- Sys.Date()             # up to today
}

# ── Confirmation ───────────────────────────────────────────────────────────────

cat(sprintf("\n  Will collect up to %d transcripts", N_TRANSCRIPTS))
if (use_date_filter)
  cat(sprintf(" published between %s and %s", DATE_START, DATE_END))
cat(sprintf(".\n  Output will be saved to: %s\n\n", OUTPUT_FILE))

go_raw <- trimws(read_line("  Start scraping? [Y/n] > "))
if (nchar(go_raw) > 0L && tolower(go_raw) %in% c("n", "no")) {
  message("Aborted by user.")
  quit(save = "no")
}
cat("\n")

# =============================================================================
# ── Browser helpers ───────────────────────────────────────────────────────────
# =============================================================================

#' Launch a new ChromoteSession with a realistic user-agent string.
start_browser <- function() {
  b <- ChromoteSession$new()
  b$Network$setUserAgentOverride(
    userAgent = paste0(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
      "AppleWebKit/537.36 (KHTML, like Gecko) ",
      "Chrome/121.0.0.0 Safari/537.36"
    )
  )
  b
}

#' Return TRUE if the Chrome session is still responsive.
session_alive <- function(b) {
  tryCatch({ b$Runtime$evaluate("1+1"); TRUE }, error = function(e) FALSE)
}

#' Close the current session and open a fresh one.
restart_browser <- function(b, reason = "memory management") {
  message(sprintf("  [browser] Restarting Chrome (%s)...", reason))
  tryCatch(b$close(), error = function(e) NULL)
  Sys.sleep(5L)
  nb <- start_browser()
  message("  [browser] Chrome restarted.")
  nb
}

#' Navigate to url, wait for Cloudflare to clear, then return invisibly.
navigate_wait <- function(b, url, wait_secs) {
  b$Page$navigate(url, wait_ = FALSE)
  Sys.sleep(wait_secs)

  for (attempt in seq_len(8L)) {
    title <- tryCatch(
      b$Runtime$evaluate("document.title")$result$value,
      error = function(e) ""
    )
    if (!grepl("just a moment|cloudflare|challenge", title, ignore.case = TRUE)) {
      return(invisible(title))
    }
    message(sprintf("    [Cloudflare] Waiting (attempt %d)...", attempt))
    Sys.sleep(5L)
  }
  warning("Cloudflare challenge may not have resolved for: ", url)
}

#' Return the full rendered HTML of the current page.
get_html <- function(b) {
  tryCatch(
    b$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
    error = function(e) NA_character_
  )
}

# =============================================================================
# ── Listing-page helper ───────────────────────────────────────────────────────
# =============================================================================

#' Navigate to a listing page and return transcript links with metadata.
#'
#' Returns a data frame with one row per unique transcript URL, including the
#' article title and publication date scraped from the listing page itself.
#' These are used to pre-filter articles by title and date range without
#' needing to visit each individual article page first.
#'
#' @param b    A ChromoteSession.
#' @param page Integer page number (1 = first page).
#' @return A data frame with columns: url, listing_date (Date|NA), listing_title.
get_listing_links <- function(b, page) {
  url <- if (page == 1L) BASE_URL else sprintf("%s/%d", BASE_URL, page)
  navigate_wait(b, url, WAIT_LIST)

  html <- get_html(b)
  empty <- data.frame(url = character(0L), listing_date = as.Date(character(0L)),
                      listing_title = character(0L), stringsAsFactors = FALSE)
  if (is.na(html)) return(empty)

  parsed <- tryCatch(read_html(html), error = function(e) NULL)
  if (is.null(parsed)) return(empty)

  # Collect anchor elements whose href matches a transcript article URL
  all_anchors   <- parsed %>% html_elements("a[href]")
  all_hrefs     <- html_attr(all_anchors, "href")
  is_trans      <- grepl("/news/transcripts/[^/]+-\\d+$", all_hrefs)
  trans_anchors <- all_anchors[is_trans]
  trans_hrefs   <- all_hrefs[is_trans]

  # Deduplicate (keep first occurrence per URL)
  dedup_idx     <- !duplicated(trans_hrefs)
  trans_anchors <- trans_anchors[dedup_idx]
  trans_hrefs   <- trans_hrefs[dedup_idx]

  if (length(trans_hrefs) == 0L) return(empty)

  urls   <- ifelse(startsWith(trans_hrefs, "http"), trans_hrefs,
                   paste0("https://www.investing.com", trans_hrefs))
  titles <- html_text(trans_anchors, trim = TRUE)

  # Extract publication dates from <time datetime="..."> elements.
  # Listing pages have one <time> per article in document order, so we
  # pair them positionally with the links.
  time_dt    <- parsed %>% html_elements("time[datetime]") %>% html_attr("datetime")
  time_dates <- suppressWarnings(as.Date(str_extract(time_dt, "^\\d{4}-\\d{2}-\\d{2}")))
  time_dates <- time_dates[!is.na(time_dates)]

  n <- length(urls)
  listing_dates <- if (length(time_dates) >= n) {
    time_dates[seq_len(n)]
  } else {
    c(time_dates, rep(NA_Date_, n - length(time_dates)))
  }

  data.frame(url = urls, listing_date = listing_dates, listing_title = titles,
             stringsAsFactors = FALSE)
}

# =============================================================================
# ── Transcript parsers ────────────────────────────────────────────────────────
# =============================================================================

#' Extract the publication date from JSON-LD structured data in the page.
#'
#' investing.com embeds schema.org JSON-LD in every article.
#' The datePublished field contains an ISO-8601 date string.
#'
#' @param page An xml_document object (from rvest::read_html).
#' @return A Date, or NA_Date_ if not found.
extract_date_jsonld <- function(page) {
  scripts <- page %>%
    html_elements('script[type="application/ld+json"]') %>%
    html_text()

  for (s in scripts) {
    data <- tryCatch(fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(data)) next

    for (field in c("datePublished", "dateCreated", "dateModified")) {
      val <- data[[field]]
      if (!is.null(val) && nchar(val) >= 10L) {
        d <- suppressWarnings(
          as.Date(str_extract(val, "^\\d{4}-\\d{2}-\\d{2}"))
        )
        if (!is.na(d)) return(d)
      }
    }
  }
  NA_Date_
}

#' Parse the "Full transcript - Company (TICKER) Q4 2025:" heading.
#'
#' @param heading_text A character string from an <h2> element.
#' @return A named list with company_name, ticker, quarter, call_year.
parse_transcript_heading <- function(heading_text) {
  body <- heading_text %>%
    str_remove("(?i)^Full transcript\\s*[-\u2013\u2014]\\s*") %>%
    str_remove(":\\s*$")

  ticker    <- str_extract(body, "\\(([A-Z]{1,6})\\)", group = 1)
  quarter   <- str_extract(body, "\\b(Q[1-4])\\b")
  call_year <- as.integer(str_extract(body, "\\b(20\\d{2})\\b"))

  company <- body %>%
    str_remove("\\s*\\(.*")     %>%   # drop "(TICKER) ..."
    str_remove("\\s+Q[1-4].*") %>%   # drop "Q4 2025 ..."
    str_remove("\\s+20\\d{2}.*") %>%
    str_trim()

  list(
    company_name = if (nchar(company) > 0L) company else NA_character_,
    ticker       = ticker,
    quarter      = quarter,
    call_year    = call_year
  )
}

#' Download, render, and parse one transcript article.
#'
#' @param b   A ChromoteSession.
#' @param url Character URL of the transcript article.
#' @return A tibble (one row per speaker turn) or NULL on failure.
parse_transcript_page <- function(b, url) {
  navigate_wait(b, url, WAIT_ARTICLE)

  raw_html <- get_html(b)
  if (is.na(raw_html) || nchar(raw_html) < 500L) stop("Empty page HTML")

  page <- read_html(raw_html)

  article_title <- page %>% html_element("h1") %>% html_text(trim = TRUE)

  # Skip anything that is not an earnings call transcript
  if (!grepl("^Earnings call transcript:", article_title, ignore.case = TRUE)) {
    message(sprintf("    SKIP  not an earnings call transcript: %s",
                    str_trunc(article_title, 70)))
    return(NULL)
  }

  call_date     <- extract_date_jsonld(page)

  # Locate the article body — prefer #article, fall back to largest div
  article_div <- page %>% html_element("#article")
  if (is.na(article_div)) {
    divs <- page %>% html_elements("div")
    lens <- sapply(divs, function(d) nchar(html_text(d, trim = TRUE)))
    article_div <- divs[[which.max(lens)]]
  }

  all_children <- article_div %>% html_elements("h2, p")
  all_texts    <- html_text(all_children, trim = TRUE)

  # Find the "Full transcript - ..." h2 → parse metadata and locate start
  h2_texts      <- page %>% html_elements("#article h2") %>% html_text(trim = TRUE)
  transcript_h2 <- h2_texts[grepl("^Full transcript", h2_texts, ignore.case = TRUE)][1]

  if (!is.na(transcript_h2)) {
    meta      <- parse_transcript_heading(transcript_h2)
    start_idx <- which(grepl("^Full transcript", all_texts, ignore.case = TRUE))[1]
    transcript_nodes <- if (!is.na(start_idx) && start_idx < length(all_children))
      all_children[(start_idx + 1L):length(all_children)]
    else
      all_children
  } else {
    # Fallback: derive metadata from the article title (h1)
    meta <- list(
      company_name = article_title %>%
        str_remove("(?i)^Earnings call transcript:\\s*") %>%
        str_remove("(?i)\\s+(Q[1-4]|FY|Annual|Fourth|Third|Second|First).*$") %>%
        str_trim(),
      ticker    = str_extract(article_title, "\\(([A-Z]{1,6})\\)", group = 1),
      quarter   = str_extract(article_title, "\\b(Q[1-4])\\b"),
      call_year = as.integer(str_extract(article_title, "\\b(20\\d{2})\\b"))
    )
    transcript_nodes <- all_children
  }

  # Keep only <p> nodes (skip sub-headings within the transcript)
  p_nodes <- transcript_nodes[html_name(transcript_nodes) == "p"]

  # Parse speaker turns: <p><b>Name, Role</b><span>: speech</span></p>
  rows <- lapply(p_nodes, function(p_node) {
    b_node    <- tryCatch(html_element(p_node, "b"),    error = function(e) NULL)
    span_node <- tryCatch(html_element(p_node, "span"), error = function(e) NULL)

    speaker_raw <- if (!is.null(b_node) && !is.na(b_node))
      html_text(b_node, trim = TRUE) else NA_character_

    if (is.na(speaker_raw) || nchar(speaker_raw) == 0L) return(NULL)

    speech <- if (!is.null(span_node) && !is.na(span_node))
      str_remove(html_text(span_node, trim = TRUE), "^:\\s*")
    else
      str_squish(str_remove(
        str_remove(html_text(p_node, trim = TRUE), fixed(speaker_raw)),
        "^:\\s*"
      ))

    # Separate speaker name from role
    # Formats: "Operator" / "Jane Doe, CFO" / "Jane Doe (CFO)"
    comma_parts <- str_split(speaker_raw, ",\\s*")[[1]]
    role_paren  <- str_extract(speaker_raw, "\\(([^)]+)\\)", group = 1)
    role_comma  <- if (length(comma_parts) >= 2L)
      paste(comma_parts[-1], collapse = ", ") else NA_character_
    role        <- coalesce(role_paren, role_comma, NA_character_)

    speaker_name <- if (!is.na(role_paren))
      str_trim(str_remove(speaker_raw, "\\s*\\(.*"))
    else
      comma_parts[1]

    tibble(
      speaker      = speaker_name,
      speaker_role = role,
      text         = str_squish(speech)
    )
  })

  speaker_df <- bind_rows(Filter(Negate(is.null), rows))

  # Last resort: store the full text as a single row if parsing found nothing
  if (nrow(speaker_df) == 0L) {
    speaker_df <- tibble(
      speaker      = NA_character_,
      speaker_role = NA_character_,
      text         = paste(html_text(p_nodes, trim = TRUE), collapse = "\n") %>%
        str_squish()
    )
  }

  speaker_df %>%
    mutate(
      url           = url,
      article_title = article_title,
      company_name  = meta$company_name,
      ticker        = meta$ticker,
      quarter       = meta$quarter,
      call_year     = meta$call_year,
      call_date     = call_date,
      .before       = 1L
    )
}

# =============================================================================
# ── Main scrape loop ──────────────────────────────────────────────────────────
# =============================================================================

message("Starting Chrome...")
b <- start_browser()
message("Chrome ready.\n")

collected      <- 0L   # transcripts kept (within date range)
visited        <- 0L   # articles visited (including skipped ones)
past_streak    <- 0L   # consecutive articles dated before DATE_START
done           <- FALSE
results        <- list()

for (pg in seq_len(500L)) {   # 500-page hard cap; loop exits via `break`

  if (done) break

  message(sprintf("── Listing page %d ──────────────────────────────────────────────────────", pg))

  links_df <- tryCatch(
    get_listing_links(b, pg),
    error = function(e) {
      message("  ERROR on listing page: ", e$message)
      data.frame(url = character(0L), listing_date = as.Date(character(0L)),
                 listing_title = character(0L), stringsAsFactors = FALSE)
    }
  )

  if (nrow(links_df) == 0L) {
    message("  No links found — reached the end of the listing or an error occurred.")
    break
  }

  message(sprintf("  Found %d transcript links on this page.", nrow(links_df)))

  for (i in seq_len(nrow(links_df))) {
    url           <- links_df$url[i]
    listing_date  <- links_df$listing_date[i]
    listing_title <- links_df$listing_title[i]

    if (collected >= N_TRANSCRIPTS) {
      message(sprintf("\n  Target of %d transcripts reached. Stopping.", N_TRANSCRIPTS))
      done <- TRUE
      break
    }

    # ── Listing-level pre-filters (no article page visit needed) ─────────────

    # Title filter: skip if clearly not an earnings call transcript
    if (!is.na(listing_title) && nchar(listing_title) > 0L &&
        !grepl("^Earnings call transcript:", listing_title, ignore.case = TRUE)) {
      message(sprintf("  SKIP (listing) not an earnings call: %s",
                      str_trunc(listing_title, 65)))
      next
    }

    # Date filter: skip articles outside the requested date range
    if (!is.na(listing_date)) {
      if (listing_date > DATE_END) {
        message(sprintf("  SKIP (listing) %s is after %s", listing_date, DATE_END))
        next
      }
      if (listing_date < DATE_START) {
        past_streak <- past_streak + 1L
        message(sprintf("  SKIP (listing) %s is before %s (streak %d/%d)",
                        listing_date, DATE_START, past_streak, PAST_START_STREAK_LIMIT))
        if (past_streak >= PAST_START_STREAK_LIMIT) {
          message(sprintf(
            "\n  %d consecutive articles predate %s. End of date range reached.",
            PAST_START_STREAK_LIMIT, DATE_START
          ))
          done <- TRUE
          break
        }
        next
      }
    }

    # ── Visit the article page ────────────────────────────────────────────────

    visited <- visited + 1L
    message(sprintf(
      "\n  [visited %d | kept %d] %s",
      visited, collected, str_trunc(basename(url), 70)
    ))

    result <- tryCatch(
      parse_transcript_page(b, url),
      error = function(e) {
        message("    ERROR: ", e$message)
        NULL
      }
    )

    # If the session died, restart and retry once
    if (is.null(result) && !session_alive(b)) {
      b <- restart_browser(b, "session died")
      message("    Retrying after session restart...")
      result <- tryCatch(
        parse_transcript_page(b, url),
        error = function(e) { message("    ERROR (retry): ", e$message); NULL }
      )
    }

    if (!is.null(result)) {
      art_date <- result$call_date[1]

      if (!is.na(art_date) && art_date > DATE_END) {
        # Article is newer than the requested range — skip
        message(sprintf("    SKIP  date %s is after %s", art_date, DATE_END))

      } else if (is.na(art_date) || art_date >= DATE_START) {
        # Article is within the requested range (or date unknown) — keep
        past_streak <- 0L
        collected   <- collected + 1L
        results[[collected]] <- result
        message(sprintf("    KEEP  %s | %s %s | %d speaker rows | date %s",
                        str_trunc(coalesce(result$company_name[1], "?"), 30),
                        coalesce(result$quarter[1], "?"),
                        coalesce(as.character(result$call_year[1]), "?"),
                        nrow(result),
                        coalesce(as.character(art_date), "unknown")))

      } else {
        # Article is older than the requested range — count streak
        past_streak <- past_streak + 1L
        message(sprintf("    SKIP  date %s is before %s (streak %d/%d)",
                        art_date, DATE_START, past_streak, PAST_START_STREAK_LIMIT))

        if (past_streak >= PAST_START_STREAK_LIMIT) {
          message(sprintf(
            "\n  %d consecutive articles predate %s. End of date range reached.",
            PAST_START_STREAK_LIMIT, DATE_START
          ))
          done <- TRUE
          break
        }
      }
    }

    # Periodic Chrome restart to prevent memory creep
    if (visited %% RESTART_CHROME_EVERY == 0L) {
      b <- restart_browser(b, "memory management")
    }

    Sys.sleep(DELAY_BETWEEN)
  }

  Sys.sleep(DELAY_BETWEEN_PAGES)
}

# =============================================================================
# ── Save output ───────────────────────────────────────────────────────────────
# =============================================================================

message(sprintf("\n── Finished. Visited %d articles, kept %d transcripts.", visited, collected))

if (length(results) == 0L) {
  message("No transcripts collected — nothing to save.")
  tryCatch(b$close(), error = function(e) NULL)
  quit(save = "no")
}

final_df <- bind_rows(results)

message(sprintf("Total speaker-turn rows  : %d", nrow(final_df)))
message(sprintf("Distinct transcripts     : %d", n_distinct(final_df$url)))

if (use_date_filter) {
  message(sprintf("Date range (actual)      : %s  to  %s",
                  min(final_df$call_date, na.rm = TRUE),
                  max(final_df$call_date, na.rm = TRUE)))
}

# Print a compact summary table
cat("\n")
print(
  final_df %>%
    distinct(url, .keep_all = TRUE) %>%
    select(company_name, ticker, quarter, call_year, call_date) %>%
    arrange(desc(call_date)),
  n = min(collected, 30L)
)

saveRDS(final_df, OUTPUT_FILE)
message(sprintf("\nSaved → %s\n", OUTPUT_FILE))

tryCatch(b$close(), error = function(e) NULL)
message("Done.")
