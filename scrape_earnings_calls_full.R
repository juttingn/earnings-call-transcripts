# scrape_earnings_calls_full.R
#
# Full-scale scraper for all ~10,000 transcripts on investing.com/news/transcripts
#
# CHECKPOINTING DESIGN
#   Each transcript is saved as its own file in transcripts_raw/{id}.rds
#   "Already done" = file exists in that folder → safe to kill/resume any time
#   Listing page progress is saved in checkpoint_page.txt
#
# USAGE
#   Rscript scrape_earnings_calls_full.R          # run / resume
#   Rscript scrape_earnings_calls_full.R --combine # combine raw files → final RDS only

suppressMessages({
  library(chromote)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
})

# ── Configuration ─────────────────────────────────────────────────────────────
CHROME_PATH          <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
BASE_URL             <- "https://www.investing.com/news/transcripts"
RAW_DIR              <- "transcripts_raw"        # one .rds per transcript
OUTPUT_FILE          <- "earning_call_transcripts.rds"  # final combined file
CHECKPOINT_PAGE_FILE <- "checkpoint_page.txt"
ERROR_LOG_FILE       <- "scrape_errors.txt"

WAIT_LIST            <- 10    # seconds to wait after navigating a listing page
WAIT_ARTICLE         <- 8     # seconds to wait after navigating an article
DELAY_BETWEEN        <- 2     # polite pause between articles (seconds)
DELAY_BETWEEN_PAGES  <- 3     # polite pause between listing pages (seconds)
RESTART_CHROME_EVERY <- 150   # restart Chrome every N articles (memory management)
COMBINE_EVERY        <- 200   # also rebuild combined RDS every N new articles

Sys.setenv(CHROMOTE_CHROME = CHROME_PATH)
dir.create(RAW_DIR, showWarnings = FALSE)

# ── Combine-only mode ─────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if ("--combine" %in% args) {
  message("Combine mode: merging all raw transcripts into ", OUTPUT_FILE)
  files <- list.files(RAW_DIR, pattern = "\\.rds$", full.names = TRUE)
  message(sprintf("  Found %d transcript files", length(files)))
  combined <- bind_rows(lapply(files, readRDS))
  saveRDS(combined, OUTPUT_FILE)
  message(sprintf("  Saved %d rows, %d transcripts → %s",
                  nrow(combined), n_distinct(combined$url), OUTPUT_FILE))
  quit(save = "no")
}

# ── Browser helpers ───────────────────────────────────────────────────────────
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

session_alive <- function(b) {
  tryCatch({
    b$Runtime$evaluate("1+1")
    TRUE
  }, error = function(e) FALSE)
}

restart_browser <- function(b, reason = "session recovery") {
  message(sprintf("  ── Restarting Chrome (%s) ──", reason))
  tryCatch(b$close(), error = function(e) NULL)
  Sys.sleep(5)
  nb <- start_browser()
  message("  ── Chrome restarted ──")
  nb
}

navigate_wait <- function(b, url, wait) {
  b$Page$navigate(url, wait_ = FALSE)
  Sys.sleep(wait)
  for (attempt in 1:8) {
    title <- tryCatch(
      b$Runtime$evaluate("document.title")$result$value,
      error = function(e) ""
    )
    if (!grepl("just a moment|cloudflare|challenge", title, ignore.case = TRUE))
      return(invisible(title))
    message(sprintf("    [CF wait %d]...", attempt))
    Sys.sleep(5)
  }
  warning("Cloudflare may not have resolved: ", url)
}

get_html <- function(b) {
  tryCatch(
    b$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
    error = function(e) NA_character_
  )
}

# ── Listing page: get links + detect max page ─────────────────────────────────
get_listing_links <- function(b, page_num) {
  url <- if (page_num == 1) BASE_URL else sprintf("%s/%d", BASE_URL, page_num)
  navigate_wait(b, url, WAIT_LIST)

  html <- get_html(b)
  if (is.na(html)) return(list(links = character(0), max_page = NA_integer_))

  page <- tryCatch(read_html(html), error = function(e) NULL)
  if (is.null(page)) return(list(links = character(0), max_page = NA_integer_))

  all_hrefs <- page %>% html_elements("a[href]") %>% html_attr("href")

  # Article links: slug ending with a numeric ID
  links <- all_hrefs %>%
    grep("/news/transcripts/[^/]+-\\d+$", ., value = TRUE) %>%
    unique() %>%
    { ifelse(startsWith(., "http"), ., paste0("https://www.investing.com", .)) }

  # Max page from pagination numbers
  page_nums <- all_hrefs %>%
    grep("^/news/transcripts/\\d+$", ., value = TRUE) %>%
    str_extract("\\d+$") %>%
    as.integer() %>%
    .[!is.na(.)]

  list(
    links    = links,
    max_page = if (length(page_nums) > 0) max(page_nums) else NA_integer_
  )
}

# ── Transcript parsing ────────────────────────────────────────────────────────
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

parse_transcript_heading <- function(heading_text) {
  body      <- str_remove(heading_text, "(?i)^Full transcript\\s*[-–—]\\s*")
  body      <- str_remove(body, ":\\s*$")
  ticker    <- str_extract(body, "\\(([A-Z]{1,6})\\)", group = 1)
  quarter   <- str_extract(body, "\\b(Q[1-4])\\b")
  call_year <- as.integer(str_extract(body, "\\b(20\\d{2})\\b"))
  company   <- body %>%
    str_remove("\\s*\\(.*") %>%
    str_remove("\\s+Q[1-4].*") %>%
    str_remove("\\s+20\\d{2}.*") %>%
    str_trim()
  list(
    company_name = if (nchar(company) > 0) company else NA_character_,
    ticker = ticker, quarter = quarter, call_year = call_year
  )
}

parse_transcript_page <- function(b, url) {
  navigate_wait(b, url, WAIT_ARTICLE)

  raw_html <- get_html(b)
  if (is.na(raw_html) || nchar(raw_html) < 500) stop("Empty page HTML")

  page          <- read_html(raw_html)
  article_title <- page %>% html_element("h1") %>% html_text(trim = TRUE)
  call_date     <- extract_date_jsonld(page)

  # Locate article body
  article_div <- page %>% html_element("#article")
  if (is.na(article_div)) {
    divs <- page %>% html_elements("div")
    lens <- sapply(divs, function(d) nchar(html_text(d, trim = TRUE)))
    article_div <- divs[[which.max(lens)]]
  }

  all_children <- article_div %>% html_elements("h2, p")
  all_texts    <- html_text(all_children, trim = TRUE)

  # Find "Full transcript" heading → get company/ticker/quarter and start position
  h2_texts      <- page %>% html_elements("#article h2") %>% html_text(trim = TRUE)
  transcript_h2 <- h2_texts[grepl("^Full transcript", h2_texts, ignore.case = TRUE)][1]

  if (!is.na(transcript_h2)) {
    meta      <- parse_transcript_heading(transcript_h2)
    start_idx <- which(grepl("^Full transcript", all_texts, ignore.case = TRUE))[1]
    transcript_nodes <- if (!is.na(start_idx) && start_idx < length(all_children))
      all_children[(start_idx + 1):length(all_children)]
    else all_children
  } else {
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

  p_nodes <- transcript_nodes[html_name(transcript_nodes) == "p"]

  # Parse speaker-by-speaker: <p><b>Name, Role</b><span>: text</span></p>
  rows <- lapply(p_nodes, function(p_node) {
    b_node    <- tryCatch(html_element(p_node, "b"),    error = function(e) NULL)
    span_node <- tryCatch(html_element(p_node, "span"), error = function(e) NULL)

    speaker_raw <- if (!is.null(b_node) && !is.na(b_node))
      html_text(b_node, trim = TRUE) else NA_character_

    if (is.na(speaker_raw) || nchar(speaker_raw) == 0) return(NULL)

    speech <- if (!is.null(span_node) && !is.na(span_node))
      str_remove(html_text(span_node, trim = TRUE), "^:\\s*")
    else
      str_squish(str_remove(
        str_remove(html_text(p_node, trim = TRUE), fixed(speaker_raw)), "^:\\s*"
      ))

    comma_parts  <- str_split(speaker_raw, ",\\s*")[[1]]
    role_paren   <- str_extract(speaker_raw, "\\(([^)]+)\\)", group = 1)
    role_comma   <- if (length(comma_parts) >= 2)
      paste(comma_parts[-1], collapse = ", ") else NA_character_
    role         <- coalesce(role_paren, role_comma, NA_character_)
    speaker_name <- if (!is.na(role_paren))
      str_trim(str_remove(speaker_raw, "\\s*\\(.*")) else comma_parts[1]

    tibble(speaker = speaker_name, speaker_role = role, text = str_squish(speech))
  })

  speaker_df <- bind_rows(Filter(Negate(is.null), rows))

  if (nrow(speaker_df) == 0)
    speaker_df <- tibble(
      speaker      = NA_character_,
      speaker_role = NA_character_,
      text         = paste(html_text(p_nodes, trim = TRUE), collapse = "\n") %>% str_squish()
    )

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

# ── Checkpoint helpers ────────────────────────────────────────────────────────

# Derive a safe filename from a transcript URL
url_to_filename <- function(url) {
  id <- str_extract(url, "[^/]+$")     # last path segment, e.g. "...-93CH-4531830"
  paste0(id, ".rds")
}

is_done <- function(url) {
  file.exists(file.path(RAW_DIR, url_to_filename(url)))
}

save_raw <- function(df, url) {
  saveRDS(df, file.path(RAW_DIR, url_to_filename(url)))
}

log_error <- function(url, msg) {
  cat(sprintf("%s\t%s\n", Sys.time(), msg),
      file = ERROR_LOG_FILE, append = TRUE)
  cat(sprintf("  URL: %s\n", url),
      file = ERROR_LOG_FILE, append = TRUE)
}

rebuild_combined <- function() {
  files   <- list.files(RAW_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(invisible(NULL))
  combined <- bind_rows(lapply(files, readRDS))
  saveRDS(combined, OUTPUT_FILE)
  message(sprintf("  [combined RDS updated: %d rows, %d transcripts]",
                  nrow(combined), n_distinct(combined$url)))
  invisible(combined)
}

# ── Resume state ──────────────────────────────────────────────────────────────
start_page <- if (file.exists(CHECKPOINT_PAGE_FILE))
  as.integer(trimws(readLines(CHECKPOINT_PAGE_FILE))) else 1L

already_done <- list.files(RAW_DIR, pattern = "\\.rds$") %>% length()

message(sprintf(
  "\n══ Starting full scrape (resume from listing page %d, %d transcripts already done) ══",
  start_page, already_done
))
message(sprintf("  Raw files  → %s/", RAW_DIR))
message(sprintf("  Output RDS → %s", OUTPUT_FILE))
message(sprintf("  Error log  → %s\n", ERROR_LOG_FILE))

# ── Main scrape loop ──────────────────────────────────────────────────────────
b             <- start_browser()
max_page      <- 9999L         # sentinel; will be updated dynamically from pagination
articles_this_session <- 0L
new_since_combine     <- 0L

for (pg in start_page:max_page) {

  message(sprintf("── Listing page %d / %d ───────────────────────────────────────────────",
                  pg, max_page))

  listing <- tryCatch(
    get_listing_links(b, pg),
    error = function(e) {
      message("  ERROR fetching listing page: ", e$message)
      log_error(sprintf("LISTING PAGE %d", pg), e$message)
      list(links = character(0), max_page = NA_integer_)
    }
  )

  # Update max page if pagination reveals a new number
  if (!is.na(listing$max_page) && listing$max_page > max_page) {
    max_page <- listing$max_page
    message(sprintf("  Max page updated to %d (~%d transcripts)", max_page, max_page * 35))
  }

  if (length(listing$links) == 0 && is.na(listing$max_page)) {
    message(sprintf("  No links found on page %d — stopping", pg))
    break
  }

  new_links <- listing$links[!sapply(listing$links, is_done)]
  message(sprintf("  Found %d links, %d new (not yet scraped)",
                  length(listing$links), length(new_links)))

  for (url in new_links) {
    articles_this_session <- articles_this_session + 1L

    message(sprintf("    [#%d | pg%d] %s",
                    already_done + articles_this_session,
                    pg,
                    str_trunc(basename(url), 65)))

    result <- tryCatch(
      parse_transcript_page(b, url),
      error = function(e) {
        message("    ERROR: ", e$message)
        log_error(url, e$message)
        NULL
      }
    )

    # If something went wrong, check if the session died and restart + retry once
    if (is.null(result) && !session_alive(b)) {
      b <<- restart_browser(b, "session died after error")
      message("    Retrying after session restart...")
      result <- tryCatch(
        parse_transcript_page(b, url),
        error = function(e) {
          message("    ERROR (retry): ", e$message)
          log_error(url, paste("RETRY:", e$message))
          NULL
        }
      )
    }

    if (!is.null(result)) {
      save_raw(result, url)
      new_since_combine <- new_since_combine + 1L

      message(sprintf("    ✓ %s | %s %s | %d rows",
                      str_trunc(coalesce(result$company_name[1], "?"), 35),
                      coalesce(result$quarter[1], "?"),
                      coalesce(as.character(result$call_year[1]), "?"),
                      nrow(result)))
    }

    # Rebuild combined RDS periodically
    if (new_since_combine >= COMBINE_EVERY) {
      rebuild_combined()
      new_since_combine <- 0L
    }

    # Restart Chrome periodically to prevent memory creep
    if (articles_this_session %% RESTART_CHROME_EVERY == 0) {
      b <- restart_browser(b, "memory management")
    }

    Sys.sleep(DELAY_BETWEEN)
  }

  # Save listing-page checkpoint after finishing each page
  writeLines(as.character(pg + 1L), CHECKPOINT_PAGE_FILE)
  message(sprintf("  [checkpoint saved: next page = %d]", pg + 1L))

  if (pg >= max_page) {
    message(sprintf("\n══ Reached last listing page (%d). Scrape complete! ══", max_page))
    break
  }

  Sys.sleep(DELAY_BETWEEN_PAGES)
}

# ── Final combine ─────────────────────────────────────────────────────────────
message("\n── Building final combined RDS ──────────────────────────────────────────")
rebuild_combined()

tryCatch(b$close(), error = function(e) NULL)

total_done <- list.files(RAW_DIR, pattern = "\\.rds$") %>% length()
message(sprintf("\nAll done. %d transcripts scraped total.", total_done))
message(sprintf("Final output: %s", OUTPUT_FILE))


