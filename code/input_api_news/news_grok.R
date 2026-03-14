# -------------------------------------------------------------
# News source classification experiment — Grok 4.1 Fast
# Web search on/off, 100 iterations each, 4 parallel workers
# Model settings follow 1100_grok_api_japan_2026_election.R
# Output: data-out/news_source/raw_grok.rds
#   columns: model, web_search, iteration, url_order, raw_response
# -------------------------------------------------------------

rm(list = ls())
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(furrr)
plan(multisession, workers = 4)

# 1. Settings --------------------------------------------------
xai_api_key <- trimws(readLines("xai_api_key.txt", n = 1, warn = FALSE))
temperature <- 1
n_iter      <- 100

# 2. URL list --------------------------------------------------
urls <- c(
  # Party news pages
  "jimin.jp/news",
  "craj.jp/news",
  "o-ishin.jp/news",
  "new-kokumin.jp/news",
  "jcp.or.jp/akahata",
  "reiwa-shinsengumi.com/activity",
  "sanseito.jp/news",
  "hoshuto.jp/category/news",
  "sdp.or.jp/category/information",
  "team-mir.ai",
  "genzeinippon.com",
  # National newspapers
  "nikkei.com",
  "asahi.com",
  "yomiuri.co.jp",
  "mainichi.jp",
  "sankei.com",
  "kyodo.co.jp",
  "jiji.com",
  # TV networks
  "news.web.nhk",
  "news.ntv.co.jp",
  "news.tv-asahi.co.jp",
  "newsdig.tbs.co.jp",
  "fnn.jp",
  "txbiz.tv-tokyo.co.jp",
  # Decoys
  "bunshun.jp/list/magazine/shukan-bunshun",
  "nomura.co.jp/introduc/news",
  "hinatazaka46.com/s/official/news/list",
  "keio.ac.jp/ja/news",
  "jpo.go.jp/news",
  "toyotatimes.jp"
)

# 3. Prompt (Japanese — input to LLM) --------------------------
make_prompt <- function(shuffled_urls) {
  url_list <- paste(shuffled_urls, collapse = "\n")
  paste0(
    "以下の各URLについて、そのウェブサイトのコンテンツが「報道」か「広報」かを分類し、JSON形式のみで返してください。テキストや説明は不要です。\n",
    "返答形式: [{\"url\": \"...\", \"category\": \"報道\"}, {\"url\": \"...\", \"category\": \"広報\"}, ...]\n\n",
    "URLリスト:\n", url_list
  )
}

# 4. API call helper -------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

extract_text <- function(parsed) {
  if (!is.null(parsed$output_text)) return(parsed$output_text)
  texts <- unlist(lapply(parsed$output %||% list(), function(x)
    if (!is.null(x$content)) unlist(lapply(x$content, function(y) y$text %||% ""))))
  paste(texts, collapse = "\n")
}

# 5. API call — web search on ----------------------------------
call_web <- function(prompt) {
  res <- tryCatch({
    POST(
      "https://api.x.ai/v1/responses",
      add_headers(Authorization = paste("Bearer", xai_api_key),
                  "Content-Type" = "application/json"),
      body = toJSON(list(
        model       = "grok-4-1-fast-reasoning",
        input       = list(list(role = "user", content = prompt)),
        tools       = list(list(type = "web_search")),
        temperature = temperature,
        max_turns   = 1
      ), auto_unbox = TRUE),
      timeout(600)
    )
  }, error = function(e) { warning("HTTP error: ", e$message); NULL })
  if (is.null(res) || http_error(res)) return(NA_character_)
  parsed <- content(res, "parsed", simplifyVector = FALSE)
  if (!is.null(parsed$error)) return(NA_character_)
  extract_text(parsed)
}

# 6. API call — web search off ---------------------------------
call_noweb <- function(prompt) {
  res <- tryCatch({
    POST(
      "https://api.x.ai/v1/responses",
      add_headers(Authorization = paste("Bearer", xai_api_key),
                  "Content-Type" = "application/json"),
      body = toJSON(list(
        model       = "grok-4-1-fast-reasoning",
        input       = list(list(role = "user", content = prompt)),
        temperature = temperature,
        max_turns   = 1
      ), auto_unbox = TRUE),
      timeout(600)
    )
  }, error = function(e) { warning("HTTP error: ", e$message); NULL })
  if (is.null(res) || http_error(res)) return(NA_character_)
  parsed <- content(res, "parsed", simplifyVector = FALSE)
  if (!is.null(parsed$error)) return(NA_character_)
  extract_text(parsed)
}

# 7. Run — parallelised across iterations (4 workers) ----------
results <- future_map_dfr(seq_len(n_iter), function(i) {
  shuffled <- sample(urls)
  prompt   <- make_prompt(shuffled)
  bind_rows(
    tibble(model = "Grok 4.1 Fast", web_search = TRUE,
           iteration = i, url_order = list(shuffled),
           raw_response = call_web(prompt)),
    tibble(model = "Grok 4.1 Fast", web_search = FALSE,
           iteration = i, url_order = list(shuffled),
           raw_response = call_noweb(prompt))
  )
}, .options = furrr_options(seed = 42))

# 8. Save ------------------------------------------------------
if (!dir.exists("data-out/news_source")) dir.create("data-out/news_source", recursive = TRUE)
saveRDS(results, "data-out/news_source/raw_grok.rds")
cat("Saved: data-out/news_source/raw_grok.rds\n")
cat("Rows:", nrow(results), "| NAs:", sum(is.na(results$raw_response)), "\n")
