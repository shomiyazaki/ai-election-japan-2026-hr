# -------------------------------------------------------------
# News source classification experiment — Gemini 2.5 Flash
# Web search on/off, 100 iterations each, 4 parallel workers
# Model settings follow 1100_gemini-25flash_api_japan_2026_election.R
# Output: data-out/news_source/raw_gemini.rds
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
google_api_key <- trimws(readLines("google_api_key_2.txt", n = 1, warn = FALSE))
n_iter         <- 100

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

# 4. Gemini API (web search on/off via use_web flag) -----------
call_gemini <- function(prompt, use_web = TRUE) {
  model <- "gemini-2.5-flash"
  url   <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                  model, ":generateContent?key=", google_api_key)
  body <- list(
    contents         = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(temperature = 1),
    safetySettings   = list(
      list(category = "HARM_CATEGORY_HARASSMENT",        threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_HATE_SPEECH",       threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_SEXUALLY_EXPLICIT", threshold = "BLOCK_NONE"),
      list(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
    )
  )
  if (use_web) {
    body$tools <- list(list(google_search = structure(list(), names = character(0))))
  }
  res <- tryCatch(
    POST(url, content_type_json(), body = toJSON(body, auto_unbox = TRUE), timeout(600)),
    error = function(e) NULL
  )
  if (is.null(res)) return("CONNECTION_ERROR")
  if (http_error(res)) {
    msg <- tryCatch(content(res, "parsed")$error$message, error = function(e) "Unknown API Error")
    return(paste("API_ERROR:", msg))
  }
  parsed <- content(res, "parsed")
  if (is.null(parsed$candidates[[1]]$content)) {
    reason <- parsed$candidates[[1]]$finishReason
    return(paste("BLOCKED:", if (is.null(reason)) "UNKNOWN" else reason))
  }
  ans <- tryCatch(parsed$candidates[[1]]$content$parts[[1]]$text, error = function(e) NULL)
  if (is.null(ans) || length(ans) == 0) return("EMPTY_RESPONSE")
  as.character(ans)
}

# 5. Run — parallelised across iterations (4 workers) ----------
results <- future_map_dfr(seq_len(n_iter), function(i) {
  shuffled <- sample(urls)
  prompt   <- make_prompt(shuffled)
  bind_rows(
    tibble(model = "Gemini 2.5 Flash", web_search = TRUE,
           iteration = i, url_order = list(shuffled),
           raw_response = call_gemini(prompt, use_web = TRUE)),
    tibble(model = "Gemini 2.5 Flash", web_search = FALSE,
           iteration = i, url_order = list(shuffled),
           raw_response = call_gemini(prompt, use_web = FALSE))
  )
}, .options = furrr_options(seed = 42))

# 6. Save ------------------------------------------------------
if (!dir.exists("data-out/news_source")) dir.create("data-out/news_source", recursive = TRUE)
saveRDS(results, "data-out/news_source/raw_gemini.rds")
cat("Saved: data-out/news_source/raw_gemini.rds\n")
cat("Rows:", nrow(results), "| NAs:", sum(is.na(results$raw_response)), "\n")
