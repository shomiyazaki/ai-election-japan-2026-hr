# =============================================================================
# 25_fig5_news_classification.R
# Figure 5 (main text): News source classification rate by URL and model.
#
# Heatmap of % classified as "News Media" across 4 models x web ON/OFF,
# for party pages, newspapers, TV networks, and decoy URLs.
# 100 iterations each; URLs shuffled per iteration.
#
# Output: draft/figures/fig_news_classification.pdf
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(ggplot2)
library(scales)

source(here::here("code", "00_constants.R"))

data_dir <- here::here("data-out/news_source")

# -----------------------------------------------------------------------------
# Load and parse raw RDS files
# -----------------------------------------------------------------------------
rds_files <- list.files(data_dir, pattern = "^raw_.*\\.rds$", full.names = TRUE)
raw <- map_dfr(rds_files, readRDS)

parse_one <- function(text, url_order, iteration, model, web_search) {
  tryCatch({
    clean    <- str_remove_all(text, "```(?:json)?\\s*|\\s*```")
    json_str <- str_extract(clean, "(?s)\\[.*\\]")
    if (is.na(json_str)) stop("no JSON array")
    fromJSON(json_str) |>
      as_tibble() |>
      select(any_of(c("url", "category"))) |>
      mutate(iteration = iteration, model = model,
             web_search = web_search, parse_ok = TRUE)
  }, error = function(e) {
    tibble(url = url_order, category = NA_character_,
           iteration = iteration, model = model,
           web_search = web_search, parse_ok = FALSE)
  })
}

df <- pmap_dfr(
  list(raw$raw_response, raw$url_order,
       raw$iteration, raw$model, raw$web_search),
  parse_one
) |>
  mutate(category = case_when(
    category == "報道" ~ "News Media",
    category == "広報" ~ "PR",
    TRUE              ~ category
  ))

# -----------------------------------------------------------------------------
# Taxonomy and URL normalization
# -----------------------------------------------------------------------------
party_urls <- c(
  "jimin.jp/news", "craj.jp/news", "o-ishin.jp/news", "new-kokumin.jp/news",
  "jcp.or.jp/akahata", "reiwa-shinsengumi.com/activity", "sanseito.jp/news",
  "hoshuto.jp/category/news", "sdp.or.jp/category/information",
  "team-mir.ai", "genzeinippon.com"
)
newspaper_urls <- c(
  "nikkei.com", "asahi.com", "yomiuri.co.jp", "mainichi.jp",
  "sankei.com", "kyodo.co.jp", "jiji.com"
)
tv_urls <- c(
  "news.web.nhk", "news.ntv.co.jp", "news.tv-asahi.co.jp",
  "newsdig.tbs.co.jp", "fnn.jp", "txbiz.tv-tokyo.co.jp"
)

df <- df |>
  mutate(
    url = url |>
      str_remove("^https?://") |>
      str_replace("reiwa-shinsenumi\\.com",  "reiwa-shinsengumi.com") |>
      str_replace("reiway-shinsengumi\\.com", "reiwa-shinsengumi.com") |>
      str_replace("mainichi\\.com",           "mainichi.jp") |>
      str_replace("toyotimes\\.jp",           "toyotatimes.jp")
  ) |>
  mutate(
    type = case_when(
      url %in% party_urls     ~ "Party",
      url %in% newspaper_urls ~ "Newspaper",
      url %in% tv_urls        ~ "TV",
      url == "bunshun.jp/list/magazine/shukan-bunshun" ~ "Decoy (News Media)",
      TRUE                    ~ "Decoy (PR)"
    ),
    type      = factor(type, levels = c("Newspaper", "TV", "Party",
                                        "Decoy (News Media)", "Decoy (PR)")),
    model     = factor(model, levels = c("GPT-4o Mini", "GPT-5 Mini",
                                         "Gemini 2.5 Flash", "Grok 4.1 Fast")),
    web_label = if_else(web_search, "Web ON", "Web OFF"),
    url_short = factor(url, levels = rev(c(
      "nikkei.com", "asahi.com", "yomiuri.co.jp", "mainichi.jp",
      "sankei.com", "kyodo.co.jp", "jiji.com",
      "news.web.nhk", "news.ntv.co.jp", "news.tv-asahi.co.jp",
      "newsdig.tbs.co.jp", "fnn.jp", "txbiz.tv-tokyo.co.jp",
      "jimin.jp/news", "craj.jp/news", "o-ishin.jp/news", "new-kokumin.jp/news",
      "jcp.or.jp/akahata", "reiwa-shinsengumi.com/activity", "sanseito.jp/news",
      "hoshuto.jp/category/news", "sdp.or.jp/category/information",
      "team-mir.ai", "genzeinippon.com",
      "bunshun.jp/list/magazine/shukan-bunshun", "nomura.co.jp/introduc/news",
      "hinatazaka46.com/s/official/news/list", "keio.ac.jp/ja/news",
      "jpo.go.jp/news", "toyotatimes.jp"
    )))
  )

# -----------------------------------------------------------------------------
# Summarise
# -----------------------------------------------------------------------------
url_sum <- df |>
  filter(!is.na(category)) |>
  group_by(model, url, url_short, type, web_label) |>
  summarise(
    n   = n(),
    k   = sum(category == "News Media"),
    pct = k / n,
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
fig5 <- ggplot(url_sum,
               aes(x = model, y = url_short, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = round(pct * 100)),
            size = 2.5, colour = "black") +
  scale_fill_gradient(low = "#01B7E4", high = "#F7AD18",
                      labels = percent, limits = c(0, 1),
                      name = "% News Media") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  facet_grid(type ~ web_label, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_bw(base_size = 9) +
  theme(strip.text.y    = element_text(angle = 0),
        axis.text.y     = element_text(size = 8),
        axis.text.x     = element_text(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"))

ggsave(file.path(out_fig_dir, "fig_news_classification.pdf"), fig5,
       width = 8, height = 8)
cat("Saved: figures/fig_news_classification.pdf\n")
