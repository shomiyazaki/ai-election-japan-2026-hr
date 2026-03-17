# =============================================================================
# 23_fig3_citations.R
# Figure 3 (main text): URL-level source composition per model (stacked bar).
#
# Each response contributes exactly 1 unit, assigned to its plurality source
# category. Ties are split equally. Responses with no URLs contribute 1 unit
# to "No citation". All bars sum to exactly 100% of responses.
#
# Supplementary top-5 domains figure split into:
#   46_sfig_citations_top5.R
#
# Output: draft/figures/fig_citations.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))
library(scales)

# --- Helpers ---

extract_urls <- function(text) {
  if (is.na(text) || text == "" || text == "CONNECTION_ERROR") return(character(0))
  str_extract_all(text, "https?://[^\\s\\)\\]\\,\"'<>\u3000]+")[[1]]
}

parse_domain <- function(url) {
  url %>%
    str_extract("(?<=://)[^/]+") %>%
    str_remove("^www\\.")
}

# Source-type classification (party domains ordered by cm_party_levels)
classify_domain <- function(domain) {
  case_when(
    domain %in% c(
      "nikkei.com", "asahi.com", "yomiuri.co.jp", "mainichi.jp",
      "sankei.com", "kyodo.co.jp", "jiji.com"
    ) ~ "National newspaper",
    domain %in% c(
      "news.web.nhk", "news.ntv.co.jp", "news.tv-asahi.co.jp",
      "newsdig.tbs.co.jp", "fnn.jp", "txbiz.tv-tokyo.co.jp"
    ) ~ "National TV network",
    domain %in% c(
      "jimin.jp",                      # LDP
      "komei.or.jp",                   # Centrist Reform (Komei predecessor)
      "o-ishin.jp",                    # Innovation
      "new-kokumin.jp",                # DPP
      "jcp.or.jp",                     # JCP
      "reiwa-shinsengumi.com",         # Reiwa
      "sanseito.jp",                   # Sanseito
      "hoshuto.jp",      # Conservative
      "sdp.or.jp",                     # SDP
      "team-mir.ai",                   # Team Mirai
      "genzeinippon.com" # Tax Cut Coalition
    ) ~ "Political party site",
    domain %in% c("x.com", "twitter.com", "youtube.com") ~ "Social media (X / YouTube)",
    domain %in% c(
      "news.yahoo.co.jp", "nippon.com", "msn.com",
      "news.livedoor.com", "news.google.com", "bing.com",
      "infoseek.co.jp", "hatena.ne.jp"
    ) ~ "Major news aggregator",
    str_detect(domain, "wikipedia\\.org") ~ "Wikipedia",
    TRUE ~ "Other"
  )
}

model_key_map <- c(
  "gemini-25flash" = "Gemini 2.5 Flash",
  "gpt4omini"      = "GPT-4o Mini",
  "gpt5mini"       = "GPT-5 Mini",
  "grok_web_x"     = "Grok 4.1 Fast (Web+X)",
  "grok_webonly"   = "Grok 4.1 Fast (Web-only)"
)

cat_levels <- c(
  "National newspaper",
  "National TV network",
  "Political party site",
  "Social media (X / YouTube)",
  "Major news aggregator",
  "Wikipedia",
  "Other"
)

cat_colors <- c(
  "National newspaper"         = "#5DCFFF",
  "National TV network"        = "#70DC63",
  "Political party site"       = "#FAE355",
  "Social media (X / YouTube)" = "#FF9B3A",
  "Major news aggregator"      = "#FF3A3E",
  "Wikipedia"                  = "#3E6CB9",
  "Other"                      = "#6F4E9A",
  "No citation"                = "#CCCCCC"
)

# Legend / stacking order: National newspaper first, No citation last
cat_order <- c(
  "National newspaper", "National TV network", "Political party site",
  "Social media (X / YouTube)", "Major news aggregator", "Wikipedia", "Other", "No citation"
)

# =============================================================================
# Read raw responses
# =============================================================================

raw_files <- list.files(cm_data_dir, pattern = "\\.csv$", full.names = TRUE)

all_responses <- map_dfr(raw_files, function(f) {
  fname     <- basename(f)
  model_key <- str_extract(fname, paste(names(model_key_map), collapse = "|"))
  if (is.na(model_key)) return(tibble())
  # GPT-4o Mini T=-1: all-NA wave, excluded to match main analysis
  if (str_detect(fname, "gpt4omini") && str_detect(fname, "2026-02-07")) return(tibble())

  model_name <- model_key_map[[model_key]]
  df         <- read_csv(f, show_col_types = FALSE)
  resp_col   <- names(df)[str_detect(names(df), "response")][1]

  df %>%
    select(resp = all_of(resp_col)) %>%
    filter(!is.na(resp), resp != "NA", resp != "CONNECTION_ERROR") %>%
    mutate(model = model_name)
})

cat(sprintf("Total responses: %d\n", nrow(all_responses)))

# URL extraction (response level)
all_responses <- all_responses %>%
  mutate(
    urls    = map(resp, extract_urls),
    n_urls  = map_int(urls, length),
    domains = map(urls, parse_domain)
  )

response_n <- all_responses %>%
  count(model, name = "n_resp") %>%
  mutate(model = factor(model, levels = model_levels))

no_cite_rates <- all_responses %>%
  group_by(model) %>%
  summarise(pct_no_cite = mean(n_urls == 0) * 100, .groups = "drop")

cat("\nResponse n and no-citation rates:\n")
print(left_join(response_n, no_cite_rates, by = "model"))

# =============================================================================
# URL-level data (kept for Figure 2 / top-5 domains)
# =============================================================================

all_urls_long <- all_responses %>%
  filter(n_urls > 0) %>%
  select(model, domains) %>%
  unnest(domains) %>%
  rename(domain = domains) %>%
  mutate(category = classify_domain(domain))

url_totals <- all_urls_long %>%
  count(model, name = "n_urls")

# =============================================================================
# Stacked bar — fractional response assignment per model
#
# Each response contributes exactly 1 unit, assigned to its plurality source
# category. Ties are split equally (e.g., 1/2 each for a two-way tie).
# Responses with no URLs contribute 1 unit to "No citation".
# All bars sum to exactly 100% of responses.
# =============================================================================

response_cat <- all_responses %>%
  mutate(
    cat_weights = map(domains, function(doms) {
      if (length(doms) == 0) {
        return(tibble(category = "No citation", weight = 1))
      }
      cats       <- classify_domain(doms)
      cat_counts <- table(cats)
      max_count  <- max(cat_counts)
      tied       <- names(cat_counts[cat_counts == max_count])
      tibble(category = tied, weight = 1 / length(tied))
    })
  ) %>%
  select(model, cat_weights) %>%
  unnest(cat_weights)

comp_weighted <- response_cat %>%
  group_by(model, category) %>%
  summarise(weight_sum = sum(weight), .groups = "drop") %>%
  left_join(response_n, by = "model") %>%
  mutate(
    share    = weight_sum / n_resp * 100,
    model    = factor(model, levels = rev(model_levels)),
    category = factor(category, levels = cat_order)
  ) %>%
  select(model, category, share)

# Annotation: response n (comparable denominator)
bar_labels <- response_n %>%
  mutate(
    model = factor(model, levels = rev(model_levels)),
    label = paste0("n=", comma(n_resp))
  )

p_comp <- ggplot(comp_weighted,
                 aes(x = model, y = share, fill = category)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.2,
           position = position_stack(reverse = TRUE)) +
  geom_text(
    data = bar_labels,
    aes(x = model, y = 102, label = label),
    inherit.aes = FALSE,
    hjust = 0, size = 2.5
  ) +
  scale_fill_manual(values = cat_colors, name = "Source type",
                    limits = cat_order) +
  scale_y_continuous(limits = c(0, 115),
                     breaks = seq(0, 100, 25),
                     labels = paste0(seq(0, 100, 25), "%")) +
  coord_flip() +
  labs(x = NULL, y = "Share of responses (%)") +
  theme_bw(base_size = 9) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 7),
    legend.title       = element_text(size = 8),
    axis.text.y        = element_text(size = 8),
    axis.text.x        = element_text(size = 7),
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(file.path(out_fig_dir, "fig_citations.pdf"), p_comp,
       width = 7, height = 3.5)
cat("Saved: draft/figures/fig_citations.pdf\n")

# =============================================================================
# Diagnostics for text
# =============================================================================

cat("\nFractional response assignment by model and category:\n")
comp_weighted %>%
  arrange(model, desc(share)) %>%
  print(n = 40)
