# =============================================================================
# 24_fig4_citations_top5.R
# Figure 4: Top 5 cited domains per model (raw URL counts).
#
# GPT-4o Mini T=-1 wave (2026-02-07) excluded: all responses are literal "NA".
#
# Output: draft/figures/fig_citations_top5.pdf
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
      "hoshuto.jp", "nihon-ho.jp",     # Conservative
      "sdp.or.jp",                     # SDP
      "team-mir.ai",                   # Team Mirai
      "genzeinippon.com", "teamfuture.jp" # Tax Cut Coalition
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
    mutate(resp = as.character(resp)) %>%
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

# =============================================================================
# URL-level data
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
# Figure: Top 5 domains per model (% of responses, raw URL counts)
# =============================================================================

# URL counts per domain per model
domain_url_counts <- all_urls_long %>%
  count(model, domain, name = "n_urls") %>%
  mutate(category = classify_domain(domain))

top5 <- domain_url_counts %>%
  group_by(model) %>%
  slice_max(n_urls, n = 5, with_ties = FALSE) %>%
  arrange(model, n_urls) %>%
  mutate(
    rank       = row_number(),
    domain_fct = paste0(as.character(model), "__", sprintf("%02d", rank))
  ) %>%
  ungroup() %>%
  mutate(
    domain_fct = factor(domain_fct, levels = unique(domain_fct)),
    model      = factor(model, levels = model_levels)
  )

# Facet label: total URL count per model
facet_label_map <- setNames(
  paste0(as.character(url_totals$model), "\n(n = ", comma(url_totals$n_urls), " URLs)"),
  as.character(url_totals$model)
)

top5 <- top5 %>%
  mutate(model_label = factor(facet_label_map[as.character(model)],
                               levels = unname(facet_label_map[model_levels])))

# Clean domain labels (strip model prefix added for ordering)
domain_labels <- setNames(top5$domain, top5$domain_fct)

p_top5 <- ggplot(top5, aes(x = domain_fct, y = n_urls, fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(n_urls)),
            hjust = -0.5, size = 2.3) +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_x_discrete(labels = domain_labels) +
  scale_fill_manual(values = cat_colors, name = "Source type",
                    limits = cat_order) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.25))
  ) +
  coord_flip() +
  labs(x = NULL, y = "Number of URLs cited") +
  theme_bw(base_size = 8) +
  theme(
    strip.text         = element_text(face = "bold", size = 7),
    legend.position    = "bottom",
    legend.text        = element_text(size = 7),
    axis.text.y        = element_text(size = 7, family = "mono"),
    axis.text.x        = element_text(size = 6),
    panel.grid.major.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(out_fig_dir, "fig_citations_top5.pdf"), p_top5,
       width = 8, height = 6)
cat("Saved: figures/fig_citations_top5.pdf\n")
