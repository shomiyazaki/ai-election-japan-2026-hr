# =============================================================================
# 60_values.R
#
# Sections:
#   1. Party recommendation shares – pooled
#   2. Party recommendation shares – control condition
#   3. NA/Error statistics
#   4. Refusal statistics (pooled + by stance + by policy)
#   5. Grok X search effects (Web+X minus Web-only share differences)
#   6. Citation statistics (URL-embedding rates + fractional source shares)
#   7. Top cited domain URL counts
#   8. Policy treatment effect ranges (from OLS regressions)
#   9. Demographic effect ranges (from OLS regressions)
#
# Output: draft/values/*.txt
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)

# --------------------------------------------------------------------------- #
# Helper: write a single number to a .txt file (no trailing newline)
# --------------------------------------------------------------------------- #
write_val <- function(x, name, fmt = "%.1f") {
  cat(sprintf(fmt, x), file = file.path(out_val_dir, paste0(name, ".txt")))
  invisible(x)
}

write_int <- function(x, name) {
  # Plain integer, no comma (for use in math mode or plain context)
  cat(as.character(as.integer(round(x))),
      file = file.path(out_val_dir, paste0(name, ".txt")))
  invisible(x)
}

write_int_fmt <- function(x, name) {
  # Integer formatted with LaTeX-safe comma separator (for text mode)
  formatted <- format(as.integer(round(x)), big.mark = ",", scientific = FALSE)
  cat(formatted, file = file.path(out_val_dir, paste0(name, ".txt")))
  invisible(x)
}

# Model key abbreviations used in file names
mk <- c(
  "GPT-4o Mini"              = "gpt4omini",
  "GPT-5 Mini"               = "gpt5mini",
  "Gemini 2.5 Flash"         = "gemini",
  "Grok 4.1 Fast (Web-only)" = "grok_webonly",
  "Grok 4.1 Fast (Web+X)"    = "grok_webx"
)

# =============================================================================
# Load panel data
# =============================================================================
cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# Subset used for regressions: exclude NA/Error (party is NA for those rows)
panel_reg <- cm_panel %>% filter(!is.na(party))

cat(sprintf("Panel: %d total obs, %d non-NA/Error obs\n",
            nrow(cm_panel), nrow(panel_reg)))

# =============================================================================
# 1. Pooled recommendation shares (all conditions, non-NA/Error denominator)
# =============================================================================
pooled <- panel_reg %>%
  group_by(model) %>%
  summarise(
    n          = n(),
    jcp        = 100 * mean(party == "JCP"),
    ldp        = 100 * mean(party == "LDP"),
    centrist   = 100 * mean(party == "Centrist Reform"),
    innov      = 100 * mean(party == "Innovation"),
    refusal    = 100 * mean(party == "Refusal"),
    .groups    = "drop"
  ) %>%
  mutate(model_key = mk[as.character(model)])

for (i in seq_len(nrow(pooled))) {
  mkey <- pooled$model_key[i]
  write_val(pooled$jcp[i],     paste0("jcp_pooled_",     mkey))
  write_val(pooled$ldp[i],     paste0("ldp_pooled_",     mkey))
  write_val(pooled$refusal[i], paste0("refusal_pooled_", mkey))
}

write_val(min(pooled$jcp), "jcp_pooled_min")
write_val(max(pooled$jcp), "jcp_pooled_max")
write_val(min(pooled$refusal), "refusal_pooled_min")
write_val(max(pooled$refusal), "refusal_pooled_max")

# Actual seat shares from 00_constants.R
seat_pcts <- setNames(actual_pr_seats$seat_pct, actual_pr_seats$party)
write_val(seat_pcts["LDP"],             "ldp_actual_seat_share")
write_val(seat_pcts["JCP"],             "jcp_actual_seat_share")
write_val(seat_pcts["Centrist Reform"], "centrist_actual_seat_share")
write_val(seat_pcts["Innovation"],      "innov_actual_seat_share")

cat("\n--- 1. Pooled JCP rates ---\n")
print(select(pooled, model, n, jcp, refusal))

# =============================================================================
# 2. Control-condition recommendation shares
# =============================================================================
control <- panel_reg %>%
  filter(policy_treatment == "Control") %>%
  group_by(model) %>%
  summarise(
    n         = n(),
    ldp       = 100 * mean(party == "LDP"),
    jcp       = 100 * mean(party == "JCP"),
    innov     = 100 * mean(party == "Innovation"),
    refusal   = 100 * mean(party == "Refusal"),
    .groups   = "drop"
  ) %>%
  mutate(model_key = mk[as.character(model)])

ldp_actual   <- seat_pcts["LDP"]
innov_actual <- seat_pcts["Innovation"]

for (i in seq_len(nrow(control))) {
  mkey <- control$model_key[i]
  write_val(control$ldp[i],                      paste0("ldp_control_",      mkey))
  write_val(control$jcp[i],                      paste0("jcp_control_",      mkey))
  write_val(control$innov[i],                    paste0("innov_control_",    mkey))
  write_val(control$refusal[i],                  paste0("refusal_control_",  mkey))
  write_val(control$ldp[i]   - ldp_actual,       paste0("ldp_control_diff_", mkey))
}

write_val(
  control$innov[control$model_key == "gpt4omini"] - innov_actual,
  "innov_control_diff_gpt4omini"
)

write_int(min(control$n), "n_control_min")
write_int(max(control$n), "n_control_max")

cat("\n--- 2. Control shares ---\n")
print(select(control, model, n, ldp, jcp, innov, refusal))

# =============================================================================
# 3. NA/Error statistics
# GPT-4o Mini T=-1 wave already excluded in 01_clean_cross_model.R
# =============================================================================
na_stats <- cm_panel %>%
  group_by(model) %>%
  summarise(
    n_total = n(),
    n_na    = sum(response_type == "NA/Error"),
    pct_na  = 100 * n_na / n_total,
    .groups = "drop"
  ) %>%
  mutate(model_key = mk[as.character(model)])

for (i in seq_len(nrow(na_stats))) {
  mkey <- na_stats$model_key[i]
  write_int_fmt(na_stats$n_na[i], paste0("n_na_",   mkey))
  write_val(na_stats$pct_na[i],   paste0("pct_na_", mkey))
}

write_int_fmt(sum(na_stats$n_total), "n_obs_total")
write_int_fmt(sum(na_stats$n_na),    "n_na_error_total")
write_val(100 * sum(na_stats$n_na) / sum(na_stats$n_total), "pct_na_error_total")

# Design constants
write_int_fmt(36300, "n_profiles_design")
write_int(1100, "n_obs_per_day")

cat("\n--- 3. NA/Error stats ---\n")
print(na_stats)

# =============================================================================
# 4. Refusal statistics
# =============================================================================

# --- 4a. Pooled refusal (non-NA/Error denominator, already in pooled above) ---
# Already written as refusal_pooled_* above.

# --- 4b. Refusal counts ---
refusal_counts <- panel_reg %>%
  group_by(model) %>%
  summarise(
    n_total   = n(),
    n_refusal = sum(party == "Refusal"),
    pct       = 100 * n_refusal / n_total,
    .groups   = "drop"
  ) %>%
  mutate(model_key = mk[as.character(model)])

for (i in seq_len(nrow(refusal_counts))) {
  mkey <- refusal_counts$model_key[i]
  write_int_fmt(refusal_counts$n_refusal[i], paste0("n_refusal_", mkey))
}

# --- 4c. Refusal by stance (Control / Right / Left) ---
refusal_stance <- panel_reg %>%
  filter(model %in% c("GPT-5 Mini", "Gemini 2.5 Flash")) %>%
  mutate(
    stance = case_when(
      policy_treatment == "Control"                        ~ "Control",
      str_ends(as.character(policy_treatment), ": Right") ~ "Right",
      str_ends(as.character(policy_treatment), ": Left")  ~ "Left"
    )
  ) %>%
  group_by(model, stance) %>%
  summarise(pct = 100 * mean(party == "Refusal"), .groups = "drop") %>%
  mutate(model_key = mk[as.character(model)])

for (i in seq_len(nrow(refusal_stance))) {
  mkey   <- refusal_stance$model_key[i]
  stance <- tolower(refusal_stance$stance[i])
  write_val(refusal_stance$pct[i], paste0("refusal_", stance, "_", mkey))
}

cat("\n--- 4. Refusal by stance ---\n")
print(refusal_stance)

# --- 4d. Refusal by specific policy issue (GPT-5 Mini) ---
refusal_policy <- panel_reg %>%
  filter(model == "GPT-5 Mini", policy_treatment != "Control") %>%
  mutate(pt = as.character(policy_treatment)) %>%
  group_by(pt) %>%
  summarise(pct = 100 * mean(party == "Refusal"), .groups = "drop") %>%
  arrange(desc(pct))

cat("\n--- 4d. GPT-5 Mini refusal by policy (top 5) ---\n")
print(head(refusal_policy, 5))

# Write specific issues mentioned in text
lookup_refusal <- function(issue) {
  val <- refusal_policy$pct[refusal_policy$pt == issue]
  if (length(val)) val[1] else NA_real_
}

write_val(lookup_refusal("Foreign Workers: Right"),   "refusal_foreign_workers_right_gpt5mini")
write_val(lookup_refusal("Permanent Residency: Right"), "refusal_perm_res_right_gpt5mini")

# =============================================================================
# 5. Grok X search effects (Web+X minus Web-only pooled share differences)
# =============================================================================
grok_shares <- panel_reg %>%
  filter(model %in% c("Grok 4.1 Fast (Web-only)", "Grok 4.1 Fast (Web+X)")) %>%
  group_by(model, party) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model) %>%
  mutate(share = 100 * n / sum(n)) %>%
  ungroup()

grok_diff <- grok_shares %>%
  select(model, party, share) %>%
  pivot_wider(names_from = model, values_from = share, values_fill = 0) %>%
  rename(
    webonly = `Grok 4.1 Fast (Web-only)`,
    webx    = `Grok 4.1 Fast (Web+X)`
  ) %>%
  mutate(diff = webx - webonly) %>%
  arrange(desc(diff))

cat("\n--- 5. Grok X effects ---\n")
print(grok_diff)

party_keys <- c(
  "LDP"            = "ldp",
  "JCP"            = "jcp",
  "Reiwa"          = "reiwa",
  "Centrist Reform" = "centrist",
  "SDP"            = "sdp",
  "Team Mirai"     = "team_mirai",
  "Conservative"   = "conservative",
  "Innovation"     = "innov",
  "DPP"            = "dpp",
  "Sanseito"       = "sanseito",
  "Refusal"        = "refusal"
)

for (party_en in names(party_keys)) {
  row <- grok_diff %>% filter(party == party_en)
  if (nrow(row) > 0) {
    write_val(row$diff, paste0("grokx_diff_", party_keys[party_en]))
  }
}

# =============================================================================
# 6. Citation statistics (URL-embedding rates + fractional source shares)
# =============================================================================

model_key_map_raw <- c(
  "gemini-25flash" = "Gemini 2.5 Flash",
  "gpt4omini"      = "GPT-4o Mini",
  "gpt5mini"       = "GPT-5 Mini",
  "grok_web_x"     = "Grok 4.1 Fast (Web+X)",
  "grok_webonly"   = "Grok 4.1 Fast (Web-only)"
)

extract_urls <- function(text) {
  if (is.na(text) || text == "" || text == "CONNECTION_ERROR") return(character(0))
  str_extract_all(text, "https?://[^\\s\\)\\]\\,\"'<>\u3000]+")[[1]]
}

parse_domain <- function(url) {
  url %>% str_extract("(?<=://)[^/]+") %>% str_remove("^www\\.")
}

classify_domain <- function(domain) {
  case_when(
    domain %in% c(
      "nikkei.com", "asahi.com", "yomiuri.co.jp", "mainichi.jp",
      "sankei.com", "kyodo.co.jp", "jiji.com"
    ) ~ "National newspaper",
    domain %in% c(
      "nhk.or.jp", "news.ntv.co.jp", "news.tv-asahi.co.jp",
      "newsdig.tbs.co.jp", "fnn.jp", "tv-tokyo.co.jp"
    ) ~ "National TV network",
    domain %in% c(
      "jimin.jp",                      # LDP
      "komei.or.jp",                   # Centrist Reform (Komei predecessor)
      "o-ishin.jp",                    # Innovation
      "dpfp.or.jp",                    # DPP
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

raw_files <- list.files(cm_data_dir, pattern = "\\.csv$", full.names = TRUE)

all_responses <- map_dfr(raw_files, function(f) {
  fname     <- basename(f)
  mkey      <- str_extract(fname, paste(names(model_key_map_raw), collapse = "|"))
  if (is.na(mkey)) return(tibble())
  # Exclude GPT-4o Mini T=-1 all-NA wave (2026-02-07)
  if (str_detect(fname, "gpt4omini") && str_detect(fname, "2026-02-07")) return(tibble())
  model_name <- model_key_map_raw[[mkey]]
  df         <- read_csv(f, show_col_types = FALSE)
  resp_col   <- names(df)[str_detect(names(df), "response")][1]
  df %>%
    select(resp = all_of(resp_col)) %>%
    filter(!is.na(resp), resp != "NA", resp != "CONNECTION_ERROR") %>%
    mutate(model = model_name)
})

all_responses <- all_responses %>%
  mutate(
    urls    = map(resp, extract_urls),
    n_urls  = map_int(urls, length),
    domains = map(urls, parse_domain)
  )

response_n <- all_responses %>% count(model, name = "n_resp")

# --- 6a. URL-embedding rates ---
url_rates <- all_responses %>%
  group_by(model) %>%
  summarise(pct_has_url = 100 * mean(n_urls > 0), .groups = "drop") %>%
  mutate(model_key = mk[as.character(model)])

for (i in seq_len(nrow(url_rates))) {
  write_val(url_rates$pct_has_url[i], paste0("cite_url_rate_", url_rates$model_key[i]))
}

cat("\n--- 6a. URL embedding rates ---\n")
print(url_rates)

# --- 6b. Fractional source shares (same method as 23_fig3_citations.R) ---
response_cat <- all_responses %>%
  mutate(
    cat_weights = map(domains, function(doms) {
      if (length(doms) == 0) return(tibble(category = "No citation", weight = 1))
      cats      <- classify_domain(doms)
      tab       <- table(cats)
      max_count <- max(tab)
      tied      <- names(tab[tab == max_count])
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
    share     = weight_sum / n_resp * 100,
    model_key = mk[as.character(model)],
    cat_key   = case_when(
      category == "National newspaper"         ~ "newspaper",
      category == "National TV network"         ~ "tv",
      category == "Political party site"       ~ "party_sites",
      category == "Social media (X / YouTube)" ~ "social",
      category == "Major news aggregator"      ~ "aggregator",
      category == "Wikipedia"                  ~ "wikipedia",
      category == "No citation"                ~ "no_citation",
      TRUE                                     ~ "other"
    )
  )

for (i in seq_len(nrow(comp_weighted))) {
  write_val(
    comp_weighted$share[i],
    paste0("cite_", comp_weighted$cat_key[i], "_", comp_weighted$model_key[i])
  )
}

cat("\n--- 6b. Fractional source shares ---\n")
comp_weighted %>% arrange(model, desc(share)) %>% print(n = 50)

# =============================================================================
# 7. Top cited domain URL counts (raw URL counts, not fractional)
# =============================================================================
all_urls_long <- all_responses %>%
  filter(n_urls > 0) %>%
  select(model, domains) %>%
  unnest(domains) %>%
  rename(domain = domains)

top_domains <- all_urls_long %>%
  count(model, domain, name = "n_urls") %>%
  group_by(model) %>%
  slice_max(n_urls, n = 10) %>%
  arrange(model, desc(n_urls))

cat("\n--- 7. Top domains by model ---\n")
print(top_domains, n = 50)

get_count <- function(model_str, domain_str) {
  val <- top_domains$n_urls[top_domains$model == model_str &
                              top_domains$domain == domain_str]
  if (length(val) == 0) NA_integer_ else val[1]
}

# GPT-5 Mini
write_int_fmt(get_count("GPT-5 Mini", "news.tv-asahi.co.jp"), "urlcount_tvasahi_gpt5mini")
write_int_fmt(get_count("GPT-5 Mini", "jcp.or.jp"),           "urlcount_jcp_gpt5mini")

# Grok Web-only
write_int_fmt(get_count("Grok 4.1 Fast (Web-only)", "nikkei.com"),  "urlcount_nikkei_grok_webonly")
write_int_fmt(get_count("Grok 4.1 Fast (Web-only)", "jcp.or.jp"),   "urlcount_jcp_grok_webonly")

# Grok Web+X
write_int_fmt(get_count("Grok 4.1 Fast (Web+X)", "x.com"),       "urlcount_xcom_grok_webx")
write_int_fmt(get_count("Grok 4.1 Fast (Web+X)", "nikkei.com"),  "urlcount_nikkei_grok_webx")
write_int_fmt(get_count("Grok 4.1 Fast (Web+X)", "jcp.or.jp"),   "urlcount_jcp_grok_webx")

# =============================================================================
# 8. Policy treatment effect ranges (OLS, Equation 2)
# Spec: Y ~ policy_treatment | gender + area_type + wave + region
# =============================================================================

get_policy_coefs <- function(outcome) {
  map_dfr(model_levels, function(mod) {
    df  <- filter(panel_reg, model == mod)
    fit <- feols(
      as.formula(paste0(outcome, " ~ policy_treatment | gender + area_type + wave + region")),
      data = df, vcov = "hetero"
    )
    coefs <- coef(fit)
    tibble(
      model    = mod,
      term     = str_remove(names(coefs), "^policy_treatment"),
      estimate = as.numeric(coefs)
    )
  })
}

cat("\n--- 8. Running policy OLS for JCP and LDP ---\n")
jcp_coefs <- get_policy_coefs("JCP")
ldp_coefs <- get_policy_coefs("LDP")

# Constitutional amendment effects
jcp_const_left  <- jcp_coefs %>%
  filter(term == "Constitutional Amendment: Left") %>% pull(estimate)
ldp_const_right <- ldp_coefs %>%
  filter(term == "Constitutional Amendment: Right") %>% pull(estimate)

write_int(round(min(jcp_const_left)  * 100), "policy_jcp_const_amend_left_min")
write_int(round(max(jcp_const_left)  * 100), "policy_jcp_const_amend_left_max")
write_int(round(min(ldp_const_right) * 100), "policy_ldp_const_amend_right_min")
write_int(round(max(ldp_const_right) * 100), "policy_ldp_const_amend_right_max")

# JCP left-leaning positive swings across all issues and models
all_left_jcp  <- jcp_coefs %>% filter(str_ends(term, ": Left"), estimate > 0) %>% pull(estimate)
all_right_ldp <- ldp_coefs %>% filter(str_ends(term, ": Right"), estimate > 0) %>% pull(estimate)

write_int(round(min(all_left_jcp)  * 100), "policy_jcp_left_min")
write_int(round(max(all_left_jcp)  * 100), "policy_jcp_left_max")
write_int(round(min(all_right_ldp) * 100), "policy_ldp_right_min")
write_int(round(max(all_right_ldp) * 100), "policy_ldp_right_max")

cat(sprintf(
  "JCP const amend Left: %.0f--%.0f pp\n",
  min(jcp_const_left)*100, max(jcp_const_left)*100
))
cat(sprintf(
  "LDP const amend Right: %.0f--%.0f pp\n",
  min(ldp_const_right)*100, max(ldp_const_right)*100
))
cat(sprintf(
  "JCP left positive swings: %d--%d pp\n",
  round(min(all_left_jcp)*100), round(max(all_left_jcp)*100)
))

# =============================================================================
# 9. Demographic effect ranges (OLS, Equation 1)
# Spec: Y ~ gender + area_type + region | wave + policy_treatment
# =============================================================================

get_demo_coefs <- function(outcome) {
  map_dfr(model_levels, function(mod) {
    df  <- filter(panel_reg, model == mod)
    fit <- feols(
      as.formula(paste0(outcome, " ~ gender + area_type + region | wave + policy_treatment")),
      data = df, vcov = "hetero"
    )
    coefs <- coef(fit)
    tibble(
      model    = mod,
      term     = names(coefs),
      estimate = as.numeric(coefs)
    )
  })
}

cat("\n--- 9. Running demographic OLS for LDP and JCP ---\n")
ldp_demo <- get_demo_coefs("LDP")
jcp_demo <- get_demo_coefs("JCP")

# Female coefficient on LDP (one decimal place, e.g. "0.5--6.9 pp")
female_ldp <- ldp_demo %>% filter(str_detect(term, "Female")) %>% pull(estimate)
write_val(min(abs(female_ldp)) * 100, "demo_female_ldp_effect_min")
write_val(max(abs(female_ldp)) * 100, "demo_female_ldp_effect_max")

# GPT-4o Mini Hokuriku-Shinetsu LDP premium (text: "+9.1pp")
hokuriku <- ldp_demo %>%
  filter(model == "GPT-4o Mini", str_detect(term, "Hokuriku")) %>%
  pull(estimate)
if (length(hokuriku) > 0) write_val(hokuriku * 100, "demo_ldp_hokuriku_gpt4omini")

cat(sprintf(
  "Female LDP effect range: %.1f--%.1f pp\n",
  min(abs(female_ldp)) * 100, max(abs(female_ldp)) * 100
))

# =============================================================================
# Summary
# =============================================================================
val_files <- list.files(out_val_dir, pattern = "\\.txt$")
cat(sprintf("\nDone. Wrote %d value files to draft/values/\n", length(val_files)))
cat(paste(sort(val_files), collapse = "\n"), "\n")
