# =============================================================================
# 48_fig_jcp_allmodels.R
# Figure: JCP policy treatment effects across all models (cross-model + flagship).
#
# Regression: feols(JCP ~ policy_stance_lr | gender + area_type)
# No region/wave FE — ensures comparability across all datasets.
#
# Output: draft/figures/fig_jcp_allmodels.pdf
#         draft/figures/fig_jcp_allmodels.jpg
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)

# =============================================================================
# 1. Constants
# =============================================================================

cross_model_levels    <- c("GPT-4o Mini", "GPT-5 Mini", "Gemini 2.5 Flash",
                           "Grok 4.1 Fast (Web-only)")
flagship_model_levels <- c("GPT-5.4", "Gemini 3.1 Pro", "Grok 4.20", "Claude Opus 4.6")
all_model_levels      <- c(cross_model_levels, flagship_model_levels)

condition_levels <- c("Pre-election (with Web Search)", "Post-election (with Web Search)", "Post-election (without Web Search)")
condition_colors <- c(
  "Pre-election (with Web Search)"     = "#63D7F1",
  "Post-election (with Web Search)"    = "#D4427E",
  "Post-election (without Web Search)" = "#B3DB7E"
)

# =============================================================================
# 2. Data source registry
# =============================================================================

pe_dir <- file.path(proj_root, "data-out", "post_election")
fm_dir <- file.path(proj_root, "data-out", "flagship_models")

sources <- tribble(
  ~dir,    ~pattern,                                        ~model,                      ~condition,
  pe_dir,  "japan2026_election_gpt4omini_wave_",            "GPT-4o Mini",               "Post-election (with Web Search)",
  pe_dir,  "japan2026_election_gpt4omini_noweb_wave_",      "GPT-4o Mini",               "Post-election (without Web Search)",
  pe_dir,  "japan2026_election_gpt5mini_wave_",             "GPT-5 Mini",                "Post-election (with Web Search)",
  pe_dir,  "japan2026_election_gpt5mini_noweb_wave_",       "GPT-5 Mini",                "Post-election (without Web Search)",
  pe_dir,  "japan2026_election_gemini-25flash_wave_",       "Gemini 2.5 Flash",          "Post-election (with Web Search)",
  pe_dir,  "japan2026_election_gemini-25flash_noweb_wave_", "Gemini 2.5 Flash",          "Post-election (without Web Search)",
  pe_dir,  "japan2026_election_grok_webonly_wave_",         "Grok 4.1 Fast (Web-only)",  "Post-election (with Web Search)",
  pe_dir,  "japan2026_election_grok_noweb_wave_",           "Grok 4.1 Fast (Web-only)",  "Post-election (without Web Search)",

  fm_dir,  "gpt54_web",                                     "GPT-5.4",                   "Post-election (with Web Search)",
  fm_dir,  "gpt54_noweb",                                   "GPT-5.4",                   "Post-election (without Web Search)",
  fm_dir,  "gemini31pro_web",                               "Gemini 3.1 Pro",            "Post-election (with Web Search)",
  fm_dir,  "gemini31pro_noweb",                             "Gemini 3.1 Pro",            "Post-election (without Web Search)",
  fm_dir,  "grok420_web",                                   "Grok 4.20",                 "Post-election (with Web Search)",
  fm_dir,  "grok420_noweb",                                 "Grok 4.20",                 "Post-election (without Web Search)",
  fm_dir,  "claude_opus46_web",                             "Claude Opus 4.6",           "Post-election (with Web Search)",
  fm_dir,  "claude_opus46_noweb",                           "Claude Opus 4.6",           "Post-election (without Web Search)"
)

# =============================================================================
# 3. Loader
# =============================================================================

load_source <- function(dir, pattern, model, condition) {
  path <- list.files(dir, pattern = pattern, full.names = TRUE)[1]
  if (is.na(path)) { message("Skipping (not found): ", pattern); return(NULL) }

  df   <- read_csv(path, show_col_types = FALSE)
  resp <- intersect(c("gpt_response", "gemini_response", "grok_response", "claude_response"),
                    names(df))[1]

  df %>%
    rename(response = !!sym(resp)) %>%
    mutate(
      party_raw        = str_trim(str_remove_all(
                           if (resp == "claude_response")
                             coalesce(str_match(response, "---\\s*([^\n]+)")[, 2],
                                      str_extract(response, "[^\n]+"))
                           else
                             str_extract(response, "^[^\n.。]+"),
                           "\\*|^#+\\s*")),
      party_matched    = map_chr(party_raw, match_party),
      party            = case_when(
        !is.na(party_matched)          ~ party_matched,
        !is_technical_error(response)  ~ "Refusal",
        TRUE                           ~ NA_character_
      ),
      JCP              = as.integer(party == "JCP"),
      gender           = case_when(gender    == "男性" ~ "Male",  gender    == "女性" ~ "Female",  TRUE ~ gender),
      area_type        = case_when(area_type == "都市部" ~ "Urban", area_type == "地方部" ~ "Rural", TRUE ~ area_type),
      policy_issue_en  = policy_map[policy_issue],
      policy_stance_lr = case_when(
        policy_stance == "control"                                    ~ "Control",
        policy_stance == "賛成" & policy_issue_en %in% pro_is_right  ~ "Right",
        policy_stance == "賛成"                                       ~ "Left",
        policy_stance == "反対" & policy_issue_en %in% pro_is_right  ~ "Left",
        policy_stance == "反対"                                       ~ "Right"
      ),
      model     = model,
      condition = condition
    ) %>%
    filter(!is.na(party)) %>%
    select(model, condition, gender, area_type, policy_stance_lr, JCP)
}

# =============================================================================
# 4. Load all data
# =============================================================================

feb_data <- read_csv(file.path(out_data_dir, "cross_model_clean.csv"),
                     show_col_types = FALSE) %>%
  filter(response_type %in% c("Valid Party", "Refusal")) %>%
  transmute(model, condition = "Pre-election (with Web Search)", gender, area_type,
            policy_stance_lr = policy_stance,
            JCP = as.integer(party == "JCP"))

raw_data <- pmap(sources, load_source) %>% bind_rows()

all_data <- bind_rows(feb_data, raw_data) %>%
  mutate(
    policy_stance_lr = factor(policy_stance_lr, levels = c("Control", "Left", "Right")),
    condition        = factor(condition,         levels = condition_levels)
  )

# =============================================================================
# 5. Regressions — JCP only
# =============================================================================

extract_jcp_coefs <- function(df_sub, keys) {
  tryCatch({
    mod <- feols(JCP ~ policy_stance_lr | gender + area_type,
                 data = df_sub, vcov = "HC1")
    as.data.frame(coeftable(mod)) %>%
      rownames_to_column("term") %>%
      filter(str_detect(term, "^policy_stance_lr")) %>%
      transmute(
        model     = keys$model,
        condition = keys$condition,
        stance    = str_remove(term, "^policy_stance_lr"),
        estimate  = Estimate,
        se        = `Std. Error`,
        ci_low    = estimate - 1.96 * se,
        ci_high   = estimate + 1.96 * se
      )
  }, error = function(e) NULL)
}

coef_df <- all_data %>%
  group_by(model, condition) %>%
  group_map(extract_jcp_coefs) %>%
  bind_rows() %>%
  mutate(
    stance    = factor(stance,    levels = c("Left", "Right")),
    condition = factor(condition, levels = condition_levels),
    model     = factor(model,     levels = all_model_levels)
  )

cat("Coefficient rows:", nrow(coef_df), "\n")

# =============================================================================
# 6. Plot
# =============================================================================

p <- coef_df %>%
  filter(!is.na(model)) %>%
  filter(!(model == "Gemini 3.1 Pro" & condition == "Post-election (without Web Search)")) %>%
  mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(x = estimate, y = stance, color = condition)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high),
                  position = position_dodge(width = 0.6),
                  size = 0.3, linewidth = 0.4) +
  facet_wrap(~model, nrow = 2) +
  scale_color_manual(values = condition_colors, name = "Condition", drop = TRUE,
                     breaks = condition_levels) +
  labs(
    x     = "OLS Coefficient vs. Control (pp)",
    y     = NULL,
    title = "Effect of Policy Stances on JCP Recommendation — All Models"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text      = element_text(size = 8.5, face = "bold"),
    legend.position = "bottom",
    panel.spacing   = unit(0.5, "lines"),
    plot.title      = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.text     = element_text(size = 9)
  ) +
  guides(color = guide_legend(nrow = 1))

# =============================================================================
# 7. Save
# =============================================================================

out_pdf <- file.path(out_fig_dir, "fig_jcp_allmodels.pdf")
out_jpg <- file.path(out_fig_dir, "fig_jcp_allmodels.jpg")

ggsave(out_pdf, p, width = 8, height = 5)
ggsave(out_jpg, p, width = 8, height = 5, dpi = 300)
cat("Saved:", out_pdf, "\n")
cat("Saved:", out_jpg, "\n")
