# =============================================================================
# 47_sfig_policy_coef_comparison.R
# Supplementary Figure: Policy treatment effects (Left/Right vs Control)
# across conditions and model generations.
#
# Regression: feols(LDP/JCP ~ policy_stance_lr | gender + area_type)
# No region/wave FE — ensures comparability across all datasets.
#
# Output: draft/figures/sfig_policy_coef_cross.pdf
#         draft/figures/sfig_policy_coef_flagship.pdf
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

condition_levels <- c("Pre (Web)", "Post (Web)", "Post (No-Web)", "Web", "No-Web")
condition_colors <- c(
  "Pre (Web)"     = "#F39A59", "Post (Web)"    = "#7181B5",
  "Post (No-Web)" = "#AAD8B4", "Web"           = "#89CCD5", "No-Web" = "#F9CD41"
)

# =============================================================================
# 2. Data source registry — one row per file
# =============================================================================

pe_dir <- file.path(proj_root, "data-out", "post_election")
fm_dir <- file.path(proj_root, "data-out", "flagship_models")

sources <- tribble(
  ~dir,    ~pattern,                                        ~model,                      ~condition,
  pe_dir,  "japan2026_election_gpt4omini_wave_",            "GPT-4o Mini",               "Post (Web)",
  pe_dir,  "japan2026_election_gpt4omini_noweb_wave_",      "GPT-4o Mini",               "Post (No-Web)",
  pe_dir,  "japan2026_election_gpt5mini_wave_",             "GPT-5 Mini",                "Post (Web)",
  pe_dir,  "japan2026_election_gpt5mini_noweb_wave_",       "GPT-5 Mini",                "Post (No-Web)",
  pe_dir,  "japan2026_election_gemini-25flash_wave_",       "Gemini 2.5 Flash",          "Post (Web)",
  pe_dir,  "japan2026_election_gemini-25flash_noweb_wave_", "Gemini 2.5 Flash",          "Post (No-Web)",
  pe_dir,  "japan2026_election_grok_webonly_wave_",         "Grok 4.1 Fast (Web-only)",  "Post (Web)",
  pe_dir,  "japan2026_election_grok_noweb_wave_",           "Grok 4.1 Fast (Web-only)",  "Post (No-Web)",

  fm_dir,  "gpt54_web",                                     "GPT-5.4",                   "Web",
  fm_dir,  "gpt54_noweb",                                   "GPT-5.4",                   "No-Web",
  fm_dir,  "gemini31pro_web",                               "Gemini 3.1 Pro",            "Web",
  fm_dir,  "gemini31pro_noweb",                             "Gemini 3.1 Pro",            "No-Web",
  fm_dir,  "grok420_web",                                   "Grok 4.20",                 "Web",
  fm_dir,  "grok420_noweb",                                 "Grok 4.20",                 "No-Web",
  fm_dir,  "claude_opus46_web",                             "Claude Opus 4.6",           "Web",
  fm_dir,  "claude_opus46_noweb",                           "Claude Opus 4.6",           "No-Web"
)

# =============================================================================
# 3. Loader: find first matching file, parse, standardise
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
        !is.na(party_matched)                                          ~ party_matched,
        !is_technical_error(response)                                  ~ "Refusal",
        TRUE                                                           ~ NA_character_
      ),
      LDP              = as.integer(party == "LDP"),
      JCP              = as.integer(party == "JCP"),
      Refusal          = as.integer(party == "Refusal"),
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
    select(model, condition, gender, area_type, policy_stance_lr, LDP, JCP, Refusal)
}

# =============================================================================
# 4. Load all data
# =============================================================================

# February: already cleaned — policy_stance is already Control/Left/Right
feb_data <- read_csv(file.path(out_data_dir, "cross_model_clean.csv"),
                     show_col_types = FALSE) %>%
  filter(response_type %in% c("Valid Party", "Refusal")) %>%
  transmute(model, condition = "Pre (Web)", gender, area_type,
            policy_stance_lr = policy_stance,
            LDP     = as.integer(party == "LDP"),
            JCP     = as.integer(party == "JCP"),
            Refusal = as.integer(party == "Refusal"))

# Post-election + flagship: loaded via registry
raw_data <- pmap(sources, load_source) %>% bind_rows()

all_data <- bind_rows(feb_data, raw_data) %>%
  mutate(
    policy_stance_lr = factor(policy_stance_lr, levels = c("Control", "Left", "Right")),
    condition        = factor(condition,         levels = condition_levels)
  )

# =============================================================================
# 5. Regressions
# =============================================================================

extract_coefs <- function(df_sub, keys) {
  map(c("LDP", "JCP", "Refusal"), function(outcome) {
    tryCatch({
      mod <- feols(as.formula(paste0(outcome, " ~ policy_stance_lr | gender + area_type")),
                   data = df_sub, vcov = "HC1")
      as.data.frame(coeftable(mod)) %>%
        rownames_to_column("term") %>%
        filter(str_detect(term, "^policy_stance_lr")) %>%
        transmute(
          model     = keys$model,
          condition = keys$condition,
          outcome,
          stance    = str_remove(term, "^policy_stance_lr"),
          estimate  = Estimate,
          se        = `Std. Error`,
          ci_low    = estimate - 1.96 * se,
          ci_high   = estimate + 1.96 * se
        )
    }, error = function(e) NULL)
  }) %>% bind_rows()
}

coef_df <- all_data %>%
  group_by(model, condition) %>%
  group_map(extract_coefs) %>%
  bind_rows() %>%
  mutate(
    outcome   = factor(outcome,   levels = c("LDP", "JCP", "Refusal")),
    stance    = factor(stance,    levels = c("Left", "Right")),
    condition = factor(condition, levels = condition_levels)
  )

cat("Coefficient rows:", nrow(coef_df), "\n")

# =============================================================================
# 6. Plot and save
# =============================================================================

make_coef_plot <- function(data, model_order) {
  data %>%
    filter(model %in% model_order) %>%
    mutate(model     = factor(model, levels = model_order),
           condition = fct_rev(condition)) %>%
    ggplot(aes(x = estimate, y = stance, color = condition)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_pointrange(aes(xmin = ci_low, xmax = ci_high),
                    position = position_dodge(width = 0.6),
                    size = 0.25, linewidth = 0.35) +
    facet_grid(outcome ~ model) +
    scale_color_manual(values = condition_colors, name = "Condition", drop = TRUE,
                       breaks = condition_levels) +
    labs(x = "OLS Coefficient vs. Control (pp)", y = NULL) +
    theme_bw(base_size = 9) +
    theme(strip.text.x = element_text(size = 7.5), strip.text.y = element_text(size = 8, face = "bold"),
          legend.position = "bottom", panel.spacing = unit(0.4, "lines"))
}

p_cross    <- make_coef_plot(coef_df, cross_model_levels)
p_flagship <- make_coef_plot(
  coef_df %>% filter(!(model == "Gemini 3.1 Pro" & condition == "No-Web")),
  flagship_model_levels
)

ggsave(file.path(out_fig_dir, "sfig_policy_coef_cross.pdf"),      p_cross,      width = 8, height = 8)
ggsave(file.path(out_fig_dir, "sfig_policy_coef_flagship.pdf"),   p_flagship,   width = 8, height = 8)
cat("Saved: sfig_policy_coef_cross.pdf\n")
cat("Saved: sfig_policy_coef_flagship.pdf\n")
