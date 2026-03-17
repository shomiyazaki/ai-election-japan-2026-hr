# =============================================================================
# 42_sfig_policy_stability.R
# Supplementary: Top-6 policy coefficient stability across sampling dates.
#
# Step 1: Identify top 3 Left and top 3 Right policy treatments by mean
#         |coefficient| across all models and parties (pooled OLS).
# Step 2: For each date x model x party, estimate per-date OLS and plot
#         those 6 coefficients over days-until-election.
#
# Output: draft/figures/sfig_policy_stability.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))
cm_reg   <- cm_panel %>% filter(!is.na(party))

parties <- c("LDP", "JCP")

# =============================================================================
# Step 1: Pooled policy OLS to identify top 6 treatments by mean |coefficient|
# Loop over model x party (5 x 2 = 10 regressions)
# =============================================================================

pooled_list <- list()

for (m in model_levels) {
  mdata <- cm_reg %>% filter(model == m)

  for (p in parties) {
    mod <- feols(
      as.formula(paste0(make.names(p), " ~ policy_treatment | gender + area_type + wave + region")),
      data = mdata, vcov = "hetero"
    )
    pooled_list <- c(pooled_list, list(
      coeftable(mod) %>%
        as.data.frame() %>%
        rownames_to_column("term") %>%
        filter(str_detect(term, "^policy_treatment")) %>%
        transmute(
          model    = m,
          party    = p,
          label    = str_remove(term, "^policy_treatment"),
          estimate = Estimate
        )
    ))
  }
}

pooled_policy <- bind_rows(pooled_list)

# Pick top 3 Left and top 3 Right by mean |coefficient|
top3_left <- pooled_policy %>%
  filter(str_detect(label, ": Left")) %>%
  group_by(label) %>%
  summarise(mean_abs = mean(abs(estimate), na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_abs, n = 3) %>%
  pull(label)

top3_right <- pooled_policy %>%
  filter(str_detect(label, ": Right")) %>%
  group_by(label) %>%
  summarise(mean_abs = mean(abs(estimate), na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_abs, n = 3) %>%
  pull(label)

top6 <- c(top3_right, top3_left)

cat("Top 3 Right:", paste(top3_right, collapse = ", "), "\n")
cat("Top 3 Left:",  paste(top3_left,  collapse = ", "), "\n")

# =============================================================================
# Step 2: Date-by-date policy OLS for the top 6 treatments
# Loop over valid model x date x party combinations (GPT-4o Mini missing T=-6)
# =============================================================================

extract_policy_date <- function(mod, model_name, party_name, date_val, keep_labels) {
  coeftable(mod) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(label = str_remove(term, "^policy_treatment")) %>%
    filter(label %in% keep_labels) %>%
    transmute(
      model        = model_name,
      party        = party_name,
      days_until   = date_val,
      policy_label = label,
      estimate     = Estimate,
      se           = `Std. Error`,
      ci_low       = estimate - 1.96 * se,
      ci_high      = estimate + 1.96 * se
    )
}

# Derive valid model x date combinations from the data
model_date_combos <- cm_reg %>% distinct(model, days_until)

policy_coefs_list <- list()

for (i in seq_len(nrow(model_date_combos))) {
  m     <- model_date_combos$model[i]
  d     <- model_date_combos$days_until[i]
  mdata <- cm_reg %>% filter(model == m, days_until == d)

  for (p in parties) {
    mod <- feols(
      as.formula(paste0(make.names(p), " ~ policy_treatment | gender + area_type + region")),
      data = mdata, vcov = "hetero"
    )
    policy_coefs_list <- c(policy_coefs_list,
                            list(extract_policy_date(mod, m, p, d, top6)))
  }
}

policy_coefs <- bind_rows(policy_coefs_list) %>%
  mutate(
    model        = factor(model, levels = model_levels),
    party        = factor(party, levels = parties),
    policy_label = factor(policy_label, levels = top6)
  )

dates <- sort(unique(policy_coefs$days_until))

# =============================================================================
# Plot: facet_grid(policy_label ~ party, scales = "free_y")
# =============================================================================

p_policy <- ggplot(policy_coefs, aes(x = days_until, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  size = 0.25, linewidth = 0.35,
                  position = position_dodge(width = 0.3)) +
  facet_grid(policy_label ~ party, scales = "free_y") +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = dates) +
  labs(
    x     = "Days until election (T)",
    y     = "OLS coefficient (vs. Control)",
    color = "Model"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text   = element_text(face = "bold", size = 8),
    strip.text.y = element_text(size = 7),
    axis.text    = element_text(size = 7),
    legend.text  = element_text(size = 8)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(out_fig_dir, "sfig_policy_stability.pdf"), p_policy,
       width = 8, height = 11)
cat("Saved: figures/sfig_policy_stability.pdf\n")
