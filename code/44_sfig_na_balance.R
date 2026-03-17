# =============================================================================
# 44_sfig_na_balance.R
# Supplementary: Balance test — NA/Error rates by demographics and policy stance.
#
# Plots NA/Error rate (with 95% CI) by model for each level of:
#   Gender, Area Type, Policy Stance.
#
# Output: draft/figures/sfig_na_balance.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds")) %>%
  mutate(is_na_error = as.integer(response_type == "NA/Error"))

# --- Helper: compute NA/Error rate with 95% CI per model x attribute level ---
balance_by <- function(data, attr_col, attr_label) {
  data %>%
    group_by(model, level = .data[[attr_col]]) %>%
    summarise(
      N        = n(),
      NA_Error = sum(is_na_error),
      NA_Pct   = 100 * NA_Error / N,
      se       = 100 * sqrt((NA_Error / N) * (1 - NA_Error / N) / N),
      .groups  = "drop"
    ) %>%
    mutate(
      attribute = attr_label,
      ci_low    = NA_Pct - 1.96 * se,
      ci_high   = NA_Pct + 1.96 * se
    )
}

bal_all <- bind_rows(
  balance_by(cm_panel, "gender",       "Gender"),
  balance_by(cm_panel, "area_type",    "Area Type"),
  balance_by(cm_panel, "policy_stance","Policy Stance")
) %>%
  mutate(
    model     = factor(model, levels = model_levels),
    attribute = factor(attribute, levels = c("Gender", "Area Type", "Policy Stance"))
  )

p_balance <- ggplot(bal_all, aes(x = level, y = NA_Pct, color = model)) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  size = 0.4, linewidth = 0.4,
                  position = position_dodge(width = 0.5)) +
  facet_wrap(~ attribute, scales = "free_x", nrow = 1) +
  scale_color_manual(values = model_colors) +
  labs(
    x     = NULL,
    y     = "NA/Error rate (%)",
    color = "Model"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text  = element_text(face = "bold", size = 9),
    axis.text   = element_text(size = 7),
    legend.text = element_text(size = 8)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(out_fig_dir, "sfig_na_balance.pdf"), p_balance,
       width = 8, height = 5)
cat("Saved: figures/sfig_na_balance.pdf\n")
