# =============================================================================
# 43_sfig_na_by_date.R
# Supplementary: NA/Error rates by sampling date and model (line plot).
#
# GPT-4o Mini T=-1 wave (2026-02-07) excluded: 100% NA rate inflates display.
#
# Output: draft/figures/sfig_na_by_date.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# Binary indicator for NA/Error
cm_panel <- cm_panel %>%
  mutate(is_na_error = as.integer(response_type == "NA/Error"))

# --- NA/Error rates by date x model ---
na_date <- cm_panel %>%
  group_by(model, days_until) %>%
  summarise(
    N        = n(),
    NA_Error = sum(is_na_error),
    NA_Pct   = 100 * NA_Error / N,
    .groups  = "drop"
  ) %>%
  mutate(model = factor(model, levels = model_levels))

dates <- sort(unique(na_date$days_until))

p_date <- ggplot(na_date, aes(x = days_until, y = NA_Pct, color = model)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = dates) +
  labs(
    x     = "Days until election (T)",
    y     = "NA/Error rate (%)",
    color = "Model"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    axis.text  = element_text(size = 7),
    legend.text = element_text(size = 8)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(out_fig_dir, "sfig_na_by_date.pdf"), p_date,
       width = 6, height = 4)
cat("Saved: figures/sfig_na_by_date.pdf\n")
