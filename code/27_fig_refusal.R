# =============================================================================
# 28_fig8_refusal.R
# Figure 8 (main text): Refusal rate by model x policy stance.
#
# Refusal = model explicitly declines to recommend a party (a valid outcome
# with partisan implications, not missing data).  Only GPT-5 Mini and
# Gemini 2.5 Flash refuse; GPT-4o Mini and both Grok variants never refuse.
#
# Supplementary outputs split into:
#   58_stab_refusal_demo.R  — OLS demographic + district table
#   45_sfig_refusal_coef.R  — policy coefficient plot
#
# Output: draft/figures/fig_refusal.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# Exclude NA/Error observations — keep Valid Party + Refusal
cm_valid <- cm_panel %>% filter(response_type != "NA/Error")

# =============================================================================
# Part A: Refusal rate by model x policy stance (figure)
# =============================================================================

refusal_stance <- cm_valid %>%
  group_by(model, policy_stance) %>%
  summarise(
    N            = n(),
    n_refusal    = sum(Refusal, na.rm = TRUE),
    refusal_rate = 100 * n_refusal / N,
    .groups      = "drop"
  ) %>%
  mutate(
    model         = factor(model, levels = model_levels),
    policy_stance = factor(policy_stance, levels = c("Left", "Control", "Right"))
  )

stance_colors <- c("Left" = "#f19db5", "Control" = "#5eb954", "Right" = "#7cc7e8")

p_stance <- ggplot(refusal_stance, aes(x = model, y = refusal_rate, fill = policy_stance)) +
  geom_col(position = position_dodge(width = 0.7), alpha = 0.85, width = 0.6) +
  scale_fill_manual(values = stance_colors) +
  labs(x = NULL, y = "Refusal rate (%)", fill = "Policy stance") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 15, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 8)
  )

ggsave(file.path(out_fig_dir, "fig_refusal.pdf"), p_stance,
       width = 7, height = 4)
cat("Saved: figures/fig_refusal.pdf\n")
