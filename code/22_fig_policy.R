# =============================================================================
# 27_fig7_policy.R
# Figure 7: Policy treatment effects on LDP and JCP recommendation probability.
#
# Layout: 2 rows (LDP, JCP) x 2 columns (Left stances, Right stances).
# Each point = OLS coefficient vs. control condition; bars = 95% CI.
# FE absorbed: gender + area_type + wave + region (Eq. 2 in paper).
# Policy treatment coefficients only shown; HC1 robust SEs.
#
# Output: draft/figures/fig_policy.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)
library(cowplot)

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))
cm_reg   <- cm_panel %>% filter(!is.na(party))

# =============================================================================
# Regressions: LDP and JCP ~ policy_treatment | gender + area_type + wave + region
# =============================================================================

data_gpt4omini    <- cm_reg %>% filter(model == "GPT-4o Mini")
data_gpt5mini     <- cm_reg %>% filter(model == "GPT-5 Mini")
data_gemini       <- cm_reg %>% filter(model == "Gemini 2.5 Flash")
data_grok_webonly <- cm_reg %>% filter(model == "Grok 4.1 Fast (Web-only)")
data_grok_webx    <- cm_reg %>% filter(model == "Grok 4.1 Fast (Web+X)")

ldp_gpt4omini    <- feols(LDP ~ policy_treatment | gender + area_type + wave + region, data = data_gpt4omini,    vcov = "hetero")
jcp_gpt4omini    <- feols(JCP ~ policy_treatment | gender + area_type + wave + region, data = data_gpt4omini,    vcov = "hetero")

ldp_gpt5mini     <- feols(LDP ~ policy_treatment | gender + area_type + wave + region, data = data_gpt5mini,     vcov = "hetero")
jcp_gpt5mini     <- feols(JCP ~ policy_treatment | gender + area_type + wave + region, data = data_gpt5mini,     vcov = "hetero")

ldp_gemini       <- feols(LDP ~ policy_treatment | gender + area_type + wave + region, data = data_gemini,       vcov = "hetero")
jcp_gemini       <- feols(JCP ~ policy_treatment | gender + area_type + wave + region, data = data_gemini,       vcov = "hetero")

ldp_grok_webonly <- feols(LDP ~ policy_treatment | gender + area_type + wave + region, data = data_grok_webonly, vcov = "hetero")
jcp_grok_webonly <- feols(JCP ~ policy_treatment | gender + area_type + wave + region, data = data_grok_webonly, vcov = "hetero")

ldp_grok_webx    <- feols(LDP ~ policy_treatment | gender + area_type + wave + region, data = data_grok_webx,    vcov = "hetero")
jcp_grok_webx    <- feols(JCP ~ policy_treatment | gender + area_type + wave + region, data = data_grok_webx,    vcov = "hetero")

# =============================================================================
# Extract policy treatment coefficients
# =============================================================================

extract_policy <- function(mod, model_name, party_name) {
  ct <- as.data.frame(coeftable(mod))
  ct$term <- rownames(ct)
  ct %>%
    filter(str_detect(term, "^policy_treatment")) %>%
    transmute(
      model    = model_name,
      party    = party_name,
      label    = str_remove(term, "^policy_treatment"),
      estimate = Estimate,
      se       = `Std. Error`,
      ci_low   = estimate - 1.96 * se,
      ci_high  = estimate + 1.96 * se
    )
}

coefs_ldp_gpt4omini    <- extract_policy(ldp_gpt4omini,    "GPT-4o Mini",              "LDP")
coefs_jcp_gpt4omini    <- extract_policy(jcp_gpt4omini,    "GPT-4o Mini",              "JCP")
coefs_ldp_gpt5mini     <- extract_policy(ldp_gpt5mini,     "GPT-5 Mini",               "LDP")
coefs_jcp_gpt5mini     <- extract_policy(jcp_gpt5mini,     "GPT-5 Mini",               "JCP")
coefs_ldp_gemini       <- extract_policy(ldp_gemini,       "Gemini 2.5 Flash",         "LDP")
coefs_jcp_gemini       <- extract_policy(jcp_gemini,       "Gemini 2.5 Flash",         "JCP")
coefs_ldp_grok_webonly <- extract_policy(ldp_grok_webonly, "Grok 4.1 Fast (Web-only)", "LDP")
coefs_jcp_grok_webonly <- extract_policy(jcp_grok_webonly, "Grok 4.1 Fast (Web-only)", "JCP")
coefs_ldp_grok_webx    <- extract_policy(ldp_grok_webx,    "Grok 4.1 Fast (Web+X)",   "LDP")
coefs_jcp_grok_webx    <- extract_policy(jcp_grok_webx,    "Grok 4.1 Fast (Web+X)",   "JCP")

policy_coefs <- bind_rows(
  coefs_ldp_gpt4omini,    coefs_jcp_gpt4omini,
  coefs_ldp_gpt5mini,     coefs_jcp_gpt5mini,
  coefs_ldp_gemini,       coefs_jcp_gemini,
  coefs_ldp_grok_webonly, coefs_jcp_grok_webonly,
  coefs_ldp_grok_webx,    coefs_jcp_grok_webx
) %>%
  mutate(
    stance = if_else(str_detect(label, ": Right"), "Right", "Left"),
    issue  = str_remove(label, ": (Right|Left)"),
    model  = factor(model, levels = model_levels),
    party  = factor(party, levels = c("LDP", "JCP"))
  )

# --- Define issue ordering ---
issue_order <- c(
  "Constitutional Amendment", "Defense Spending", "Espionage Law",
  "Nuclear Power", "China Relations", "Permanent Residency",
  "Foreign Workers", "Diet Seat Reduction", "Social Insurance",
  "Corporate Donations", "Consumption Tax", "Dual Surnames"
)

policy_coefs <- policy_coefs %>%
  mutate(issue = factor(issue, levels = rev(issue_order)))

# --- Universal x-axis range across all 4 panels ---
x_range <- range(c(policy_coefs$ci_low, policy_coefs$ci_high), na.rm = TRUE)
x_pad   <- diff(x_range) * 0.05
x_lims  <- c(x_range[1] - x_pad, x_range[2] + x_pad)

# --- Build individual panel ---
# show_y: whether to show y-axis labels (FALSE for right column)
# show_x: whether to show x-axis title (FALSE for top row)
make_panel <- function(data, panel_title, show_y = TRUE, show_x = TRUE) {
  n_issues <- length(levels(data$issue))
  shade_df <- tibble(
    issue = factor(levels(data$issue), levels = levels(data$issue)),
    y_num = seq_len(n_issues)
  ) %>%
    filter(y_num %% 2 == 0)

  p <- ggplot(data, aes(x = estimate, y = issue, color = model)) +
    geom_rect(
      data = shade_df,
      aes(ymin = y_num - 0.5, ymax = y_num + 0.5, xmin = -Inf, xmax = Inf),
      fill = "gray92", inherit.aes = FALSE
    ) +
    geom_pointrange(
      aes(xmin = ci_low, xmax = ci_high),
      position = position_dodge(width = 0.6),
      size = 0.2, linewidth = 0.3
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = model_colors) +
    scale_x_continuous(limits = x_lims) +
    labs(
      title = panel_title,
      x     = if (show_x) "OLS Coefficient (vs. Control)" else NULL,
      y     = NULL,
      color = "Model"
    ) +
    theme_bw(base_size = 9) +
    theme(
      axis.text.y    = if (show_y) element_text(size = 7) else element_blank(),
      axis.ticks.y   = if (show_y) element_line() else element_blank(),
      plot.title     = element_text(size = 10, hjust = 0.5),
      legend.position = "none",
      plot.margin    = margin(5, 5, 5, if (show_y) 5 else 2)
    )

  p
}

# --- Build 4 panels ---
# Layout: rows = LDP (top), JCP (bottom); cols = Left (left), Right (right)
p_ldp_left <- make_panel(
  policy_coefs %>% filter(party == "LDP", stance == "Left"),
  "LDP - Left Policy Stances", show_y = TRUE, show_x = FALSE
)
p_ldp_right <- make_panel(
  policy_coefs %>% filter(party == "LDP", stance == "Right"),
  "LDP - Right Policy Stances", show_y = TRUE, show_x = FALSE
)
p_jcp_left <- make_panel(
  policy_coefs %>% filter(party == "JCP", stance == "Left"),
  "JCP - Left Policy Stances", show_y = TRUE, show_x = TRUE
)
p_jcp_right <- make_panel(
  policy_coefs %>% filter(party == "JCP", stance == "Right"),
  "JCP - Right Policy Stances", show_y = TRUE, show_x = TRUE
)

# --- Extract legend via ggplotGrob ---
p_for_legend <- ggplot(
  policy_coefs %>% filter(party == "LDP", stance == "Right"),
  aes(x = estimate, y = issue, color = model)
) +
  geom_point(size = 3) +
  scale_color_manual(values = model_colors, name = "Model") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9)) +
  guides(color = guide_legend(nrow = 1))

grob <- ggplotGrob(p_for_legend)
legend_idx <- which(sapply(grob$grobs, function(x) x$name) == "guide-box")
legend <- grob$grobs[[legend_idx]]

# --- Combine with cowplot ---
grid <- plot_grid(
  p_ldp_left, p_ldp_right,
  p_jcp_left, p_jcp_right,
  ncol = 2, nrow = 2,
  align = "hv",
  rel_widths = c(1, 1)
)

p_final <- plot_grid(grid, legend, ncol = 1, rel_heights = c(1, 0.07))

ggsave(file.path(out_fig_dir, "fig_policy.pdf"), p_final,
       width = 12, height = 12)
cat("Saved: figures/fig_policy.pdf\n")
