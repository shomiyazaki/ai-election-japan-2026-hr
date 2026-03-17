# =============================================================================
# 45_sfig_refusal_coef.R
# Supplementary: Policy treatment coefficient plot for Pr(Refusal).
#
# Only models with at least one refusal are included (excludes Grok, which has 0).
#
# Specification:
#   Refusal ~ policy_treatment | gender + area_type + wave + region
# FE absorbed: gender, area type, date, district.
# HC1 robust standard errors.
# Coefficients ordered by policy_label_order (from 00_constants.R).
#
# Output: draft/figures/sfig_refusal_coef.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# Exclude NA/Error observations — keep Valid Party + Refusal
cm_valid <- cm_panel %>% filter(response_type != "NA/Error")

# =============================================================================
# Regressions: Refusal ~ policy_treatment | gender + area_type + wave + region
# Only models with at least one refusal are included.
# =============================================================================

refusal_models <- map(set_names(model_levels), \(m) {
  mdata <- cm_valid %>% filter(model == m)
  if (sum(mdata$Refusal, na.rm = TRUE) == 0) return(NULL)
  feols(Refusal ~ policy_treatment | gender + area_type + wave + region,
        data = mdata, vcov = "hetero")
}) %>%
  compact()

cat("Models with > 0 refusals:", paste(names(refusal_models), collapse = ", "), "\n")

# =============================================================================
# Extract and order policy treatment coefficients
# =============================================================================

refusal_coefs <- imap_dfr(refusal_models, \(mod, model_name) {
  coeftable(mod) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(str_detect(term, "^policy_treatment")) %>%
    transmute(
      model    = model_name,
      label    = str_remove(term, "^policy_treatment"),
      estimate = Estimate,
      se       = `Std. Error`,
      ci_low   = estimate - 1.96 * se,
      ci_high  = estimate + 1.96 * se
    )
})

refusal_coefs <- refusal_coefs %>%
  mutate(
    label = factor(label, levels = policy_label_order),
    model = factor(model, levels = model_levels)
  )

p_policy <- ggplot(refusal_coefs, aes(x = estimate, y = label, color = model)) +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high),
                  position = position_dodge(width = 0.6),
                  size = 0.15, linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = model_colors) +
  labs(
    x     = "Coefficient (change in Pr(Refusal) vs. Control)",
    y     = NULL,
    color = "Model"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 8)
  )

ggsave(file.path(out_fig_dir, "sfig_refusal_coef.pdf"), p_policy,
       width = 7, height = 9)
cat("Saved: figures/sfig_refusal_coef.pdf\n")
