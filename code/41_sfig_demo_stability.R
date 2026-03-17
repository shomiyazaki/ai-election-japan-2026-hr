# =============================================================================
# 41_sfig_demo_stability.R
# Supplementary: Demographic coefficient stability across sampling dates.
#
# For each date x model x party, estimates:
#   Y ~ gender + area_type + region | policy_treatment
# Plots coefficients for Female and Rural over days-until-election to confirm
# temporal stability.
#
# Output: draft/figures/sfig_demo_stability.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))
cm_reg   <- cm_panel %>% filter(!is.na(party))

parties <- c("LDP", "JCP")

# =============================================================================
# Extract demographic coefficients from a fitted model
# =============================================================================

extract_demo <- function(mod, model_name, party_name, date_val) {
  coeftable(mod) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(term %in% c("genderFemale", "area_typeRural")) %>%
    transmute(
      model      = model_name,
      party      = party_name,
      days_until = date_val,
      coef_label = case_when(
        term == "genderFemale"   ~ "Female",
        term == "area_typeRural" ~ "Rural"
      ),
      estimate = Estimate,
      se       = `Std. Error`,
      ci_low   = estimate - 1.96 * se,
      ci_high  = estimate + 1.96 * se
    )
}

# =============================================================================
# Regressions: Y ~ gender + area_type + region | policy_treatment
# =============================================================================

# Derive valid model x date combinations from the data
model_date_combos <- cm_reg %>% distinct(model, days_until)

demo_coefs_list <- list()
for (i in seq_len(nrow(model_date_combos))) {
  m     <- model_date_combos$model[i]
  d     <- model_date_combos$days_until[i]
  mdata <- cm_reg %>% filter(model == m, days_until == d)

  for (p in parties) {
    mod <- feols(
      as.formula(paste0(make.names(p), " ~ gender + area_type + region | policy_treatment")),
      data = mdata, vcov = "hetero"
    )
    demo_coefs_list <- c(demo_coefs_list, list(extract_demo(mod, m, p, d)))
  }
}

demo_coefs <- bind_rows(demo_coefs_list) %>%
  mutate(
    model      = factor(model, levels = model_levels),
    party      = factor(party, levels = parties),
    coef_label = factor(coef_label, levels = c("Female", "Rural"))
  )

dates <- sort(unique(demo_coefs$days_until))

# =============================================================================
# Plot: facet_grid(coef_label ~ party)
# =============================================================================

p_demo <- ggplot(demo_coefs, aes(x = days_until, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  size = 0.3, linewidth = 0.4,
                  position = position_dodge(width = 0.3)) +
  facet_grid(coef_label ~ party, scales = "free_y") +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = dates) +
  labs(
    x     = "Days until election (T)",
    y     = "OLS coefficient (vs. baseline)",
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

ggsave(file.path(out_fig_dir, "sfig_demo_stability.pdf"), p_demo,
       width = 8, height = 5.5)
cat("Saved: figures/sfig_demo_stability.pdf\n")
