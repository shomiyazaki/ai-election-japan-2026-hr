# =============================================================================
# 26_fig6_grok.R
# Figure 6: Effect of X search — Grok (Web+X) minus Grok (Web-only) party
# recommendation share differences.
#
# Output: draft/figures/fig_grok.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# --- Compute shares for Grok variants ---
grok_shares <- cm_panel %>%
  filter(!is.na(party), str_detect(model, "Grok")) %>%
  group_by(model, party) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model) %>%
  mutate(share = n / sum(n) * 100) %>%
  ungroup() %>%
  select(model, party, share)

# --- Compute difference: (Web+X) - (Web-only) ---
grok_diff <- grok_shares %>%
  pivot_wider(names_from = model, values_from = share, values_fill = 0) %>%
  mutate(
    diff = `Grok 4.1 Fast (Web+X)` - `Grok 4.1 Fast (Web-only)`,
    party_label = party_display[as.character(party)]
  ) %>%
  filter(party != "Refusal") %>%
  arrange(diff) %>%
  mutate(party_label = fct_inorder(party_label))

# --- Plot ---
p <- ggplot(grok_diff, aes(x = diff, y = party_label)) +
  geom_col(aes(fill = diff > 0), width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#75BCE0", "FALSE" = "#741D7F")) +
  geom_vline(xintercept = 0, color = "gray40", linewidth = 0.4) +
  labs(
    x = "Difference in share (pp): Grok (Web+X) minus Grok (Web-only)",
    y = NULL
  ) +
  theme_bw(base_size = 9) +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(out_fig_dir, "fig_grok.pdf"), p,
       width = 7, height = 4.5)
cat("Saved: figures/fig_grok.pdf\n")
