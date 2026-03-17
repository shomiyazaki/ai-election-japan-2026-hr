# =============================================================================
# 56_stab_na_model.R
# Supplementary: NA/Error rate table by model.
#
# Response type classification (from 01_clean_cross_model.R):
#   Valid Party = first text segment matches a known party name pattern.
#   Refusal     = Japanese-language response without a party recommendation.
#   NA/Error    = response is NA, empty, CONNECTION_ERROR, or contains no
#                 Japanese text (hiragana/katakana/kanji); i.e., is_technical_error().
#
# Output: draft/figures/stab_na_model.tex
# =============================================================================

source(here::here("code", "00_constants.R"))
library(kableExtra)

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# Binary indicator for NA/Error
cm_panel <- cm_panel %>%
  mutate(is_na_error = as.integer(response_type == "NA/Error"))

# --- NA/Error rate by model ---
na_model <- cm_panel %>%
  group_by(model) %>%
  summarise(
    N           = n(),
    Valid_Party  = sum(response_type == "Valid Party"),
    Refusal     = sum(response_type == "Refusal"),
    NA_Error    = sum(is_na_error),
    NA_Pct      = 100 * NA_Error / N,
    .groups     = "drop"
  ) %>%
  mutate(model = factor(model, levels = model_levels)) %>%
  arrange(model)

# Add total row
na_total <- tibble(
  model       = "Total",
  N           = sum(na_model$N),
  Valid_Party = sum(na_model$Valid_Party),
  Refusal     = sum(na_model$Refusal),
  NA_Error    = sum(na_model$NA_Error),
  NA_Pct      = 100 * sum(na_model$NA_Error) / sum(na_model$N)
)
na_model_out <- bind_rows(na_model, na_total) %>%
  mutate(NA_Pct = sprintf("%.1f", NA_Pct))

tab_a <- kbl(
  na_model_out,
  format    = "latex",
  booktabs  = TRUE,
  col.names = c("Model", "N", "Valid Party", "Refusal", "NA/Error", "NA/Error (\\%)"),
  escape    = FALSE,
  align     = c("l", rep("r", 5)),
  linesep   = ""
) %>%
  kable_styling(latex_options = "hold_position", font_size = 9)

# Extract just the tabular environment (strip table wrapper)
tab_a_lines <- strsplit(as.character(tab_a), "\n")[[1]]
# Remove \begin{table}, \end{table}, \centering lines
tab_a_lines <- tab_a_lines[!grepl("^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\centering|^\\\\begingroup|^\\\\endgroup", tab_a_lines)]

out_lines_a <- c(
  "\\begin{table}[p]",
  "\\caption{Response type counts by model. Valid Party = first text segment matches a known party name pattern; Refusal = Japanese-language response without a party recommendation; NA/Error = response is NA, empty, or contains no Japanese text.}",
  "\\label{stab:na-model}",
  "\\centering",
  "\\footnotesize",
  tab_a_lines,
  "\\end{table}"
)

writeLines(out_lines_a, file.path(out_fig_dir, "stab_na_model.tex"))
cat("Saved: figures/stab_na_model.tex\n")
