# =============================================================================
# 54_stab_policy_ldp.R
# Supplementary: Full policy treatment coefficient table for LDP.
#
# Specification (Eq. 2 in paper):
#   LDP ~ policy_treatment | gender + area_type + wave + region
# FE absorbed: gender, area type, date, district.
# HC1 robust standard errors.
#
# Output: draft/figures/stab_policy_ldp.tex   (Table S4)
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)
library(modelsummary)

options("modelsummary_format_numeric_latex" = "plain")

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))
cm_reg   <- cm_panel %>% filter(!is.na(party))

# =============================================================================
# Regressions: LDP ~ policy_treatment | gender + area_type + wave + region
# Grok names abbreviated to fit table columns
# =============================================================================

table_labels <- c("GPT-4o Mini", "GPT-5 Mini", "Gemini 2.5 Flash",
                  "Grok (Web-only)", "Grok (Web+X)")

models_ldp <- map(set_names(model_levels, table_labels), \(m) {
  feols(LDP ~ policy_treatment | gender + area_type + wave + region,
        data = filter(cm_reg, model == m), vcov = "hetero")
})

# =============================================================================
# Table output
# =============================================================================

policy_coef_map <- setNames(
  policy_treatment_levels[-1],
  paste0("policy_treatment", policy_treatment_levels[-1])
)

gm <- tribble(
  ~raw,                      ~clean,            ~fmt,
  "nobs",                    "Observations",    0,
  "FE: gender",              "Gender FE",       0,
  "FE: area_type",           "Area Type FE",    0,
  "FE: wave",                "Date FEs",        0,
  "FE: region",              "District FEs",    0
)

tmp <- tempfile(fileext = ".tex")
modelsummary(models_ldp, output = tmp, stars = FALSE,
             coef_map = policy_coef_map, gof_map = gm, fmt = 3, escape = FALSE)

# Post-process LaTeX output
tex_lines <- readLines(tmp)
fe_rows   <- str_detect(tex_lines, "Gender FE|Area Type FE|Date FEs|District FEs")
tex_lines[fe_rows] <- str_replace_all(tex_lines[fe_rows], " X([ \\\\])", " Yes\\1")
tex_lines <- tex_lines[!str_detect(tex_lines, "^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\centering|^\\\\caption")]

colspec_i         <- str_which(tex_lines, "colspec=")[1]
tex_lines[colspec_i] <- str_replace(tex_lines[colspec_i], "colspec=", "rowsep=1pt,colspec=")

ncols     <- length(models_ldp) + 1
note_line <- paste0(
  "\\SetCell[c=", ncols, "]{l} {\\scriptsize HC1 robust standard errors in parentheses. ",
  "Reference: Control (no policy stance).} ",
  paste(rep("&", ncols - 1), collapse = " "), " \\\\"
)
end_i     <- str_which(tex_lines, "^\\\\end\\{tblr\\}")[1]
tex_lines <- c(tex_lines[seq_len(end_i - 1)], note_line, tex_lines[end_i:length(tex_lines)])

writeLines(c(
  "\\begin{table}[p]",
  "\\caption{Policy treatment coefficients for Pr(LDP) by model.}",
  "\\label{stab:policy-ldp}",
  "\\centering",
  "\\footnotesize",
  "\\resizebox{\\textwidth}{!}{",
  tex_lines,
  "}",
  "\\end{table}"
), file.path(out_fig_dir, "stab_policy_ldp.tex"))

cat("Saved: figures/stab_policy_ldp.tex\n")
