# =============================================================================
# 52_stab_ols_ldp.R
# Supplementary: Demographic and district OLS table for LDP.
#
# Specification (Eq. 1 in paper):
#   LDP ~ gender + area_type + region | wave + policy_treatment
# FE absorbed: date (wave) + policy treatment.
# Show: Gender: Female, Area: Rural, District (10 dummies, ref = Tokyo).
# HC1 robust standard errors.
#
# Output: draft/figures/stab_ols_ldp.tex   (Table S2)
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)
library(modelsummary)

options("modelsummary_format_numeric_latex" = "plain")

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))
cm_reg   <- cm_panel %>% filter(!is.na(party))

# =============================================================================
# Regressions: LDP ~ gender + area_type + region | wave + policy_treatment
# =============================================================================

models_ldp <- map(set_names(model_levels), \(m) {
  feols(LDP ~ gender + area_type + region | wave + policy_treatment,
        data = filter(cm_reg, model == m), vcov = "hetero")
})

# =============================================================================
# Table output
# =============================================================================

coef_map <- c("genderFemale" = "Gender: Female", "area_typeRural" = "Area: Rural")
for (r in region_levels[-1]) coef_map[paste0("region", r)] <- paste0("District: ", r)

gm <- tribble(
  ~raw,                      ~clean,            ~fmt,
  "nobs",                    "Observations",    0,
  "FE: wave",                "Date FEs",        0,
  "FE: policy_treatment",    "Policy FEs",      0
)

tmp <- tempfile(fileext = ".tex")
modelsummary(models_ldp, output = tmp, stars = FALSE,
             coef_map = coef_map, gof_map = gm, fmt = 3, escape = FALSE)

# Post-process LaTeX output
tex_lines <- readLines(tmp)
fe_rows   <- str_detect(tex_lines, "Date FEs|Policy FEs")
tex_lines[fe_rows] <- str_replace_all(tex_lines[fe_rows], " X([ \\\\])", " Yes\\1")
tex_lines <- tex_lines[!str_detect(tex_lines, "^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\centering|^\\\\caption")]

colspec_i         <- str_which(tex_lines, "colspec=")[1]
tex_lines[colspec_i] <- str_replace(tex_lines[colspec_i], "colspec=", "rowsep=1pt,colspec=")

note_line <- "\\SetCell[c=6]{l} {\\scriptsize HC1 robust standard errors in parentheses. Baseline: Male, Urban, Tokyo.} & & & & & \\\\"
end_i     <- str_which(tex_lines, "^\\\\end\\{tblr\\}")[1]
tex_lines <- c(tex_lines[seq_len(end_i - 1)], note_line, tex_lines[end_i:length(tex_lines)])

writeLines(c(
  "\\begin{table}[p]",
  "\\caption{OLS estimates for Pr(LDP) by model.}",
  "\\label{stab:ols-ldp}",
  "\\centering",
  "\\footnotesize",
  "\\resizebox{\\textwidth}{!}{",
  tex_lines,
  "}",
  "\\end{table}"
), file.path(out_fig_dir, "stab_ols_ldp.tex"))

cat("Saved: figures/stab_ols_ldp.tex\n")
