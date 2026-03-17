# =============================================================================
# 57_stab_na_ols.R
# Supplementary: OLS regression of NA/Error on demographic and policy covariates.
#
# Specification:
#   is_na_error ~ gender + area_type + policy_stance | wave + region
# FE absorbed: date (wave), district.
# HC1 robust standard errors.
#
# Output: draft/figures/stab_na_ols.tex
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)
library(modelsummary)

options("modelsummary_format_numeric_latex" = "plain")

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds")) %>%
  mutate(is_na_error = as.integer(response_type == "NA/Error"))

# =============================================================================
# Regressions: is_na_error ~ gender + area_type + policy_stance | wave + region
# =============================================================================

# Grok names abbreviated to fit table columns
table_labels <- c("GPT-4o Mini", "GPT-5 Mini", "Gemini 2.5 Flash",
                  "Grok (Web-only)", "Grok (Web+X)")

models_na <- map(set_names(model_levels, table_labels), \(m) {
  feols(is_na_error ~ gender + area_type + policy_stance | wave + region,
        data = filter(cm_panel, model == m), vcov = "hetero")
})

# =============================================================================
# Table output
# =============================================================================

coef_map_na <- c(
  "genderFemale"       = "Gender: Female",
  "area_typeRural"     = "Area: Rural",
  "policy_stanceLeft"  = "Policy stance: Left",
  "policy_stanceRight" = "Policy stance: Right"
)

gm_na <- tribble(
  ~raw,                      ~clean,            ~fmt,
  "nobs",                    "Observations",    0,
  "FE: wave",                "Date FEs",        0,
  "FE: region",              "District FEs",    0
)

tmp <- tempfile(fileext = ".tex")
modelsummary(models_na, output = tmp, stars = FALSE,
             coef_map = coef_map_na, gof_map = gm_na, fmt = 4, escape = FALSE)

# Post-process LaTeX output
tex_lines <- readLines(tmp)
fe_rows   <- str_detect(tex_lines, "Date FEs|District FEs")
tex_lines[fe_rows] <- str_replace_all(tex_lines[fe_rows], " X([ \\\\])", " Yes\\1")
tex_lines <- tex_lines[!str_detect(tex_lines, "^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\centering|^\\\\caption")]

colspec_i         <- str_which(tex_lines, "colspec=")[1]
tex_lines[colspec_i] <- str_replace(tex_lines[colspec_i], "colspec=", "rowsep=1pt,colspec=")

note_line <- "\\SetCell[c=6]{l} {\\scriptsize HC1 robust SEs in parentheses. Baseline: Male, Urban, Control. Dep.\\ var.\\ = 1 if NA/Error.} & & & & & \\\\"
end_i     <- str_which(tex_lines, "^\\\\end\\{tblr\\}")[1]
tex_lines <- c(tex_lines[seq_len(end_i - 1)], note_line, tex_lines[end_i:length(tex_lines)])

writeLines(c(
  "\\begin{table}[p]",
  "\\caption{OLS estimates for Pr(NA/Error) by model.}",
  "\\label{stab:na-ols}",
  "\\centering",
  "\\footnotesize",
  "\\resizebox{\\textwidth}{!}{",
  tex_lines,
  "}",
  "\\end{table}"
), file.path(out_fig_dir, "stab_na_ols.tex"))

cat("Saved: figures/stab_na_ols.tex\n")
