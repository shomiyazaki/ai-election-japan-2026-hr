# =============================================================================
# 58_stab_refusal_demo.R
# Supplementary: OLS demographic + district table for Pr(Refusal).
#
# All models with at least one refusal are included; models with zero refusals
# (currently both Grok variants) are excluded automatically.
#
# Specification:
#   Refusal ~ gender + area_type + region | wave + policy_treatment
# FE absorbed: date (wave), policy treatment.
# HC1 robust standard errors.
#
# Output: draft/figures/stab_refusal_demo.tex
# =============================================================================

source(here::here("code", "00_constants.R"))
library(fixest)
library(modelsummary)

options("modelsummary_format_numeric_latex" = "plain")

cm_panel <- readRDS(file.path(out_data_dir, "cm_panel.rds"))

# Exclude NA/Error observations — keep Valid Party + Refusal
cm_valid <- cm_panel %>% filter(response_type != "NA/Error")

# =============================================================================
# Regressions: Refusal ~ gender + area_type + region | wave + policy_treatment
# All models with at least one refusal are included.
# =============================================================================

models_ref <- map(set_names(model_levels), \(m) {
  mdata <- filter(cm_valid, model == m)
  if (sum(mdata$Refusal, na.rm = TRUE) == 0) return(NULL)
  feols(Refusal ~ gender + area_type + region | wave + policy_treatment,
        data = mdata, vcov = "hetero")
}) %>%
  compact()

cat("Models included:", paste(names(models_ref), collapse = ", "), "\n")

# =============================================================================
# Table output
# =============================================================================

coef_map_ref <- c("genderFemale" = "Gender: Female", "area_typeRural" = "Area: Rural")
for (r in region_levels[-1]) coef_map_ref[paste0("region", r)] <- paste0("District: ", r)

gm_ref <- tribble(
  ~raw,                      ~clean,            ~fmt,
  "nobs",                    "Observations",    0,
  "FE: wave",                "Date FEs",        0,
  "FE: policy_treatment",    "Policy FEs",      0
)

tmp <- tempfile(fileext = ".tex")
modelsummary(models_ref, output = tmp, stars = FALSE,
             coef_map = coef_map_ref, gof_map = gm_ref, fmt = 3, escape = FALSE)

# Post-process LaTeX output
tex_lines <- readLines(tmp)
fe_rows   <- str_detect(tex_lines, "Date FEs|Policy FEs")
tex_lines[fe_rows] <- str_replace_all(tex_lines[fe_rows], " X([ \\\\])", " Yes\\1")
tex_lines <- tex_lines[!str_detect(tex_lines, "^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\centering|^\\\\caption")]

colspec_i         <- str_which(tex_lines, "colspec=")[1]
tex_lines[colspec_i] <- str_replace(tex_lines[colspec_i], "colspec=", "rowsep=1pt,colspec=")

ncols     <- length(models_ref) + 1
note_line <- paste0(
  "\\SetCell[c=", ncols, "]{l} {\\scriptsize HC1 robust SEs in parentheses. ",
  "Baseline: Male, Urban, Tokyo. Dep.\\ var.\\ = 1 if Refusal.} ",
  paste(rep("&", ncols - 1), collapse = " "), " \\\\"
)
end_i     <- str_which(tex_lines, "^\\\\end\\{tblr\\}")[1]
tex_lines <- c(tex_lines[seq_len(end_i - 1)], note_line, tex_lines[end_i:length(tex_lines)])

writeLines(c(
  "\\begin{table}[p]",
  "\\caption{OLS estimates for Pr(Refusal) by model.}",
  "\\label{stab:refusal-demo}",
  "\\centering",
  "\\footnotesize",
  "\\resizebox{\\textwidth}{!}{",
  tex_lines,
  "}",
  "\\end{table}"
), file.path(out_fig_dir, "stab_refusal_demo.tex"))

cat("Saved: figures/stab_refusal_demo.tex\n")
