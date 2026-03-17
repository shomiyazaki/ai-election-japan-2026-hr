# =============================================================================
# 46_sfig_party_heatmap.R
# Supplementary: Party policy positions heatmap (Yomiuri data).
#
# Source: Yomiuri Shimbun party questionnaire (Q1,Q3,Q4,Q7,Q8,Q11,Q14-16,Q19).
#         Waseda IDI (Permanent Residency, Espionage Law — no Yomiuri item).
# Rows = 12 policy issues; columns = 11 parties ordered left -> right by mean
# score across the 10 available Yomiuri items.
# Scale: -2 (strongly left) to +2 (strongly right); 0 = neither.
# Permanent Residency and Espionage Law cells are dark grey (no Yomiuri item).
#
# Input:  data-modified/yomiuri_party_stances.csv
# Output: draft/figures/sfig_party_heatmap.pdf
# =============================================================================

source(here::here("code", "00_constants.R"))

# ── 1. Read CSV ───────────────────────────────────────────────────────────────
raw <- read_csv(
  file.path(out_data_dir, "yomiuri_party_stances.csv"),
  show_col_types = FALSE
)

# ── 2. Subset to 10 Yomiuri questions used in the experiment ──────────────────
yomiuri_qs <- c("Q1", "Q3", "Q4", "Q7", "Q8", "Q11", "Q14", "Q15", "Q16", "Q19")
df <- raw |> filter(question_number %in% yomiuri_qs)

# ── 3. Score responses: -2 (strongly left) to +2 (strongly right) ─────────────
#
# Direction conventions — what "right-leaning" (+) means per question:
#   A_is_left  : A option = left (-2), B option = right (+2)
#   A_is_right : A option = right (+2), B option = left (-2)
#   agree_is_right: 賛成 = +2
#   agree_is_left : 賛成 = -2
#   special    : non-standard options handled case-by-case
#
direction_map <- c(
  Q1  = "A_is_left",       # A = maintain benefits (left);  B = reduce (right)
  Q3  = "A_is_left",       # A = increase acceptance (left); B = decrease (right)
  Q4  = "special",         # 4-option consumption tax scale
  Q7  = "A_is_right",      # A = continue nuclear (right);   B = phase out (left)
  Q8  = "special",         # 3-option defense spending scale
  Q11 = "agree_is_left",   # agree = strengthen China relations (left)
  Q14 = "agree_is_right",  # agree = support constitutional amendment (right)
  Q15 = "special",         # 3-option dual-surnames scale
  Q16 = "A_is_right",      # A = maintain donations (right); B = ban (left)
  Q19 = "agree_is_right"   # agree = support seat reduction (right)
)

# Generic scorer for standard 5-point A/B and agree/disagree scales
score_standard <- function(response, direction) {
  ab <- c(
    "Aに近い"              =  2,
    "どちらかといえばAに近い" =  1,
    "どちらともいえない"     =  0,
    "どちらかといえばBに近い" = -1,
    "Bに近い"              = -2
  )
  ag <- c(
    "賛成"               =  2,
    "どちらかといえば賛成"   =  1,
    "どちらともいえない"    =  0,
    "どちらかといえば反対"   = -1,
    "反対"               = -2
  )
  s <- if (str_starts(direction, "A")) ab[response] else ag[response]
  if (str_ends(direction, "_left")) s <- -s
  unname(s)
}

# Special scorer for questions with non-standard response options
score_special <- function(q, response) {
  case_when(
    # Q4: Consumption Tax (right = raise/maintain current level)
    q == "Q4" & response == "増税するべきだ"              ~  2L,
    q == "Q4" & response == "現状を維持するべきだ"          ~  1L,
    q == "Q4" & response == "限定的に減税するべきだ"        ~ -1L,
    q == "Q4" & response == "恒久的に減税・廃止するべきだ"   ~ -2L,
    # Q8: Defense Spending (right = increase above 2% GDP)
    q == "Q8" & response == "GDPの2%より増やすべきだ"       ~  2L,
    q == "Q8" & response == "GDPの2%程度とすべきだ"         ~  0L,
    q == "Q8" & response == "GDPの2%より減らすべきだ"       ~ -2L,
    # Q15: Dual Surnames (right = maintain same-surname rule)
    q == "Q15" & str_detect(response, "今の制度を維持する$") ~  2L,
    q == "Q15" & str_detect(response, "機会を拡大する")      ~  1L,
    q == "Q15" & str_detect(response, "選択的夫婦別姓")      ~ -2L,
    .default = NA_integer_
  )
}

df <- df |>
  mutate(
    dir = direction_map[question_number],
    score = case_when(
      is.na(response) | response == "" ~ NA_real_,
      dir == "special" ~ as.double(score_special(question_number, response)),
      TRUE ~ map2_dbl(response, dir, score_standard)
    )
  )

# ── 4. Add blank rows for Espionage Law and Permanent Residency ───────────────
parties_jp <- unique(df$party)
na_rows <- crossing(
  party           = parties_jp,
  question_number = c("Espionage", "PermRes")
) |> mutate(question = NA_character_, response = NA_character_,
            dir = NA_character_,     score    = NA_real_)

df <- bind_rows(df, na_rows)

# ── 5. Party name mapping (Japanese -> English display) ───────────────────────
party_en_map <- c(
  "自由民主党"           = "LDP",
  "中道改革連合"         = "Centrist\nReform",
  "日本維新の会"         = "Innovation",
  "国民民主党"           = "DPP",
  "参政党"              = "Sanseito",
  "日本共産党"           = "JCP",
  "れいわ新選組"         = "Reiwa",
  "日本保守党"           = "Conservative",
  "社会民主党"           = "SDP",
  "チームみらい"         = "Team Mirai",
  "減税日本・ゆうこく連合" = "Tax Cut\nCoalition"
)
df <- df |> mutate(party_en = party_en_map[party])

# ── 6. Party ordering: left to right by mean score ────────────────────────────
party_order <- df |>
  group_by(party_en) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |>
  arrange(mean_score) |>
  pull(party_en)

# ── 7. Issue ordering and labels ──────────────────────────────────────────────
issue_order <- c(
  "Q14", "Q8",      "Espionage", "Q11",   # Security & Diplomacy
  "Q3",  "PermRes", "Q15",                # Social
  "Q7",  "Q4",      "Q16",       "Q19",   # Economic & Governance
  "Q1"
)
issue_labels <- c(
  Q14       = "Constitutional Amendment",
  Q8        = "Defense Spending",
  Espionage = "Espionage Law",
  Q11       = "China Relations",
  Q3        = "Foreign Workers",
  PermRes   = "Permanent Residency",
  Q15       = "Dual Surnames",
  Q7        = "Nuclear Power",
  Q4        = "Consumption Tax",
  Q16       = "Corporate Donations",
  Q19       = "Diet Seat Reduction",
  Q1        = "Social Insurance"
)

score_levels  <- c(-2, -1, 0, 1, 2)
score_labels  <- c("Strong\nLeft", "Lean\nLeft", "Neither", "Lean\nRight", "Strong\nRight")
score_colors  <- c(
  "Strong\nLeft"  = "#1C58AB",   # dark blue
  "Lean\nLeft"    = "#99FAFE",   # light blue
  "Neither"       = "#F5F5F5",   # near white
  "Lean\nRight"   = "#F5B4CE",   # pink
  "Strong\nRight" = "#F1089A"    # red
)

df <- df |>
  mutate(
    score_f  = factor(score, levels = score_levels, labels = score_labels),
    party_en = factor(party_en, levels = party_order),
    issue    = factor(question_number,
                      levels = rev(issue_order),
                      labels = issue_labels[rev(issue_order)])
  )

# ── 8. Plot constants ─────────────────────────────────────────────────────────
jcp_x <- which(levels(df$party_en) == "JCP")
ldp_x <- which(levels(df$party_en) == "LDP")

# ── 9. Plot ───────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(x = party_en, y = issue, fill = score_f)) +
  geom_tile(color = "white", linewidth = 0.6) +
  # Highlight JCP column
  annotate("rect",
           xmin = jcp_x - 0.5, xmax = jcp_x + 0.5,
           ymin = 0.5,          ymax = 12.5,
           color = "black", fill = NA, linewidth = 1.1) +
  # Highlight LDP column
  annotate("rect",
           xmin = ldp_x - 0.5, xmax = ldp_x + 0.5,
           ymin = 0.5,          ymax = 12.5,
           color = "black", fill = NA, linewidth = 1.1) +
  scale_fill_manual(
    values   = score_colors,
    labels   = score_labels,
    na.value = "#AAAAAA",
    name     = "Policy direction",
    guide    = guide_legend(
      title.position = "top",
      nrow           = 1,
      keywidth       = unit(1.2, "cm"),
      keyheight      = unit(0.4, "cm")
    )
  ) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x    = element_text(size = 7.5, angle = 30, hjust = 1),
    axis.text.y    = element_text(size = 8),
    panel.grid     = element_blank(),
    legend.position = "bottom",
    legend.title   = element_text(size = 8),
    legend.text    = element_text(size = 7),
    plot.margin    = margin(5, 10, 5, 5)
  )

# ── 10. Save ──────────────────────────────────────────────────────────────────
ggsave(file.path(out_fig_dir, "sfig_party_heatmap.pdf"), p,
       width = 7.5, height = 5.5)
cat("Saved: figures/sfig_party_heatmap.pdf\n")
