# =============================================================================
# PNAD Contínua — Regression Models and Inference
# Pipeline 3 of 3
#
# Description:
#   Fits OLS and logistic regression models with svyglm(), correctly
#   incorporating PNAD Contínua's complex sample design. All categorical
#   variables use labels from get_pnadc(labels = TRUE).
#
# Requirements:
#   install.packages(c("PNADcIBGE", "tidyverse", "survey", "broom"))
#
# Output:
#   Charts and model tables saved to output/
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Setup
# -----------------------------------------------------------------------------

library(PNADcIBGE)
library(tidyverse)
library(survey)
library(broom)

options(survey.lonely.psu = "adjust")
dir.create("output", showWarnings = FALSE)

theme_pnadc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey40", size = 10),
      plot.caption     = element_text(color = "grey55", size = 8),
      axis.title       = element_text(size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

PALETTE <- c(
  "main"   = "#2E86AB",
  "accent" = "#E24B4A",
  "mid"    = "#F18F01",
  "soft"   = "#B0C4DE",
  "green"  = "#4CAF50"
)

cat("Setup complete.\n")


# -----------------------------------------------------------------------------
# 2. Data download
# -----------------------------------------------------------------------------

vars_needed <- c(
  "UF", "UPA", "Estrato", "V1028",
  "V2007", "V2009", "V2010",
  "VD3005", "VD4001", "VD4002", "VD4009", "VD4020"
)

cat("Downloading PNAD Contínua microdata from IBGE...\n")

pnadc_design <- get_pnadc(
  year     = 2023,
  quarter  = 4,
  vars     = vars_needed,
  labels   = TRUE,
  deflator = FALSE,
  design   = TRUE
)

cat(sprintf("Download complete: %d respondents.\n", nrow(pnadc_design$variables)))


# -----------------------------------------------------------------------------
# 3. Inspect labels
# -----------------------------------------------------------------------------

cat("\n=== Factor levels — confirm before recoding ===\n")
cat("V2007:\n");  print(levels(pnadc_design$variables$V2007))
cat("V2010:\n");  print(levels(pnadc_design$variables$V2010))
cat("VD3005:\n"); print(levels(pnadc_design$variables$VD3005))
cat("VD4001 (labor force condition):\n"); print(levels(pnadc_design$variables$VD4001))
cat("VD4002 (employment condition):\n");  print(levels(pnadc_design$variables$VD4002))
cat("VD4009:\n"); print(levels(pnadc_design$variables$VD4009))


# -----------------------------------------------------------------------------
# 4. Variable recoding inside the design object
#
# Reference categories for regression:
#   education    -> "Secondary" (relevel below)
#   V2007        -> "Homem" (first level by default from IBGE labels)
#   V2010        -> "Branca" (relevel below)
#   macro_region -> "Southeast" (relevel below)
# -----------------------------------------------------------------------------

pnadc_design <- update(pnadc_design,
                       
                       education = factor(case_when(
                         VD3005 == "Sem instrução e menos de 1 ano de estudo" ~ "No schooling",
                         VD3005 %in% c(
                           "1 ano de estudo","2 anos de estudo","3 anos de estudo","4 anos de estudo"
                         ) ~ "Primary incomplete",
                         VD3005 %in% c(
                           "5 anos de estudo","6 anos de estudo","7 anos de estudo","8 anos de estudo"
                         ) ~ "Primary complete",
                         VD3005 %in% c(
                           "9 anos de estudo","10 anos de estudo","11 anos de estudo"
                         ) ~ "Secondary",
                         VD3005 %in% c(
                           "12 anos de estudo","13 anos de estudo","14 anos de estudo",
                           "15 anos de estudo","16 anos ou mais de estudo"
                         ) ~ "Higher education",
                         TRUE ~ NA_character_
                       ), levels = c("Secondary","No schooling","Primary incomplete",
                                     "Primary complete","Higher education")),
                       # "Secondary" is the first level = reference category in regression
                       
                       race = factor(case_when(
                         V2010 == "Branca"   ~ "White",
                         V2010 == "Preta"    ~ "Black",
                         V2010 == "Parda"    ~ "Brown (Pardo)",
                         V2010 == "Amarela"  ~ "Asian",
                         V2010 == "Indígena" ~ "Indigenous",
                         TRUE                ~ NA_character_
                       ), levels = c("White","Black","Brown (Pardo)","Asian","Indigenous")),
                       # "White" is the first level = reference category
                       
                       macro_region = factor(case_when(
                         UF %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "North",
                         UF %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
                                   "Pernambuco","Alagoas","Sergipe","Bahia") ~ "Northeast",
                         UF %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Southeast",
                         UF %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "South",
                         UF %in% c("Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal") ~ "Center-West",
                         TRUE ~ NA_character_
                       ), levels = c("Southeast","North","Northeast","South","Center-West")),
                       # "Southeast" is the first level = reference category
                       
                       age    = as.numeric(V2009),
                       age_sq = as.numeric(V2009)^2,
                       
                       log_income = ifelse(!is.na(VD4020) & VD4020 > 0, log(VD4020), NA_real_),
                       
                       # PEA: pessoas na força de trabalho (VD4001)
                       in_pea = !is.na(VD4001) & VD4001 == "Pessoas na força de trabalho",
                       
                       # Ocupação: usando VD4002 com labels confirmados via levels()
                       # "Pessoas ocupadas" = 1, "Pessoas desocupadas" = 0
                       employed = case_when(
                         VD4002 == "Pessoas ocupadas"    ~ 1L,
                         VD4002 == "Pessoas desocupadas" ~ 0L,
                         TRUE ~ NA_integer_
                       )
)

# Subsets
# OLS: apenas ocupados com renda declarada
employed_design <- subset(pnadc_design, employed == 1L & !is.na(log_income))

# Logístico: PEA completa (ocupados + desocupados)
# Exclui inativos — pessoas fora da força de trabalho
pea_design <- subset(pnadc_design, in_pea == TRUE & !is.na(employed))

cat("Recoding complete.\n")
cat(sprintf("Employed with income: %d | PEA: %d\n",
            nrow(employed_design$variables),
            nrow(pea_design$variables)))


# -----------------------------------------------------------------------------
# 5. Model 1 — OLS: determinants of log income
#
# log(income) ~ education + V2007 + race + age + age² + macro_region
#
# svyglm() with gaussian() applies OLS with correct SE for the survey design.
# Coefficients on log scale: exp(coef) - 1 gives approximate % change in income.
# -----------------------------------------------------------------------------

cat("\n=== Model 1: OLS — determinants of log income ===\n")

model_ols <- svyglm(
  log_income ~ education + V2007 + race + age + age_sq + macro_region,
  design = employed_design,
  family = gaussian()
)

tidy_ols <- tidy(model_ols, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    pct_effect = round((exp(estimate) - 1) * 100, 2),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE             ~ ""
    )
  )

cat("\nCoefficients (log income scale):\n")
tidy_ols |>
  select(term, estimate, std.error, statistic, p.value, pct_effect, sig) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print(n = 30)

write_csv(tidy_ols, "output/model1_ols_coefficients.csv")
cat("Table saved: output/model1_ols_coefficients.csv\n")


# -----------------------------------------------------------------------------
# 6. OLS coefficient plot
# -----------------------------------------------------------------------------

p1 <- tidy_ols |>
  mutate(
    direction = ifelse(estimate > 0, "positive", "negative"),
    term_clean = term |>
      str_remove("^education|^V2007|^race|^macro_region") |>
      str_replace("age_sq", "Age²") |>
      str_replace("^age$", "Age")
  ) |>
  ggplot(aes(x = estimate, y = reorder(term_clean, estimate),
             color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.3, orientation = "y") +
  scale_color_manual(
    values = c("positive" = unname(PALETTE["main"]),
               "negative" = unname(PALETTE["accent"])),
    guide  = "none"
  ) +
  labs(
    title    = "OLS regression — determinants of log income",
    subtitle = "Coefficients with 95% CI | svyglm() with PNAD Contínua design",
    x        = "Coefficient (log income scale)",
    y        = NULL,
    caption  = paste(
      "Reference categories: Secondary (education), Homem (gender),",
      "White (race), Southeast (region).",
      "\nSource: IBGE, PNAD Contínua 4Q2023."
    )
  ) +
  theme_pnadc()

print(p1)
ggsave("output/09_ols_coefplot.png", p1, width = 9, height = 7, dpi = 150)
cat("Chart saved: 09_ols_coefplot.png\n")


# -----------------------------------------------------------------------------
# 7. Model 2 — Logistic: probability of employment
#
# P(employed = 1) ~ education + V2007 + race + age + age² + macro_region
#
# svyglm() with quasibinomial() is recommended for binary outcomes in surveys.
# tidy(exponentiate = TRUE) returns odds ratios directly.
# -----------------------------------------------------------------------------

cat("\n=== Model 2: Logistic — probability of employment ===\n")

model_logit <- svyglm(
  employed ~ education + V2007 + race + age + age_sq + macro_region,
  design = pea_design,
  family = quasibinomial()
)

tidy_logit <- tidy(model_logit, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE             ~ ""
    )
  )

cat("\nOdds Ratios:\n")
tidy_logit |>
  rename(odds_ratio = estimate, or_ci_low = conf.low, or_ci_high = conf.high) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print(n = 30)

write_csv(tidy_logit, "output/model2_logit_oddsratios.csv")
cat("Table saved: output/model2_logit_oddsratios.csv\n")


# -----------------------------------------------------------------------------
# 8. Odds ratio plot
# -----------------------------------------------------------------------------

p2 <- tidy_logit |>
  mutate(
    direction  = ifelse(estimate > 1, "higher odds", "lower odds"),
    term_clean = term |>
      str_remove("^education|^V2007|^race|^macro_region") |>
      str_replace("age_sq", "Age²") |>
      str_replace("^age$", "Age")
  ) |>
  ggplot(aes(x = estimate, y = reorder(term_clean, estimate),
             color = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.3, orientation = "y") +
  scale_x_log10() +
  scale_color_manual(
    values = c("higher odds" = unname(PALETTE["green"]),
               "lower odds"  = unname(PALETTE["accent"])),
    guide  = "none"
  ) +
  labs(
    title    = "Logistic regression — odds ratios for employment",
    subtitle = "OR with 95% CI (log scale) | svyglm() with PNAD Contínua design",
    x        = "Odds ratio (log scale) — OR > 1 = higher probability of employment",
    y        = NULL,
    caption  = paste(
      "Reference categories: Secondary (education), Homem (gender),",
      "White (race), Southeast (region).",
      "\nSource: IBGE, PNAD Contínua 4Q2023."
    )
  ) +
  theme_pnadc()

print(p2)
ggsave("output/10_logit_orplot.png", p2, width = 9, height = 7, dpi = 150)
cat("Chart saved: 10_logit_orplot.png\n")


# -----------------------------------------------------------------------------
# 9. Odds ratios — education coefficients from logistic model
#
# The predicted probability plot is not informative in this context because
# the unemployment rate in Brazil 4Q2023 was ~7.5%, leaving little variation
# to display across groups. Instead, we focus on the education odds ratios
# directly, which show the relative employment odds vs. the reference category.
# -----------------------------------------------------------------------------

cat("\n=== Odds ratios — education coefficients ===\n")

edu_or <- tidy_logit |>
  filter(str_detect(term, "^education")) |>
  mutate(
    term_clean = str_remove(term, "^education"),
    term_clean = factor(term_clean,
                        levels = c("No schooling","Primary incomplete",
                                   "Primary complete","Higher education")),
    direction = ifelse(estimate > 1, "higher odds", "lower odds")
  )

print(edu_or |> select(term_clean, estimate, conf.low, conf.high, p.value, sig))

p3 <- edu_or |>
  ggplot(aes(x = estimate, y = reorder(term_clean, estimate),
             color = direction)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.25, orientation = "y", linewidth = 0.8) +
  geom_text(aes(label = sprintf("OR = %.2f", estimate)),
            hjust = -0.2, size = 3.2, color = "grey20") +
  scale_x_log10(limits = c(0.5, 2.5)) +
  scale_color_manual(
    values = c("higher odds" = unname(PALETTE["main"]),
               "lower odds"  = unname(PALETTE["accent"])),
    guide  = "none"
  ) +
  labs(
    title    = "Employment odds ratios by education level",
    subtitle = "Reference category: Secondary | PEA | PNAD Contínua 4Q2023",
    x        = "Odds ratio (log scale) — OR > 1 = higher odds of employment vs. Secondary",
    y        = NULL,
    caption  = "Source: IBGE, PNAD Contínua. svyglm() with quasibinomial() and complex sample design."
  ) +
  theme_pnadc() +
  theme(panel.grid.major.y = element_blank())

print(p3)
ggsave("output/11_education_or_employment.png", p3, width = 8, height = 4, dpi = 150)
cat("Chart saved: 11_education_or_employment.png\n")


# -----------------------------------------------------------------------------
# 10. Summary
# -----------------------------------------------------------------------------

cat("\n=== Pipeline complete ===\n")
cat("Charts and tables saved to output/\n")
cat("\nPipeline series complete:\n")
cat("  01_descriptive.R    -- distributions, cross-tabs, ggplot2 charts\n")
cat("  02_survey_weights.R -- weighted estimates, DEFF, correct SE\n")
cat("  03_regression.R     -- OLS and logistic models with svyglm()\n")