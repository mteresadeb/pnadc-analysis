# =============================================================================
# PNAD Contínua — Survey Weights and Complex Sample Design
# Pipeline 2 of 3
#
# Description:
#   Builds the survey design object and produces weighted estimates with
#   correct standard errors. All categorical variables use the labels applied
#   by get_pnadc(labels = TRUE) — no manual numeric recoding.
#
# Requirements:
#   install.packages(c("PNADcIBGE", "tidyverse", "survey", "srvyr"))
#
# Output:
#   Charts saved to output/
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Setup
# -----------------------------------------------------------------------------

library(PNADcIBGE)
library(tidyverse)
library(survey)
library(srvyr)

options(survey.lonely.psu = "adjust")
dir.create("output", showWarnings = FALSE)

theme_pnadc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey40", size = 10),
      plot.caption    = element_text(color = "grey55", size = 8),
      axis.title      = element_text(size = 10),
      legend.position = "bottom"
    )
}

PALETTE <- c(
  "main"   = "#2E86AB",
  "accent" = "#E24B4A",
  "mid"    = "#F18F01",
  "soft"   = "#B0C4DE"
)

cat("Setup complete.\n")


# -----------------------------------------------------------------------------
# 2. Data download with survey design
#
# design = TRUE returns a svydesign object with weights, strata and PSU
# already set. labels = TRUE (default) applies the official IBGE dictionary.
# -----------------------------------------------------------------------------

vars_needed <- c(
  "UF", "UPA", "Estrato", "V1028",
  "V2007", "V2009", "V2010",
  "VD3005", "VD4009", "VD4020"
)

cat("Downloading PNAD Contínua microdata from IBGE...\n")

pnadc_design <- get_pnadc(
  year     = 2023,
  quarter  = 4,
  vars     = vars_needed,
  labels   = TRUE,   # apply official IBGE dictionary
  deflator = FALSE,
  design   = TRUE    # returns svydesign object
)

cat(sprintf("Design object created: %d respondents.\n",
            nrow(pnadc_design$variables)))


# -----------------------------------------------------------------------------
# 3. Inspect labels before recoding
#
# Run this block first whenever working with a new edition of the PNAD.
# The exact strings returned here are what must be used in case_when below.
# -----------------------------------------------------------------------------

cat("\n=== Factor levels — confirm before recoding ===\n")
cat("V2007 (Gender):\n");      print(levels(pnadc_design$variables$V2007))
cat("V2010 (Race):\n");        print(levels(pnadc_design$variables$V2010))
cat("VD3005 (Schooling):\n");  print(levels(pnadc_design$variables$VD3005))
cat("VD4009 (Employment):\n"); print(levels(pnadc_design$variables$VD4009))


# -----------------------------------------------------------------------------
# 4. Variable recoding inside the design object
#
# Always use update() to recode variables inside a svydesign object.
# Never extract data, recode, and rebuild — this breaks the design structure.
# Labels come from IBGE dictionary via labels = TRUE.
# -----------------------------------------------------------------------------

pnadc_design <- update(pnadc_design,
                       
                       # Education grouped from VD3005 labels
                       # VD3005 goes from "Sem instrução..." up to "16 anos ou mais de estudo"
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
                       ), levels = c("No schooling","Primary incomplete","Primary complete",
                                     "Secondary","Higher education")),
                       
                       # Employment: Employed vs. Unemployed vs. Inactive from VD4009 labels
                       emp_status = case_when(
                         VD4009 %in% c(
                           "Empregado no setor privado com carteira de trabalho assinada",
                           "Empregado no setor privado sem carteira de trabalho assinada",
                           "Empregado no setor público (inclusive empresas de economia mista) com carteira de trabalho assinada",
                           "Empregado no setor público (inclusive empresas de economia mista) sem carteira de trabalho assinada",
                           "Trabalhador doméstico com carteira de trabalho assinada",
                           "Trabalhador doméstico sem carteira de trabalho assinada",
                           "Empregador",
                           "Conta própria",
                           "Trabalhador familiar auxiliar"
                         ) ~ "Employed",
                         TRUE ~ "Not employed"
                       ),
                       
                       # Macro-region from UF label
                       macro_region = case_when(
                         UF %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "North",
                         UF %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
                                   "Pernambuco","Alagoas","Sergipe","Bahia") ~ "Northeast",
                         UF %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Southeast",
                         UF %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "South",
                         UF %in% c("Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal") ~ "Center-West",
                         TRUE ~ NA_character_
                       ),
                       
                       has_income = !is.na(VD4020) & VD4020 > 0
)

# Subset to employed population with income
employed_design <- subset(pnadc_design, emp_status == "Employed" & has_income)

cat("\nRecoding complete.\n")


# -----------------------------------------------------------------------------
# 5. Weighted vs. unweighted comparison
# -----------------------------------------------------------------------------

cat("\n=== Weighted vs. Unweighted Mean Income (R$) ===\n")

naive_mean   <- mean(employed_design$variables$VD4020, na.rm = TRUE)
weighted_est <- svymean(~VD4020, employed_design, na.rm = TRUE)
ci           <- confint(weighted_est)

fmt_brl <- function(x) format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)

cat(sprintf("  Unweighted mean: R$ %s\n", fmt_brl(naive_mean)))
cat(sprintf("  Weighted mean:   R$ %s\n", fmt_brl(coef(weighted_est))))
cat(sprintf("  SE (weighted):   R$ %s\n", fmt_brl(SE(weighted_est))))
cat(sprintf("  95%% CI: R$ %s to R$ %s\n", fmt_brl(ci[1]), fmt_brl(ci[2])))


# -----------------------------------------------------------------------------
# 6. Weighted estimates by subgroup
#
# svyby() column naming varies by package version.
# We use set_names() to rename positionally instead of by name.
# -----------------------------------------------------------------------------

cat("\n=== Weighted mean income by gender (R$) ===\n")
svyby(~VD4020, ~V2007, employed_design, svymean, na.rm = TRUE) |>
  as_tibble() |>
  set_names(c("V2007", "mean_income", "se")) |>
  mutate(
    ci_lower = mean_income - 1.96 * se,
    ci_upper = mean_income + 1.96 * se,
    cv_pct   = round(se / mean_income * 100, 1)
  ) |>
  print()

cat("\n=== Weighted mean income by education (R$) ===\n")
svyby(~VD4020, ~education, employed_design, svymean, na.rm = TRUE) |>
  as_tibble() |>
  set_names(c("education", "mean_income", "se")) |>
  mutate(cv_pct = round(se / mean_income * 100, 1)) |>
  arrange(desc(mean_income)) |>
  print()


# -----------------------------------------------------------------------------
# 7. Design effect (DEFF)
# -----------------------------------------------------------------------------

cat("\n=== Design Effects (DEFF) by macro-region ===\n")

deff_df <- svyby(~VD4020, ~macro_region, employed_design,
                 svymean, na.rm = TRUE, deff = TRUE) |>
  as_tibble() |>
  set_names(c("macro_region", "mean_income", "se", "deff")) |>
  mutate(cv_pct = round(se / mean_income * 100, 1)) |>
  select(macro_region, mean_income, se, cv_pct, deff)

print(deff_df)


# -----------------------------------------------------------------------------
# 8. Weighted cross-tabulation with srvyr
# -----------------------------------------------------------------------------

cat("\n=== Employment status by gender — weighted (%) ===\n")

as_survey_rep(pnadc_design) |>
  filter(!is.na(emp_status), !is.na(V2007)) |>
  group_by(V2007, emp_status) |>
  summarise(pct = survey_mean() * 100, .groups = "drop") |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  print()


# -----------------------------------------------------------------------------
# 9. Visualizations
# -----------------------------------------------------------------------------

## 9.1 Weighted mean income by education with CI error bars
income_edu <- svyby(~VD4020, ~education, employed_design,
                    svymean, na.rm = TRUE) |>
  as_tibble() |>
  set_names(c("education", "mean_income", "se")) |>
  mutate(
    ci_lower = mean_income - 1.96 * se,
    ci_upper = mean_income + 1.96 * se
  ) |>
  filter(!is.na(education))

p1 <- ggplot(income_edu,
             aes(x = factor(education,
                            levels = c("No schooling","Primary incomplete",
                                       "Primary complete","Secondary",
                                       "Higher education")),
                 y = mean_income)) +
  geom_col(fill = PALETTE["main"], width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = PALETTE["accent"], linewidth = 0.8) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Weighted mean income by education level",
    subtitle = "Error bars = 95% confidence interval | PNAD Contínua 4Q2023",
    x = NULL, y = "Weighted mean monthly income (R$)",
    caption  = "Source: IBGE, PNAD Contínua. Complex sample design incorporated."
  ) +
  theme_pnadc() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p1)
ggsave("output/06_weighted_income_education_ci.png", p1, width = 8, height = 5, dpi = 150)
cat("Chart saved: 06_weighted_income_education_ci.png\n")


## 9.2 Design effect by macro-region
p2 <- deff_df |>
  ggplot(aes(x = reorder(macro_region, deff), y = deff)) +
  geom_col(fill = PALETTE["mid"], width = 0.65, alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_text(aes(label = round(deff, 2)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title    = "Design effect (DEFF) by macro-region",
    subtitle = "DEFF > 1 = variance inflation vs. simple random sampling",
    x = NULL, y = "Design effect",
    caption  = "Source: IBGE, PNAD Contínua 4Q2023. Author's own analysis."
  ) +
  theme_pnadc()

print(p2)
ggsave("output/07_design_effect_region.png", p2, width = 8, height = 5, dpi = 150)
cat("Chart saved: 07_design_effect_region.png\n")


# -----------------------------------------------------------------------------
# 10. Summary
# -----------------------------------------------------------------------------

cat("\n=== Pipeline complete ===\n")
cat("Charts saved to output/\n")
cat("Key takeaway: always incorporate the survey design before reporting estimates.\n")
cat("Next: pnadc-regression — regression models with svyglm()\n")