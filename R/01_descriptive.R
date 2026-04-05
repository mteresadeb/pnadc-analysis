# =============================================================================
# PNAD Contínua — Descriptive Analysis and Visualization
# Pipeline 1 of 3
#
# Description:
#   Downloads PNAD Contínua microdata directly from IBGE with labels = TRUE,
#   which applies the official data dictionary to all categorical variables.
#   Variables arrive as labeled factors — no manual numeric recoding needed.
#
# Requirements:
#   install.packages(c("PNADcIBGE", "tidyverse"))
#
# Note on labels:
#   get_pnadc(labels = TRUE) is the default. It applies the official IBGE
#   dictionary to categorical variables, returning them as labeled factors.
#   Always inspect levels() before filtering or grouping to confirm exact labels.
#
# Output:
#   Charts saved to output/
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Setup
# -----------------------------------------------------------------------------

library(PNADcIBGE)
library(tidyverse)

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
  "soft"   = "#B0C4DE",
  "green"  = "#4CAF50"
)

cat("Setup complete.\n")


# -----------------------------------------------------------------------------
# 2. Data download
#
# labels = TRUE (default): applies the official IBGE dictionary.
# Categorical variables arrive as labeled factors.
# deflator = FALSE: not needed for descriptive analysis.
# design = FALSE: raw data only; design object is built in pipeline 2.
# -----------------------------------------------------------------------------

vars_needed <- c(
  "UF",     # State
  "V1028",  # Survey weight
  "V2007",  # Gender
  "V2009",  # Age
  "V2010",  # Race/color
  "VD3005", # Years of schooling (grouped)
  "VD4009", # Employment status
  "VD4020"  # Monthly income from all jobs (R$)
)

cat("Downloading PNAD Contínua microdata from IBGE...\n")

pnadc <- get_pnadc(
  year      = 2023,
  quarter   = 4,
  vars      = vars_needed,
  labels    = TRUE,    # apply official IBGE dictionary to all categorical vars
  deflator  = FALSE,
  design    = FALSE
)

cat(sprintf("Download complete: %d rows, %d columns\n", nrow(pnadc), ncol(pnadc)))


# -----------------------------------------------------------------------------
# 3. Inspect labels before using them
#
# This is the critical step that replaces manual numeric recoding.
# Always run this block first on a new dataset to confirm the exact labels
# returned by the IBGE dictionary before writing any filter or group_by.
# -----------------------------------------------------------------------------

cat("\n=== Factor levels (labels applied by IBGE dictionary) ===\n")
cat("V2007 (Gender):\n");      print(levels(pnadc$V2007))
cat("V2010 (Race):\n");        print(levels(pnadc$V2010))
cat("VD3005 (Schooling):\n");  print(levels(pnadc$VD3005))
cat("VD4009 (Employment):\n"); print(levels(pnadc$VD4009))


# -----------------------------------------------------------------------------
# 4. Variable recoding using labels (not numeric codes)
#
# We build derived variables from the labeled factors.
# The exact strings used in case_when must match levels() output above.
# Groupings below follow IBGE's standard classifications.
# -----------------------------------------------------------------------------

pnadc <- pnadc |>
  mutate(
    
    # Education: grouped from VD3005 labels
    # VD3005 goes from "Sem instrução..." up to "16 anos ou mais de estudo"
    # Higher education = 12+ years (confirmed from levels() output)
    education = case_when(
      VD3005 == "Sem instrução e menos de 1 ano de estudo" ~ "No schooling",
      VD3005 %in% c(
        "1 ano de estudo", "2 anos de estudo", "3 anos de estudo", "4 anos de estudo"
      ) ~ "Primary incomplete",
      VD3005 %in% c(
        "5 anos de estudo", "6 anos de estudo", "7 anos de estudo", "8 anos de estudo"
      ) ~ "Primary complete",
      VD3005 %in% c(
        "9 anos de estudo", "10 anos de estudo", "11 anos de estudo"
      ) ~ "Secondary",
      VD3005 %in% c(
        "12 anos de estudo", "13 anos de estudo", "14 anos de estudo",
        "15 anos de estudo", "16 anos ou mais de estudo"
      ) ~ "Higher education",
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = c(
      "No schooling", "Primary incomplete", "Primary complete",
      "Secondary", "Higher education"
    )),
    
    # Employment: grouped from VD4009 labels
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
      TRUE ~ NA_character_  # pipelines 2 and 3 handle unemployed/inactive
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
    
    # Age group
    age_group = case_when(
      V2009 %in% 14:24 ~ "14-24",
      V2009 %in% 25:34 ~ "25-34",
      V2009 %in% 35:44 ~ "35-44",
      V2009 %in% 45:54 ~ "45-54",
      V2009 >= 55       ~ "55+",
      TRUE              ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("14-24","25-34","35-44","45-54","55+")),
    
    # Log income
    log_income = ifelse(!is.na(VD4020) & VD4020 > 0, log(VD4020), NA_real_)
    
  )

cat("\nRecoding complete.\n")


# -----------------------------------------------------------------------------
# 5. Frequency tables
# -----------------------------------------------------------------------------

cat("\n=== Gender distribution ===\n")
pnadc |>
  filter(!is.na(V2007)) |>
  count(V2007) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\n=== Education level distribution ===\n")
pnadc |>
  filter(!is.na(education)) |>
  count(education) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\n=== Race/color distribution ===\n")
pnadc |>
  filter(!is.na(V2010)) |>
  count(V2010) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n)) |>
  print()

cat("\n=== Income summary — employed only (R$) ===\n")
pnadc |>
  filter(emp_status == "Employed", !is.na(VD4020), VD4020 > 0) |>
  summarise(
    n      = n(),
    mean   = round(mean(VD4020), 2),
    median = round(median(VD4020), 2),
    sd     = round(sd(VD4020), 2),
    p25    = round(quantile(VD4020, 0.25), 2),
    p75    = round(quantile(VD4020, 0.75), 2)
  ) |>
  print()


# -----------------------------------------------------------------------------
# 6. Cross-tabulations
# -----------------------------------------------------------------------------

cat("\n=== Mean income by gender and education (R$) ===\n")
pnadc |>
  filter(emp_status == "Employed", !is.na(VD4020), VD4020 > 0,
         !is.na(V2007), !is.na(education)) |>
  group_by(education, V2007) |>
  summarise(
    n           = n(),
    mean_income = round(mean(VD4020), 0),
    .groups     = "drop"
  ) |>
  pivot_wider(names_from = V2007, values_from = c(n, mean_income)) |>
  print()


# -----------------------------------------------------------------------------
# 7. Visualizations
# -----------------------------------------------------------------------------

## 7.1 Income distribution by gender (density, log scale)
p1 <- pnadc |>
  filter(emp_status == "Employed", !is.na(log_income), !is.na(V2007)) |>
  mutate(V2007 = as.character(V2007)) |>
  ggplot(aes(x = log_income, fill = V2007)) +
  geom_density(alpha = 0.55, color = NA) +
  scale_fill_manual(values = c("Homem" = unname(PALETTE["main"]), "Mulher" = unname(PALETTE["accent"]))) +
  labs(
    title    = "Income distribution by gender",
    subtitle = "Employed population — log scale | PNAD Contínua 4Q2023",
    x        = "Log of monthly income (R$)",
    y        = "Density",
    fill     = NULL,
    caption  = "Source: IBGE, PNAD Contínua. Author's own analysis."
  ) +
  theme_pnadc()

print(p1)
ggsave("output/01_income_distribution_gender.png", p1, width = 8, height = 5, dpi = 150)
cat("Chart saved: 01_income_distribution_gender.png\n")


## 7.2 Mean income by education level
p2 <- pnadc |>
  filter(emp_status == "Employed", !is.na(VD4020), VD4020 > 0, !is.na(education)) |>
  group_by(education) |>
  summarise(mean_income = mean(VD4020), .groups = "drop") |>
  ggplot(aes(x = education, y = mean_income, fill = education)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = scales::dollar(mean_income, prefix = "R$",
                                       big.mark = ".", decimal.mark = ",")),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = colorRampPalette(c(PALETTE["soft"], PALETTE["main"]))(5)) +
  scale_y_continuous(
    labels  = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ","),
    expand  = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Mean monthly income by education level",
    subtitle = "Employed population | PNAD Contínua 4Q2023",
    x        = NULL, y = "Mean income (R$)",
    caption  = "Source: IBGE, PNAD Contínua. Author's own analysis."
  ) +
  theme_pnadc() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p2)
ggsave("output/02_income_by_education.png", p2, width = 8, height = 5, dpi = 150)
cat("Chart saved: 02_income_by_education.png\n")


## 7.3 Income by race and gender
p3 <- pnadc |>
  filter(emp_status == "Employed", !is.na(VD4020), VD4020 > 0,
         !is.na(V2010), !is.na(V2007),
         V2010 %in% c("Branca", "Preta", "Parda")) |>
  group_by(V2010, V2007) |>
  summarise(mean_income = mean(VD4020), .groups = "drop") |>
  mutate(V2007 = as.character(V2007),
         V2010 = as.character(V2010)) |>
  ggplot(aes(x = reorder(V2010, mean_income), y = mean_income, fill = V2007)) +
  geom_col(position = "dodge", width = 0.65) +
  scale_fill_manual(values = c("Homem" = unname(PALETTE["main"]), "Mulher" = unname(PALETTE["accent"]))) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_flip() +
  labs(
    title    = "Mean income by race and gender",
    subtitle = "Employed population | PNAD Contínua 4Q2023",
    x = NULL, y = "Mean monthly income (R$)", fill = NULL,
    caption  = "Source: IBGE, PNAD Contínua. Author's own analysis."
  ) +
  theme_pnadc()

print(p3)
ggsave("output/03_income_by_race_gender.png", p3, width = 8, height = 5, dpi = 150)
cat("Chart saved: 03_income_by_race_gender.png\n")


## 7.4 Mean income by macro-region with confidence interval
region_summary <- pnadc |>
  filter(emp_status == "Employed", !is.na(VD4020), VD4020 > 0, !is.na(macro_region)) |>
  group_by(macro_region) |>
  summarise(
    mean_income = mean(VD4020),
    se          = sd(VD4020) / sqrt(n()),
    ci_lower    = mean_income - 1.96 * se,
    ci_upper    = mean_income + 1.96 * se,
    .groups     = "drop"
  )

p4 <- region_summary |>
  ggplot(aes(x = reorder(macro_region, mean_income), y = mean_income,
             fill = reorder(macro_region, mean_income))) +
  geom_col(width = 0.65, alpha = 0.9, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.25, color = "grey30", linewidth = 0.7
  ) +
  geom_text(
    aes(label = scales::dollar(mean_income, prefix = "R$",
                               big.mark = ".", decimal.mark = ",")),
    hjust = -0.15, size = 3.2, color = "grey20"
  ) +
  scale_fill_manual(values = colorRampPalette(c(PALETTE["soft"], PALETTE["main"]))(5)) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "R$", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.20))
  ) +
  coord_flip() +
  labs(
    title    = "Mean monthly income by macro-region",
    subtitle = "Employed population | Error bars = 95% CI | PNAD Contínua 4Q2023",
    x = NULL, y = "Mean monthly income (R$)",
    caption  = "Source: IBGE, PNAD Contínua. Author's own analysis."
  ) +
  theme_pnadc() +
  theme(panel.grid.major.y = element_blank())

print(p4)
ggsave("output/04_income_by_region.png", p4, width = 8, height = 4.5, dpi = 150)
cat("Chart saved: 04_income_by_region.png\n")


# -----------------------------------------------------------------------------
# 8. Summary
# -----------------------------------------------------------------------------

cat("\n=== Pipeline complete ===\n")
cat("Charts saved to output/\n")
cat(sprintf("Dataset: %d respondents | PNAD Contínua 4Q2023\n", nrow(pnadc)))
cat("Next: pnadc-survey-weights — weighted estimates with correct standard errors\n")