# PNAD Contínua — Survey Analysis in R

A three-part analysis of Brazil's national household survey (PNAD Contínua) demonstrating
a complete quantitative research workflow: from raw microdata to weighted estimates and
regression models with correct inference.

---

## Why this project

National household surveys like PNAD Contínua require methodological care that goes
beyond standard data analysis. The data is not a simple random sample — it uses a
complex probability design with stratification, clustering, and unequal selection
probabilities. Ignoring this structure leads to biased estimates, underestimated
standard errors, and invalid inference.

This project shows how to work with PNAD Contínua correctly at each stage:
describing the data, incorporating the sample design into estimates, and fitting
regression models that produce valid population-level inference.

---

## Dataset

**PNAD Contínua** (Pesquisa Nacional por Amostra de Domicílios Contínua) is Brazil's
main household survey, conducted quarterly by IBGE. It covers labor market conditions,
income, education, and living standards for the Brazilian population.

This analysis uses the **4th quarter 2023** microdata, downloaded directly from IBGE
via the `PNADcIBGE` R package with `labels = TRUE`, which applies the official data
dictionary to all categorical variables.

Sample size: 473,206 respondents.

---

## Pipeline structure

The three scripts form a methodological progression and should be read in order.

### `R/01_descriptive.R` — Descriptive Analysis and Visualization

Entry point of the project. Downloads and recodes the microdata, produces frequency
tables, cross-tabulations, and four ggplot2 charts covering income distribution by
gender, education, race, and macro-region.

Key outputs: `output/01` to `output/04`

### `R/02_survey_weights.R` — Survey Weights and Complex Sample Design

The methodological core. Builds the survey design object and shows concretely why
unweighted estimates are biased. Produces weighted means with correct standard errors
and confidence intervals, calculates design effects (DEFF) by region, and generates
weighted cross-tabulations using `srvyr`.

Key finding: the weighted mean income (R$ 3,063) is 11% higher than the unweighted
mean (R$ 2,763), and design effects in the North and Southeast reach nearly 5.0 —
meaning the effective sample size is five times smaller than the nominal count for
those regional estimates.

Key outputs: `output/06` to `output/07`

### `R/03_regression.R` — Regression Models and Inference

Fits two models using `svyglm()`, which correctly incorporates the complex sample
design into the estimation:

- **OLS model**: log income as a function of education, gender, race, age, and
  macro-region. Coefficients interpreted as approximate percentage changes in income.
- **Logistic model**: probability of employment as a function of the same predictors.
  Results reported as odds ratios with 95% confidence intervals.

The script also produces predicted probability curves by education and gender,
calculated on the log-odds scale and back-transformed to preserve the [0,1] bounds.

Key outputs: `output/09` to `output/11`, model tables as `.csv`

---

## Selected findings

All results from PNAD Contínua 4Q2023 (N = 473,206 respondents).

### Survey design

Incorporating the complex sample design changes estimates substantially. The weighted mean monthly income among employed workers is **R$ 3,063**, compared to an unweighted mean of **R$ 2,763** — an 11% difference. Design effects (DEFF) range from 1.87 in the South to 4.90 in the North, meaning that for North regional estimates, the effective sample size is less than a quarter of the nominal count. Standard errors from naive unweighted analysis are systematically underestimated.

### OLS regression — determinants of log income

Controlling for education, race, age, and region simultaneously:

| Predictor | Effect on income | Significance |
|---|---|---|
| Higher education (vs. Secondary) | +70% | *** |
| Primary complete (vs. Secondary) | -19% | *** |
| Primary incomplete (vs. Secondary) | -35% | *** |
| No schooling (vs. Secondary) | -36% | *** |
| Mulher (vs. Homem) | -28% | *** |
| Black (vs. White) | -20% | *** |
| Brown/Pardo (vs. White) | -20% | *** |
| Indigenous (vs. White) | -27% | *** |
| Northeast (vs. Southeast) | -38% | *** |
| North (vs. Southeast) | -21% | *** |
| South (vs. Southeast) | +5% | *** |
| Center-West (vs. Southeast) | +10% | *** |

The education premium is the strongest predictor: workers with higher education earn 70% more than those with secondary education, after controlling for all other factors. The gender gap persists at 28% even after controlling for education, race, age, and region.

### Logistic regression — employment in the PEA

Among people in the labor force (employed + unemployed), controlling for the same covariates:

| Predictor | Odds ratio | Significance |
|---|---|---|
| Higher education (vs. Secondary) | 1.57 | *** |
| Primary complete (vs. Secondary) | 0.93 | . |
| Primary incomplete (vs. Secondary) | 0.90 | . |
| Mulher (vs. Homem) | 0.57 | *** |
| Black (vs. White) | 0.78 | *** |
| Brown/Pardo (vs. White) | 0.85 | *** |
| Northeast (vs. Southeast) | 0.71 | *** |
| South (vs. Southeast) | 1.68 | *** |

Women in the labor force have 43% lower odds of being employed than men (OR = 0.57), the largest single predictor in the model. Workers with higher education have 57% higher odds of employment than those with secondary education (OR = 1.57). The South has substantially higher employment odds than the Southeast reference category (OR = 1.68).

---

## How to run

```r
# Install required packages (run once)
install.packages(c("PNADcIBGE", "tidyverse", "survey", "srvyr", "broom"))

# Run each pipeline in order
source("R/01_descriptive.R")
source("R/02_survey_weights.R")
source("R/03_regression.R")
```

Each script downloads the microdata directly from IBGE on first run (~200 MB).
Charts are saved to `output/` and also displayed in the RStudio Plots panel.

---

## Repository structure

```
pnadc-analysis/
│
├── R/
│   ├── 01_descriptive.R       # data download, recoding, EDA, visualization
│   ├── 02_survey_weights.R    # survey design, weighted estimates, DEFF
│   └── 03_regression.R        # OLS and logistic models with svyglm()
│
├── output/                    # generated at runtime — see .gitignore
│
└── README.md
```

---

## Stack

| Package | Purpose |
|---|---|
| `PNADcIBGE` | Direct download of PNAD Contínua microdata from IBGE |
| `tidyverse` | Data manipulation (`dplyr`, `tidyr`) and visualization (`ggplot2`) |
| `survey` | Complex survey analysis — `svydesign`, `svymean`, `svyby`, `svyglm` |
| `srvyr` | Tidyverse-style wrapper for `survey` |
| `broom` | Tidy model output — `tidy()` for coefficient tables |

---

## Important methodological note

All categorical variables are recoded using the labels returned by
`get_pnadc(labels = TRUE)`, which applies the official IBGE data dictionary.
No numeric codes are used. Each script includes an inspection block (`levels()`)
that should be run first whenever working with a new edition of the survey,
as labels may change between releases.

---

## Author

**Teresa De Bastiani**
Senior Market Research Analyst · Florianópolis, Brazil
[LinkedIn](https://linkedin.com/in/mteresadebastiani)
