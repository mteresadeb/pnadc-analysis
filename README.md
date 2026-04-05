# PNAD Contínua: Survey Analysis in R

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

| Result | Value |
|---|---|
| Gender income gap (weighted, OLS) | Women earn ~30% less than men, controlling for education, race, age and region |
| Education premium | Higher education associated with ~57% income increase vs. secondary |
| Northeast penalty | Northeast residents earn ~40% less than Southeast, controlling for other factors |
| Design effect (North) | DEFF = 4.90 — unweighted SE underestimates true variance by factor of ~2.2 |
| Employment — higher education | OR = 0.79 vs. secondary — more educated people are less likely to be formally employed (more self-employed, informal, or inactive) |

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
Senior Market Research Analyst · 
[LinkedIn](https://linkedin.com/in/mteresadebastiani)
