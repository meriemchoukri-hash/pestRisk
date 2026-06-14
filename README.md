# pestRisk <img src="man/figures/logo.png" align="right" height="100" alt="pestRisk logo"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/meriembenali/pestRisk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/meriembenali/pestRisk/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

> **A complete R toolkit for modelling the current invasion risk of agricultural pests**  
> using GBIF occurrence data, WorldClim bioclimatic variables, and Species Distribution Models (SDMs).

---

## Overview

`pestRisk` implements a reproducible, end-to-end workflow for phytosanitary
risk assessment:

```
┌─────────────────────────────────────────────────────────────────┐
│                       pestRisk pipeline                         │
├──────────────┬────────────────────────────────────────────────┤
│  DATA        │  download_gbif()       download_worldclim()    │
│  CLEANING    │  clean_occurrences()   prepare_predictors()    │
│  MODELLING   │  generate_background_points()                  │
│              │  train_rf_model()      train_maxent()          │
│  EVALUATION  │  evaluate_models()                             │
│  PREDICTION  │  predict_risk_map()   summarize_country_risk() │
│  OUTPUT      │  plot_risk_map()      plot_variable_importance()│
│              │  generate_report()                             │
└──────────────┴────────────────────────────────────────────────┘
```

### Target species

| Species | Common name | Main crops affected |
|---------|-------------|---------------------|
| *Spodoptera frugiperda* | Fall armyworm | Maize, sorghum, millet |
| *Tuta absoluta* | Tomato leafminer | Tomato, pepper, potato |
| *Ceratitis capitata* | Mediterranean fruit fly | Citrus, stone fruits |

---

## Installation

```r
# Install from GitHub (recommended)
# install.packages("remotes")
remotes::install_github("meriembenali/pestRisk")

# Or clone and install locally
# devtools::install("path/to/pestRisk")
```

### Dependencies

```r
install.packages(c(
  "rgbif", "terra", "sf", "geodata",
  "randomForest", "maxnet", "pROC",
  "ggplot2", "dplyr", "tidyr",
  "viridis", "rmarkdown", "knitr"
))
```

---
## Data Sources

| Data | Source | Citation |
|------|--------|----------|
| *Spodoptera frugiperda* occurrences | GBIF.org via rgbif | GBIF.org (2026) |
| *Tuta absoluta* occurrences | GBIF.org via rgbif | GBIF.org (2026) |
| *Ceratitis capitata* occurrences | GBIF.org via rgbif | GBIF.org (2026) |
| Bioclimatic variables | WorldClim v2.1 | Fick & Hijmans (2017) |

**References:**
- GBIF.org (2026) GBIF Occurrence Download. https://www.gbif.org
- Fick, S.E. & Hijmans, R.J. (2017) WorldClim 2. 
  *International Journal of Climatology* 37: 4302–4315.
  https://doi.org/10.1002/joc.5086
- Chamberlain et al. (2024) rgbif: Interface to the Global Biodiversity 
  Information Facility API. https://CRAN.R-project.org/package=rgbif
## Quick start

```r
library(pestRisk)

# ── 1. Download GBIF occurrences ─────────────────────────────────
occ <- download_gbif("Spodoptera frugiperda",
                      limit = 1500, year = "2010,2023")

# ── 2. Clean occurrences ─────────────────────────────────────────
clean <- clean_occurrences(occ, thin_dist = 20)
print(clean$report)

# ── 3. Download WorldClim (Africa + Mediterranean) ───────────────
clim <- download_worldclim(
  path   = "~/pestRisk_data",
  res    = "10",
  extent = c(-20, 55, -40, 40)
)

# ── 4. Background points ─────────────────────────────────────────
bg <- generate_background_points(clim, clean$data, n = 5000)

# ── 5. Variable selection (VIF + Pearson) ────────────────────────
prep <- prepare_predictors(clean$data, bg, clim)
cat("Selected:", prep$selected_vars, "\n")

# ── 6. Train models ───────────────────────────────────────────────
rf_out <- train_rf_model(prep$model_data)
mx_out <- train_maxent(prep$model_data)

# ── 7. Evaluate ───────────────────────────────────────────────────
eval <- evaluate_models(rf_out, mx_out,
                         model_names = c("Random Forest", "MaxEnt"))
print(eval$metrics)
print(eval$roc_plot)

# ── 8. Predict risk map ───────────────────────────────────────────
risk <- predict_risk_map(rf_out, clim)

# ── 9. Visualise ──────────────────────────────────────────────────
plot_risk_map(risk, type = "continuous",
              occurrences_sf = clean$data,
              species_name   = "Spodoptera frugiperda")
plot_risk_map(risk, type = "classified")

# ── 10. Country risk ranking ─────────────────────────────────────
country_tbl <- summarize_country_risk(risk, top_n = 15)

# ── 11. Automatic report ─────────────────────────────────────────
generate_report(
  species_name    = "Spodoptera frugiperda",
  occurrences_sf  = clean$data,
  clean_report    = clean$report,
  eval_results    = eval,
  risk_map_obj    = risk,
  country_summary = country_tbl,
  model_obj       = rf_out,
  output_file     = "Sf_risk_report.html"
)
```

---
## Vignette

[Voir la vignette avec les outputs](https://htmlpreview.github.io/?https://raw.githubusercontent.com/meriemchoukri-hash/pestRisk/main/vignettes/introduction-to-pestRisk.html)

## Data Sources

The CSV files used in this package were downloaded from 
reliable open-access resources (GBIF, WorldClim, etc.)



## Function reference

| Function | Description |
|----------|-------------|
| `download_gbif()` | Download and pre-filter GBIF occurrences |
| `clean_occurrences()` | 6-step spatial cleaning pipeline |
| `download_worldclim()` | Download BIO1–19 from WorldClim v2.1 |
| `prepare_predictors()` | Pearson + VIF variable selection |
| `generate_background_points()` | Pseudo-absence generation with exclusion buffer |
| `train_rf_model()` | Random Forest SDM with balanced training |
| `train_maxent()` | MaxEnt via `maxnet` with response curves |
| `evaluate_models()` | AUC, Accuracy, Sensitivity, Specificity, F1 + ROC plots |
| `predict_risk_map()` | Apply SDM to raster → suitability + risk class |
| `summarize_country_risk()` | Country-level risk statistics |
| `plot_risk_map()` | Continuous or classified risk maps (ggplot2) |
| `plot_variable_importance()` | Importance barplot + MaxEnt response curves |
| `generate_report()` | Automatic HTML/PDF phytosanitary risk report |

---

## Ecological rationale

### Why these bioclimatic variables?

The six default BIO variables (BIO1, BIO4, BIO5, BIO6, BIO12, BIO15) were
selected based on insect pest physiology:

- **Temperature** (BIO1, BIO5, BIO6): Controls development rate, thermal limits,
  and overwintering survival. *S. frugiperda* requires a minimum winter
  temperature of ~10°C for year-round population persistence.
- **Seasonality** (BIO4, BIO15): Drives pest phenology and crop synchrony.
- **Precipitation** (BIO12): Affects host plant quality, humidity, and larval
  survival.

### Why Random Forest + MaxEnt?

Both models are complementary:

| Aspect | Random Forest | MaxEnt |
|--------|---------------|--------|
| Data | Presence + background | Presence-only |
| Output | Binary probability | Continuous suitability |
| Strength | Non-linear interactions | Few presences, no absences needed |
| Use case | Ensemble base learner | Exploratory, climate matching |

An ensemble of both provides more robust risk estimates.

---

## Project structure

```
pestRisk/
├── R/                          # Package functions (13 files)
│   ├── download_gbif.R
│   ├── clean_occurrences.R
│   ├── download_worldclim.R
│   ├── prepare_predictors.R
│   ├── generate_background_points.R
│   ├── train_rf_model.R
│   ├── train_maxent.R
│   ├── evaluate_models.R
│   ├── predict_risk_map.R
│   ├── summarize_country_risk.R
│   ├── plot_risk_map.R         # also contains plot_variable_importance()
│   ├── generate_report.R
│   └── utils.R                 # internal helpers
├── data/                       # Example datasets (.rda)
├── data-raw/                   # Scripts to recreate example data
├── inst/extdata/               # Report Rmd template
├── tests/testthat/             # Unit tests (testthat)
├── vignettes/                  # Reproducible walkthrough
├── man/                        # Documentation (auto-generated by roxygen2)
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## Citation

```
Choukri M. (2026). pestRisk: Modelling Current Agricultural Pest Invasion Risk
Using SDMs. R package version 0.1.0.
https://github.com/meriemchoukri-hash/pestRisk
```

---

## License

MIT © 2026 Meriem Choukri – iav rabat

---

## References

- Fick, S.E. & Hijmans, R.J. (2017). WorldClim 2. *Int. J. Climatol.* **37**, 4302–4315.
- Phillips, S.J. *et al.* (2017). Opening the black box: MaxEnt. *Ecography* **40**, 887–893.
- Barbet-Massin, M. *et al.* (2012). Selecting pseudo-absences. *Methods Ecol. Evol.* **3**, 327–338.
- Dormann, C.F. *et al.* (2013). Collinearity in SDMs. *Ecography* **36**, 27–46.
