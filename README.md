# r-bootcamp-project
# Divorce Trends and Demographics in Switzerland  
R-Bootcamp Group Project — Enerel Khuyag & Daniela Hellberg

---

## 📌 Project Overview

This project analyzes divorce patterns across Swiss cantons and explores how demographic structure relates to divorce counts.

We combine:

- canton-level divorce statistics
- demographic population data (sex + marital status)

to understand:

- regional divorce variation
- population structure effects
- statistical relationships through regression modeling

The project demonstrates a full reproducible workflow in R:

data cleaning → merging → visualization → modeling → reporting.

---

## 📁 Project Structure

```
r-bootcamp-project/
│
├── data_raw/              # original datasets (untouched)
├── data_clean/            # cleaned + merged datasets
│
├── outputs/               # exported plots (.png)
│
├── R/
│   ├── 01_cleaning.R      # data cleaning pipeline
│   ├── 03_visuals.R       # all visualization code
│   ├── 04_model.R         # regression modeling
│
├── Report.Rmd             # final R Markdown report
├── Report.html            # rendered report output
│
└── README.md              # this file
```

---

## 🔹 What Has Already Been Done

### ✅ Data cleaning

- standardized canton names
- verified numeric population values
- formatted categorical variables
- created clean canton-year datasets
- removed aggregate “Switzerland” rows for modeling

### ✅ Dataset merging

- merged demographic + divorce datasets
- join key: canton + year
- ensured compatibility for modeling + plotting

### ✅ Visualization pipeline

We created multiple plot types:

- national time trends
- canton rankings
- rate-adjusted comparisons
- volatility charts
- heatmaps
- demographic structure visuals
- scatter relationships

All plots are exported to:

```
outputs/
```

and automatically included in the report.

### ✅ Statistical modeling

Linear regression model:

```
divorces ~ population + sex + marital_status + year
```

Includes:

- diagnostics plots
- predicted vs actual comparison
- interpretation in report

### ✅ Interactive geographic visualization

Leaflet-based canton map showing divorce rates.

---

## ▶ How to Run the Project

### Step 1 — open project in RStudio

Open the project folder:

```
r-bootcamp-project
```

---

### Step 2 — install required packages (first time only)

Run:

```r
install.packages(c(
  "dplyr",
  "readr",
  "ggplot2",
  "knitr",
  "sf",
  "leaflet",
  "rnaturalearth"
))
```

---

### Step 3 — regenerate plots (optional)

Run scripts in order:

```
R/01_cleaning.R
R/03_visuals.R
R/04_model.R
```

This rebuilds datasets and figures.

---

### Step 4 — build report

Open:

```
Report.Rmd
```

Click:

👉 **Knit → HTML**

This produces:

```
Report.html
```

---

## 📊 What the Report Contains

- Introduction & objectives
- Data preparation explanation
- 10+ visual analyses
- regression modeling
- diagnostics interpretation
- interactive canton map
- conclusions + limitations
- reflection on AI use

The document is written for a client-style audience.

---

## 🤝 Collaboration Notes

Daniela — everything is modular:

You can safely:

- edit report text
- adjust plots
- change model variables
- improve interpretation
- add visuals

Just re-knit the report to update output.

No data will break — pipeline is reproducible.

---

## ⚠ Known limitations

- aggregated demographic data
- simple linear model
- not causal inference
- social factors not explicitly modeled

---

## 🎯 Project Goal

Showcase:

- reproducible R workflow
- clean visualization practices
- interpretable modeling
- professional reporting structure

---

## 🚀 Next Possible Improvements

- alternative models (Poisson / GLM)
- demographic normalization
- more geographic visualization
- cross-year comparisons
- improved styling
- no proper storytelling exists

---

## 💬 Notes

If anything errors:

- re-run cleaning script
- check working directory
- reinstall missing packages

Everything should knit end-to-end.

---

End of README.