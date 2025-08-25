# Used Car Pricing & Inventory Optimization (Capstone)

End-to-end project integrating **EDA → Ridge Regression → Linear Programming** to predict used-car prices and select an optimal inventory under a fixed budget.

---

## 🔎 Overview
This capstone applies a **full data science workflow** to the used car market:

1) **Data Loading & Cleaning** (fix headers, type coercion, NA imputation)  
2) **Exploratory Data Analysis** (price distributions, mileage–price relationships, category comparisons)  
3) **Predictive Modeling** with **Ridge Regression** (one-hot encoding, CV for λ selection)  
4) **Inventory Optimization** using **Linear Programming** (knapsack under a budget)

**Outcome:** A reproducible pipeline that turns price predictions into **actionable inventory choices**.

---

## 🧰 Tech Stack & Skills
- **R:** readxl, dplyr, tidyr, ggplot2, forcats, stringr, scales, caret, glmnet, lpSolve  
- **Skills:** data cleaning & imputation, EDA & visualization, feature engineering, regularization, model evaluation, linear programming, reproducible research  

---

## 📁 Project Structure
```
.
├─ capstone_used_cars.R           # Main script
├─ Used Car Data.xlsx             # Input data (not committed if sensitive)
├─ figures/                       # Exported plots
│  ├─ 01_price_hist_log.png
│  ├─ 02_mileage_vs_price_loglog.png
│  ├─ 03_price_by_transmission.png
│  └─ 04_price_by_fuel.png
├─ outputs/                       # Tables, metrics, artifacts
│  ├─ summary_numeric.csv
│  ├─ median_price_by_transmission.csv
│  ├─ median_price_by_fuel.csv
│  ├─ model_performance.csv
│  ├─ ridge_coefficients.csv
│  ├─ selected_inventory.csv
│  └─ session_info.txt
└─ README.md
```

---

## ▶️ How to Run
1. Install required R packages:
   ```r
   install.packages(c("readxl","dplyr","tidyr","ggplot2","forcats","stringr","scales",
                      "caret","glmnet","lpSolve"))
   ```
2. Place the data file (`Used Car Data.xlsx`) in the repo root.  
3. Run the script:
   ```r
   source("capstone_used_cars.R")
   ```
4. Outputs will appear in `figures/` and `outputs/`.

---

## 📊 Key Results
**Model Performance** (`outputs/model_performance.csv`)  
- RMSE: …  
- R²: …  
- Lambda: …  

**Selected Inventory** (`outputs/selected_inventory.csv`)  
- Budget: 500,000  
- Vehicles selected: …  
- Total predicted cost: …  

---

## 🖼️ Figures
- `figures/01_price_hist_log.png` — Price distribution (log scale)  
- `figures/02_mileage_vs_price_loglog.png` — Mileage vs Price (log–log)  
- `figures/03_price_by_transmission.png` — Price by transmission  
- `figures/04_price_by_fuel.png` — Price by fuel type  

---

## 🚀 Roadmap
- Extend to other models (GBM, Random Forest, Elastic Net)  
- Add cost/margin assumptions for ROI-based optimization  
- Build a Shiny app to interactively adjust `BUDGET`  

---

## 👤 Author
**Zoe Vanover** — Data Science M.S., WGU    
- LinkedIn: <https://www.linkedin.com/in/zoe-vanover-65a996258/>
- Email: <zoe.vanover93@gmail.com>  
