###############################################################################
# Capstone: Used Car Pricing & Inventory Optimization
# Author: <Your Name>
# Date:   <YYYY-MM-DD>
# Notes:
# - End-to-end workflow: data loading → cleaning/EDA → Ridge regression →
#   inventory optimization (knapsack-style under budget).
# - Dependencies: readxl, dplyr, ggplot2, forcats, stringr, scales, caret,
#   glmnet, lpSolve
# Reproducibility:
# - Set seed, print session info, and write artifacts (figures & summaries).
###############################################################################

# ============================== 0) CONFIG ====================================
options(stringsAsFactors = FALSE)
set.seed(123)

# Paths
DATA_PATH   <- "Used Car Data.xlsx"
FIG_DIR     <- "figures"
OUT_DIR     <- "outputs"

# Modeling / EDA parameters
TOP_N_TRANSMISSION <- 8
TOP_N_FUEL         <- 6
TRAIN_PCT          <- 0.80

# Inventory optimization parameters
BUDGET             <- 500000            # acquisition budget (in predicted-price units)
OPTIMIZE_FOR       <- "predicted_price" # keep original behavior; options: "predicted_price"

# Create directories if needed
if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR, recursive = TRUE)
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ======================= 1) LIBRARIES & UTILITIES ============================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(stringr)
  library(scales)
  library(caret)
  library(glmnet)
  library(lpSolve)
})

log_info <- function(...) cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] "), ..., "\n")

# Safe mean for NA handling
mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# ====================== 2) DATA LOADING & CLEANING ===========================
log_info("Loading data from: ", DATA_PATH)
used_car_data <- read_excel(DATA_PATH, skip = 1)

# Standardize names (fix known typo)
names(used_car_data)[names(used_car_data) == "milage"] <- "mileage"

# Identify columns (only keep ones that exist to avoid errors)
cat_cols_all <- c("fuel_type", "transmission", "ext_col", "int_col", "accident", "clean_title")
cat_cols     <- intersect(cat_cols_all, names(used_car_data))
num_cols     <- setdiff(names(used_car_data), cat_cols)

# Replace missing values in categorical columns with "Unknown"
if (length(cat_cols) > 0) {
  used_car_data <- used_car_data %>%
    mutate(across(all_of(cat_cols), ~ ifelse(is.na(.), "Unknown", as.character(.))))
}

# Replace missing values in numeric columns with column means (robust)
used_car_data <- used_car_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean_na(.), .)))

# Coerce and tidy text fields, standardize numeric fields
used_car_data <- used_car_data %>%
  mutate(
    # Ensure price numeric
    price   = suppressWarnings(as.numeric(price)),
    # Remove commas/units from mileage like "51,000 mi."
    mileage = suppressWarnings(as.numeric(gsub("[^0-9.]", "", mileage))),
    across(all_of(cat_cols), ~ str_squish(as.character(.)))
  )

# Final NA check
na_counts <- colSums(is.na(used_car_data))
log_info("NA counts by column:")
print(na_counts)

# ============================ 3) EDA PREP ====================================
# Group long-tail categories for cleaner plots
used_car_data <- used_car_data %>%
  mutate(
    transmission_grp = if ("transmission" %in% names(.))
      fct_lump_n(transmission, n = TOP_N_TRANSMISSION, other_level = "Other/Unknown") else factor("Unknown"),
    fuel_grp         = if ("fuel_type" %in% names(.))
      fct_lump_n(fuel_type,   n = TOP_N_FUEL,         other_level = "Other/Unknown") else factor("Unknown")
  )

# Price range for focusing central 98%
p_lo <- quantile(used_car_data$price, 0.01, na.rm = TRUE)
p_hi <- quantile(used_car_data$price, 0.99, na.rm = TRUE)

# =============================== 4) EDA ======================================
log_info("Generating EDA figures...")

p_hist <- ggplot(used_car_data, aes(x = price)) +
  geom_histogram(bins = 40, color = "white") +
  scale_x_log10(labels = label_dollar()) +
  labs(title = "Price Distribution (log scale)", x = "Price (log10)", y = "Count") +
  theme_minimal(base_size = 12)

p_scatter <- ggplot(used_car_data, aes(x = mileage, y = price)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_dollar()) +
  labs(title = "Mileage vs Price (log–log with linear fit)",
       x = "Mileage (log10)", y = "Price (log10)") +
  theme_minimal(base_size = 12)

p_trans <- used_car_data %>%
  mutate(transmission_ord = fct_reorder(transmission_grp, price, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = transmission_ord, y = price)) +
  geom_violin(fill = "grey85", color = "grey60") +
  geom_boxplot(width = 0.15, outlier.alpha = 0.15) +
  coord_cartesian(ylim = c(p_lo, p_hi)) +
  labs(title = "Price by Transmission (central 98% of prices)",
       x = "Transmission (grouped)", y = "Price") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = label_dollar())

p_fuel <- used_car_data %>%
  mutate(fuel_ord = fct_reorder(fuel_grp, price, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = fuel_ord, y = price)) +
  geom_violin(fill = "grey85", color = "grey60") +
  geom_boxplot(width = 0.15, outlier.alpha = 0.15) +
  coord_cartesian(ylim = c(p_lo, p_hi)) +
  labs(title = "Price by Fuel Type (central 98% of prices)",
       x = "Fuel Type (grouped)", y = "Price") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels = label_dollar())

ggsave(file.path(FIG_DIR, "01_price_hist_log.png"), p_hist,   width = 8,  height = 5,  dpi = 300)
ggsave(file.path(FIG_DIR, "02_mileage_vs_price_loglog.png"), p_scatter, width = 8,  height = 5,  dpi = 300)
ggsave(file.path(FIG_DIR, "03_price_by_transmission.png"), p_trans, width = 9,  height = 5.5, dpi = 300)
ggsave(file.path(FIG_DIR, "04_price_by_fuel.png"), p_fuel,   width = 7.5,height = 5,  dpi = 300)

# Summaries to cite
num_summary <- used_car_data %>%
  summarise(
    n               = n(),
    price_mean      = mean(price,   na.rm = TRUE),
    price_median    = median(price, na.rm = TRUE),
    mileage_mean    = mean(mileage, na.rm = TRUE),
    mileage_median  = median(mileage, na.rm = TRUE),
    cor_mileage_price = cor(mileage, price, use = "complete.obs")
  )

write.csv(num_summary, file.path(OUT_DIR, "summary_numeric.csv"), row.names = FALSE)
log_info("Numeric summary:")
print(num_summary)

med_price_by_trans <- used_car_data %>%
  group_by(transmission_grp) %>%
  summarise(median_price = median(price, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(desc(median_price))
write.csv(med_price_by_trans, file.path(OUT_DIR, "median_price_by_transmission.csv"), row.names = FALSE)

med_price_by_fuel <- used_car_data %>%
  group_by(fuel_grp) %>%
  summarise(median_price = median(price, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(desc(median_price))
write.csv(med_price_by_fuel, file.path(OUT_DIR, "median_price_by_fuel.csv"), row.names = FALSE)

# ===================== 5) PREDICTIVE MODELING (RIDGE) ========================
log_info("Starting modeling... one-hot encoding & train/test split.")

# Convert character categoricals to factors (only if present)
if (length(cat_cols) > 0) {
  used_car_data[cat_cols] <- lapply(used_car_data[cat_cols], factor)
}

# Build design matrix via caret::dummyVars
# Exclude price from predictors in the formula
dummies     <- dummyVars(price ~ ., data = used_car_data)
encoded_x   <- predict(dummies, newdata = used_car_data) %>% as.data.frame()
model_data  <- cbind(encoded_x, price = used_car_data$price)

# Train/test split
train_idx   <- createDataPartition(model_data$price, p = TRAIN_PCT, list = FALSE)
train_data  <- model_data[train_idx, ]
test_data   <- model_data[-train_idx, ]

x_train <- as.matrix(select(train_data, -price))
y_train <- train_data$price
x_test  <- as.matrix(select(test_data,  -price))
y_test  <- test_data$price

# Ridge regression with CV
log_info("Fitting Ridge model with cross-validation...")
cv_fit      <- cv.glmnet(x_train, y_train, alpha = 0)
best_lambda <- cv_fit$lambda.min

# Predict using the CV model at lambda.min (keeps behavior aligned to your intent)
predictions <- predict(cv_fit, s = "lambda.min", newx = x_test)
predictions <- as.numeric(predictions)

# Evaluate
rmse <- sqrt(mean((predictions - y_test)^2))
r2   <- 1 - sum((predictions - y_test)^2) / sum((y_test - mean(y_test))^2)

perf <- data.frame(RMSE = rmse, R2 = r2, lambda_min = best_lambda)
write.csv(perf, file.path(OUT_DIR, "model_performance.csv"), row.names = FALSE)
log_info("Model Performance: RMSE = ", round(rmse, 2), ", R^2 = ", round(r2, 3))

# Coefficients for interpretability (sorted by absolute magnitude)
coef_df <- as.matrix(coef(cv_fit, s = "lambda.min")) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("feature") %>%
  rename(coefficient = 2) %>%
  arrange(desc(abs(coefficient)))

write.csv(coef_df, file.path(OUT_DIR, "ridge_coefficients.csv"), row.names = FALSE)

# ====================== 6) INVENTORY OPTIMIZATION (LP) =======================
log_info("Preparing data for inventory optimization...")

test_data_with_preds <- test_data %>%
  mutate(predicted_price = predictions)

# Value score — keep original functionality (maximize predicted_price)
test_data_with_preds <- test_data_with_preds %>%
  mutate(value_score = dplyr::case_when(
    OPTIMIZE_FOR == "predicted_price" ~ predicted_price,
    TRUE                              ~ predicted_price  # default is the same
  ))

vehicle_costs <- test_data_with_preds$predicted_price
vehicle_vals  <- test_data_with_preds$value_score
n_vehicles    <- length(vehicle_costs)

# LP: maximize sum(value_score * x_i) subject to sum(cost_i * x_i) <= BUDGET, x_i ∈ {0,1}
log_info("Solving knapsack under budget = ", BUDGET)
lp_result <- lp(
  direction   = "max",
  objective.in = vehicle_vals,
  const.mat    = matrix(vehicle_costs, nrow = 1),
  const.dir    = "<=",
  const.rhs    = BUDGET,
  all.bin      = TRUE
)

if (lp_result$status != 0) {
  warning("LP did not converge to an optimal solution. Status: ", lp_result$status)
}

selected_idx       <- which(lp_result$solution == 1)
selected_inventory <- test_data_with_preds[selected_idx, , drop = FALSE]

total_selected <- nrow(selected_inventory)
total_cost     <- sum(selected_inventory$predicted_price)

log_info("Total vehicles selected: ", total_selected)
log_info("Total predicted cost:   ", round(total_cost, 2))

write.csv(selected_inventory, file.path(OUT_DIR, "selected_inventory.csv"), row.names = FALSE)

# ============================= 7) SESSION INFO ===============================
log_info("Writing session info...")
sink(file.path(OUT_DIR, "session_info.txt"))
sessionInfo()
sink()

log_info("Done.")

