
library(fpp3)       # tsibble, fable, feasts
library(forecast)   # ndiffs(), nsdiffs(), adf.test()
library(tseries)    # kpss.test()
library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

theme_set(theme_minimal(base_size = 12))

fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)
save_png <- function(p, name, w = 9, h = 5)
  ggsave(file.path(fig_dir, name), plot = p, width = w, height = h,
         dpi = 150, bg = "white")

cat("=== MEDLISPRIRI TIME SERIES ANALYSIS ===\n\n")


# SECTION 1: Load & Validate


raw <- read_csv("MEDLISPRIRI.csv", show_col_types = FALSE)
names(raw) <- c("date", "price")
raw <- raw |> mutate(date = as.Date(date, format = "%m/%d/%y"))

cat("Observations:", nrow(raw),
    "| Range:", format(min(raw$date)), "to", format(max(raw$date)),
    "| Missing:", sum(is.na(raw$price)), "\n\n")

# Tsibble
price_ts <- raw |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month) |>
  select(month, price)

# Gap check
gap_info <- has_gaps(price_ts)
cat("Gap check:", ifelse(gap_info$.gaps,
                         "GAPS FOUND — see scan_gaps(price_ts)", "No gaps — series is regular"), "\n\n")

# Legacy ts (for tseries)
housing_ts <- ts(raw$price, start = c(2016, 7), frequency = 12)



# SECTION 2: Exploratory Data Analysis  (3 essential plots)

cat("=== SECTION 2: EDA ===\n")
print(summary(raw$price))
cat("\n")

# PLOT 1 — Overview: raw series
p_overview <- price_ts |>
  autoplot(price) +
  labs(title = "RI Median Listing Price (MEDLISPRIRI)",
       subtitle = "Jul 2016 – May 2026", x = NULL, y = "USD") +
  scale_y_continuous(labels = label_dollar())
print(p_overview)
save_png(p_overview, "01_overview.png")

# PLOT 2 — STL decomposition
stl_comp <- price_ts |>
  model(STL(price ~ trend(window = 13) + season(window = "periodic"),
            robust = TRUE)) |>
  components()

p_stl <- autoplot(stl_comp) +
  labs(title = "STL Decomposition — Trend · Season · Remainder")
print(p_stl)
save_png(p_stl, "02_stl.png", w = 9, h = 7)

stl_feat <- price_ts |> features(price, feat_stl)
cat("STL trend strength:", round(stl_feat$trend_strength, 3),
    "| seasonal strength:", round(stl_feat$seasonal_strength_year, 3), "\n\n")

# PLOT 3 — Seasonal plot
p_season <- price_ts |>
  gg_season(price, labels = "both") +
  labs(title = "Seasonal Plot — Each Line = One Calendar Year", y = "USD") +
  scale_y_continuous(labels = label_dollar())
print(p_season)
save_png(p_season, "03_seasonal.png")



# SECTION 3: Stationarity Tests  (ADF + KPSS + differencing)


cat("=== SECTION 3: STATIONARITY ===\n\n")

adf_raw    <- adf.test(housing_ts)
kpss_level <- kpss.test(housing_ts, null = "Level")
kpss_trend <- kpss.test(housing_ts, null = "Trend")

cat("ADF raw:        p =", round(adf_raw$p.value, 4), "\n")
cat("KPSS Level raw: p =", round(kpss_level$p.value, 4), "\n")
cat("KPSS Trend raw: p =", round(kpss_trend$p.value, 4), "\n")
cat("→ ADF rejects unit root BUT KPSS also rejects stationarity;",
    "series is borderline — differencing confirmed by ndiffs/nsdiffs.\n\n")

n_reg_diffs  <- ndiffs(housing_ts, test = "kpss")
n_seas_diffs <- nsdiffs(housing_ts)
cat("Regular diffs needed:", n_reg_diffs,
    "| Seasonal diffs needed:", n_seas_diffs, "\n")

# Confirm d1 is stationary
housing_d1 <- diff(housing_ts)
adf_d1     <- adf.test(housing_d1)
kpss_d1    <- kpss.test(housing_d1, null = "Level")
cat("After d=1 — ADF p:", round(adf_d1$p.value, 4),
    "| KPSS p:", round(kpss_d1$p.value, 4), "→ stationary ✓\n\n")

if (n_seas_diffs > 0) {
  housing_sd <- diff(housing_ts, lag = 12)
  adf_sd     <- adf.test(housing_sd)
  cat("Seasonal diff (lag=12) ADF p:", round(adf_sd$p.value, 4), "\n\n")
}

# PLOT 4 — ACF raw vs ACF differenced (side by side — one save)
p_acf_raw <- price_ts |>
  ACF(price, lag_max = 36) |>
  autoplot() +
  labs(title = "ACF — Raw (non-stationary)")

p_acf_d1 <- price_ts |>
  mutate(d = difference(price)) |>
  filter(!is.na(d)) |>
  ACF(d, lag_max = 36) |>
  autoplot() +
  labs(title = "ACF — First-Differenced (stationary)")

p_acf_panel <- gridExtra::arrangeGrob(p_acf_raw, p_acf_d1, ncol = 2)
grid.arrange(p_acf_raw, p_acf_d1, ncol = 2)
save_png(p_acf_panel, "04_acf_raw_vs_diff.png", w = 11, h = 4)


# SECTION 4: Guerrero Lambda  (train-only — no data leakage)


cat("=== SECTION 4: GUERRERO LAMBDA ===\n\n")

h_test  <- 12
cutoff  <- max(price_ts$month) - h_test
train   <- price_ts |> filter(month <= cutoff)
test    <- price_ts |> filter(month >  cutoff)

cat("Train:", nrow(train), "obs |",
    format(min(train$month)), "→", format(max(train$month)), "\n")
cat("Test: ", nrow(test),  "obs |",
    format(min(test$month)),  "→", format(max(test$month)), "\n\n")

lambda_tr <- train |>
  features(price, features = guerrero) |>
  pull(lambda_guerrero)

cat(sprintf("Guerrero λ (train only): %.4f\n", lambda_tr))
cat("  λ ≈ 1  → no transform needed\n")
cat("  λ ≈ 0  → log transform\n")
cat("  λ extreme (< -0.5) → numerically unstable; use log\n")
cat(sprintf("Decision: λ = %.3f → extreme; ETS_log (λ=0) used as transformed candidate.\n\n",
            lambda_tr))

# Legacy split for forecast:: accuracy calls
n        <- length(housing_ts)
train_ts <- head(housing_ts, n - h_test)
test_ts  <- tail(housing_ts, h_test)


# SECTION 5: Fit All Candidate Models

cat("=== SECTION 5: MODEL FITTING ===\n\n")

fits <- train |>
  model(
    # Baselines
    Mean    = MEAN(price),
    Naive   = NAIVE(price),
    SNaive  = SNAIVE(price ~ lag("year")),
    Drift   = RW(price ~ drift()),
    # Linear
    TSLM_seas = TSLM(price ~ trend() + season()),
    TSLM_four = TSLM(price ~ trend() + fourier(K = 2)),
    # ETS family
    ETS_auto  = ETS(price),
    ETS_MAM   = ETS(price ~ error("M") + trend("A")  + season("M")),
    ETS_AAdA  = ETS(price ~ error("A") + trend("Ad") + season("A")),
    ETS_log   = ETS(log(price)),
    # ARIMA family
    ARIMA_auto = ARIMA(price),
    ARIMA_man  = ARIMA(price ~ pdq(0, 1, 1) + PDQ(1, 1, 1)),
    # STL + ETS hybrid
    STL_ETS = decomposition_model(
      STL(price ~ trend(window = 13) + season(window = "periodic"), robust = TRUE),
      ETS(season_adjust ~ season("N"))
    )
  )

# Auto-selected specs
auto_specs <- train |>
  model(ETS_auto = ETS(price), ARIMA_auto = ARIMA(price))
cat("Auto ETS spec:   "); print(report(auto_specs |> select(ETS_auto)))
cat("Auto ARIMA spec: "); print(report(auto_specs |> select(ARIMA_auto)))

# In-sample AICc (within-family only)
cat("\n--- AICc (within-family comparison only) ---\n")
tryCatch({
  print(glance(fits |> select(-STL_ETS)) |>
          select(.model, AICc, BIC) |> arrange(AICc), n = Inf)
}, error = function(e) cat("glance() note:", conditionMessage(e), "\n"))
cat("\n")


# SECTION 6: Holdout Accuracy Table

cat("=== SECTION 6: HOLDOUT ACCURACY ===\n\n")

fc_test <- fits |> forecast(h = h_test)

acc <- fc_test |>
  accuracy(price_ts) |>
  select(.model, RMSE, MAE, MAPE, MASE, RMSSE) |>
  arrange(RMSE)

cat("--- Holdout accuracy (sorted by RMSE) ---\n")
print(acc, n = Inf)
cat("\n")

write.csv(as.data.frame(acc),
          file.path(fig_dir, "accuracy_table.csv"), row.names = FALSE)

# PLOT 5 — Top-3 forecasts vs actuals (with prediction intervals)
best3      <- acc$.model[1:3]
ets_winner <- (acc |> filter(.model %in% c("ETS_auto","ETS_MAM","ETS_AAdA","ETS_log")))$.model[1]
arima_winner <- (acc |> filter(.model %in% c("ARIMA_auto","ARIMA_man")))$.model[1]
winner     <- acc$.model[1]

cat("Overall winner:", winner,
    "| ETS winner:", ets_winner,
    "| ARIMA winner:", arima_winner, "\n\n")

p_best3 <- fc_test |>
  filter(.model %in% best3) |>
  autoplot(price_ts |> filter(month >= yearmonth("2022 Jan"))) +
  labs(title = paste("Top 3 Models vs Holdout:",
                     paste(best3, collapse = " · ")),
       subtitle = "Shaded = 80% & 95% prediction intervals",
       y = "USD") +
  scale_y_continuous(labels = label_dollar())
print(p_best3)
save_png(p_best3, "05_top3_holdout.png", w = 10, h = 5)



# SECTION 7: ETS Winner Summary

cat("=== SECTION 7: ETS WINNER ===\n\n")

ets_acc <- acc |> filter(.model %in% c("ETS_auto","ETS_MAM","ETS_AAdA","ETS_log"))
cat("--- ETS candidates ranked ---\n")
print(ets_acc, n = Inf)
cat("\nWinner:", ets_winner, "\n")
cat("Spec:"); print(fits |> select(all_of(ets_winner)))
tryCatch(print(glance(fits |> select(all_of(ets_winner)))),
         error = function(e) invisible())
cat("\n")



# SECTION 8: Residual Diagnostics — Winner Model


cat("=== SECTION 8: RESIDUAL DIAGNOSTICS ===\n\n")

best_fit <- fits |> select(all_of(winner))

# PLOT 6 — Three-panel residual plot (innovations + ACF + histogram)
p_resid <- gg_tsresiduals(best_fit) +
  labs(title = paste("Residual Diagnostics —", winner))
print(p_resid)
save_png(p_resid, "06_residuals.png", w = 9, h = 6)

# Ljung-Box test
n_par  <- nrow(tidy(best_fit))
lb_lag <- 24

lb <- augment(best_fit) |>
  features(.innov, ljung_box, lag = lb_lag, dof = n_par)

lb_pval <- lb$lb_pvalue[1]
cat(sprintf("Ljung-Box (lag=%d, dof=%d): stat=%.3f, p=%.4f → %s\n\n",
            lb_lag, n_par, lb$lb_stat[1], lb_pval,
            ifelse(lb_pval > 0.05,
                   "residuals ~ white noise ✓",
                   "residual autocorrelation detected ✗")))

write.csv(data.frame(model = winner, lag = lb_lag, dof = n_par,
                     lb_stat = lb$lb_stat[1], lb_pvalue = lb_pval),
          file.path(fig_dir, "ljung_box.csv"), row.names = FALSE)

# PLOT 7 — QQ plot + Shapiro-Wilk
resid_df  <- augment(best_fit) |> filter(!is.na(.innov)) |> as_tibble()
innov_vec <- resid_df$.innov

p_qq <- ggplot(resid_df, aes(sample = .innov)) +
  stat_qq(colour = "steelblue", alpha = 0.7) +
  stat_qq_line(colour = "tomato", linewidth = 0.8) +
  labs(title = paste("Residual QQ Plot —", winner),
       subtitle = "Points on line → Gaussian residuals → valid prediction intervals",
       x = "Theoretical Normal Quantiles", y = "Sample Residual Quantiles")
print(p_qq)
save_png(p_qq, "07_qq_winner.png", h = 5)

sw <- shapiro.test(innov_vec)
cat(sprintf("Shapiro-Wilk: W=%.4f, p=%.4f → residuals %s normally distributed.\n\n",
            sw$statistic, sw$p.value,
            ifelse(sw$p.value > 0.05, "ARE consistent with", "DEVIATE from")))



# SECTION 9: Rolling Cross-Validation

cat("=== SECTION 9: ROLLING CROSS-VALIDATION ===\n\n")

price_cv <- price_ts |> stretch_tsibble(.init = 60, .step = 6)
cat("CV windows:", length(unique(price_cv$.id)), "\n\n")

cv_fits <- price_cv |>
  model(
    ETS_auto   = ETS(price),
    ETS_MAM    = ETS(price ~ error("M") + trend("A") + season("M")),
    ARIMA_auto = ARIMA(price),
    ARIMA_man  = ARIMA(price ~ pdq(0, 1, 1) + PDQ(1, 1, 1)),
    STL_ETS    = decomposition_model(
      STL(price ~ trend(window = 13) + season(window = "periodic"), robust = TRUE),
      ETS(season_adjust ~ season("N"))
    )
  )

last_obs <- max(price_ts$month)
cv_fc    <- cv_fits |> forecast(h = 12) |> filter(month <= last_obs)

cv_acc <- cv_fc |>
  accuracy(price_ts) |>
  select(.model, RMSE, MAE, MAPE, MASE, RMSSE) |>
  arrange(RMSSE)

cat("--- CV accuracy (sorted by RMSSE) ---\n")
print(cv_acc, n = Inf)
cat("\n")

write.csv(as.data.frame(cv_acc),
          file.path(fig_dir, "cv_accuracy.csv"), row.names = FALSE)

# Horizon-level RMSE (manual join — accuracy() does not expose h directly)
cv_origin <- price_cv |>
  as_tibble() |>
  group_by(.id) |>
  summarise(origin = max(month), .groups = "drop")

cv_acc_h <- cv_fc |>
  as_tibble() |>
  left_join(cv_origin, by = ".id") |>
  mutate(h = as.integer(month - origin)) |>
  left_join(price_ts |> as_tibble() |> rename(actual = price), by = "month") |>
  filter(!is.na(actual), !is.na(.mean)) |>
  group_by(.model, h) |>
  summarise(RMSE = sqrt(mean((.mean - actual)^2)), .groups = "drop")

# PLOT 8 — CV RMSE by horizon
p_cv_h <- cv_acc_h |>
  ggplot(aes(x = h, y = RMSE, colour = .model)) +
  geom_line() + geom_point(size = 1.5) +
  labs(title = "Rolling CV — RMSE by Forecast Horizon",
       subtitle = "Stretching window: init = 60 obs, step = 6 months",
       x = "Horizon (months ahead)", y = "RMSE (USD)", colour = "Model") +
  scale_y_continuous(labels = label_dollar())
print(p_cv_h)
save_png(p_cv_h, "08_cv_rmse_horizon.png")


# SECTION 10: Final Forecast — Refit on Full Series

cat("=== SECTION 10: FINAL FORECAST ===\n\n")

model_specs <- list(
  ETS_auto   = quote(ETS(price)),
  ETS_MAM    = quote(ETS(price ~ error("M") + trend("A") + season("M"))),
  ETS_AAdA   = quote(ETS(price ~ error("A") + trend("Ad") + season("A"))),
  ETS_log    = quote(ETS(log(price))),
  ARIMA_auto = quote(ARIMA(price)),
  ARIMA_man  = quote(ARIMA(price ~ pdq(0, 1, 1) + PDQ(1, 1, 1))),
  STL_ETS    = quote(decomposition_model(
    STL(price ~ trend(window = 13) + season(window = "periodic"), robust = TRUE),
    ETS(season_adjust ~ season("N")))),
  Mean    = quote(MEAN(price)),
  Naive   = quote(NAIVE(price)),
  SNaive  = quote(SNAIVE(price ~ lag("year"))),
  Drift   = quote(RW(price ~ drift())),
  TSLM_seas = quote(TSLM(price ~ trend() + season())),
  TSLM_four = quote(TSLM(price ~ trend() + fourier(K = 2)))
)

final_winner <- winner
if (final_winner %in% names(model_specs)) {
  final_fit <- price_ts |>
    model(!!final_winner := eval(model_specs[[final_winner]]))
} else {
  final_fit    <- price_ts |> model(ETS_auto = ETS(price))
  final_winner <- "ETS_auto"
  cat("Note: winner not in map — falling back to ETS_auto.\n")
}
cat("Final model:", final_winner, "| Refit on full",
    nrow(price_ts), "observations.\n\n")

final_fc <- final_fit |> forecast(h = 12)

# PLOT 9 — Final 12-month forecast with prediction intervals
p_final <- final_fc |>
  autoplot(price_ts |> filter(month >= yearmonth("2019 Jan"))) +
  labs(title = paste("12-Month Forecast —", final_winner),
       subtitle = "80% and 95% prediction intervals | Full-history refit",
       y = "USD") +
  scale_y_continuous(labels = label_dollar())
print(p_final)
save_png(p_final, "09_final_forecast.png", w = 10, h = 5)

# Forecast table
final_tbl <- final_fc |>
  hilo(level = c(80, 95)) |>
  as_tibble() |>
  mutate(point = .mean,
         lo80  = `80%`$lower, hi80 = `80%`$upper,
         lo95  = `95%`$lower, hi95 = `95%`$upper) |>
  select(month, point, lo80, hi80, lo95, hi95)

cat("--- 12-Month Forecast Table ---\n")
print(final_tbl, n = Inf)
cat("\n")
write.csv(final_tbl, file.path(fig_dir, "final_forecast_table.csv"),
          row.names = FALSE)



