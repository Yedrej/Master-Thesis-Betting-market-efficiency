# STEP 08 — Format the table columns
# Reads:  objects left in memory by steps 05 and 07
# Writes: console output only
# Thesis: Formats estimates and intervals as they appear in the tables.
# Note:   Must run in the same session as steps 05 and 07.

# ============================================================
# CONFIDENCE INTERVALS FOR THE THESIS TABLES
#
# Reads objects already in the session and prints each table with a
# formatted 95% interval. Run order in the session:
#   1. 05_model_analysis.R      (the master script)
#   2. 07_cluster_bootstrap.R   (creates score_bootstrap, roi_bootstrap)
#   3. this file
#
# Coefficient intervals (Tables 8A, 12, 14) are analytic: estimate +/- 1.96*SE.
# Score and ROI intervals (Tables 9-11, 13, 15, 16) come from the bootstrap.
# ============================================================

library(dplyr)
z95 <- qnorm(0.975)

fmt_ci <- function(lower, upper, digits = 3) {
  sprintf("[%.*f, %.*f]", digits, lower, digits, upper)
}

# ------------------------------------------------------------
# TABLE 8, PANEL A — full-sample calibration coefficients
# ------------------------------------------------------------
# Slopes are tested against 1, so the interval is informative when it
# excludes 1 (slopes) or 0 (intercepts).
table_8A <- calibration_coefficients |>
  mutate(ci_lower = estimate - z95 * standard_error,
         ci_upper = estimate + z95 * standard_error,
         estimate_ci = sprintf("%.3f %s", estimate, fmt_ci(ci_lower, ci_upper)),
         excludes_efficient = (efficient_value < ci_lower) | (efficient_value > ci_upper)) |>
  select(term, estimate_ci, efficient_value, p_against_efficiency, excludes_efficient)

cat("\n\n===== TABLE 8, PANEL A: calibration coefficients (95% CI) =====\n")
print(table_8A, n = Inf, width = Inf)

# ------------------------------------------------------------
# TABLES 12 and 14 — final-window M_F and M_OF coefficients
# ------------------------------------------------------------
# last_model_coefficients already carries standard_error. For M_OF the
# market slopes are tested against 1, everything else against 0.
coef_ci <- last_model_coefficients |>
  mutate(ci_lower = estimate - z95 * standard_error,
         ci_upper = estimate + z95 * standard_error,
         null_value = if_else(grepl("market_log_ratio", term), 1, 0),
         estimate_ci = sprintf("%.4f %s", estimate, fmt_ci(ci_lower, ci_upper, 4)),
         excludes_null = (null_value < ci_lower) | (null_value > ci_upper))

cat("\n\n===== TABLE 12: M_F core coefficients (95% CI) =====\n")
print(coef_ci |>
        filter(model == "M_F", !grepl("Intercept|LeagueKey", term)) |>
        select(term, estimate_ci, null_value, p_value, excludes_null),
      n = Inf, width = Inf)

cat("\n\n===== TABLE 14: M_OF conditional coefficients (95% CI) =====\n")
print(coef_ci |>
        filter(model == "M_OF", !grepl("LeagueKey", term)) |>
        select(term, estimate_ci, null_value, p_value, excludes_null),
      n = Inf, width = Inf)

# ------------------------------------------------------------
# TABLES 9, 11, 15 — forecast scores with the difference interval
# ------------------------------------------------------------
# score_overall gives the levels; score_bootstrap gives the interval on
# the difference versus the relevant benchmark. This prints the levels
# and, separately, the difference intervals to place in the table notes.
cat("\n\n===== TABLES 9/11/15: forecast score levels =====\n")
print(score_overall, n = Inf, width = Inf)

cat("\n\n===== Score DIFFERENCES with 95% CI (for table notes) =====\n")
print(score_bootstrap |>
        mutate(estimate_ci = sprintf("%.6f %s", estimate,
                                     fmt_ci(ci_lower, ci_upper, 6))) |>
        select(statistic, estimate_ci, p_value),
      n = Inf, width = Inf)

# ------------------------------------------------------------
# TABLES 10, 13, 16 — pooled ROI with interval inside the cell
# ------------------------------------------------------------
# roi_bootstrap covers every price_basis x model x threshold cell.
# Formats ROI as a percentage with the interval appended, ready to drop
# into the existing ROI column.
roi_formatted <- roi_bootstrap |>
  mutate(roi_ci = sprintf("%.2f%% [%.2f, %.2f]",
                          100 * pooled_roi, 100 * ci_lower, 100 * ci_upper),
         excludes_zero = (ci_lower > 0) | (ci_upper < 0)) |>
  select(price_basis, model, threshold, bets, roi_ci,
         p_value_roi_positive, excludes_zero) |>
  arrange(price_basis, match(model, c("Market", "M_O", "M_F", "M_OF")), threshold)

cat("\n\n===== TABLE 10: odds-only economic results (95% CI) =====\n")
print(roi_formatted |> filter(model %in% c("Market", "M_O")), n = Inf, width = Inf)

cat("\n\n===== TABLE 13: fundamentals-only economic results (95% CI) =====\n")
print(roi_formatted |> filter(model == "M_F"), n = Inf, width = Inf)

cat("\n\n===== TABLE 16: combined-model economic results (95% CI) =====\n")
print(roi_formatted |> filter(model %in% c("Market", "M_O", "M_OF")), n = Inf, width = Inf)
