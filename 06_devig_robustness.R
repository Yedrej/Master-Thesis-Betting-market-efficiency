# STEP 06 — De-vigging robustness
# Reads:  analysis_features.csv; re-runs step 05 internally
# Writes: console output only
# Thesis: Section 6.5. Rebuilds the market probabilities under the Shin
#         and power methods and repeats the analysis under each.

# ============================================================
# DE-VIGGING ROBUSTNESS
# Rebuilds mkt_pre_p_home/draw/away under alternative margin-removal
# methods and re-runs the master analysis script for each one.
# ============================================================

library(dplyr)
project_dir   <- Sys.getenv("THESIS_PROJECT_DIR", unset = ".")
master_path <- "05_model_analysis.R"
features_path <- file.path(project_dir, "analysis_test", "data", "analysis_features.csv")
master_path   <- "05_model_analysis.R"
output_dir    <- tempdir()

# ------------------------------------------------------------
# 1. Margin-removal methods
# ------------------------------------------------------------
# Each takes a matrix of raw implied probabilities pi_ij = 1/O_ij
# (rows = matches, columns = H, D, A) and returns probabilities
# summing to one across each row.

devig_proportional <- function(pi_mat) {
  # Baseline used in the thesis. Margin is removed in proportion to
  # each implied probability, so pi_j / p_j = B for every outcome.
  pi_mat / rowSums(pi_mat)
}

devig_additive <- function(pi_mat) {
  # Equal absolute share of the margin removed from every outcome.
  excess <- (rowSums(pi_mat) - 1) / ncol(pi_mat)
  out <- pi_mat - excess
  out[out < 1e-6] <- 1e-6
  out / rowSums(out)
}

devig_power <- function(pi_mat) {
  # Logarithmic / power method: find k such that sum(pi_j^k) = 1.
  # k > 1 whenever the book is overround, which shrinks small
  # probabilities more than large ones in relative terms.
  k <- vapply(seq_len(nrow(pi_mat)), function(i) {
    p <- pi_mat[i, ]
    if (!all(is.finite(p)) || any(p <= 0) || sum(p) <= 1) return(1)
    f <- function(k) sum(p^k) - 1
    uniroot(f, c(1, 25), tol = 1e-10)$root
  }, numeric(1))
  out <- pi_mat^matrix(k, nrow(pi_mat), ncol(pi_mat))
  out / rowSums(out)
}

devig_shin <- function(pi_mat) {
  # Shin (1993). z is the implied proportion of insider money; the
  # margin loading it produces is larger on longshots than on
  # favourites, which is the standard structural alternative to
  # proportional normalisation.
  B <- rowSums(pi_mat)
  z <- vapply(seq_len(nrow(pi_mat)), function(i) {
    p <- pi_mat[i, ]; b <- B[i]
    if (!all(is.finite(p)) || any(p <= 0) || b <= 1) return(0)
    f <- function(z) sum((sqrt(z^2 + 4 * (1 - z) * p^2 / b) - z) / (2 * (1 - z))) - 1
    root <- tryCatch(uniroot(f, c(1e-10, 0.9), tol = 1e-12)$root,
                     error = function(e) NA_real_)
    if (is.na(root)) 0 else root
  }, numeric(1))
  zm <- matrix(z, nrow(pi_mat), ncol(pi_mat))
  Bm <- matrix(B, nrow(pi_mat), ncol(pi_mat))
  out <- (sqrt(zm^2 + 4 * (1 - zm) * pi_mat^2 / Bm) - zm) / (2 * (1 - zm))
  out / rowSums(out)
}

devig_odds_ratio <- function(pi_mat) {
  # Cheung's odds-ratio method: a single odds ratio c links the quoted
  # and fair odds of every outcome.
  cc <- vapply(seq_len(nrow(pi_mat)), function(i) {
    p <- pi_mat[i, ]
    if (!all(is.finite(p)) || any(p <= 0) || sum(p) <= 1) return(1)
    f <- function(cc) sum(p / (cc + p - cc * p)) - 1
    uniroot(f, c(1, 100), tol = 1e-10)$root
  }, numeric(1))
  cm <- matrix(cc, nrow(pi_mat), ncol(pi_mat))
  out <- pi_mat / (cm + pi_mat - cm * pi_mat)
  out / rowSums(out)
}

devig_methods <- list(
  proportional = devig_proportional,
  shin         = devig_shin,
  power        = devig_power
)

# ------------------------------------------------------------
# 2. Diagnostic: how each method allocates the margin
# ------------------------------------------------------------
# This is the check that explains any difference in the results.
#Under proportional de-vigging the loading pi_j / p_j is identical
# for every outcome by construction; the alternatives place more of
# the margin on longshots. If the Table 8 slopes above one are an
# artefact of the transformation rather than a market property, it
# will show up here first.

features <- read.csv(features_path, check.names = FALSE)

pi_mat <- with(features, cbind(H = 1 / odds_avg_pre_home,
                               D = 1 / odds_avg_pre_draw,
                               A = 1 / odds_avg_pre_away))
usable <- apply(pi_mat, 1, function(r) all(is.finite(r) & r > 0)) & rowSums(pi_mat) > 1

margin_loading <- lapply(names(devig_methods), function(m) {
  p <- devig_methods[[m]](pi_mat[usable, , drop = FALSE])
  tibble::tibble(
    method  = m,
    p_fair  = as.vector(p),
    loading = as.vector(pi_mat[usable, ] / p) - 1
  )
}) |>
  bind_rows() |>
  mutate(probability_band = cut(p_fair, breaks = c(0, .05, .10, .20, .30, .50, .70, 1),
                                include.lowest = TRUE)) |>
  group_by(method, probability_band) |>
  summarise(observations = n(),
            mean_loading_pct = 100 * mean(loading), .groups = "drop") |>
  tidyr::pivot_wider(names_from = method, values_from = mean_loading_pct,
                     id_cols = c(probability_band, observations))

print(margin_loading, n = Inf)

# ------------------------------------------------------------
# 3. Re-run the full analysis under each method
# ------------------------------------------------------------
# The master script reads its input path from ANALYSIS_FEATURES_PATH,
# so each method only needs a rewritten CSV and a fresh environment.
# Expect this to take as long as the master script times five.

run_master_with <- function(method_name) {
  p <- matrix(NA_real_, nrow(features), 3,
              dimnames = list(NULL, c("H", "D", "A")))
  p[usable, ] <- devig_methods[[method_name]](pi_mat[usable, , drop = FALSE])

  variant <- features
  variant$mkt_pre_p_home <- p[, "H"]
  variant$mkt_pre_p_draw <- p[, "D"]
  variant$mkt_pre_p_away <- p[, "A"]
  # Overround and margin are properties of the quoted odds, not of the
  # transformation, so they are deliberately left unchanged.

  num_cols <- vapply(variant, is.numeric, logical(1))
  variant[num_cols] <- lapply(variant[num_cols], function(x) {
    out <- sprintf("%.17g", x)
    out[is.na(x)] <- NA_character_
    out
  })
  path <- file.path(output_dir, paste0("analysis_features_", method_name, ".csv"))
  write.csv(variant, path, row.names = FALSE)
  
  Sys.setenv(ANALYSIS_FEATURES_PATH = path)
  env <- new.env()
  invisible(capture.output(source(master_path, local = env)))

  list(
    method        = method_name,
    calibration   = get("calibration_coefficients", envir = env),
    joint_test    = get("joint_calibration_test", envir = env),
    scores        = get("score_overall", envir = env),
    economics     = get("one_bet_overall", envir = env),
    stability     = get("one_bet_stability", envir = env)
  )
}

devig_runs <- lapply(names(devig_methods), run_master_with)
names(devig_runs) <- names(devig_methods)

# ------------------------------------------------------------
# 4. Comparison tables for the robustness section
# ------------------------------------------------------------

devig_calibration_table <- lapply(devig_runs, function(r) {
  r$calibration |> mutate(method = r$method)
}) |>
  bind_rows() |>
  select(method, term, estimate, standard_error,
         efficient_value, p_against_efficiency) |>
  arrange(term, method)

devig_joint_test_table <- lapply(devig_runs, function(r) {
  r$joint_test |> mutate(method = r$method)
}) |>
  bind_rows() |>
  select(method, observations, LR, df, p_value)

devig_score_table <- lapply(devig_runs, function(r) {
  r$scores |> mutate(method = r$method)
}) |>
  bind_rows() |>
  select(method, model, matches, mean_rps, mean_log_loss, accuracy) |>
  arrange(model, method)

devig_economic_table <- lapply(devig_runs, function(r) {
  r$stability |> mutate(method = r$method)
}) |>
  bind_rows() |>
  select(method, price_basis, model, threshold, total_bets,
         pooled_roi, median_active_season_roi, profitable_seasons) |>
  arrange(price_basis, model, threshold, method)

print(devig_joint_test_table, n = Inf)
print(devig_calibration_table, n = Inf)
print(devig_score_table, n = Inf)
print(devig_economic_table, n = Inf)

table(
  valid_odds = apply(
    pi_mat,
    1,
    function(r) all(is.finite(r) & r > 0)
  ),
  overround = rowSums(pi_mat) > 1,
  useNA = "ifany"
)

print(devig_joint_test_table, n = Inf)

if (is.na(root)) 0 else root
usable <- apply(
  pi_mat,
  1,
  function(r) all(is.finite(r) & r > 0)
)



if (!requireNamespace("implied", quietly = TRUE)) {
  install.packages("implied")
}

set.seed(123)
check_rows <- sample(
  which(usable),
  min(1000, sum(usable))
)

check_odds <- 1 / pi_mat[check_rows, , drop = FALSE]

shin_package <- implied::implied_probabilities(
  check_odds,
  method = "shin"
)$probabilities

power_package <- implied::implied_probabilities(
  check_odds,
  method = "power"
)$probabilities

shin_custom <- devig_shin(
  pi_mat[check_rows, , drop = FALSE]
)

power_custom <- devig_power(
  pi_mat[check_rows, , drop = FALSE]
)

tibble(
  method = c("Shin", "Power"),
  mean_absolute_difference = c(
    mean(abs(shin_custom - shin_package)),
    mean(abs(power_custom - power_package))
  ),
  maximum_absolute_difference = c(
    max(abs(shin_custom - shin_package)),
    max(abs(power_custom - power_package))
  )
)

Sys.unsetenv("ANALYSIS_FEATURES_PATH")
