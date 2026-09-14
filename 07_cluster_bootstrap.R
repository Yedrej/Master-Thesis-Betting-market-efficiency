# STEP 07 — Cluster bootstrap
# Reads:  objects left in memory by step 05
# Writes: console output only
# Thesis: The 95% confidence intervals throughout Chapter 6.
# Note:   Must run in the same session as step 05.

# ============================================================
# CLUSTER BOOTSTRAP AT THE LEAGUE-SEASON LEVEL
# Uncertainty for (a) paired forecast-score differences and
# (b) pooled betting ROI, including the paired comparison
# between two strategies on their common bets.

#
# What is resampled: league-seasons, with replacement, from the
# out-of-sample period (5 leagues x 8 test seasons = 40 clusters).
# The forecasts themselves are held fixed. This captures sampling
# variation in realised results, not uncertainty in the estimated
# coefficients, and the thesis should say so explicitly.
# ============================================================
Sys.unsetenv("ANALYSIS_FEATURES_PATH")
library(dplyr)
library(tidyr)

set.seed(1234)
n_replicates <- 10000

# ------------------------------------------------------------
# 1. General Bootstrap mechanism
# ------------------------------------------------------------

make_cluster_index <- function(data) {
  paste(data$LeagueName, data$SeasonStart, sep = "_")
}

# numerator and denominator are per-cluster sums; returns the
# bootstrap distribution of numerator_total / denominator_total.
bootstrap_ratio <- function(numerator, denominator, clusters, R = n_replicates) {
  n_clusters <- length(clusters)
  draws <- matrix(sample.int(n_clusters, n_clusters * R, replace = TRUE), nrow = R)
  num <- matrix(numerator[draws], nrow = R)
  den <- matrix(denominator[draws], nrow = R)
  rowSums(num) / rowSums(den)
}

summarise_bootstrap <- function(point, replicates, label) {
  tibble::tibble(
    statistic     = label,
    estimate      = point,
    boot_se       = sd(replicates),
    ci_lower      = unname(quantile(replicates, 0.025)),
    ci_upper      = unname(quantile(replicates, 0.975)),
    # Two-sided bootstrap p-value for H0: statistic = 0, obtained by
    # recentring the replicates on zero.
    p_value       = mean(abs(replicates - point) >= abs(point))
  )
}

# ------------------------------------------------------------
# 2. Paired forecast-score differences
# ------------------------------------------------------------

score_pairs <- forecast_long |>
  select(match_id_analysis, LeagueName, SeasonStart, model, rps, log_loss) |>
  pivot_wider(names_from = model, values_from = c(rps, log_loss), names_sep = "_") |>
  mutate(cluster = paste(LeagueName, SeasonStart, sep = "_"))

sscore_comparisons <- list(
  "RPS: Market minus M_O"      = with(score_pairs, rps_Market - rps_M_O),
  "RPS: Market minus M_F"      = with(score_pairs, rps_Market - rps_M_F),
  "RPS: M_O minus M_OF"        = with(score_pairs, rps_M_O - rps_M_OF),
  "Log loss: Market minus M_O" = with(score_pairs, log_loss_Market - log_loss_M_O),
  "Log loss: Market minus M_F" = with(score_pairs, log_loss_Market - log_loss_M_F),
  "Log loss: M_O minus M_OF"   = with(score_pairs, log_loss_M_O - log_loss_M_OF)
)

cluster_ids <- unique(score_pairs$cluster)

score_bootstrap <- lapply(names(score_comparisons), function(nm) {
  d <- score_comparisons[[nm]]
  agg <- tapply(d, score_pairs$cluster, sum)[cluster_ids]
  cnt <- tapply(d, score_pairs$cluster, length)[cluster_ids]
  reps <- bootstrap_ratio(as.numeric(agg), as.numeric(cnt), cluster_ids)
  summarise_bootstrap(sum(d) / length(d), reps, nm)
}) |>
  bind_rows()

print(score_bootstrap, n = Inf)

# ------------------------------------------------------------
# 3. Pooled ROI for every strategy cell
# ------------------------------------------------------------

bets <- one_bet_model_bets |>
  mutate(cluster = paste(LeagueName, SeasonStart, sep = "_"))

cell_keys <- bets |>
  distinct(price_basis, model, threshold) |>
  arrange(price_basis, model, threshold)

cluster_ids <- sort(unique(bets$cluster))
n_clusters <- length(cluster_ids)

profit_matrix <- matrix(0, nrow = nrow(cell_keys), ncol = n_clusters)
stake_matrix  <- matrix(0, nrow = nrow(cell_keys), ncol = n_clusters)

for (i in seq_len(nrow(cell_keys))) {
  cell <- bets |>
    filter(price_basis == cell_keys$price_basis[i],
           model       == cell_keys$model[i],
           threshold   == cell_keys$threshold[i]) |>
    group_by(cluster) |>
    summarise(profit = sum(net_profit), stake = n(), .groups = "drop")
  idx <- match(cell$cluster, cluster_ids)
  profit_matrix[i, idx] <- cell$profit
  stake_matrix[i, idx]  <- cell$stake
}

draws <- matrix(sample.int(n_clusters, n_clusters * n_replicates, replace = TRUE),
                nrow = n_replicates)

roi_replicates <- matrix(NA_real_, nrow = n_replicates, ncol = nrow(cell_keys))
for (r in seq_len(n_replicates)) {
  d <- draws[r, ]
  roi_replicates[r, ] <- rowSums(profit_matrix[, d, drop = FALSE]) /
    pmax(rowSums(stake_matrix[, d, drop = FALSE]), 1)
}

roi_point <- rowSums(profit_matrix) / rowSums(stake_matrix)

roi_bootstrap <- cell_keys |>
  mutate(bets = rowSums(stake_matrix),
         pooled_roi = roi_point,
         boot_se = apply(roi_replicates, 2, sd),
         ci_lower = apply(roi_replicates, 2, quantile, 0.025),
         ci_upper = apply(roi_replicates, 2, quantile, 0.975),
         # One-sided: probability of an ROI at or below zero under the
         # bootstrap distribution recentred on the null.
         p_value_roi_positive = colMeans(
           sweep(roi_replicates, 2, roi_point, "-") >= matrix(roi_point,
                 n_replicates, nrow(cell_keys), byrow = TRUE)))

print(roi_bootstrap, n = Inf)


# ------------------------------------------------------------
# 4. Paired ROI difference on common bets
# ------------------------------------------------------------
# Comparing two strategies by their separate pooled ROIs is not a
# paired test, because they place different bets. This compares
# M_O and M_OF on the match-outcomes both select, which is the
# comparison Section 6.4.3 makes informally.

paired_roi <- function(model_a, model_b, basis, tau) {
  a <- bets |> filter(model == model_a, price_basis == basis, threshold == tau)
  b <- bets |> filter(model == model_b, price_basis == basis, threshold == tau)
  common <- inner_join(
    a |> select(match_id_analysis, cluster, outcome, profit_a = net_profit),
    b |> select(match_id_analysis, outcome, profit_b = net_profit),
    by = c("match_id_analysis", "outcome"))
  if (!nrow(common)) return(NULL)

  d <- common$profit_a - common$profit_b
  ids <- sort(unique(common$cluster))
  agg <- as.numeric(tapply(d, common$cluster, sum)[ids])
  cnt <- as.numeric(tapply(d, common$cluster, length)[ids])
  agg[is.na(agg)] <- 0; cnt[is.na(cnt)] <- 0
  reps <- bootstrap_ratio(agg, cnt, ids)
  summarise_bootstrap(sum(d) / length(d), reps,
                      paste0(model_a, " minus ", model_b, " on common bets (",
                             basis, ", tau = ", tau, ", n = ", nrow(common), ")"))
}

paired_results <- bind_rows(
  paired_roi("M_OF", "M_O", "Maximum odds", 0),
  paired_roi("M_OF", "M_O", "Maximum odds", 0.05),
  paired_roi("M_O", "Market", "Maximum odds", 0),
  paired_roi("M_O", "Market", "Maximum odds", 0.025)
)

print(paired_results, n = Inf)

# ------------------------------------------------------------
# 5. Reality check across the strategy grid
# ------------------------------------------------------------
# The maximum-price panels report roughly twenty strategy cells.
# The best of twenty is positive some of the time even when no cell
# has an edge, so the relevant question is whether the best observed
# ROI exceeds what the best of twenty would produce under the null.

max_price <- which(cell_keys$price_basis == "Maximum odds")
se_cells <- apply(roi_replicates[, max_price, drop = FALSE], 2, sd)
t_observed <- roi_point[max_price] / se_cells
t_null <- sweep(roi_replicates[, max_price, drop = FALSE], 2, roi_point[max_price], "-")
t_null <- sweep(t_null, 2, se_cells, "/")

reality_check <- tibble::tibble(
  best_cell        = paste(cell_keys$model[max_price], cell_keys$threshold[max_price])[which.max(t_observed)],
  max_t_observed   = max(t_observed),
  reality_check_p  = mean(apply(t_null, 1, max) >= max(t_observed))
)

print(reality_check)

