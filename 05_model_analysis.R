# STEP 05 — Main analysis
# Reads:  analysis_features.csv
# Writes: console output only
# Thesis: Sections 6.2 to 6.4. Calibration tests, the three models, the
#         walk-forward forecasts and scores, betting results, and the
#         four-season and single-bookmaker robustness checks.
# Note:   Leave the session open; steps 07 and 08 read its objects.

# ============================================================
# THESIS MODEL ANALYSIS — CLEAN MASTER SCRIPT
# M_O, M_F and M_OF: calibration, walk-forward forecasts and betting tests
# ============================================================

### 1. Packages, data and settings ----------------------------------------
packages <- c("dplyr", "tidyr", "ggplot2", "tibble")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install first: ", paste(missing_packages, collapse = ", "))
invisible(lapply(packages, library, character.only = TRUE))
options(tibble.print_max = Inf, tibble.width = Inf, pillar.sigfig = 6)

data_path <- Sys.getenv(
  "ANALYSIS_FEATURES_PATH",
  unset = file.path(Sys.getenv("THESIS_PROJECT_DIR", unset = "."),
                    "analysis_test", "data", "analysis_features.csv")
)
analysis_features <- read.csv(data_path, check.names = FALSE) |>
  mutate(MatchDate = as.Date(MatchDate), SeasonStart = as.integer(SeasonStart))

initial_train_seasons <- 3L
ev_thresholds <- c(0, 0.025, 0.05, 0.075, 0.10)
calibration_bin_width <- 0.05
fixed_normal_market_return <- -0.046
outcomes <- c("H", "D", "A")
model_order <- c("Market", "M_O", "M_F", "M_OF")

continuous_features <- c(
  "pi_rating_diff_pre",
  "matchup_xg_for_5",
  "matchup_xg_against_5",
  "matchup_ppg_5")

binary_features <- c("home_promoted", "away_promoted", "early_season_lt5")
fundamental_features <- c(continuous_features, binary_features)
required_columns <- c(
  "match_id_analysis", "LeagueKey", "LeagueName", "SeasonStart", "SeasonLabel",
  "MatchDate", "HomeTeam", "AwayTeam", "FTR", fundamental_features,
  "mkt_pre_p_home", "mkt_pre_p_draw", "mkt_pre_p_away", "mkt_pre_margin",
  "odds_avg_pre_home", "odds_avg_pre_draw", "odds_avg_pre_away",
  "odds_max_pre_home", "odds_max_pre_draw", "odds_max_pre_away"
)
missing_columns <- setdiff(required_columns, names(analysis_features))
if (length(missing_columns)) stop("Missing columns: ", paste(missing_columns, collapse = ", "))
if (anyDuplicated(analysis_features$match_id_analysis)) stop("match_id_analysis is not unique.")

league_levels <- sort(unique(analysis_features$LeagueKey))
analysis_features <- analysis_features |>
  mutate(LeagueKey = factor(LeagueKey, levels = league_levels), FTR = as.character(FTR)) |>
  filter(FTR %in% outcomes) |>
  arrange(SeasonStart, MatchDate, match_id_analysis)

show_table <- function(title, x) {
  cat("\n\n==================== ", title, " ====================\n", sep = "")
  print(tibble::as_tibble(x), n = Inf, width = Inf)
  invisible(x)
}

### 2. Model helper functions ---------------------------------------------
clip_probability <- function(x, eps = 1e-12) pmin(pmax(x, eps), 1 - eps)

softmax_hda <- function(eta_home, eta_away) {
  anchor <- pmax(0, eta_home, eta_away)
  denominator <- exp(-anchor) + exp(eta_home - anchor) + exp(eta_away - anchor)
  cbind(H = exp(eta_home - anchor) / denominator,
        D = exp(-anchor) / denominator,
        A = exp(eta_away - anchor) / denominator)
}

# One multinomial model: H relative to D and A relative to D.
fit_mnl <- function(y, x_home, x_away, start = NULL) {
  y_home <- as.numeric(y == "H"); y_away <- as.numeric(y == "A")
  k_home <- ncol(x_home); k_away <- ncol(x_away)
  if (is.null(start)) start <- rep(0, k_home + k_away)

  objective <- function(theta) {
    eta_home <- drop(x_home %*% theta[seq_len(k_home)])
    eta_away <- drop(x_away %*% theta[k_home + seq_len(k_away)])
    p <- softmax_hda(eta_home, eta_away)
    -sum(as.numeric(y == "H") * log(clip_probability(p[, "H"])) +
           as.numeric(y == "D") * log(clip_probability(p[, "D"])) +
           as.numeric(y == "A") * log(clip_probability(p[, "A"])))
  }

  gradient <- function(theta) {
    eta_home <- drop(x_home %*% theta[seq_len(k_home)])
    eta_away <- drop(x_away %*% theta[k_home + seq_len(k_away)])
    p <- softmax_hda(eta_home, eta_away)
    -c(drop(crossprod(x_home, y_home - p[, "H"])),
       drop(crossprod(x_away, y_away - p[, "A"])))
  }

  result <- optim(start, objective, gradient, method = "BFGS", hessian = TRUE,
                  control = list(maxit = 1000, reltol = 1e-10))
  coefficient_names <- c(paste0("H:", colnames(x_home)),
                         paste0("A:", colnames(x_away)))
  names(result$par) <- coefficient_names
  vcov_matrix <- tryCatch(solve(result$hessian), error = function(e)
    matrix(NA_real_, length(result$par), length(result$par)))
  dimnames(vcov_matrix) <- list(coefficient_names, coefficient_names)
  fit <- list(coefficients = result$par, vcov = vcov_matrix,
              logLik = -result$value, convergence = result$convergence,
              counts = result$counts, k_home = k_home,
              x_home_names = colnames(x_home), x_away_names = colnames(x_away))
  class(fit) <- "thesis_mnl"
  fit
}

predict_mnl <- function(model, x_home, x_away) {
  k <- model$k_home
  softmax_hda(drop(x_home %*% model$coefficients[seq_len(k)]),
              drop(x_away %*% model$coefficients[k + seq_len(ncol(x_away))]))
}

tidy_mnl <- function(fit, model_name = NA_character_) {
  if (!is.list(fit) || is.null(fit$coefficients)) stop("Not a thesis_mnl fit.")
  estimates <- fit$coefficients
  standard_error <- sqrt(diag(fit$vcov))
  tibble(model = model_name, term = names(estimates), estimate = unname(estimates),
         standard_error, z = estimate / standard_error,
         p_value = 2 * pnorm(abs(z), lower.tail = FALSE))
}

prepare_fundamentals <- function(train, test) {
  medians <- vapply(train[fundamental_features], function(x) median(x, na.rm = TRUE), numeric(1))
  if (any(!is.finite(medians))) stop("A fundamental is entirely missing in a training window.")
  impute <- function(data) {
    for (v in fundamental_features) data[[v]][!is.finite(data[[v]])] <- medians[[v]]
    data
  }
  train <- impute(train); test <- impute(test)
  centres <- vapply(train[continuous_features], mean, numeric(1))
  scales <- vapply(train[continuous_features], sd, numeric(1))
  scales[!is.finite(scales) | scales == 0] <- 1
  for (v in continuous_features) {
    train[[v]] <- (train[[v]] - centres[[v]]) / scales[[v]]
    test[[v]] <- (test[[v]] - centres[[v]]) / scales[[v]]
  }
  list(train = train, test = test, medians = medians, centres = centres, scales = scales)
}

fundamental_formula <- reformulate(c(fundamental_features, "LeagueKey"))
make_odds_designs <- function(data) {
  log_hd <- log(clip_probability(data$mkt_pre_p_home) /
                  clip_probability(data$mkt_pre_p_draw))
  log_ad <- log(clip_probability(data$mkt_pre_p_away) /
                  clip_probability(data$mkt_pre_p_draw))
  list(O_H = cbind(`(Intercept)` = 1, market_log_ratio = log_hd),
       O_A = cbind(`(Intercept)` = 1, market_log_ratio = log_ad))
}

make_designs <- function(data) {
  fundamental_matrix <- model.matrix(fundamental_formula, data)
  fundamental_terms <- fundamental_matrix[, -1, drop = FALSE]
  odds_design <- make_odds_designs(data)
  list(O_H = odds_design$O_H, O_A = odds_design$O_A,
       F_H = fundamental_matrix, F_A = fundamental_matrix,
       OF_H = cbind(odds_design$O_H, fundamental_terms),
       OF_A = cbind(odds_design$O_A, fundamental_terms))
}

### 3. Full-sample opening-market calibration -----------------------------
calibration_sample <- analysis_features |>
  filter(if_all(c(mkt_pre_p_home, mkt_pre_p_draw, mkt_pre_p_away),
                ~ is.finite(.x) & .x > 0))
cal_design <- make_odds_designs(calibration_sample)
cal_fit <- fit_mnl(calibration_sample$FTR, cal_design$O_H, cal_design$O_A,
                   start = c(0, 1, 0, 1))

null_probability <- case_when(
  calibration_sample$FTR == "H" ~ calibration_sample$mkt_pre_p_home,
  calibration_sample$FTR == "D" ~ calibration_sample$mkt_pre_p_draw,
  TRUE ~ calibration_sample$mkt_pre_p_away
)
joint_calibration_test <- tibble(
  observations = nrow(calibration_sample),
  logLik_market = sum(log(clip_probability(null_probability))),
  logLik_recalibrated = cal_fit$logLik,
  LR = 2 * (logLik_recalibrated - logLik_market),
  df = length(cal_fit$coefficients),
  p_value = pchisq(LR, df, lower.tail = FALSE),
  convergence = cal_fit$convergence
)

# These p-values test intercept = 0 and slope = 1
calibration_coefficients <- tidy_mnl(cal_fit, "M_O full sample") |>
  mutate(efficient_value = if_else(grepl("Intercept", term, fixed = TRUE), 0, 1),
         z_against_efficiency = (estimate - efficient_value) / standard_error,
         p_against_efficiency = 2 * pnorm(abs(z_against_efficiency), lower.tail = FALSE)) |>
  select(model, term, estimate, standard_error, efficient_value,
         z_against_efficiency, p_against_efficiency)

binary_calibration <- function(outcome, probability) {
  binary_data <- tibble(y = as.integer(calibration_sample$FTR == outcome),
                        market_logit = qlogis(clip_probability(probability)))
  fit <- glm(y ~ market_logit, data = binary_data, family = binomial())
  difference <- c(coef(fit)[1], coef(fit)[2] - 1)
  statistic <- drop(t(difference) %*% solve(vcov(fit)) %*% difference)
  intercept <- unname(coef(fit)[1]); slope <- unname(coef(fit)[2])
  tibble(outcome, intercept, slope,
         crossover_probability = if_else(abs(slope - 1) > 1e-10,
                                          plogis(-intercept / (slope - 1)), NA_real_),
         joint_chi2 = statistic, df = 2L,
         p_value = pchisq(statistic, 2, lower.tail = FALSE))
}

outcome_calibration_tests <- bind_rows(
  binary_calibration("H", calibration_sample$mkt_pre_p_home),
  binary_calibration("D", calibration_sample$mkt_pre_p_draw),
  binary_calibration("A", calibration_sample$mkt_pre_p_away)
)

### 4. Five-percentage-point calibration diagnostics ---------------------
market_probability_long <- analysis_features |>
  select(match_id_analysis, LeagueName, SeasonStart, SeasonLabel, FTR,
         mkt_pre_p_home, mkt_pre_p_draw, mkt_pre_p_away) |>
  pivot_longer(starts_with("mkt_pre_p_"), names_to = "probability_name",
               values_to = "market_probability") |>
  mutate(outcome = recode(probability_name, mkt_pre_p_home = "H",
                          mkt_pre_p_draw = "D", mkt_pre_p_away = "A"),
         observed = as.integer(FTR == outcome),
         bin_lower = pmin(floor(market_probability / calibration_bin_width) *
                            calibration_bin_width, 1 - calibration_bin_width),
         bin_upper = bin_lower + calibration_bin_width,
         probability_bin = sprintf("%.0f--%.0f%%", 100 * bin_lower, 100 * bin_upper)) |>
  filter(is.finite(market_probability), between(market_probability, 0, 1))

market_calibration_bins_by_outcome <- market_probability_long |>
  group_by(outcome, bin_lower, bin_upper, probability_bin) |>
  summarise(observations = n(), mean_market_probability = mean(market_probability),
            realised_frequency = mean(observed),
            calibration_error = realised_frequency - mean_market_probability,
            absolute_calibration_error = abs(calibration_error), .groups = "drop") |>
  mutate(outcome = factor(outcome, levels = outcomes)) |>
  arrange(outcome, bin_lower)

calibration_bins_core <- market_calibration_bins_by_outcome |>
  filter(observations >= 100) |>
  transmute(outcome, probability_bin, observations,
            market_probability = 100 * mean_market_probability,
            realised_frequency_pct = 100 * realised_frequency,
            calibration_error_pp = 100 * calibration_error)

largest_calibration_gaps <- calibration_bins_core |>
  group_by(outcome) |>
  slice_max(abs(calibration_error_pp), n = 3, with_ties = FALSE) |>
  ungroup() |>
  arrange(outcome, desc(abs(calibration_error_pp)))

### 5. Expanding-window estimation of M_O, M_F and M_OF -------------------
valid_market <- with(analysis_features,
  is.finite(mkt_pre_p_home) & is.finite(mkt_pre_p_draw) &
    is.finite(mkt_pre_p_away) & mkt_pre_p_home > 0 &
    mkt_pre_p_draw > 0 & mkt_pre_p_away > 0)
model_data <- analysis_features[valid_market, ]
all_seasons <- sort(unique(model_data$SeasonStart))
test_seasons <- all_seasons[(initial_train_seasons + 1L):length(all_seasons)]
if (!length(test_seasons)) stop("Not enough seasons for the requested walk-forward design.")

forecast_list <- fit_log_list <- vector("list", length(test_seasons))
for (s in seq_along(test_seasons)) {
  test_season <- test_seasons[s]
  raw_train <- filter(model_data, SeasonStart < test_season)
  raw_test <- filter(model_data, SeasonStart == test_season)
  prepared <- prepare_fundamentals(raw_train, raw_test)
  train <- prepared$train; test <- prepared$test
  x_train <- make_designs(train); x_test <- make_designs(test)

  fit_O <- fit_mnl(train$FTR, x_train$O_H, x_train$O_A, start = c(0, 1, 0, 1))
  fit_F <- fit_mnl(train$FTR, x_train$F_H, x_train$F_A)
  of_start <- c(0, 1, rep(0, ncol(x_train$OF_H) - 2),
                0, 1, rep(0, ncol(x_train$OF_A) - 2))
  fit_OF <- fit_mnl(train$FTR, x_train$OF_H, x_train$OF_A, start = of_start)
  convergence <- c(M_O = fit_O$convergence, M_F = fit_F$convergence,
                   M_OF = fit_OF$convergence)
  if (any(convergence != 0)) warning("Non-zero convergence code in test season ", test_season)

  p_O <- predict_mnl(fit_O, x_test$O_H, x_test$O_A)
  p_F <- predict_mnl(fit_F, x_test$F_H, x_test$F_A)
  p_OF <- predict_mnl(fit_OF, x_test$OF_H, x_test$OF_A)
  forecast_list[[s]] <- test |>
    transmute(
      match_id_analysis, LeagueKey, LeagueName, SeasonStart, SeasonLabel,
      MatchDate, HomeTeam, AwayTeam, FTR, market_margin = mkt_pre_margin,
      odds_avg_home = odds_avg_pre_home, odds_avg_draw = odds_avg_pre_draw,
      odds_avg_away = odds_avg_pre_away, odds_max_home = odds_max_pre_home,
      odds_max_draw = odds_max_pre_draw, odds_max_away = odds_max_pre_away,
      market_home = mkt_pre_p_home, market_draw = mkt_pre_p_draw,
      market_away = mkt_pre_p_away,
      MO_home = p_O[, "H"], MO_draw = p_O[, "D"], MO_away = p_O[, "A"],
      MF_home = p_F[, "H"], MF_draw = p_F[, "D"], MF_away = p_F[, "A"],
      MOF_home = p_OF[, "H"], MOF_draw = p_OF[, "D"], MOF_away = p_OF[, "A"]
    )
  fit_log_list[[s]] <- tibble(
    test_season, training_matches = nrow(train), test_matches = nrow(test),
    M_O_logLik = fit_O$logLik, M_F_logLik = fit_F$logLik, M_OF_logLik = fit_OF$logLik,
    M_O_convergence = convergence["M_O"], M_F_convergence = convergence["M_F"],
    M_OF_convergence = convergence["M_OF"],
    LR_fundamentals_given_odds = 2 * (fit_OF$logLik - fit_O$logLik),
    LR_df = length(fit_OF$coefficients) - length(fit_O$coefficients),
    LR_p_value = pchisq(LR_fundamentals_given_odds, LR_df, lower.tail = FALSE)
  )
  last_models <- list(M_O = fit_O, M_F = fit_F, M_OF = fit_OF)
}

oos_forecasts <- bind_rows(forecast_list)
fit_log <- bind_rows(fit_log_list)
last_model_coefficients <- bind_rows(
  tidy_mnl(last_models$M_O, "M_O"),
  tidy_mnl(last_models$M_F, "M_F"),
  tidy_mnl(last_models$M_OF, "M_OF")
)
training_likelihood_tests <- fit_log |>
  select(test_season, training_matches, test_matches,
         LR_fundamentals_given_odds, LR_df, LR_p_value)

### 6. Statistical out-of-sample evaluation ------------------------------
metadata_columns <- c(
  "match_id_analysis", "LeagueKey", "LeagueName", "SeasonStart", "SeasonLabel",
  "MatchDate", "HomeTeam", "AwayTeam", "FTR", "market_margin",
  "odds_avg_home", "odds_avg_draw", "odds_avg_away",
  "odds_max_home", "odds_max_draw", "odds_max_away"
)
forecast_long <- bind_rows(
  oos_forecasts |> transmute(across(all_of(metadata_columns)), model = "Market",
                             p_home = market_home, p_draw = market_draw, p_away = market_away),
  oos_forecasts |> transmute(across(all_of(metadata_columns)), model = "M_O",
                             p_home = MO_home, p_draw = MO_draw, p_away = MO_away),
  oos_forecasts |> transmute(across(all_of(metadata_columns)), model = "M_F",
                             p_home = MF_home, p_draw = MF_draw, p_away = MF_away),
  oos_forecasts |> transmute(across(all_of(metadata_columns)), model = "M_OF",
                             p_home = MOF_home, p_draw = MOF_draw, p_away = MOF_away)
) |>
  mutate(y_home = as.numeric(FTR == "H"), y_draw = as.numeric(FTR == "D"),
         rps = ((p_home - y_home)^2 +
                  ((p_home + p_draw) - (y_home + y_draw))^2) / 2,
         actual_probability = case_when(FTR == "H" ~ p_home, FTR == "D" ~ p_draw,
                                        TRUE ~ p_away),
         log_loss = -log(clip_probability(actual_probability)),
         predicted = outcomes[max.col(cbind(p_home, p_draw, p_away), ties.method = "first")],
         correct = predicted == FTR)

score_overall <- forecast_long |>
  group_by(model) |>
  summarise(matches = n(), mean_rps = mean(rps), mean_log_loss = mean(log_loss),
            accuracy = mean(correct), .groups = "drop") |>
  arrange(match(model, model_order))

score_by_season <- forecast_long |>
  group_by(SeasonStart, SeasonLabel, model) |>
  summarise(matches = n(), mean_rps = mean(rps), mean_log_loss = mean(log_loss),
            accuracy = mean(correct), .groups = "drop")

score_by_league <- forecast_long |>
  group_by(LeagueName, model) |>
  summarise(matches = n(), mean_rps = mean(rps), mean_log_loss = mean(log_loss),
            accuracy = mean(correct), .groups = "drop")

score_wide <- forecast_long |>
  select(match_id_analysis, SeasonStart, SeasonLabel, LeagueName, model, rps, log_loss) |>
  pivot_wider(names_from = model, values_from = c(rps, log_loss), names_sep = "_")

comparison_table <- bind_rows(
  transmute(score_wide, comparison = "Market minus M_O",
            d_rps = rps_Market - rps_M_O,
            d_log_loss = log_loss_Market - log_loss_M_O),
  transmute(score_wide, comparison = "Market minus M_F",
            d_rps = rps_Market - rps_M_F,
            d_log_loss = log_loss_Market - log_loss_M_F),
  transmute(score_wide, comparison = "M_O minus M_OF",
            d_rps = rps_M_O - rps_M_OF,
            d_log_loss = log_loss_M_O - log_loss_M_OF)
) |>
  group_by(comparison) |>
  summarise(matches = n(), mean_rps_improvement = mean(d_rps),
            mean_log_loss_improvement = mean(d_log_loss), .groups = "drop")

score_season_comparison <- score_by_season |>
  select(SeasonStart, SeasonLabel, model, mean_rps, mean_log_loss, accuracy) |>
  pivot_wider(names_from = model, values_from = c(mean_rps, mean_log_loss, accuracy)) |>
  mutate(rps_gain_recalibration = mean_rps_Market - mean_rps_M_O,
         rps_gain_fundamentals_only = mean_rps_Market - mean_rps_M_F,
         rps_gain_fundamentals_given_odds = mean_rps_M_O - mean_rps_M_OF)

score_league_comparison <- score_by_league |>
  select(LeagueName, model, mean_rps, mean_log_loss, accuracy) |>
  pivot_wider(names_from = model, values_from = c(mean_rps, mean_log_loss, accuracy)) |>
  mutate(rps_gain_recalibration = mean_rps_Market - mean_rps_M_O,
         rps_gain_fundamentals_only = mean_rps_Market - mean_rps_M_F,
         rps_gain_fundamentals_given_odds = mean_rps_M_O - mean_rps_M_OF)

plot_oos_rps <- ggplot(score_by_season, aes(SeasonStart, mean_rps, colour = model)) +
  geom_line(linewidth = 0.8) + geom_point() +
  labs(x = NULL, y = "Mean RPS", colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

### 7. Economic evaluation: one bet per match -----------------------------
one_bet_base <- forecast_long |>
  select(match_id_analysis, LeagueName, SeasonStart, SeasonLabel, MatchDate, FTR,
         model, market_margin, p_home, p_draw, p_away,
         starts_with("odds_avg_"), starts_with("odds_max_")) |>
  pivot_longer(c(p_home, p_draw, p_away), names_to = "probability_name",
               values_to = "model_probability") |>
  mutate(outcome = recode(probability_name, p_home = "H", p_draw = "D", p_away = "A"),
         average_odds = case_when(outcome == "H" ~ odds_avg_home,
                                  outcome == "D" ~ odds_avg_draw,
                                  TRUE ~ odds_avg_away),
         maximum_odds = case_when(outcome == "H" ~ odds_max_home,
                                  outcome == "D" ~ odds_max_draw,
                                  TRUE ~ odds_max_away)) |>
  select(-probability_name, -starts_with("odds_avg_"), -starts_with("odds_max_"))

one_bet_opportunities <- bind_rows(
  one_bet_base |> transmute(across(-c(average_odds, maximum_odds)),
                            price_basis = "Average odds", odds = average_odds),
  one_bet_base |> transmute(across(-c(average_odds, maximum_odds)),
                            price_basis = "Maximum odds", odds = maximum_odds)
) |>
  filter(is.finite(odds), odds > 1, is.finite(model_probability)) |>
  mutate(break_even_probability = 1 / odds,
         expected_return = model_probability * odds - 1,
         outcome_order = match(outcome, outcomes))

one_bet_model_bets <- tidyr::crossing(one_bet_opportunities, threshold = ev_thresholds) |>
  filter(expected_return > threshold) |>
  group_by(price_basis, model, threshold, match_id_analysis) |>
  arrange(desc(expected_return), outcome_order, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(win = outcome == FTR, net_profit = if_else(win, odds - 1, -1))

summarise_one_bet <- function(data, ...) data |>
  group_by(...) |>
  summarise(bets = n(), wins = sum(win), hit_rate = mean(win),
            average_odds = mean(odds), average_predicted_edge = mean(expected_return),
            total_staked = n(), net_profit = sum(net_profit),
            roi = net_profit / total_staked,
            normal_market_return = fixed_normal_market_return,
            abnormal_return = roi - normal_market_return, .groups = "drop")

one_bet_overall <- summarise_one_bet(one_bet_model_bets, price_basis, model, threshold) |>
  arrange(price_basis, threshold, match(model, model_order))
one_bet_by_season <- summarise_one_bet(
  one_bet_model_bets, SeasonStart, SeasonLabel, price_basis, model, threshold)
one_bet_by_league <- summarise_one_bet(
  one_bet_model_bets, LeagueName, price_basis, model, threshold)
one_bet_by_outcome <- summarise_one_bet(
  one_bet_model_bets, outcome, price_basis, model, threshold)

one_bet_stability <- one_bet_by_season |>
  group_by(price_basis, model, threshold) |>
  summarise(total_test_seasons = length(test_seasons),
            seasons_with_bets = n_distinct(SeasonStart),
            total_bets = sum(bets), total_profit = sum(net_profit),
            pooled_roi = total_profit / sum(total_staked),
            profitable_seasons = sum(roi > 0),
            median_active_season_roi = median(roi),
            minimum_season_roi = min(roi), maximum_season_roi = max(roi),
            .groups = "drop") |>
  arrange(price_basis, threshold, match(model, model_order))

# The compact table used to write Section 6.2.
section_6_2_economic_table <- one_bet_overall |>
  filter((price_basis == "Average odds" & model == "M_O" & threshold == 0) |
           (price_basis == "Maximum odds" & model %in% c("Market", "M_O"))) |>
  left_join(one_bet_stability,
            by = c("price_basis", "model", "threshold"),
            suffix = c("", "_stability")) |>
  select(price_basis, model, threshold, bets, wins, hit_rate, average_odds,
         average_predicted_edge, net_profit, roi, abnormal_return,
         total_test_seasons, seasons_with_bets, profitable_seasons,
         median_active_season_roi, minimum_season_roi, maximum_season_roi) |>
  arrange(price_basis, threshold, match(model, c("Market", "M_O")))

average_mo_missing_seasons <- tibble(SeasonStart = test_seasons) |>
  left_join(distinct(analysis_features, SeasonStart, SeasonLabel), by = "SeasonStart") |>
  anti_join(one_bet_by_season |>
              filter(price_basis == "Average odds", model == "M_O", threshold == 0) |>
              distinct(SeasonStart), by = "SeasonStart")

oos_price_diagnostic <- oos_forecasts |>
  distinct(match_id_analysis, odds_avg_home, odds_avg_draw, odds_avg_away,
           odds_max_home, odds_max_draw, odds_max_away) |>
  mutate(average_booksum = 1 / odds_avg_home + 1 / odds_avg_draw + 1 / odds_avg_away,
         maximum_booksum = 1 / odds_max_home + 1 / odds_max_draw + 1 / odds_max_away) |>
  summarise(matches = n(), mean_average_booksum = mean(average_booksum),
            mean_maximum_booksum = mean(maximum_booksum),
            negative_average_overround_share = mean(average_booksum < 1),
            negative_maximum_overround_share = mean(maximum_booksum < 1))

plot_betting_roi <- one_bet_by_season |>
  filter(threshold == 0, model %in% c("Market", "M_O", "M_F", "M_OF")) |>
  ggplot(aes(SeasonStart, roi, colour = model)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_line() + geom_point() +
  facet_wrap(~price_basis) + labs(x = NULL, y = "ROI", colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

### 8. Ordered results used in the thesis ---------------------------------
show_table("Joint opening-market calibration test", joint_calibration_test)
show_table("Joint multinomial coefficients tested against efficiency", calibration_coefficients)
show_table("Outcome-specific binary calibration diagnostics", outcome_calibration_tests)
show_table("Largest five-percentage-point calibration gaps", largest_calibration_gaps)

show_table("Expanding-window estimation log", fit_log)
show_table("Overall out-of-sample forecast scores", score_overall)
show_table("Overall pairwise score comparisons", comparison_table)
show_table("Out-of-sample scores by season", score_season_comparison)
show_table("Out-of-sample scores by league", score_league_comparison)
show_table("Training likelihood tests: M_O versus M_OF", training_likelihood_tests)

### SECTION ODDS ONLY####
show_table("Section 6.2 economic results", section_6_2_economic_table)
show_table("All one-bet strategies: overall", one_bet_overall)
show_table("All one-bet strategies: seasonal stability", one_bet_stability)
show_table("One-bet strategies by outcome", one_bet_by_outcome)
show_table("OOS average- and maximum-price diagnostics", oos_price_diagnostic)
show_table("Average-odds M_O seasons without a bet", average_mo_missing_seasons)
show_table("Final training-window model coefficients", last_model_coefficients)

###New table for odds only, max odds####
economic_table_input <- one_bet_overall |>
  filter((price_basis == "Average odds" & model == "M_O" & threshold == 0) |
           (price_basis == "Maximum odds" & model %in% c("Market", "M_O"))) |>
  left_join(
    one_bet_stability |>
      select(price_basis, model, threshold, seasons_with_bets,
             profitable_seasons, median_active_season_roi),
    by = c("price_basis", "model", "threshold")
  ) |>
  transmute(
    panel = recode(price_basis,
                   "Average odds" = "Average market odds",
                   "Maximum odds" = "Maximum available odds"),
    forecast = model, minimum_edge = threshold, bets, hit_rate,
    average_odds, pooled_roi = roi,
    median_season_roi = median_active_season_roi,
    profitable_seasons, seasons_with_bets
  ) |>
  arrange(match(panel, c("Average market odds", "Maximum available odds")),
          minimum_edge, match(forecast, c("Market", "M_O")))

print(economic_table_input, n = Inf, width = Inf)

###Output for 6.3, fundamentals###
# M_F: compact output
show_table("M_F convergence",
           fit_log |>
             select(test_season, training_matches, test_matches, M_F_convergence))

show_table("M_F overall performance",
           score_overall |>
             filter(model %in% c("Market", "M_F")))

show_table("Market versus M_F",
           comparison_table |>
             filter(comparison == "Market minus M_F"))

show_table("M_F by season",
           score_season_comparison |>
             select(SeasonLabel, contains("Market"), contains("M_F"),
                    rps_gain_fundamentals_only))

show_table("M_F by league",
           score_league_comparison |>
             select(LeagueName, contains("Market"), contains("M_F"),
                    rps_gain_fundamentals_only))

show_table("M_F betting results",
           one_bet_overall |>
             filter(model == "M_F") |>
             select(price_basis, threshold, bets, hit_rate, average_odds,
                    average_predicted_edge, net_profit, roi))

show_table("M_F betting stability",
           one_bet_stability |>
             filter(model == "M_F") |>
             select(price_basis, threshold, seasons_with_bets, profitable_seasons,
                    pooled_roi, median_active_season_roi))

show_table("M_F coefficients",
           last_model_coefficients |>
             filter(model == "M_F"))

show_table("M_F ROI by season",
           one_bet_by_season |>
             filter(model == "M_F") |>
             select(SeasonLabel, price_basis, threshold, bets,
                    average_odds, net_profit, roi) |>
             arrange(price_basis, threshold, SeasonLabel))

# ============================================================
# COMPACT OUTPUT FOR SECTION 6.3: FUNDAMENTALS-ONLY MODEL
# ============================================================
# 1. Estimation diagnostic
show_table(
  "6.3 M_F estimation diagnostic",
  fit_log |>
    summarise(
      estimation_windows = n(),
      minimum_training_matches = min(training_matches),
      maximum_training_matches = max(training_matches),
      total_test_matches = sum(test_matches),
      all_models_converged = all(M_F_convergence == 0)
    )
)

# 2. Overall statistical performance
show_table(
  "6.3 M_F overall performance",
  score_overall |>
    filter(model %in% c("Market", "M_F")) |>
    select(model, matches, mean_rps, mean_log_loss, accuracy)
)

show_table(
  "6.3 Market versus M_F",
  comparison_table |>
    filter(comparison == "Market minus M_F")
)

# 3. Performance by season
show_table(
  "6.3 M_F performance by season",
  score_season_comparison |>
    select(
      SeasonLabel,
      mean_rps_Market, mean_rps_M_F,
      rps_gain_fundamentals_only,
      mean_log_loss_Market, mean_log_loss_M_F,
      accuracy_Market, accuracy_M_F
    )
)

# 4. Performance by league
show_table(
  "6.3 M_F performance by league",
  score_league_comparison |>
    select(
      LeagueName,
      mean_rps_Market, mean_rps_M_F,
      rps_gain_fundamentals_only,
      mean_log_loss_Market, mean_log_loss_M_F,
      accuracy_Market, accuracy_M_F
    )
)

# 5. Core coefficients from the final training window
# League effects and intercepts are controls and omitted here.
show_table(
  "6.3 M_F core coefficients",
  last_model_coefficients |>
    filter(
      model == "M_F",
      !grepl("Intercept|LeagueKey", term)
    ) |>
    select(term, estimate, standard_error, z, p_value)
)

# 6. Economic performance and seasonal stability
mf_economic <- one_bet_overall |>
  filter(model == "M_F") |>
  left_join(
    one_bet_stability |>
      filter(model == "M_F") |>
      select(
        price_basis, threshold, seasons_with_bets,
        profitable_seasons, median_active_season_roi,
        minimum_season_roi, maximum_season_roi
      ),
    by = c("price_basis", "threshold")
  ) |>
  select(
    price_basis, threshold, bets, hit_rate, average_odds,
    average_predicted_edge, net_profit, roi,
    profitable_seasons, seasons_with_bets,
    median_active_season_roi,
    minimum_season_roi, maximum_season_roi
  ) |>
  arrange(price_basis, threshold)

show_table("6.3 M_F economic performance", mf_economic)

# 7. Zero-threshold performance by selected outcome
show_table(
  "6.3 M_F betting performance by outcome",
  one_bet_by_outcome |>
    filter(model == "M_F", threshold == 0) |>
    select(
      price_basis, outcome, bets, hit_rate,
      average_odds, net_profit, roi
    ) |>
    arrange(price_basis, match(outcome, outcomes))
)

####M_F zero threshold by outcome###
mf_outcome_seasons <- summarise_one_bet(
  one_bet_model_bets |>
    filter(model == "M_F", threshold == 0),
  SeasonStart, SeasonLabel, price_basis, outcome
)

mf_outcome_stability <- mf_outcome_seasons |>
  group_by(price_basis, outcome) |>
  summarise(
    bets = sum(bets),
    pooled_roi = sum(net_profit) / sum(total_staked),
    profitable_seasons = sum(roi > 0),
    seasons_with_bets = n(),
    median_season_roi = median(roi),
    .groups = "drop"
  )

show_table("M_F outcome stability", mf_outcome_stability)

show_table(
  "M_F maximum-price draw ROI by season",
  mf_outcome_seasons |>
    filter(price_basis == "Maximum odds", outcome == "D") |>
    select(SeasonLabel, bets, average_odds, net_profit, roi)
)

# Section 6.2: zero-threshold stability by selected outcome
mo_outcome_seasons <- summarise_one_bet(
  one_bet_model_bets |>
    filter(model %in% c("Market", "M_O"), threshold == 0),
  SeasonStart, SeasonLabel, price_basis, model, outcome
)

mo_outcome_stability <- mo_outcome_seasons |>
  group_by(price_basis, model, outcome) |>
  summarise(
    bets = sum(bets),
    pooled_roi = sum(net_profit) / sum(total_staked),
    profitable_seasons = sum(roi > 0),
    seasons_with_bets = n_distinct(SeasonStart),
    median_season_roi = median(roi),
    .groups = "drop"
  ) |>
  arrange(price_basis, model, match(outcome, outcomes))

show_table("Section 6.2 outcome stability", mo_outcome_stability)

show_table(
  "M_O maximum-price ROI by outcome and season",
  mo_outcome_seasons |>
    filter(model == "M_O", price_basis == "Maximum odds") |>
    select(SeasonLabel, outcome, bets, average_odds, net_profit, roi) |>
    arrange(outcome, SeasonLabel)
)

# ============================================================
# SECTION 6.4: M_OF COMPACT OUTPUT
# ============================================================
# 1. Convergence
show_table(
  "6.4 M_OF convergence",
  fit_log |>
    select(test_season, training_matches, test_matches, M_OF_convergence)
)

# 2. Overall forecast performance
show_table(
  "6.4 M_OF overall performance",
  score_overall |>
    filter(model %in% c("Market", "M_O", "M_OF"))
)

# 3. Consistency across seasons and leagues
mof_consistency <- tibble(
  overall_rps_gain =
    comparison_table$mean_rps_improvement[
      comparison_table$comparison == "M_O minus M_OF"
    ],
  overall_log_loss_gain =
    comparison_table$mean_log_loss_improvement[
      comparison_table$comparison == "M_O minus M_OF"
    ],
  seasons_improved = sum(
    score_season_comparison$rps_gain_fundamentals_given_odds > 0
  ),
  seasons_tested = nrow(score_season_comparison),
  leagues_improved = sum(
    score_league_comparison$rps_gain_fundamentals_given_odds > 0
  ),
  leagues_tested = nrow(score_league_comparison)
)

show_table("6.4 M_OF consistency summary", mof_consistency)

show_table(
  "6.4 M_OF by season",
  score_season_comparison |>
    select(
      SeasonLabel,
      mean_rps_M_O, mean_rps_M_OF,
      rps_gain_fundamentals_given_odds,
      mean_log_loss_M_O, mean_log_loss_M_OF,
      accuracy_M_O, accuracy_M_OF
    )
)

show_table(
  "6.4 M_OF by league",
  score_league_comparison |>
    select(
      LeagueName,
      mean_rps_M_O, mean_rps_M_OF,
      rps_gain_fundamentals_given_odds,
      mean_log_loss_M_O, mean_log_loss_M_OF,
      accuracy_M_O, accuracy_M_OF
    )
)

# 4. Joint significance of fundamentals conditional on odds
show_table(
  "6.4 fundamentals conditional on odds",
  training_likelihood_tests
)

# 5. Core coefficients from the final training window
# Market slopes are tested against 1; fundamentals are tested against 0.
mof_core_coefficients <- last_model_coefficients |>
  filter(
    model == "M_OF",
    grepl(
      "market_log_ratio|pi_rating_diff_pre|matchup_xg_|matchup_ppg_5",
      term
    )
  ) |>
  mutate(
    null_value = if_else(grepl("market_log_ratio", term), 1, 0),
    z_against_null = (estimate - null_value) / standard_error,
    p_against_null =
      2 * pnorm(abs(z_against_null), lower.tail = FALSE)
  ) |>
  select(
    term, estimate, standard_error, null_value,
    z_against_null, p_against_null
  )

show_table("6.4 M_OF core coefficients", mof_core_coefficients)

# 6. Economic comparison with the same columns as Sections 6.2 and 6.3
mof_economic <- one_bet_overall |>
  filter(model %in% c("Market", "M_O", "M_OF")) |>
  left_join(
    one_bet_stability |>
      select(
        price_basis, model, threshold, seasons_with_bets,
        profitable_seasons, pooled_roi, median_active_season_roi
      ),
    by = c("price_basis", "model", "threshold")
  ) |>
  transmute(
    price_basis,
    forecast = model,
    minimum_edge = threshold,
    bets, hit_rate, average_odds,
    pooled_roi,
    median_season_roi = median_active_season_roi,
    profitable_seasons,
    seasons_with_bets
  ) |>
  arrange(
    match(price_basis, c("Average odds", "Maximum odds")),
    minimum_edge,
    match(forecast, c("Market", "M_O", "M_OF"))
  )

show_table("6.4 M_OF economic comparison", mof_economic)

# 7. Seasonal stability at the core zero threshold
show_table(
  "6.4 M_OF zero-threshold ROI by season",
  one_bet_by_season |>
    filter(model == "M_OF", threshold == 0) |>
    select(
      SeasonLabel, price_basis, bets,
      hit_rate, average_odds, net_profit, roi
    ) |>
    arrange(price_basis, SeasonLabel)
)


# 1. Is M_OF profitability concentrated in particular leagues?
show_table(
  "6.4 M_OF zero-threshold performance by league",
  one_bet_by_league |>
    filter(model == "M_OF", threshold == 0) |>
    select(
      LeagueName, price_basis, bets, hit_rate,
      average_odds, net_profit, roi
    ) |>
    arrange(price_basis, LeagueName))

# 2. Common versus model-specific M_O and M_OF bets
comparison_bets <- one_bet_model_bets |>
  filter(
    model %in% c("M_O", "M_OF"),
    (price_basis == "Average odds" & threshold == 0) |
      (price_basis == "Maximum odds" &
         threshold %in% c(0, 0.025, 0.05))
  )

bet_keys <- c(
  "price_basis", "threshold",
  "match_id_analysis", "outcome"
)

mo_bets  <- comparison_bets |> filter(model == "M_O")
mof_bets <- comparison_bets |> filter(model == "M_OF")

common_keys <- inner_join(
  mo_bets |> select(all_of(bet_keys)),
  mof_bets |> select(all_of(bet_keys)),
  by = bet_keys
) |>
  distinct()

bet_components <- bind_rows(
  mof_bets |>
    semi_join(common_keys, by = bet_keys) |>
    mutate(component = "Common exact bet"),
  mof_bets |>
    anti_join(common_keys, by = bet_keys) |>
    mutate(component = "M_OF-only bet"),
  mo_bets |>
    anti_join(common_keys, by = bet_keys) |>
    mutate(component = "M_O-only bet")
)

component_seasons <- bet_components |>
  group_by(price_basis, threshold, component, SeasonStart) |>
  summarise(
    bets = n(),
    profit = sum(net_profit),
    roi = profit / bets,
    .groups = "drop"
  )

component_summary <- component_seasons |>
  group_by(price_basis, threshold, component) |>
  summarise(
    bets = sum(bets),
    net_profit = sum(profit),
    pooled_roi = sum(profit) / sum(bets),
    seasons_with_bets = n(),
    profitable_seasons = sum(roi > 0),
    median_season_roi = median(roi),
    .groups = "drop"
  ) |>
  arrange(price_basis, threshold, component)

show_table(
  "6.4 common and model-specific betting performance",
  component_summary
)


# ============================================================
# FULL-SAMPLE COEFFICIENT MODELS FOR REPORTING
# ============================================================

# Prepare all observations using full-sample medians, means and SDs.
# These fits are descriptive and are NOT used for OOS forecasts.
full_prepared <- prepare_fundamentals(model_data, model_data)
full_data <- full_prepared$train
full_x <- make_designs(full_data)

# Full-sample M_O
fit_O_full <- fit_mnl(
  full_data$FTR,
  full_x$O_H,
  full_x$O_A,
  start = c(0, 1, 0, 1)
)

# Full-sample M_F
fit_F_full <- fit_mnl(
  full_data$FTR,
  full_x$F_H,
  full_x$F_A
)

# Full-sample M_OF
of_start_full <- c(
  0, 1, rep(0, ncol(full_x$OF_H) - 2),
  0, 1, rep(0, ncol(full_x$OF_A) - 2)
)

fit_OF_full <- fit_mnl(
  full_data$FTR,
  full_x$OF_H,
  full_x$OF_A,
  start = of_start_full
)

# ------------------------------------------------------------
# 1. Sample and convergence
# ------------------------------------------------------------

full_sample_diagnostic <- tibble(
  matches = nrow(full_data),
  seasons = n_distinct(full_data$SeasonStart),
  first_season_start = min(full_data$SeasonStart),
  final_season_start = max(full_data$SeasonStart),
  M_O_convergence = fit_O_full$convergence,
  M_F_convergence = fit_F_full$convergence,
  M_OF_convergence = fit_OF_full$convergence
)

show_table(
  "Full-sample coefficient-model diagnostic",
  full_sample_diagnostic
)

# ------------------------------------------------------------
# 2. Full-sample M_F core coefficients
# Conventional null: coefficient = 0
# ------------------------------------------------------------

core_fundamental_pattern <- paste(
  c(
    "pi_rating_diff_pre",
    "matchup_xg_for_5",
    "matchup_xg_against_5",
    "matchup_ppg_5"
  ),
  collapse = "|"
)

mf_full_core_coefficients <- tidy_mnl(
  fit_F_full,
  "M_F full sample"
) |>
  filter(grepl(core_fundamental_pattern, term)) |>
  mutate(
    comparison = if_else(
      grepl("^H:", term),
      "Home-draw",
      "Away-draw"
    ),
    predictor = case_when(
      grepl("pi_rating_diff_pre", term) ~
        "Pi-rating difference",
      grepl("matchup_xg_for_5", term) ~
        "Five-match xG-for difference",
      grepl("matchup_xg_against_5", term) ~
        "Five-match xG-against difference",
      grepl("matchup_ppg_5", term) ~
        "Five-match PPG difference"
    ),
    null_value = 0,
    z_against_null = estimate / standard_error,
    p_against_null =
      2 * pnorm(abs(z_against_null), lower.tail = FALSE)
  ) |>
  select(
    comparison, predictor, estimate, standard_error,
    null_value, z_against_null, p_against_null
  )

show_table(
  "Full-sample M_F core coefficients",
  mf_full_core_coefficients
)

# ------------------------------------------------------------
# 3. Full-sample M_OF core coefficients
# Market slopes tested against 1; fundamentals against 0
# ------------------------------------------------------------

mof_full_core_coefficients <- tidy_mnl(
  fit_OF_full,
  "M_OF full sample"
) |>
  filter(
    grepl(
      paste(
        c("market_log_ratio", core_fundamental_pattern),
        collapse = "|"
      ),
      term
    )
  ) |>
  mutate(
    comparison = if_else(
      grepl("^H:", term),
      "Home-draw",
      "Away-draw"
    ),
    predictor = case_when(
      grepl("market_log_ratio", term) ~
        "Market log ratio",
      grepl("pi_rating_diff_pre", term) ~
        "Pi-rating difference",
      grepl("matchup_xg_for_5", term) ~
        "Five-match xG-for difference",
      grepl("matchup_xg_against_5", term) ~
        "Five-match xG-against difference",
      grepl("matchup_ppg_5", term) ~
        "Five-match PPG difference"
    ),
    null_value = if_else(
      grepl("market_log_ratio", term),
      1,
      0
    ),
    z_against_null =
      (estimate - null_value) / standard_error,
    p_against_null =
      2 * pnorm(abs(z_against_null), lower.tail = FALSE)
  ) |>
  select(
    comparison, predictor, estimate, standard_error,
    null_value, z_against_null, p_against_null
  )

show_table(
  "Full-sample M_OF core coefficients",
  mof_full_core_coefficients
)

# ------------------------------------------------------------
# 4. Joint incremental test: M_OF versus M_O
# Tests all fundamentals and controls added to M_O
# ------------------------------------------------------------

full_sample_incremental_test <- tibble(
  matches = nrow(full_data),
  logLik_M_O = fit_O_full$logLik,
  logLik_M_OF = fit_OF_full$logLik,
  LR = 2 * (logLik_M_OF - logLik_M_O),
  df = length(fit_OF_full$coefficients) -
    length(fit_O_full$coefficients),
  p_value = pchisq(LR, df, lower.tail = FALSE)
)

show_table(
  "Full-sample M_OF versus M_O likelihood-ratio test",
  full_sample_incremental_test
)

#
mof_full_intercepts <- tidy_mnl(
  fit_OF_full,
  "M_OF full sample"
) |>
  filter(grepl("Intercept", term)) |>
  mutate(
    comparison = if_else(
      grepl("^H:", term),
      "Home-draw",
      "Away-draw"
    ),
    predictor = "Intercept",
    null_value = 0,
    z_against_null = estimate / standard_error,
    p_against_null =
      2 * pnorm(abs(z_against_null), lower.tail = FALSE)
  ) |>
  select(
    comparison, predictor, estimate, standard_error,
    null_value, z_against_null, p_against_null
  )

show_table(
  "Full-sample M_OF intercepts",
  mof_full_intercepts
)


# M_OF: statistical evaluation, tables
show_table("M_OF overall performance",
           score_overall |>
             filter(model %in% c("Market", "M_O", "M_OF")))

show_table("M_O versus M_OF",
           comparison_table |>
             filter(comparison == "M_O minus M_OF"))

show_table("M_OF by season",
           score_season_comparison |>
             select(
               SeasonLabel,
               mean_rps_M_O, mean_rps_M_OF,
               rps_gain_fundamentals_given_odds,
               mean_log_loss_M_O, mean_log_loss_M_OF,
               accuracy_M_O, accuracy_M_OF
             ))

show_table("Fundamentals conditional on odds",
           training_likelihood_tests)

# M_OF: economic evaluation
show_table("M_OF betting comparison",
           one_bet_overall |>
             filter(model %in% c("Market", "M_O", "M_OF")) |>
             select(
               price_basis, model, threshold, bets, hit_rate,
               average_odds, average_predicted_edge, net_profit, roi
             ))

show_table("M_OF betting stability",
           one_bet_stability |>
             filter(model %in% c("Market", "M_O", "M_OF")) |>
             select(
               price_basis, model, threshold, seasons_with_bets,
               profitable_seasons, pooled_roi, median_active_season_roi
             ))

show_table("M_OF ROI by season",
           one_bet_by_season |>
             filter(model == "M_OF") |>
             select(
               SeasonLabel, price_basis, threshold,
               bets, average_odds, net_profit, roi
             ) |>
             arrange(price_basis, threshold, SeasonLabel))

show_table("M_OF coefficients",
           last_model_coefficients |>
             filter(model == "M_OF"))

# League-varying slopes: does interacting fundamentals with league improve fit?
# Full-sample LR test, pooled vs league-interacted M_F.

league_interaction_test <- local({
  d <- model_data
  prep <- prepare_fundamentals(d, d)     # standardise on the full sample
  train <- prep$train
  
  # Pooled M_F design (what you already estimate)
  x_pool <- make_designs(train)
  fit_pool <- fit_mnl(train$FTR, x_pool$F_H, x_pool$F_A)
  
  # League-interacted design: every continuous fundamental x LeagueKey
  inter_formula <- reformulate(c(
    fundamental_features, "LeagueKey",
    paste0("LeagueKey:", continuous_features)
  ))
  Xi <- model.matrix(inter_formula, train)
  fit_inter <- fit_mnl(train$FTR, Xi, Xi)
  
  k_pool  <- length(fit_pool$coefficients)
  k_inter <- length(fit_inter$coefficients)
  LR <- 2 * (fit_inter$logLik - fit_pool$logLik)
  df <- k_inter - k_pool
  
  tibble::tibble(
    logLik_pooled = fit_pool$logLik,
    logLik_interacted = fit_inter$logLik,
    LR = LR, df = df,
    p_value = pchisq(LR, df, lower.tail = FALSE)
  )
})

print(league_interaction_test)

#-----------------------------------------------------#
##Bins of 5%##
library(dplyr)

bin_table <- calibration_bins_core |>
  mutate(outcome = recode(outcome, H = "Home win", D = "Draw", A = "Away win")) |>
  arrange(outcome, market_probability) |>
  transmute(
    outcome,
    probability_bin,
    observations = format(observations, big.mark = ","),
    market   = sprintf("%.1f\\%%", market_probability),
    realised = sprintf("%.1f\\%%", realised_frequency_pct),
    gap      = sprintf("$%+.2f$", calibration_error_pp)
  )

for (o in c("Home win", "Draw", "Away win")) {
  cat("\\addlinespace\n\\multicolumn{5}{l}{\\textit{", o, "}}\\\\\n", sep = "")
  bin_table |>
    filter(outcome == o) |>
    select(-outcome) |>
    apply(1, \(r) cat(paste(r, collapse = " & "), "\\\\\n"))
}


#pooled across outcomes
pooled_bins <- market_probability_long |>
  group_by(bin_lower, probability_bin) |>
  summarise(observations = n(),
            mean_market = mean(market_probability),
            realised    = mean(observed),
            gap_pp      = 100 * (realised - mean_market),
            .groups = "drop") |>
  arrange(bin_lower)

print(pooled_bins, n = Inf)

pooled_bins |>
  filter(observations >= 100) |>
  transmute(probability_bin,
            observations = format(observations, big.mark = ","),
            market   = sprintf("%.1f\\%%", 100 * mean_market),
            realised = sprintf("%.1f\\%%", 100 * realised),
            gap      = sprintf("$%+.2f$", gap_pp)) |>
  apply(1, \(r) cat(paste(r, collapse = " & "), "\\\\\n"))





#=======================================================#
####Robustness####
###running for last 4 seaons
#=======================================================#
names(one_bet_by_season)

four_season_labels <- c("2021/22", "2022/23", "2023/24", "2024/25")

four_season_summary <- one_bet_by_season |>
  dplyr::filter(SeasonLabel %in% four_season_labels,
                model %in% c("M_O", "M_F", "M_OF")) |>
  dplyr::group_by(price_basis, model, threshold) |>
  dplyr::summarise(
    seasons            = dplyr::n(),
    bets               = sum(bets),
    hit_rate           = sum(wins) / sum(bets),
    pooled_roi         = sum(net_profit) / sum(bets),
    profitable_seasons = sum(roi > 0),
    .groups = "drop"
  ) |>
  dplyr::arrange(price_basis,
                 match(model, c("M_O", "M_F", "M_OF")),
                 threshold)

print(four_season_summary, n = Inf, width = Inf)

ls()[grepl("boot|ci|interval", ls(), ignore.case = TRUE)]


recent <- c(2021, 2022, 2023, 2024)

# 1. Point estimates, all four forecasts
four_season_summary <- one_bet_by_season |>
  filter(SeasonStart %in% recent) |>
  group_by(price_basis, model, threshold) |>
  summarise(total_bets         = sum(bets),
            hit_rate           = sum(wins) / sum(bets),
            average_odds       = sum(average_odds * bets) / sum(bets),
            pooled_roi         = sum(net_profit) / sum(bets),
            profitable_seasons = sum(roi > 0),
            median_season_roi  = median(roi),
            .groups = "drop") |>
  rename(bets = total_bets) |>
  arrange(price_basis, threshold, match(model, model_order))

print(four_season_summary, n = Inf, width = Inf)

# 2. Season-by-season detail
one_bet_by_season |>
  filter(SeasonStart %in% recent, threshold == 0) |>
  select(SeasonLabel, price_basis, model, bets, average_odds, net_profit, roi) |>
  arrange(price_basis, match(model, model_order), SeasonLabel) |>
  print(n = Inf, width = Inf)

bets4 <- one_bet_model_bets |>
  filter(SeasonStart %in% recent) |>
  mutate(cluster = paste(LeagueName, SeasonStart, sep = "_"))

keys4 <- bets4 |> distinct(price_basis, model, threshold) |>
  arrange(price_basis, model, threshold)
ids4  <- sort(unique(bets4$cluster))

profit4 <- stake4 <- matrix(0, nrow(keys4), length(ids4))
for (i in seq_len(nrow(keys4))) {
  cell <- bets4 |>
    filter(price_basis == keys4$price_basis[i],
           model       == keys4$model[i],
           threshold   == keys4$threshold[i]) |>
    group_by(cluster) |>
    summarise(profit = sum(net_profit), stake = n(), .groups = "drop")
  j <- match(cell$cluster, ids4)
  profit4[i, j] <- cell$profit
  stake4[i, j]  <- cell$stake
}

###BOOTSTRAP ROBUSTNESS 4 SEASONS###
set.seed(1234)
draws4 <- matrix(sample.int(length(ids4), length(ids4) * n_replicates, replace = TRUE),
                 nrow = n_replicates)
reps4 <- t(apply(draws4, 1, \(d)
                 rowSums(profit4[, d, drop = FALSE]) / pmax(rowSums(stake4[, d, drop = FALSE]), 1)))

roi_bootstrap_4 <- keys4 |>
  mutate(bets       = rowSums(stake4),
         pooled_roi = rowSums(profit4) / rowSums(stake4),
         ci_lower   = apply(reps4, 2, quantile, 0.025),
         ci_upper   = apply(reps4, 2, quantile, 0.975))

print(roi_bootstrap_4, n = Inf, width = Inf)

four_season_table <- roi_bootstrap_4 |>
  left_join(
    one_bet_by_season |>
      filter(SeasonStart %in% recent) |>
      group_by(price_basis, model, threshold) |>
      summarise(hit_rate           = sum(wins) / sum(bets),
                average_odds       = sum(average_odds * bets) / sum(bets),
                profitable_seasons = sum(roi > 0),
                
                .groups = "drop"),
    by = c("price_basis", "model", "threshold")
  ) |>
  select(price_basis, model, threshold, bets, hit_rate, average_odds,
         pooled_roi, ci_lower, ci_upper, profitable_seasons) |>
  arrange(price_basis, threshold, match(model, model_order))

print(four_season_table, n = Inf, width = Inf)

#=============================================#
#Robustness max odds USING ONE BOOKAMKERS
#=============================================#
names(analysis_features)[grepl("^(B365|PS|PSC|Ps|Max|Avg)", names(analysis_features))]

analysis_features |>
  group_by(SeasonStart) |>
  summarise(across(any_of(c("B365H","PSH","PSCH")), ~ mean(is.finite(.x))))

single_book_bets <- function(h, d, a, label) {
  cols <- c(H = h, D = d, A = a)
  stopifnot(all(cols %in% names(analysis_features)))
  
  odds_long <- analysis_features |>
    select(match_id_analysis, all_of(unname(cols))) |>
    pivot_longer(-match_id_analysis, names_to = "col", values_to = "odds") |>
    mutate(outcome = names(cols)[match(col, cols)]) |>
    select(-col)
  
  forecast_long |>
    select(match_id_analysis, LeagueName, SeasonStart, SeasonLabel, FTR, model,
           p_home, p_draw, p_away) |>
    pivot_longer(c(p_home, p_draw, p_away),
                 names_to = "pn", values_to = "model_probability") |>
    mutate(outcome = recode(pn, p_home = "H", p_draw = "D", p_away = "A")) |>
    select(-pn) |>
    inner_join(odds_long, by = c("match_id_analysis", "outcome")) |>
    filter(is.finite(odds), odds > 1) |>
    mutate(expected_return = model_probability * odds - 1,
           outcome_order = match(outcome, outcomes)) |>
    crossing(threshold = ev_thresholds) |>
    filter(expected_return > threshold) |>
    group_by(model, threshold, match_id_analysis) |>
    arrange(desc(expected_return), outcome_order, .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(win = outcome == FTR,
           net_profit = if_else(win, odds - 1, -1),
           book = label)
}

book_bets <- bind_rows(
  single_book_bets("B365H", "B365D", "B365A", "Bet365"),
  single_book_bets("PSH",   "PSD",   "PSA",   "Pinnacle opening"),
  single_book_bets("PSCH",  "PSCD",  "PSCA",  "Pinnacle closing")
)

book_seasons <- book_bets |>
  group_by(book, model, threshold, SeasonStart) |>
  summarise(bets = n(), wins = sum(win), profit = sum(net_profit),
            roi = profit / bets, avg_odds = mean(odds), .groups = "drop")

book_summary <- book_bets |>
  group_by(book, model, threshold) |>
  summarise(bets = n(), hit_rate = mean(win),
            average_odds = mean(odds),
            pooled_roi = sum(net_profit)/n(), .groups = "drop") |>
  left_join(
    book_bets |>
      group_by(book, model, threshold, SeasonStart) |>
      summarise(roi = sum(net_profit)/n(), .groups = "drop") |>
      group_by(book, model, threshold) |>
      summarise(profitable_seasons = sum(roi > 0), seasons = n(), .groups = "drop"),
    by = c("book", "model", "threshold")
  ) |>
  arrange(book, threshold, match(model, model_order))

print(book_summary, n = Inf, width = Inf)

###Only 0 threholds###
###Bet365 and Pinnacle####
set.seed(1234)

book_boot <- bind_rows(lapply(unique(book_bets$book), function(bk) {
  d <- book_bets |>
    filter(book == bk, threshold == 0) |>
    mutate(cluster = paste(LeagueName, SeasonStart, sep = "_"))
  
  keys <- d |> distinct(model) |> arrange(match(model, model_order))
  ids  <- sort(unique(d$cluster))
  
  profit <- stake <- matrix(0, nrow(keys), length(ids))
  for (i in seq_len(nrow(keys))) {
    cell <- d |>
      filter(model == keys$model[i]) |>
      group_by(cluster) |>
      summarise(profit = sum(net_profit), stake = n(), .groups = "drop")
    j <- match(cell$cluster, ids)
    profit[i, j] <- cell$profit
    stake[i, j]  <- cell$stake
  }
  
  draws <- matrix(sample.int(length(ids), length(ids) * n_replicates, replace = TRUE),
                  nrow = n_replicates)
  reps <- t(apply(draws, 1, \(x)
                  rowSums(profit[, x, drop = FALSE]) / pmax(rowSums(stake[, x, drop = FALSE]), 1)))
  
  keys |>
    mutate(book       = bk,
           bets       = rowSums(stake),
           hit_rate   = sapply(keys$model, \(m) mean(d$win[d$model == m])),
           avg_odds   = sapply(keys$model, \(m) mean(d$odds[d$model == m])),
           pooled_roi = rowSums(profit) / rowSums(stake),
           ci_lower   = apply(reps, 2, quantile, 0.025),
           ci_upper   = apply(reps, 2, quantile, 0.975))
})) |>
  left_join(
    book_bets |>
      filter(threshold == 0) |>
      group_by(book, model, SeasonStart) |>
      summarise(roi = sum(net_profit) / n(), .groups = "drop") |>
      group_by(book, model) |>
      summarise(profitable_seasons = sum(roi > 0), seasons = n(), .groups = "drop"),
    by = c("book", "model")
  ) |>
  select(book, model, bets, hit_rate, avg_odds, pooled_roi,
         ci_lower, ci_upper, profitable_seasons, seasons) |>
  arrange(book, match(model, model_order))

print(book_boot, n = Inf, width = Inf)

###All threholds###
set.seed(1234)

book_boot <- bind_rows(lapply(unique(book_bets$book), function(bk) {
  d <- book_bets |>
    filter(book == bk) |>
    mutate(cluster = paste(LeagueName, SeasonStart, sep = "_"))
  
  keys <- d |> distinct(model, threshold) |>
    arrange(threshold, match(model, model_order))
  ids <- sort(unique(d$cluster))
  
  profit <- stake <- matrix(0, nrow(keys), length(ids))
  for (i in seq_len(nrow(keys))) {
    cell <- d |>
      filter(model == keys$model[i], threshold == keys$threshold[i]) |>
      group_by(cluster) |>
      summarise(profit = sum(net_profit), stake = n(), .groups = "drop")
    j <- match(cell$cluster, ids)
    profit[i, j] <- cell$profit
    stake[i, j]  <- cell$stake
  }
  
  draws <- matrix(sample.int(length(ids), length(ids) * n_replicates, replace = TRUE),
                  nrow = n_replicates)
  reps <- t(apply(draws, 1, \(x)
                  rowSums(profit[, x, drop = FALSE]) / pmax(rowSums(stake[, x, drop = FALSE]), 1)))
  
  keys |>
    mutate(book       = bk,
           bets       = rowSums(stake),
           hit_rate   = mapply(\(m, t) mean(d$win[d$model == m & d$threshold == t]),
                               keys$model, keys$threshold),
           avg_odds   = mapply(\(m, t) mean(d$odds[d$model == m & d$threshold == t]),
                               keys$model, keys$threshold),
           pooled_roi = rowSums(profit) / pmax(rowSums(stake), 1),
           ci_lower   = apply(reps, 2, quantile, 0.025),
           ci_upper   = apply(reps, 2, quantile, 0.975))
})) |>
  left_join(
    book_bets |>
      group_by(book, model, threshold, SeasonStart) |>
      summarise(roi = sum(net_profit) / n(), .groups = "drop") |>
      group_by(book, model, threshold) |>
      summarise(profitable_seasons = sum(roi > 0), seasons = n(), .groups = "drop"),
    by = c("book", "model", "threshold")
  ) |>
  select(book, model, threshold, bets, hit_rate, avg_odds, pooled_roi,
         ci_lower, ci_upper, profitable_seasons, seasons) |>
  arrange(book, threshold, match(model, model_order))

print(book_boot, n = Inf, width = Inf)



###Devig Functions###
shin_p <- function(pi) {
  B <- sum(pi)
  f <- function(z) sum((sqrt(z^2 + 4*(1-z)*pi^2/B) - z) / (2*(1-z))) - 1
  z <- uniroot(f, c(1e-8, 0.5))$root
  (sqrt(z^2 + 4*(1-z)*pi^2/B) - z) / (2*(1-z))
}

power_p <- function(pi) {
  k <- uniroot(\(k) sum(pi^k) - 1, c(0.5, 2))$root
  pi^k
}

pi <- c(1/1.75, 1/4.00, 1/4.20)
round(pi/sum(pi), 3); round(shin_p(pi), 3); round(power_p(pi), 3)