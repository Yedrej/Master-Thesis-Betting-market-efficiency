# STEP 04b — Descriptive tables and the Monte Carlo
# Reads:  analysis_features.rds
# Writes: tables and the random-betting figure
# Thesis: Section 6.1. Outcome rates, forecast benchmarks, the five naive
#         strategies, Figure 3, and the calibration bins in the appendix.

# ============================================================
# DESCRIPTIVE ANALYSIS: THESIS SECTION 6.1
# Top-five European leagues, 2014/15--2024/25
# ============================================================
# Outputs:
#   1. Match-outcome rates by league
#   2. Market, climatology and favourite forecast benchmarks
#   3. Naive strategies at average and maximum odds
#   4. Random-betting simulation at both price bases
#   5. Descriptive market calibration in fixed 5% bins

# ============================================================
# 0. SETUP
# ============================================================
required_packages <- c("dplyr", "tidyr", "readr", "ggplot2", "tibble",
                       "scales", "knitr", "kableExtra")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tibble)

project_dir <- Sys.getenv("THESIS_PROJECT_DIR", unset = ".")
feature_input_path <- file.path(project_dir, "analysis_test", "data", "analysis_features.rds")
if (!file.exists(feature_input_path)) stop("Feature dataset not found: ", feature_input_path)

analysis_features <- readr::read_rds(feature_input_path) |>
  mutate(MatchDate = as.Date(MatchDate)) |>
  arrange(LeagueKey, MatchDate, match_id_analysis)

required_columns <- c(
  "match_id_analysis", "LeagueKey", "LeagueName", "SeasonStart", "SeasonLabel",
  "MatchDate", "FTR", "mkt_pre_p_home", "mkt_pre_p_draw", "mkt_pre_p_away",
  "odds_avg_pre_home", "odds_avg_pre_draw", "odds_avg_pre_away",
  "odds_max_pre_home", "odds_max_pre_draw", "odds_max_pre_away"
)
missing_columns <- setdiff(required_columns, names(analysis_features))
if (length(missing_columns) > 0) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}

league_order <- c("Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1")
strategy_order <- c("Always home", "Always draw", "Always away", "Favourite", "Longshot")
accent_col <- "#B4436C"

calculate_rps <- function(p_home, p_draw, p_away, outcome) {
  y_home <- as.integer(outcome == "H")
  y_home_draw <- as.integer(outcome %in% c("H", "D"))
  ((p_home - y_home)^2 + (p_home + p_draw - y_home_draw)^2) / 2
}

# ============================================================
# 1. MATCH-OUTCOME RATES
# ============================================================
outcome_sample <- analysis_features |>
  filter(FTR %in% c("H", "D", "A"))

home_win_ranges <- outcome_sample |>
  group_by(LeagueName, SeasonStart) |>
  summarise(home_win_share = mean(FTR == "H"), .groups = "drop") |>
  group_by(LeagueName) |>
  summarise(home_win_min = min(home_win_share),
            home_win_max = max(home_win_share), .groups = "drop")

outcome_rates <- outcome_sample |>
  group_by(LeagueName) |>
  summarise(matches = n(), home_win_share = mean(FTR == "H"),
            draw_share = mean(FTR == "D"), away_win_share = mean(FTR == "A"),
            .groups = "drop") |>
  left_join(home_win_ranges, by = "LeagueName") |>
  mutate(LeagueName = factor(LeagueName, levels = league_order)) |>
  arrange(LeagueName)

pooled_outcome_rates <- outcome_sample |>
  summarise(LeagueName = "Pooled sample", matches = n(),
            home_win_share = mean(FTR == "H"), draw_share = mean(FTR == "D"),
            away_win_share = mean(FTR == "A"),
            home_win_min = min(home_win_ranges$home_win_min),
            home_win_max = max(home_win_ranges$home_win_max))

outcome_rates_final <- bind_rows(
  outcome_rates |> mutate(LeagueName = as.character(LeagueName)), pooled_outcome_rates
) |>
  transmute(
    League = LeagueName, Matches = matches,
    `Home wins` = sprintf("%.1f\\%%", 100 * home_win_share),
    `Home-win range` = sprintf("%.1f--%.1f\\%%", 100 * home_win_min, 100 * home_win_max),
    Draws = sprintf("%.1f\\%%", 100 * draw_share),
    `Away wins` = sprintf("%.1f\\%%", 100 * away_win_share)
  )

outcome_rates_latex <- knitr::kable(
  outcome_rates_final, format = "latex", booktabs = TRUE, escape = FALSE,
  align = c("l", "r", "r", "r", "r", "r"),
  caption = paste("Match outcome frequencies by league.",
                  "The home-win range reports the minimum and maximum",
                  "league-season home-win rate."),
  label = "tab:outcome-rates"
) |>
  kableExtra::kable_styling(latex_options = "hold_position", position = "center")

cat("\nMATCH-OUTCOME RATES\n")
print(outcome_rates_final, n = Inf, width = Inf)
cat("\nLATEX: MATCH-OUTCOME RATES\n", outcome_rates_latex, "\n")

# ============================================================
# 2. FORECAST BENCHMARKS
# ============================================================
# The league climatology is an ex-post descriptive benchmark based on the full
# sample. It is not used as an out-of-sample model in the main analysis.
league_climatology <- outcome_sample |>
  group_by(LeagueName) |>
  summarise(clim_home = mean(FTR == "H"), clim_draw = mean(FTR == "D"),
            clim_away = mean(FTR == "A"), .groups = "drop")

benchmark_match_results <- outcome_sample |>
  left_join(league_climatology, by = "LeagueName") |>
  filter(if_all(c(mkt_pre_p_home, mkt_pre_p_draw, mkt_pre_p_away), is.finite)) |>
  mutate(
    market_prediction = c("H", "D", "A")[max.col(
      cbind(mkt_pre_p_home, mkt_pre_p_draw, mkt_pre_p_away), ties.method = "first")],
    climatology_prediction = c("H", "D", "A")[max.col(
      cbind(clim_home, clim_draw, clim_away), ties.method = "first")],
    market_rps = calculate_rps(mkt_pre_p_home, mkt_pre_p_draw, mkt_pre_p_away, FTR),
    climatology_rps = calculate_rps(clim_home, clim_draw, clim_away, FTR),
    favourite_rps = calculate_rps(
      as.integer(market_prediction == "H"), as.integer(market_prediction == "D"),
      as.integer(market_prediction == "A"), FTR
    )
  )

forecast_benchmark_table <- bind_rows(
  benchmark_match_results |>
    summarise(forecast = "Market average", matches = n(),
              accuracy = mean(market_prediction == FTR), mean_rps = mean(market_rps)),
  benchmark_match_results |>
    summarise(forecast = "League climatology", matches = n(),
              accuracy = mean(climatology_prediction == FTR), mean_rps = mean(climatology_rps)),
  benchmark_match_results |>
    summarise(forecast = "Always favourite", matches = n(),
              accuracy = mean(market_prediction == FTR), mean_rps = mean(favourite_rps))
) |>
  mutate(forecast = factor(forecast,
                           levels = c("Market average", "League climatology", "Always favourite"))) |>
  arrange(forecast)

forecast_benchmark_display <- forecast_benchmark_table |>
  transmute(Forecast = as.character(forecast), Matches = scales::comma(matches),
            Accuracy = scales::percent(accuracy, accuracy = 0.1),
            `Mean RPS` = sprintf("%.3f", mean_rps))

forecast_benchmark_latex <- knitr::kable(
  forecast_benchmark_display, format = "latex", booktabs = TRUE,
  align = c("l", "r", "r", "r"),
  caption = "Forecast performance of the market and naive probability benchmarks.",
  label = "tab:forecast-benchmarks"
) |>
  kableExtra::kable_styling(latex_options = "hold_position", position = "center")

forecast_benchmark_plot <- ggplot(
  forecast_benchmark_table,
  aes(x = reorder(as.character(forecast), mean_rps), y = mean_rps)
) +
  geom_col(fill = accent_col, width = 0.62) +
  geom_text(aes(label = sprintf("%.3f", mean_rps)), hjust = -0.15,
            family = "serif", size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(forecast_benchmark_table$mean_rps) * 1.14),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Mean Ranked Probability Score (lower is better)") +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

cat("\nFORECAST BENCHMARKS\n")
print(forecast_benchmark_display, n = Inf, width = Inf)
cat("\nLATEX: FORECAST BENCHMARKS\n", forecast_benchmark_latex, "\n")
print(forecast_benchmark_plot)

# ============================================================
# 3. NAIVE BETTING STRATEGIES: AVERAGE AND MAXIMUM ODDS
# ============================================================
naive_base <- analysis_features |>
  transmute(
    match_id_analysis, LeagueName, SeasonStart, SeasonLabel, actual_outcome = FTR,
    p_home = mkt_pre_p_home, p_draw = mkt_pre_p_draw, p_away = mkt_pre_p_away,
    avg_home = odds_avg_pre_home, avg_draw = odds_avg_pre_draw, avg_away = odds_avg_pre_away,
    max_home = odds_max_pre_home, max_draw = odds_max_pre_draw, max_away = odds_max_pre_away
  ) |>
  filter(actual_outcome %in% c("H", "D", "A"),
         if_all(c(p_home, p_draw, p_away, avg_home, avg_draw, avg_away,
                  max_home, max_draw, max_away), ~ is.finite(.x) & .x > 0)) |>
  mutate(
    favourite = c("H", "D", "A")[max.col(cbind(p_home, p_draw, p_away), ties.method = "first")],
    longshot = c("H", "D", "A")[max.col(-cbind(p_home, p_draw, p_away), ties.method = "first")]
  )

naive_bets <- tidyr::expand_grid(naive_base, strategy = strategy_order) |>
  mutate(
    selected_outcome = case_when(
      strategy == "Always home" ~ "H", strategy == "Always draw" ~ "D",
      strategy == "Always away" ~ "A", strategy == "Favourite" ~ favourite,
      strategy == "Longshot" ~ longshot
    ),
    selected_avg_odds = case_when(
      selected_outcome == "H" ~ avg_home, selected_outcome == "D" ~ avg_draw,
      selected_outcome == "A" ~ avg_away
    ),
    selected_max_odds = case_when(
      selected_outcome == "H" ~ max_home, selected_outcome == "D" ~ max_draw,
      selected_outcome == "A" ~ max_away
    ),
    win = as.integer(selected_outcome == actual_outcome),
    return_avg = if_else(win == 1L, selected_avg_odds - 1, -1),
    return_max = if_else(win == 1L, selected_max_odds - 1, -1),
    strategy = factor(strategy, levels = strategy_order)
  )

naive_strategy_overall <- naive_bets |>
  group_by(strategy) |>
  summarise(bets = n(), hit_rate = mean(win), mean_avg_odds = mean(selected_avg_odds),
            roi_avg = mean(return_avg), mean_max_odds = mean(selected_max_odds),
            roi_max = mean(return_max), roi_gain = roi_max - roi_avg, .groups = "drop")

naive_strategy_cells <- naive_bets |>
  group_by(strategy, LeagueName, SeasonStart) |>
  summarise(roi_avg = mean(return_avg), roi_max = mean(return_max), .groups = "drop")

naive_strategy_stability <- naive_strategy_cells |>
  group_by(strategy) |>
  summarise(cells = n(), profitable_avg = sum(roi_avg > 0),
            profitable_max = sum(roi_max > 0), median_avg = median(roi_avg),
            median_max = median(roi_max), .groups = "drop")

naive_strategy_display <- naive_strategy_overall |>
  transmute(
    Strategy = as.character(strategy), `Hit rate` = scales::percent(hit_rate, accuracy = 0.1),
    avg_odds = sprintf("%.2f", mean_avg_odds), avg_roi = scales::percent(roi_avg, accuracy = 0.1),
    max_odds = sprintf("%.2f", mean_max_odds), max_roi = scales::percent(roi_max, accuracy = 0.1),
    gain = scales::percent(roi_gain, accuracy = 0.1)
  )

# The grouped header deliberately keeps the visible column names short.
naive_strategy_latex <- knitr::kable(
  naive_strategy_display, format = "latex", booktabs = TRUE, escape = FALSE,
  col.names = c("Strategy", "Hit rate", "Odds", "ROI", "Odds", "ROI", "$\\Delta$ ROI"),
  align = c("l", "r", "r", "r", "r", "r", "r"),
  caption = paste("Returns from naive betting strategies at average market odds",
                  "and maximum available odds."),
  label = "tab:naive-strategy-returns"
) |>
  kableExtra::add_header_above(c(" " = 2, "Average price" = 2,
                                  "Maximum price" = 2, " " = 1)) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"),
                            position = "center")

cat("\nNAIVE STRATEGIES: OVERALL\n")
print(naive_strategy_overall, n = Inf, width = Inf)
cat("\nNAIVE STRATEGIES: LEAGUE-SEASON STABILITY\n")
print(naive_strategy_stability, n = Inf, width = Inf)
cat("\nLATEX: NAIVE STRATEGIES\n", naive_strategy_latex, "\n")

# ============================================================
# 3.1 RANDOM-BETTING SIMULATION
# ============================================================
# This is a Monte Carlo no-information benchmark, not a bootstrap.
simulate_random_betting <- function(data, simulations = 10000, seed = 666) {
  set.seed(seed)
  outcomes <- c("H", "D", "A")
  realised <- match(data$actual_outcome, outcomes)
  avg_odds <- as.matrix(data[, c("avg_home", "avg_draw", "avg_away")])
  max_odds <- as.matrix(data[, c("max_home", "max_draw", "max_away")])
  rows <- seq_len(nrow(data))

  simulated <- t(vapply(seq_len(simulations), function(i) {
    selection <- sample.int(3L, nrow(data), replace = TRUE)
    won <- selection == realised
    c(
      average = mean(ifelse(won, avg_odds[cbind(rows, selection)] - 1, -1)),
      maximum = mean(ifelse(won, max_odds[cbind(rows, selection)] - 1, -1))
    )
  }, numeric(2)))

  as_tibble(simulated) |>
    mutate(simulation = row_number()) |>
    pivot_longer(c(average, maximum), names_to = "price_basis", values_to = "roi") |>
    mutate(price_basis = recode(price_basis,
                                average = "Average market odds",
                                maximum = "Maximum available odds"))
}

random_betting_results <- simulate_random_betting(naive_base)
random_betting_summary <- random_betting_results |>
  group_by(price_basis) |>
  summarise(mean_roi = mean(roi), lower_95 = quantile(roi, 0.025),
            upper_95 = quantile(roi, 0.975), .groups = "drop")

random_betting_plot <- ggplot(random_betting_results,
                              aes(x = roi, fill = price_basis, colour = price_basis)) +
  geom_density(alpha = 0.18, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35") +
  geom_vline(data = random_betting_summary, aes(xintercept = mean_roi, colour = price_basis),
             linewidth = 0.8, show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Average market odds" = "grey65",
                               "Maximum available odds" = accent_col)) +
  scale_colour_manual(values = c("Average market odds" = "grey35",
                                 "Maximum available odds" = accent_col)) +
  labs(x = "Return on investment", y = "Density", fill = NULL, colour = NULL) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(legend.position = "bottom")

cat("\nRANDOM-BETTING SIMULATION\n")
print(random_betting_summary, n = Inf, width = Inf)
print(random_betting_plot)

# ============================================================
# 4. DESCRIPTIVE CALIBRATION IN FIXED 5% BINS
# ============================================================
# Home, draw and away observations are deliberately pooled here. Formal joint
# and outcome-specific calibration tests belong in Section 6.2.
bin_width <- 0.05
calibration_long <- analysis_features |>
  filter(FTR %in% c("H", "D", "A")) |>
  transmute(FTR, H = mkt_pre_p_home, D = mkt_pre_p_draw, A = mkt_pre_p_away) |>
  pivot_longer(c(H, D, A), names_to = "outcome", values_to = "probability") |>
  filter(is.finite(probability), probability >= 0, probability <= 1) |>
  mutate(realised = as.integer(FTR == outcome),
         bin_lower = pmin(floor(probability / bin_width) * bin_width, 1 - bin_width))

calibration_bins <- calibration_long |>
  group_by(bin_lower) |>
  summarise(observations = n(), mean_probability = mean(probability),
            realised_frequency = mean(realised),
            calibration_gap = realised_frequency - mean_probability,
            .groups = "drop") |>
  filter(observations >= 30) |>
  arrange(bin_lower)

calibration_plot <- ggplot(
  calibration_bins, aes(x = mean_probability, y = realised_frequency)
) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55",
              linetype = "dashed", linewidth = 0.7) +
  geom_line(colour = accent_col, linewidth = 0.9) +
  geom_point(aes(size = observations), colour = accent_col) +
  scale_size_continuous(range = c(1.5, 4.2), guide = "none") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  coord_equal() +
  labs(x = "Mean de-vigged market probability", y = "Realised outcome frequency") +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank())

cat("\nDESCRIPTIVE CALIBRATION: FIXED 5% BINS\n")
print(calibration_bins, n = Inf, width = Inf)
print(calibration_plot)
