# STEP 04a — Descriptive figures
# Reads:  analysis_features.rds
# Writes: figures
# Thesis: Figure 1 (bookmaker margin by season) and Figure 2 (descriptive
#         calibration). Also contains exploratory output not used.

# ============================================================
# RUN DESCRIPTIVE ANALYSIS SUITE
# Top-5 European leagues, 2014/15-2024/25
# ============================================================

# Required input:
#   analysis_features.rds, created by 03_build_feature_dataset.R
#
#Main graphs used are marked with "USED IN THESIS"

# Main sections:
#   1. Market forecast benchmarks
#   2. Favourite-longshot calibration
#   3. xG validity checks
#   4. xG versus shots and shots on target
#   5. Market and bookmaker margin structure
#   6. Outcome base rates and home advantage
#   7. Pi-rating validity
#   8. Correlation among candidate predictors
# ============================================================

# ------------------------------------------------------------
# 0. Packages and paths
# ------------------------------------------------------------
required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "tibble",
  "stringr",
  "purrr",
  "broom"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tibble)
library(stringr)
library(purrr)
library(broom)

project_dir <- Sys.getenv("THESIS_PROJECT_DIR", unset = ".")
analysis_dir <- file.path(project_dir, "analysis_test")
data_dir <- file.path(analysis_dir, "data")
tables_dir <- file.path(analysis_dir, "tables", "descriptives")
figures_dir <- file.path(analysis_dir, "figures", "descriptives")

feature_input_path <- file.path(
  data_dir,
  "analysis_features.rds"
)

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(feature_input_path)) {
  stop(
    "Feature dataset not found. Run 03_build_feature_dataset.R first: ",
    feature_input_path
  )
}

analysis_features <- readr::read_rds(feature_input_path) %>%
  mutate(MatchDate = as.Date(MatchDate)) %>%
  arrange(LeagueKey, MatchDate, match_id_analysis)


# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------
clip_probability <- function(p, epsilon = 1e-15) {
  pmin(pmax(p, epsilon), 1 - epsilon)
}

score_forecasts <- function(data, label, p_home, p_draw, p_away) {
  p_home <- rlang::enquo(p_home)
  p_draw <- rlang::enquo(p_draw)
  p_away <- rlang::enquo(p_away)

  data %>%
    transmute(
      forecast = label,
      FTR,
      p_home = !!p_home,
      p_draw = !!p_draw,
      p_away = !!p_away,
      y_home = as.integer(FTR == "H"),
      y_draw = as.integer(FTR == "D"),
      y_away = as.integer(FTR == "A")
    ) %>%
    filter(
      !is.na(p_home),
      !is.na(p_draw),
      !is.na(p_away),
      FTR %in% c("H", "D", "A")
    ) %>%
    mutate(
      prediction = case_when(
        p_home >= p_draw & p_home >= p_away ~ "H",
        p_draw > p_home & p_draw >= p_away ~ "D",
        TRUE ~ "A"
      ),
      correct = prediction == FTR,
      rps = (
        (p_home - y_home)^2 +
          ((p_home + p_draw) - (y_home + y_draw))^2
      ) / 2,
      brier =
        (p_home - y_home)^2 +
        (p_draw - y_draw)^2 +
        (p_away - y_away)^2,
      realised_probability = case_when(
        FTR == "H" ~ p_home,
        FTR == "D" ~ p_draw,
        FTR == "A" ~ p_away
      ),
      log_loss = -log(clip_probability(realised_probability))
    )
}

model_metrics <- function(model, data, outcome) {
  fitted_values <- predict(model, newdata = data)
  residuals <- data[[outcome]] - fitted_values

  tibble(
    n = nrow(data),
    slope = if (length(coef(model)) == 2) unname(coef(model)[2]) else NA_real_,
    intercept = unname(coef(model)[1]),
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    rmse = sqrt(mean(residuals^2)),
    aic = AIC(model)
  )
}

save_table <- function(data, filename) {
  readr::write_csv(data, file.path(tables_dir, filename))
}

save_plot <- function(plot, filename, width = 9, height = 6) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# ============================================================
# 1. MARKET EFFICIENCY BASELINES
# ============================================================
# ------------------------------------------------------------
# 1.1 League-specific climatological probabilities
# ------------------------------------------------------------
league_climatology <- analysis_features %>%
  group_by(LeagueKey, LeagueName) %>%
  summarise(
    clim_p_home = mean(FTR == "H", na.rm = TRUE),
    clim_p_draw = mean(FTR == "D", na.rm = TRUE),
    clim_p_away = mean(FTR == "A", na.rm = TRUE),
    .groups = "drop"
  )

benchmark_data <- analysis_features %>%
  left_join(
    league_climatology %>%
      select(
        LeagueKey,
        clim_p_home,
        clim_p_draw,
        clim_p_away
      ),
    by = "LeagueKey"
  ) %>%
  mutate(
    favourite = case_when(
      mkt_pre_p_home >= mkt_pre_p_draw &
        mkt_pre_p_home >= mkt_pre_p_away ~ "H",
      mkt_pre_p_draw > mkt_pre_p_home &
        mkt_pre_p_draw >= mkt_pre_p_away ~ "D",
      TRUE ~ "A"
    ),
    fav_p_home = as.numeric(favourite == "H"),
    fav_p_draw = as.numeric(favourite == "D"),
    fav_p_away = as.numeric(favourite == "A")
  )

# Create the three forecast datasets with identifiers retained.
market_scores <- benchmark_data %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    FTR,
    p_home = mkt_pre_p_home,
    p_draw = mkt_pre_p_draw,
    p_away = mkt_pre_p_away,
    forecast = "Average market probabilities"
  )

favourite_scores <- benchmark_data %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    FTR,
    p_home = fav_p_home,
    p_draw = fav_p_draw,
    p_away = fav_p_away,
    forecast = "Always favourite"
  )

climatology_scores <- benchmark_data %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    FTR,
    p_home = clim_p_home,
    p_draw = clim_p_draw,
    p_away = clim_p_away,
    forecast = "League climatology"
  )

benchmark_scores <- bind_rows(
  market_scores, favourite_scores, climatology_scores
) %>%
  filter(!is.na(p_home), !is.na(p_draw), !is.na(p_away),
         FTR %in% c("H", "D", "A")) %>%
  mutate(
    y_home = as.integer(FTR == "H"),
    y_draw = as.integer(FTR == "D"),
    prediction = case_when(
      p_home >= p_draw & p_home >= p_away ~ "H",
      p_draw >  p_home & p_draw >= p_away ~ "D",
      TRUE ~ "A"
    ),
    correct = prediction == FTR,
    rps = ((p_home - y_home)^2 +
             ((p_home + p_draw) - (y_home + y_draw))^2) / 2
  )

benchmark_overall <- benchmark_scores %>%
  group_by(forecast) %>%
  summarise(
    matches  = n(),
    accuracy = mean(correct),
    mean_rps = mean(rps),
    .groups = "drop"
  ) %>%
  arrange(mean_rps)   # keep for overall & by-season; by-league arranges (LeagueName, mean_rps)

benchmark_by_league <- benchmark_scores %>%
  group_by(LeagueKey, LeagueName, forecast) %>%
  summarise(
    matches  = n(),
    accuracy = mean(correct),
    mean_rps = mean(rps),
    .groups = "drop"
  ) %>%
  arrange(mean_rps)   # keep for overall & by-season; by-league arranges (LeagueName, mean_rps)

benchmark_by_season <- benchmark_scores %>%
  group_by(SeasonStart, SeasonLabel, forecast) %>%
  summarise(
    matches  = n(),
    accuracy = mean(correct),
    mean_rps = mean(rps),
    .groups = "drop"
  ) %>%
  arrange(mean_rps)   # keep for overall & by-season; by-league arranges (LeagueName, mean_rps)


#Displaying benchmarks
cat("\nMARKET BENCHMARKS — OVERALL\n")
print(
  benchmark_overall,
  n = Inf,
  width = Inf
)

cat("\nMARKET BENCHMARKS — BY LEAGUE\n")
print(
  benchmark_by_league,
  n = Inf,
  width = Inf
)

cat("\nMARKET BENCHMARKS — BY SEASON\n")
print(
  benchmark_by_season,
  n = Inf,
  width = Inf
)


# ============================================================
# 2. FAVOURITE-LONGSHOT CALIBRATION
# ============================================================
calibration_long <- analysis_features %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    FTR,
    Home = mkt_pre_p_home,
    Draw = mkt_pre_p_draw,
    Away = mkt_pre_p_away
  ) %>%
  pivot_longer(
    cols = c(Home, Draw, Away),
    names_to = "outcome",
    values_to = "implied_probability"
  ) %>%
  mutate(
    realised = case_when(
      outcome == "Home" ~ as.integer(FTR == "H"),
      outcome == "Draw" ~ as.integer(FTR == "D"),
      outcome == "Away" ~ as.integer(FTR == "A")
    )
  ) %>%
  filter(!is.na(implied_probability))

calibration_overall <- calibration_long %>%
  group_by(outcome) %>%
  mutate(probability_decile = ntile(implied_probability, 10)) %>%
  group_by(outcome, probability_decile) %>%
  summarise(
    observations = n(),
    mean_implied_probability = mean(implied_probability),
    realised_frequency = mean(realised),
    calibration_gap = realised_frequency - mean_implied_probability,
    .groups = "drop"
  )

calibration_by_league <- calibration_long %>%
  group_by(LeagueKey, LeagueName, outcome) %>%
  mutate(probability_decile = ntile(implied_probability, 10)) %>%
  group_by(
    LeagueKey,
    LeagueName,
    outcome,
    probability_decile
  ) %>%
  summarise(
    observations = n(),
    mean_implied_probability = mean(implied_probability),
    realised_frequency = mean(realised),
    calibration_gap = realised_frequency - mean_implied_probability,
    .groups = "drop"
  )

calibration_plot_overall <- ggplot(
  calibration_overall,
  aes(
    x = mean_implied_probability,
    y = realised_frequency
  )
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line() +
  facet_wrap(~ outcome) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Market calibration by outcome",
    x = "Mean de-vigged implied probability",
    y = "Realised outcome frequency"
  ) +
  theme_minimal()

calibration_plot_league <- ggplot(
  calibration_by_league,
  aes(
    x = mean_implied_probability,
    y = realised_frequency
  )
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line() +
  facet_grid(LeagueName ~ outcome) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Market calibration by league and outcome",
    x = "Mean de-vigged implied probability",
    y = "Realised outcome frequency"
  ) +
  theme_minimal()

save_table(calibration_overall, "02_favourite_longshot_calibration_overall.csv")
save_table(calibration_by_league, "02_favourite_longshot_calibration_by_league.csv")
save_plot(calibration_plot_overall, "02_calibration_overall.png", 9, 5)
save_plot(calibration_plot_league, "02_calibration_by_league.png", 11, 13)

cat("\nFAVOURITE-LONGSHOT CALIBRATION — OVERALL\n")
print(
  calibration_overall,
  n = Inf,
  width = Inf
)

cat("\nFAVOURITE-LONGSHOT CALIBRATION — BY LEAGUE\n")
print(
  calibration_by_league,
  n = Inf,
  width = Inf
)

print(calibration_plot_overall)
print(calibration_plot_league)


# ------------------------------------------------------------
# Overall favourite-longshot calibration
# Pool home, draw and away outcomes by implied probability
# ------------------------------------------------------------

favourite_longshot_overall <- calibration_long %>%
  mutate(
    probability_decile = ntile(implied_probability, 10)
  ) %>%
  group_by(probability_decile) %>%
  summarise(
    observations = n(),
    mean_implied_probability = mean(implied_probability),
    realised_frequency = mean(realised),
    calibration_gap =
      realised_frequency - mean_implied_probability,
    .groups = "drop"
  ) %>%
  arrange(probability_decile)


favourite_longshot_plot <- ggplot(
  favourite_longshot_overall,
  aes(
    x = mean_implied_probability,
    y = realised_frequency
  )
) +
  # Perfect calibration
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_line(
    aes(group = 1),
    linewidth = 0.8
  ) +
  geom_point(
    size = 3
  ) +
  coord_equal(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Favourite–longshot calibration",
    subtitle = paste(
      "All home, draw and away outcomes pooled into",
      "deciles of de-vigged implied probability"
    ),
    x = "Mean implied probability",
    y = "Realised outcome frequency",
    caption = "Dashed line represents perfect calibration"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )


# Display the table and plot
print(
  favourite_longshot_overall,
  n = Inf,
  width = Inf
)

print(favourite_longshot_plot)

# ============================================================
# 3. xG AS A STRENGTH SIGNAL
# ============================================================
# ------------------------------------------------------------
# 3.1 Same-match concordance
# ------------------------------------------------------------
xg_concordance <- analysis_features %>%
  mutate(
    xg_diff_match = home_xG - away_xG,
    goal_diff_match = FTHG - FTAG,
    higher_xg_side = case_when(
      xg_diff_match > 0 ~ "H",
      xg_diff_match < 0 ~ "A",
      TRUE ~ "Tie"
    ),
    higher_xg_side_won = case_when(
      higher_xg_side == "H" ~ FTR == "H",
      higher_xg_side == "A" ~ FTR == "A",
      TRUE ~ NA
    ),
    higher_xg_side_scored_more = case_when(
      higher_xg_side == "H" ~ FTHG > FTAG,
      higher_xg_side == "A" ~ FTAG > FTHG,
      TRUE ~ NA
    ),
    xg_and_goal_direction_agree = case_when(
      xg_diff_match == 0 | goal_diff_match == 0 ~ NA,
      sign(xg_diff_match) == sign(goal_diff_match) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  summarise(
    matches = n(),
    non_tied_xg_matches = sum(higher_xg_side != "Tie"),
    higher_xg_team_win_rate = mean(higher_xg_side_won, na.rm = TRUE),
    higher_xg_team_scored_more_rate = mean(
      higher_xg_side_scored_more,
      na.rm = TRUE
    ),
    xg_goal_direction_agreement = mean(
      xg_and_goal_direction_agree,
      na.rm = TRUE
    )
  )

# ------------------------------------------------------------
# 3.2 Season-team cross-sectional validity
# ------------------------------------------------------------
team_season_xg <- bind_rows(
  analysis_features %>%
    transmute(
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      team = HomeTeam,
      goals_for = FTHG,
      goals_against = FTAG,
      xg_for = home_xG,
      xg_against = away_xG
    ),
  analysis_features %>%
    transmute(
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      team = AwayTeam,
      goals_for = FTAG,
      goals_against = FTHG,
      xg_for = away_xG,
      xg_against = home_xG
    )
) %>%
  group_by(
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    team
  ) %>%
  summarise(
    matches = n(),
    goals_for = sum(goals_for),
    goals_against = sum(goals_against),
    goal_diff = goals_for - goals_against,
    xg_for = sum(xg_for),
    xg_against = sum(xg_against),
    xg_diff = xg_for - xg_against,
    .groups = "drop"
  )

model_goals_on_xg <- lm(goals_for ~ xg_for, data = team_season_xg)
model_gd_on_xgd <- lm(goal_diff ~ xg_diff, data = team_season_xg)

xg_cross_sectional_models <- bind_rows(
  model_metrics(
    model_goals_on_xg,
    team_season_xg,
    "goals_for"
  ) %>%
    mutate(model = "Season goals for on season xG for"),
  model_metrics(
    model_gd_on_xgd,
    team_season_xg,
    "goal_diff"
  ) %>%
    mutate(model = "Season goal difference on season xG difference")
) %>%
  select(model, everything())

# ------------------------------------------------------------
# 3.3 Predictive validity of lagged rolling xG versus goals
# ------------------------------------------------------------
predictive_xg_comparison <- map_dfr(
  c(3L, 5L),
  function(window) {
    xg_variable <- paste0("matchup_xg_diff_", window)
    goal_variable <- paste0("matchup_goal_diff_", window)
    
    common_data <- analysis_features %>%
      transmute(
        future_goal_diff = FTHG - FTAG,
        rolling_xg_diff = .data[[xg_variable]],
        rolling_goal_diff = .data[[goal_variable]]
      ) %>%
      drop_na()
    
    xg_model <- lm(
      future_goal_diff ~ rolling_xg_diff,
      data = common_data
    )
    
    goal_model <- lm(
      future_goal_diff ~ rolling_goal_diff,
      data = common_data
    )
    
    bind_rows(
      model_metrics(
        xg_model,
        common_data,
        "future_goal_diff"
      ) %>%
        mutate(window = window, predictor = "Rolling xG difference"),
      model_metrics(
        goal_model,
        common_data,
        "future_goal_diff"
      ) %>%
        mutate(window = window, predictor = "Rolling actual goal difference")
    )
  }
) %>%
  select(window, predictor, everything())

save_table(xg_concordance, "03_xg_concordance.csv")
save_table(team_season_xg, "03_team_season_xg_aggregates.csv")
save_table(xg_cross_sectional_models, "03_xg_cross_sectional_models.csv")
save_table(predictive_xg_comparison, "03_xg_predictive_comparison.csv")


# ------------------------------------------------------------
# 3.4 Display xG validity results
# ------------------------------------------------------------
cat("\nxG CONCORDANCE\n")
print(
  xg_concordance,
  width = Inf
)

cat("\nxG CROSS-SECTIONAL MODELS\n")
print(
  xg_cross_sectional_models,
  n = Inf,
  width = Inf
)

cat("\nROLLING xG VERSUS ROLLING GOALS — PREDICTIVE VALIDITY\n")
print(
  predictive_xg_comparison,
  n = Inf,
  width = Inf
)


# ============================================================
# 4. DOES xG ADD INFORMATION BEYOND SHOTS?
# ============================================================
shot_xg_correlations <- map_dfr(
  c(3L, 5L),
  function(window) {
    variables <- c(
      paste0("matchup_xg_diff_", window),
      paste0("matchup_shot_diff_", window),
      paste0("matchup_sot_diff_", window)
    )
    
    correlation_matrix <- analysis_features %>%
      select(all_of(variables)) %>%
      cor(use = "pairwise.complete.obs")
    
    as.data.frame(correlation_matrix) %>%
      rownames_to_column("variable_1") %>%
      pivot_longer(
        -variable_1,
        names_to = "variable_2",
        values_to = "correlation"
      ) %>%
      mutate(window = window, .before = 1)
  }
)

shot_xg_incremental_models <- map_dfr(
  c(3L, 5L),
  function(window) {
    xg_variable <- paste0("matchup_xg_diff_", window)
    shot_variable <- paste0("matchup_shot_diff_", window)
    sot_variable <- paste0("matchup_sot_diff_", window)
    
    common_data <- analysis_features %>%
      transmute(
        future_goal_diff = FTHG - FTAG,
        rolling_xg_diff = .data[[xg_variable]],
        rolling_shot_diff = .data[[shot_variable]],
        rolling_sot_diff = .data[[sot_variable]]
      ) %>%
      drop_na()
    
    models <- list(
      "Shots only" = lm(
        future_goal_diff ~ rolling_shot_diff,
        data = common_data
      ),
      "Shots plus xG" = lm(
        future_goal_diff ~ rolling_shot_diff + rolling_xg_diff,
        data = common_data
      ),
      "Shots on target only" = lm(
        future_goal_diff ~ rolling_sot_diff,
        data = common_data
      ),
      "Shots on target plus xG" = lm(
        future_goal_diff ~ rolling_sot_diff + rolling_xg_diff,
        data = common_data
      )
    )
    
    imap_dfr(
      models,
      ~ model_metrics(.x, common_data, "future_goal_diff") %>%
        mutate(window = window, model = .y)
    )
  }
) %>%
  select(window, model, everything())

save_table(shot_xg_correlations, "04_xg_shot_correlations.csv")
save_table(shot_xg_incremental_models, "04_xg_incremental_models.csv")


# ------------------------------------------------------------
# 4.1 Display xG-versus-shots results
# ------------------------------------------------------------
cat("\nCORRELATIONS BETWEEN ROLLING xG, SHOTS AND SHOTS ON TARGET\n")
print(
  shot_xg_correlations,
  n = Inf,
  width = Inf
)

cat("\nINCREMENTAL PREDICTIVE VALUE OF xG BEYOND SHOTS\n")
print(
  shot_xg_incremental_models,
  n = Inf,
  width = Inf
)

# ============================================================
# 5. MARGIN STRUCTURE OVER TIME AND BY BOOKMAKER
# ============================================================
market_margin_by_season <- analysis_features %>%
  group_by(SeasonStart, SeasonLabel) %>%
  summarise(
    matches = sum(!is.na(mkt_pre_margin)),
    avg_market_margin = mean(mkt_pre_margin, na.rm = TRUE),
    median_market_margin = median(mkt_pre_margin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SeasonStart)

market_margin_by_league_season <- analysis_features %>%
  group_by(
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel
  ) %>%
  summarise(
    matches = sum(!is.na(mkt_pre_margin)),
    avg_market_margin = mean(mkt_pre_margin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(LeagueName, SeasonStart)

bookmaker_lookup <- tribble(
  ~bookmaker,      ~home_col, ~draw_col, ~away_col,
  "Bet365",        "B365H",   "B365D",   "B365A",
  "Bwin",          "BWH",     "BWD",     "BWA",
  "Interwetten",   "IWH",     "IWD",     "IWA",
  "William Hill",  "WHH",     "WHD",     "WHA",
  "VC Bet",        "VCH",     "VCD",     "VCA",
  "Pinnacle",      "PSH",     "PSD",     "PSA"
) %>%
  filter(home_col %in% names(analysis_features),
         draw_col %in% names(analysis_features),
         away_col %in% names(analysis_features))

bookmaker_margin_long <- pmap_dfr(
  bookmaker_lookup,
  function(bookmaker, home_col, draw_col, away_col) {
    analysis_features %>%
      transmute(
        LeagueKey,
        LeagueName,
        SeasonStart,
        SeasonLabel,
        bookmaker = bookmaker,
        margin =
          1 / .data[[home_col]] +
          1 / .data[[draw_col]] +
          1 / .data[[away_col]] - 1
      ) %>%
      filter(is.finite(margin))
  }
)

bookmaker_margin_by_season <- bookmaker_margin_long %>%
  group_by(bookmaker, SeasonStart, SeasonLabel) %>%
  summarise(matches = n(),
            avg_margin = mean(margin),
            median_margin = median(margin),
            .groups = "drop") %>%
  filter(matches >= 200) %>%          # drop thin book-seasons
  arrange(bookmaker, SeasonStart)

pinnacle_vs_market <- analysis_features %>%
  mutate(
    pinnacle_margin = 1 / PSH + 1 / PSD + 1 / PSA - 1
  ) %>%
  group_by(SeasonStart, SeasonLabel) %>%
  summarise(
    market_matches = sum(!is.na(mkt_pre_margin)),
    pinnacle_matches = sum(is.finite(pinnacle_margin)),
    avg_market_margin = mean(mkt_pre_margin, na.rm = TRUE),
    avg_pinnacle_margin = mean(pinnacle_margin, na.rm = TRUE),
    margin_difference = avg_market_margin - avg_pinnacle_margin,
    .groups = "drop"
  ) %>%
  arrange(SeasonStart)

margin_plot <- ggplot(
  pinnacle_vs_market,
  aes(x = SeasonStart)
) +
  geom_line(aes(y = avg_market_margin, linetype = "Market average")) +
  geom_point(aes(y = avg_market_margin, shape = "Market average")) +
  geom_line(aes(y = avg_pinnacle_margin, linetype = "Pinnacle")) +
  geom_point(aes(y = avg_pinnacle_margin, shape = "Pinnacle")) +
  scale_x_continuous(breaks = pinnacle_vs_market$SeasonStart) +
  labs(
    title = "Pre-match bookmaker margin over time",
    x = "Season start year",
    y = "Average overround margin",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal()

save_table(market_margin_by_season, "05_market_margin_by_season.csv")
save_table(
  market_margin_by_league_season,
  "05_market_margin_by_league_season.csv"
)
save_table(bookmaker_margin_by_season, "05_bookmaker_margin_by_season.csv")
save_table(pinnacle_vs_market, "05_pinnacle_vs_market_margin.csv")
save_plot(margin_plot, "05_margin_over_time.png", 9, 5)


# ------------------------------------------------------------
# 5.1 Display margin results
# ------------------------------------------------------------

cat("\nMARKET MARGIN BY SEASON\n")
print(
  market_margin_by_season,
  n = Inf,
  width = Inf
)

cat("\nMARKET MARGIN BY LEAGUE AND SEASON\n")
print(
  market_margin_by_league_season,
  n = Inf,
  width = Inf
)

cat("\nBOOKMAKER MARGIN BY BOOKMAKER AND SEASON\n")
print(
  bookmaker_margin_by_season,
  n = Inf,
  width = Inf
)

cat("\nPINNACLE VERSUS MARKET-AVERAGE MARGIN\n")
print(
  pinnacle_vs_market,
  n = Inf,
  width = Inf
)

print(margin_plot)


# ------------------------------------------------------------
# Bookmaker margins over time
# ------------------------------------------------------------
bookmaker_margin_plot <- ggplot(
  bookmaker_margin_by_season,
  aes(
    x = SeasonStart,
    y = avg_margin,
    group = bookmaker,
    colour = bookmaker
  )
) +
  geom_line(
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = sort(
      unique(bookmaker_margin_by_season$SeasonStart)
    ),
    labels = bookmaker_margin_by_season %>%
      distinct(SeasonStart, SeasonLabel) %>%
      arrange(SeasonStart) %>%
      pull(SeasonLabel)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 0.1
    )
  ) +
  labs(
    title = "Pre-match bookmaker margins over time",
    subtitle = "Average 1X2 overround by bookmaker and season",
    x = "Season",
    y = "Average margin",
    colour = "Bookmaker"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(bookmaker_margin_plot)





# ============================================================
# 6. OUTCOME BASE RATES AND HOME ADVANTAGE
# ============================================================
home_advantage_by_league <- analysis_features %>%
  group_by(LeagueKey, LeagueName) %>%
  summarise(
    matches = n(),
    home_win_share = mean(FTR == "H"),
    draw_share = mean(FTR == "D"),
    away_win_share = mean(FTR == "A"),
    mean_home_goals = mean(FTHG),
    mean_away_goals = mean(FTAG),
    mean_goal_advantage = mean(FTHG - FTAG),
    mean_home_xg = mean(home_xG),
    mean_away_xg = mean(away_xG),
    mean_xg_advantage = mean(home_xG - away_xG),
    .groups = "drop"
  ) %>%
  arrange(LeagueName)

home_advantage_by_league_season <- analysis_features %>%
  group_by(
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel
  ) %>%
  summarise(
    matches = n(),
    home_win_share = mean(FTR == "H"),
    draw_share = mean(FTR == "D"),
    away_win_share = mean(FTR == "A"),
    mean_goal_advantage = mean(FTHG - FTAG),
    mean_xg_advantage = mean(home_xG - away_xG),
    .groups = "drop"
  ) %>%
  mutate(
    covid_season = SeasonLabel %in% c("2019/20", "2020/21")
  ) %>%
  arrange(LeagueName, SeasonStart)

home_advantage_plot <- ggplot(
  home_advantage_by_league_season,
  aes(
    x = SeasonStart,
    y = home_win_share,
    group = LeagueName
  )
) +
  geom_line() +
  geom_point(aes(shape = covid_season)) +
  facet_wrap(~ LeagueName) +
  scale_x_continuous(
    breaks = sort(unique(home_advantage_by_league_season$SeasonStart))
  ) +
  labs(
    title = "Home-win share by league and season",
    x = "Season start year",
    y = "Home-win share",
    shape = "2019/20 or 2020/21"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_table(home_advantage_by_league, "06_home_advantage_by_league.csv")
save_table(
  home_advantage_by_league_season,
  "06_home_advantage_by_league_season.csv"
)
save_plot(home_advantage_plot, "06_home_win_share_by_season.png", 11, 7)

#plots, showing tables and desciptives
cat("\nOUTCOME BASE RATES AND HOME ADVANTAGE BY LEAGUE\n")
print(
  home_advantage_by_league,
  n = Inf,
  width = Inf
)

cat("\nOUTCOME BASE RATES AND HOME ADVANTAGE BY LEAGUE AND SEASON\n")
print(
  home_advantage_by_league_season,
  n = Inf,
  width = Inf
)

cat("\nHOME-WIN SHARE OVER TIME\n")
print(home_advantage_plot)


# ============================================================
# 7. PI-RATING VALIDITY CHECK
# ============================================================
pi_calibration_overall <- analysis_features %>%
  filter(!is.na(pi_rating_diff_pre)) %>%
  mutate(pi_bin = ntile(pi_rating_diff_pre, 10)) %>%
  group_by(pi_bin) %>%
  summarise(
    matches = n(),
    mean_pi_diff = mean(pi_rating_diff_pre),
    home_win_rate = mean(FTR == "H"),
    draw_rate = mean(FTR == "D"),
    away_win_rate = mean(FTR == "A"),
    .groups = "drop"
  )

pi_calibration_by_league <- analysis_features %>%
  filter(!is.na(pi_rating_diff_pre)) %>%
  group_by(LeagueKey, LeagueName) %>%
  mutate(pi_bin = ntile(pi_rating_diff_pre, 10)) %>%
  group_by(LeagueKey, LeagueName, pi_bin) %>%
  summarise(
    matches = n(),
    mean_pi_diff = mean(pi_rating_diff_pre),
    home_win_rate = mean(FTR == "H"),
    .groups = "drop"
  )

pi_plot_overall <- ggplot(
  pi_calibration_overall,
  aes(x = mean_pi_diff, y = home_win_rate)
) +
  geom_point() +
  geom_line() +
  labs(
    title = "Pi-rating difference and realised home-win rate",
    x = "Mean pre-match Pi-rating difference",
    y = "Realised home-win rate"
  ) +
  theme_minimal()

pi_plot_league <- ggplot(
  pi_calibration_by_league,
  aes(x = mean_pi_diff, y = home_win_rate)
) +
  geom_point() +
  geom_line() +
  facet_wrap(~ LeagueName) +
  labs(
    title = "Pi-rating validity by league",
    x = "Mean pre-match Pi-rating difference",
    y = "Realised home-win rate"
  ) +
  theme_minimal()

save_table(pi_calibration_overall, "07_pi_calibration_overall.csv")
save_table(pi_calibration_by_league, "07_pi_calibration_by_league.csv")
save_plot(pi_plot_overall, "07_pi_calibration_overall.png", 8, 5)
save_plot(pi_plot_league, "07_pi_calibration_by_league.png", 10, 7)

cat("\nPI-RATING CALIBRATION — OVERALL\n")
print(
  pi_calibration_overall,
  n = Inf,
  width = Inf
)

cat("\nPI-RATING CALIBRATION — BY LEAGUE\n")
print(
  pi_calibration_by_league,
  n = Inf,
  width = Inf
)

cat("\nPI-RATING CALIBRATION — OVERALL PLOT\n")
print(pi_plot_overall)

cat("\nPI-RATING CALIBRATION — BY LEAGUE PLOT\n")
print(pi_plot_league)

# ============================================================
# 8. CORRELATION AMONG CANDIDATE PREDICTORS
# ============================================================
candidate_predictors <- c(
  "pi_rating_diff_pre",
  "mkt_pre_p_home",
  "matchup_xg_diff_3",
  "matchup_xg_diff_5",
  "matchup_goal_diff_3",
  "matchup_goal_diff_5",
  "matchup_ppg_3",
  "matchup_ppg_5",
  "matchup_shot_diff_3",
  "matchup_shot_diff_5",
  "matchup_sot_diff_3",
  "matchup_sot_diff_5",
  "rest_days_diff"
)

candidate_predictors <- intersect(
  candidate_predictors,
  names(analysis_features)
)

predictor_correlation_matrix <- analysis_features %>%
  select(all_of(candidate_predictors)) %>%
  cor(use = "pairwise.complete.obs")

predictor_correlation_table <- as.data.frame(
  predictor_correlation_matrix
) %>%
  rownames_to_column("variable")

predictor_correlation_long <- predictor_correlation_table %>%
  pivot_longer(
    -variable,
    names_to = "variable_2",
    values_to = "correlation"
  ) %>%
  rename(variable_1 = variable)

correlation_plot <- ggplot(
  predictor_correlation_long,
  aes(x = variable_1, y = variable_2, fill = correlation)
) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 2.5) +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation among candidate predictors",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_table(
  predictor_correlation_table,
  "08_predictor_correlation_matrix.csv"
)
save_table(
  predictor_correlation_long,
  "08_predictor_correlation_long.csv"
)
save_plot(correlation_plot, "08_predictor_correlation_matrix.png", 11, 9)

# ------------------------------------------------------------
# 8.2 Display predictor-correlation results
# ------------------------------------------------------------
cat("\nPREDICTOR CORRELATION MATRIX\n")
print(
  tibble::as_tibble(predictor_correlation_table) %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    ),
  n = Inf,
  width = Inf
)

cat("\nPREDICTOR CORRELATION HEATMAP\n")
print(correlation_plot)

#Creating a clearer correlation heatmap
# ------------------------------------------------------------
# Clear lower-triangle correlation heatmap
# ------------------------------------------------------------

predictor_labels <- c(
  pi_rating_diff_pre  = "Pi-rating",
  mkt_pre_p_home      = "Market probability",
  matchup_xg_diff_3   = "xG diff. (3)",
  matchup_xg_diff_5   = "xG diff. (5)",
  matchup_goal_diff_3 = "Goal diff. (3)",
  matchup_goal_diff_5 = "Goal diff. (5)",
  matchup_ppg_3       = "PPG diff. (3)",
  matchup_ppg_5       = "PPG diff. (5)",
  matchup_shot_diff_3 = "Shot diff. (3)",
  matchup_shot_diff_5 = "Shot diff. (5)",
  matchup_sot_diff_3  = "SoT diff. (3)",
  matchup_sot_diff_5  = "SoT diff. (5)",
  rest_days_diff      = "Rest-day diff."
)

predictor_correlation_long <- as.data.frame(
  as.table(predictor_correlation_matrix)
) %>%
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    correlation = Freq
  ) %>%
  mutate(
    variable_1 = as.character(variable_1),
    variable_2 = as.character(variable_2),
    row_number = match(variable_1, candidate_predictors),
    column_number = match(variable_2, candidate_predictors),
    variable_1_label = predictor_labels[variable_1],
    variable_2_label = predictor_labels[variable_2]
  ) %>%
  filter(
    row_number > column_number
  ) %>%
  mutate(
    variable_1_label = factor(
      variable_1_label,
      levels = rev(predictor_labels[candidate_predictors])
    ),
    variable_2_label = factor(
      variable_2_label,
      levels = predictor_labels[candidate_predictors]
    )
  )

correlation_plot <- ggplot(
  predictor_correlation_long,
  aes(
    x = variable_2_label,
    y = variable_1_label,
    fill = correlation
  )
) +
  geom_tile(
    colour = "white"
  ) +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    size = 3
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_fixed() +
  labs(
    title = "Correlation among candidate predictors",
    subtitle = "Only unique correlations are shown",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


cat("\nPREDICTOR CORRELATION HEATMAP\n")
print(correlation_plot)


# ============================================================
# 9. NAIVE BETTING STRATEGIES
# ============================================================
# ------------------------------------------------------------
# 9.1 Prepare match-level odds and strategy classifications
# ------------------------------------------------------------
naive_strategy_base <- analysis_features %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    actual_outcome = FTR,
    
    # Maximum available odds used for returns
    home_odds = odds_max_pre_home,
    draw_odds = odds_max_pre_draw,
    away_odds = odds_max_pre_away,
    
    # Average market probabilities used to classify outcomes
    p_home = mkt_pre_p_home,
    p_draw = mkt_pre_p_draw,
    p_away = mkt_pre_p_away
  ) %>%
  filter(
    actual_outcome %in% c("H", "D", "A"),
    is.finite(home_odds),
    is.finite(draw_odds),
    is.finite(away_odds),
    is.finite(p_home),
    is.finite(p_draw),
    is.finite(p_away)
  ) %>%
  mutate(
    favourite_outcome = c("H", "D", "A")[
      max.col(
        cbind(p_home, p_draw, p_away),
        ties.method = "first"
      )
    ],
    
    longshot_outcome = c("H", "D", "A")[
      max.col(
        -cbind(p_home, p_draw, p_away),
        ties.method = "first"
      )
    ]
  )

# ------------------------------------------------------------
# 9.2 Create one observation per strategy and match
# ------------------------------------------------------------
naive_strategy_bets <- bind_rows(
  
  naive_strategy_base %>%
    transmute(
      match_id_analysis,
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      actual_outcome,
      strategy = "Always home",
      selected_outcome = "H",
      selected_odds = home_odds
    ),
  
  naive_strategy_base %>%
    transmute(
      match_id_analysis,
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      actual_outcome,
      strategy = "Always draw",
      selected_outcome = "D",
      selected_odds = draw_odds
    ),
  
  naive_strategy_base %>%
    transmute(
      match_id_analysis,
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      actual_outcome,
      strategy = "Always away",
      selected_outcome = "A",
      selected_odds = away_odds
    ),
  
  naive_strategy_base %>%
    transmute(
      match_id_analysis,
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      actual_outcome,
      strategy = "Favourite",
      selected_outcome = favourite_outcome,
      
      selected_odds = case_when(
        favourite_outcome == "H" ~ home_odds,
        favourite_outcome == "D" ~ draw_odds,
        favourite_outcome == "A" ~ away_odds
      )
    ),
  
  naive_strategy_base %>%
    transmute(
      match_id_analysis,
      LeagueKey,
      LeagueName,
      SeasonStart,
      SeasonLabel,
      actual_outcome,
      strategy = "Longshot",
      selected_outcome = longshot_outcome,
      
      selected_odds = case_when(
        longshot_outcome == "H" ~ home_odds,
        longshot_outcome == "D" ~ draw_odds,
        longshot_outcome == "A" ~ away_odds
      )
    )
) %>%
  mutate(
    strategy = factor(
      strategy,
      levels = c(
        "Always home",
        "Always draw",
        "Always away",
        "Favourite",
        "Longshot"
      )
    ),
    
    win = as.integer(
      selected_outcome == actual_outcome
    ),
    
    unit_profit = if_else(
      win == 1L,
      selected_odds - 1,
      -1
    )
  )


# ------------------------------------------------------------
# 9.3 Results by league, season and strategy
# ------------------------------------------------------------
naive_strategy_by_league_season <- naive_strategy_bets %>%
  group_by(
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    strategy
  ) %>%
  summarise(
    bets = n(),
    wins = sum(win),
    hit_rate = mean(win),
    average_odds = mean(selected_odds),
    total_staked = n(),
    total_profit = sum(unit_profit),
    roi = sum(unit_profit) / n(),
    .groups = "drop"
  ) %>%
  arrange(
    LeagueName,
    SeasonStart,
    strategy
  )

# ------------------------------------------------------------
# 9.4 Plot ROI by league, season and strategy
# ------------------------------------------------------------
naive_strategy_plot <- ggplot(
  naive_strategy_by_league_season,
  aes(
    x = SeasonStart,
    y = roi,
    group = strategy,
    colour = strategy
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_wrap(
    ~ LeagueName,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = sort(
      unique(naive_strategy_by_league_season$SeasonStart)
    ),
    labels = analysis_features %>%
      distinct(SeasonStart, SeasonLabel) %>%
      arrange(SeasonStart) %>%
      pull(SeasonLabel)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Returns from naive betting strategies",
    subtitle = "One unit staked per match using average pre-match market odds",
    x = "Season",
    y = "Return on investment",
    colour = "Strategy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


# ------------------------------------------------------------
# 1. Overall results by strategy
# ------------------------------------------------------------
naive_strategy_overall <- naive_strategy_bets %>%
  group_by(strategy) %>%
  summarise(
    bets = n(),
    wins = sum(win),
    hit_rate = mean(win),
    average_odds = mean(selected_odds),
    total_staked = n(),
    total_profit = sum(unit_profit),
    roi = total_profit / total_staked,
    .groups = "drop"
  ) %>%
  arrange(strategy)


cat("NAIVE BETTING STRATEGIES — OVERALL\n")
print(
  naive_strategy_overall,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 2. Results by league and strategy
# ------------------------------------------------------------
naive_strategy_by_league <- naive_strategy_bets %>%
  group_by(
    LeagueKey,
    LeagueName,
    strategy
  ) %>%
  summarise(
    bets = n(),
    wins = sum(win),
    hit_rate = mean(win),
    average_odds = mean(selected_odds),
    total_staked = n(),
    total_profit = sum(unit_profit),
    roi = total_profit / total_staked,
    .groups = "drop"
  ) %>%
  arrange(
    LeagueName,
    strategy
  )


cat("NAIVE BETTING STRATEGIES — BY LEAGUE\n")
print(
  naive_strategy_by_league,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 3. Results by season and strategy
# ------------------------------------------------------------

naive_strategy_by_season <- naive_strategy_bets %>%
  group_by(
    SeasonStart,
    SeasonLabel,
    strategy
  ) %>%
  summarise(
    bets = n(),
    wins = sum(win),
    hit_rate = mean(win),
    average_odds = mean(selected_odds),
    total_staked = n(),
    total_profit = sum(unit_profit),
    roi = total_profit / total_staked,
    .groups = "drop"
  ) %>%
  arrange(
    SeasonStart,
    strategy
  )


cat("NAIVE BETTING STRATEGIES — BY SEASON\n")
print(
  naive_strategy_by_season,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 4. Results by league, season and strategy
# ------------------------------------------------------------
naive_strategy_by_league_season <- naive_strategy_bets %>%
  group_by(
    LeagueName,
    SeasonStart,
    strategy
  ) %>%
  summarise(
    bets = n(),
    wins = sum(win),
    hit_rate = mean(win),
    average_odds = mean(selected_odds),
    total_profit = sum(unit_profit),
    roi = mean(unit_profit),
    .groups = "drop"
  ) %>%
  arrange(
    LeagueName,
    SeasonStart,
    strategy
  )

cat("NAIVE BETTING STRATEGIES — BY LEAGUE AND SEASON\n")

print(
  naive_strategy_by_league_season,
  n = Inf,
  width = Inf
)






# ============================================================
# CLEAN BOOKMAKER-MARGIN PLOT
# ============================================================

# Keep bookmakers and seasons in a deliberate order
bookmaker_order <- bookmaker_lookup$bookmaker

season_axis <- bookmaker_margin_by_season %>%
  distinct(
    SeasonStart,
    SeasonLabel
  ) %>%
  arrange(SeasonStart)

bookmaker_margin_plot_data <- bookmaker_margin_by_season %>%
  mutate(
    bookmaker = factor(
      bookmaker,
      levels = bookmaker_order
    )
  ) %>%
  arrange(
    bookmaker,
    SeasonStart
  )


# ------------------------------------------------------------
# Plot average bookmaker margins over time
#USED IN THESIS
# ------------------------------------------------------------
library(ggplot2)

# Put Pinnacle last so it draws on top; flag it for styling
library(ggplot2)

bookmaker_margin_plot_data <- bookmaker_margin_plot_data %>%
  mutate(
    is_pinnacle = bookmaker == "Pinnacle",
    bookmaker = forcats::fct_relevel(bookmaker, "Pinnacle", after = Inf)
  )

pinnacle_col <- "#B4436C"
soft_books   <- c("Bet365", "Bwin", "Interwetten", "William Hill", "VC Bet")

bookmaker_margin_plot <- ggplot(
  bookmaker_margin_plot_data,
  aes(x = SeasonStart, y = avg_margin, group = bookmaker)
) +
  # soft books: single dark-grey colour, distinguished by linetype + shape
  geom_line(
    data = ~ dplyr::filter(.x, !is_pinnacle),
    aes(linetype = bookmaker), colour = "grey35", linewidth = 0.6, na.rm = TRUE
  ) +
  geom_point(
    data = ~ dplyr::filter(.x, !is_pinnacle),
    aes(shape = bookmaker), colour = "grey35", size = 2.2, na.rm = TRUE
  ) +
  # Pinnacle: bold coloured solid line, filled circles
  geom_line(
    data = ~ dplyr::filter(.x, is_pinnacle),
    colour = pinnacle_col, linewidth = 1.25, na.rm = TRUE
  ) +
  geom_point(
    data = ~ dplyr::filter(.x, is_pinnacle),
    colour = pinnacle_col, size = 2.9, na.rm = TRUE
  ) +
  scale_linetype_manual(
    values = c(
      "Bet365"       = "solid",
      "Bwin"         = "dashed",
      "Interwetten"  = "dotted",
      "William Hill" = "dotdash",
      "VC Bet"       = "longdash"
    ),
    limits = soft_books
  ) +
  scale_shape_manual(
    values = c(
      "Bet365"       = 16,  # circle
      "Bwin"         = 15,  # square
      "Interwetten"  = 17,  # triangle
      "William Hill" = 18,  # diamond
      "VC Bet"       = 4    # cross
    ),
    limits = soft_books
  ) +
  scale_x_continuous(
    breaks = season_axis$SeasonStart,
    labels = season_axis$SeasonLabel,
    expand = expansion(mult = c(0.01, 0.10))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = scales::breaks_width(0.01),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  annotate(
    "text",
    x = max(bookmaker_margin_plot_data$SeasonStart),
    y = dplyr::last(dplyr::filter(bookmaker_margin_plot_data, is_pinnacle)$avg_margin),
    label = "Pinnacle", colour = pinnacle_col, family = "serif",
    fontface = "bold", size = 4.2, hjust = -0.12
  ) +
  labs(x = "Season", y = "Bookmaker (overround)",
       linetype = NULL, shape = NULL) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.line          = element_line(linewidth = 0.4),
    axis.ticks         = element_line(linewidth = 0.4),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y        = element_text(size = 12),
    axis.title         = element_text(size = 13),
    legend.position    = "bottom",
    legend.text        = element_text(size = 12),
    legend.key.width   = grid::unit(1.5, "cm"),
    plot.margin        = margin(t = 10, r = 24, b = 5, l = 6)
  ) +
  guides(
    linetype = guide_legend(nrow = 1, keywidth = grid::unit(1.5, "cm")),
    shape    = guide_legend(nrow = 1)
  )

print(bookmaker_margin_plot)


# ============================================================
# CHECK WHICH BOOKMAKERS EXIST IN THE DATASET
# ============================================================
bookmaker_columns_found <- tibble(
  bookmaker_code = names(analysis_features) %>%
    stringr::str_subset("H$") %>%
    stringr::str_remove("H$")
) %>%
  distinct() %>%
  mutate(
    home_col = paste0(bookmaker_code, "H"),
    draw_col = paste0(bookmaker_code, "D"),
    away_col = paste0(bookmaker_code, "A")
  ) %>%
  filter(
    draw_col %in% names(analysis_features),
    away_col %in% names(analysis_features),
    
    # Exclude average and maximum market aggregates
    !bookmaker_code %in% c(
      "Avg",
      "Max",
      "BbAv",
      "BbMx"
    ),
    
    # Exclude closing-odds versions such as B365CH/B365CD/B365CA
    !stringr::str_ends(bookmaker_code, "C")
  ) %>%
  arrange(bookmaker_code)

print(
  bookmaker_columns_found,
  n = Inf,
  width = Inf
)


# Check the current modelling dataset
intersect(
  c(
    "SOH", "SOD", "SOA",
    "SBH", "SBD", "SBA"
  ),
  names(analysis_features)
)



# ============================================================
# 6.1 THE MARKET AS AN EFFICIENT BENCHMARK
# ============================================================

# ------------------------------------------------------------
# Helper: Ranked Probability Score for H / D / A outcomes
# ------------------------------------------------------------

calculate_rps <- function(p_home, p_draw, p_away, outcome) {
  
  observed_cumulative_1 <- as.integer(outcome == "H")
  observed_cumulative_2 <- as.integer(outcome %in% c("H", "D"))
  
  predicted_cumulative_1 <- p_home
  predicted_cumulative_2 <- p_home + p_draw
  
  (
    (predicted_cumulative_1 - observed_cumulative_1)^2 +
      (predicted_cumulative_2 - observed_cumulative_2)^2
  ) / 2
}


# ============================================================
# TABLE 1: OUTCOME RATES BY LEAGUE
# ============================================================

league_order <- c(
  "ENG",
  "ESP",
  "GER",
  "ITA",
  "FRA"
)

outcome_rates_by_league <- analysis_features %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  group_by(
    LeagueKey,
    LeagueName
  ) %>%
  summarise(
    matches = n(),
    home_win_share = mean(FTR == "H"),
    draw_share = mean(FTR == "D"),
    away_win_share = mean(FTR == "A"),
    .groups = "drop"
  ) %>%
  mutate(
    league_order = match(
      LeagueKey,
      league_order
    )
  ) %>%
  arrange(league_order) %>%
  select(-league_order)


# Add pooled sample as the final row
outcome_rates_pooled <- analysis_features %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  summarise(
    LeagueKey = "ALL",
    LeagueName = "Pooled sample",
    matches = n(),
    home_win_share = mean(FTR == "H"),
    draw_share = mean(FTR == "D"),
    away_win_share = mean(FTR == "A")
  )


outcome_rates_table <- bind_rows(
  outcome_rates_by_league,
  outcome_rates_pooled
)


# Formatted display version
outcome_rates_display <- outcome_rates_table %>%
  transmute(
    League = LeagueName,
    Matches = matches,
    `Home wins` = scales::percent(
      home_win_share,
      accuracy = 0.1
    ),
    Draws = scales::percent(
      draw_share,
      accuracy = 0.1
    ),
    `Away wins` = scales::percent(
      away_win_share,
      accuracy = 0.1
    )
  )


cat("\n============================================================\n")
cat("TABLE 1: OUTCOME RATES BY LEAGUE\n")
cat("============================================================\n")

print(
  outcome_rates_display,
  n = Inf,
  width = Inf
)


# ============================================================
# TABLE 2: FORECAST BENCHMARK
# ============================================================
# ============================================================
# 6.1 THE MARKET AS AN EFFICIENT BENCHMARK
# ============================================================

# ------------------------------------------------------------
# Helper: Ranked Probability Score for H / D / A outcomes
# ------------------------------------------------------------

calculate_rps <- function(p_home, p_draw, p_away, outcome) {
  
  observed_cumulative_1 <- as.integer(outcome == "H")
  observed_cumulative_2 <- as.integer(outcome %in% c("H", "D"))
  
  predicted_cumulative_1 <- p_home
  predicted_cumulative_2 <- p_home + p_draw
  
  (
    (predicted_cumulative_1 - observed_cumulative_1)^2 +
      (predicted_cumulative_2 - observed_cumulative_2)^2
  ) / 2
}


# ============================================================
# TABLE 1: OUTCOME RATES BY LEAGUE
# ============================================================

league_order <- c(
  "ENG",
  "ESP",
  "GER",
  "ITA",
  "FRA"
)

outcome_rates_by_league <- analysis_features %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  group_by(
    LeagueKey,
    LeagueName
  ) %>%
  summarise(
    matches = n(),
    home_win_share = mean(FTR == "H"),
    draw_share = mean(FTR == "D"),
    away_win_share = mean(FTR == "A"),
    .groups = "drop"
  ) %>%
  mutate(
    league_order = match(
      LeagueKey,
      league_order
    )
  ) %>%
  arrange(league_order) %>%
  select(-league_order)


# Add pooled sample as the final row
outcome_rates_pooled <- analysis_features %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  summarise(
    LeagueKey = "ALL",
    LeagueName = "Pooled sample",
    matches = n(),
    home_win_share = mean(FTR == "H"),
    draw_share = mean(FTR == "D"),
    away_win_share = mean(FTR == "A")
  )


outcome_rates_table <- bind_rows(
  outcome_rates_by_league,
  outcome_rates_pooled
)


# Formatted display version
outcome_rates_display <- outcome_rates_table %>%
  transmute(
    League = LeagueName,
    Matches = matches,
    `Home wins` = scales::percent(
      home_win_share,
      accuracy = 0.1
    ),
    Draws = scales::percent(
      draw_share,
      accuracy = 0.1
    ),
    `Away wins` = scales::percent(
      away_win_share,
      accuracy = 0.1
    )
  )


cat("\n============================================================\n")
cat("TABLE 1: OUTCOME RATES BY LEAGUE\n")
cat("============================================================\n")

print(
  outcome_rates_display,
  n = Inf,
  width = Inf
)


# ============================================================
# TABLE 2: FORECAST BENCHMARK
# ============================================================

# Restrict all forecasts to the same set of matches
benchmark_sample <- analysis_features %>%
  filter(
    FTR %in% c("H", "D", "A"),
    is.finite(mkt_pre_p_home),
    is.finite(mkt_pre_p_draw),
    is.finite(mkt_pre_p_away)
  )


# ------------------------------------------------------------
# League-specific climatology
#
# Each match receives the historical H / D / A frequencies
# of its own league, rather than pooled sample frequencies.
# ------------------------------------------------------------

league_climatology <- benchmark_sample %>%
  group_by(
    LeagueKey,
    LeagueName
  ) %>%
  summarise(
    climatology_p_home = mean(FTR == "H"),
    climatology_p_draw = mean(FTR == "D"),
    climatology_p_away = mean(FTR == "A"),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Construct match-level forecasts
# ------------------------------------------------------------

benchmark_match_results <- benchmark_sample %>%
  left_join(
    league_climatology,
    by = c(
      "LeagueKey",
      "LeagueName"
    )
  ) %>%
  mutate(
    
    # Market's most likely outcome
    market_prediction = c("H", "D", "A")[
      max.col(
        cbind(
          mkt_pre_p_home,
          mkt_pre_p_draw,
          mkt_pre_p_away
        ),
        ties.method = "first"
      )
    ],
    
    # Most likely outcome according to league climatology
    climatology_prediction = c("H", "D", "A")[
      max.col(
        cbind(
          climatology_p_home,
          climatology_p_draw,
          climatology_p_away
        ),
        ties.method = "first"
      )
    ],
    
    # Always-favourite is a deterministic forecast:
    # probability one is placed on the market favourite
    favourite_p_home = as.integer(
      market_prediction == "H"
    ),
    
    favourite_p_draw = as.integer(
      market_prediction == "D"
    ),
    
    favourite_p_away = as.integer(
      market_prediction == "A"
    ),
    
    # Match-level RPS values
    market_rps = calculate_rps(
      mkt_pre_p_home,
      mkt_pre_p_draw,
      mkt_pre_p_away,
      FTR
    ),
    
    climatology_rps = calculate_rps(
      climatology_p_home,
      climatology_p_draw,
      climatology_p_away,
      FTR
    ),
    
    favourite_rps = calculate_rps(
      favourite_p_home,
      favourite_p_draw,
      favourite_p_away,
      FTR
    )
  )


# ------------------------------------------------------------
# Aggregate forecast performance
# ------------------------------------------------------------

forecast_benchmark_table <- bind_rows(
  
  benchmark_match_results %>%
    summarise(
      forecast = "Market average",
      matches = n(),
      accuracy = mean(
        market_prediction == FTR
      ),
      mean_rps = mean(market_rps)
    ),
  
  benchmark_match_results %>%
    summarise(
      forecast = "League climatology",
      matches = n(),
      accuracy = mean(
        climatology_prediction == FTR
      ),
      mean_rps = mean(climatology_rps)
    ),
  
  benchmark_match_results %>%
    summarise(
      forecast = "Always favourite",
      matches = n(),
      accuracy = mean(
        market_prediction == FTR
      ),
      mean_rps = mean(favourite_rps)
    )
) %>%
  mutate(
    forecast = factor(
      forecast,
      levels = c(
        "Market average",
        "League climatology",
        "Always favourite"
      )
    )
  ) %>%
  arrange(forecast)

#USED IN THESIS
# Formatted display version
forecast_benchmark_display <- forecast_benchmark_table %>%
  transmute(
    Forecast = as.character(forecast),
    Matches = matches,
    Accuracy = scales::percent(
      accuracy,
      accuracy = 0.1
    ),
    `Mean RPS` = sprintf(
      "%.3f",
      mean_rps
    )
  )

cat("TABLE 2: FORECAST BENCHMARK\n")
print(
  forecast_benchmark_display,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# Seasonal home-win range by league
# ------------------------------------------------------------
home_win_range_by_league <- analysis_features %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  group_by(
    LeagueKey,
    LeagueName,
    SeasonStart
  ) %>%
  summarise(
    seasonal_home_win_share = mean(FTR == "H"),
    .groups = "drop"
  ) %>%
  group_by(
    LeagueKey,
    LeagueName
  ) %>%
  summarise(
    home_win_min = min(seasonal_home_win_share),
    home_win_max = max(seasonal_home_win_share),
    .groups = "drop"
  )


# Pooled seasonal range across all five leagues
# Overall range across all league-season observations
home_win_range_pooled <- analysis_features %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  group_by(
    LeagueKey,
    LeagueName,
    SeasonStart
  ) %>%
  summarise(
    seasonal_home_win_share = mean(FTR == "H"),
    .groups = "drop"
  ) %>%
  summarise(
    LeagueKey = "ALL",
    LeagueName = "Pooled sample",
    home_win_min = min(seasonal_home_win_share),
    home_win_max = max(seasonal_home_win_share)
  )

home_win_ranges <- bind_rows(
  home_win_range_by_league,
  home_win_range_pooled
)


# ------------------------------------------------------------
# Final thesis table of OUTCOME rates, USED IN THESIS
# ------------------------------------------------------------
outcome_rates_final <- outcome_rates_table %>%
  left_join(
    home_win_ranges,
    by = c(
      "LeagueKey",
      "LeagueName"
    )
  ) %>%
  transmute(
    League = LeagueName,
    Matches = matches,
    `Home wins` = sprintf(
      "%.1f\\%%",
      100 * home_win_share
    ),
    `Home-win range` = sprintf(
      "%.1f--%.1f\\%%",
      100 * home_win_min,
      100 * home_win_max
    ),
    Draws = sprintf(
      "%.1f\\%%",
      100 * draw_share
    ),
    `Away wins` = sprintf(
      "%.1f\\%%",
      100 * away_win_share
    )
  )


# Inspect in R
print(
  outcome_rates_final,
  n = Inf,
  width = Inf
)

#table Latex format
outcome_rates_latex <- knitr::kable(
  outcome_rates_final,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  align = c("l", "r", "r", "r", "r", "r"),
  caption = paste(
    "Match outcome frequencies by league.",
    "The home-win range reports the minimum and maximum",
    "seasonal home-win rate."
  ),
  label = "tab:outcome-rates"
) %>%
  kableExtra::kable_styling(
    latex_options = "hold_position",
    position = "center"
  )

cat(outcome_rates_latex)



####NAIVE STRATEGIES TABLES, USED IN THESIS####
# ============================================================
# NAIVE STRATEGY SUMMARY TABLE
# ============================================================
strategy_cell_roi <- naive_strategy_bets %>%
  group_by(strategy, LeagueName, SeasonStart) %>%
  summarise(
    cell_roi = mean(unit_profit),
    .groups = "drop"
  )

naive_strategy_table <- naive_strategy_bets %>%
  group_by(strategy) %>%
  summarise(
    Bets = n(),
    `Avg odds` = mean(selected_odds),
    `Hit rate` = mean(win),
    `Mean ROI` = mean(unit_profit),
    .groups = "drop"
  ) %>%
  left_join(
    strategy_cell_roi %>%
      group_by(strategy) %>%
      summarise(
        Worst = min(cell_roi),
        Best = max(cell_roi),
        `Profitable seasons` = sprintf(
          "%d/%d",
          sum(cell_roi > 0),
          n()
        ),
        .groups = "drop"
      ),
    by = "strategy"
  ) %>%
  mutate(
    Bets = scales::comma(Bets, accuracy = 1),
    `Avg odds` = sprintf("%.2f", `Avg odds`),
    across(
      c(`Hit rate`, `Mean ROI`, Worst, Best),
      ~ sprintf("%.1f\\%%", 100 * .x)
    )
  )

print(
  naive_strategy_table,
  n = Inf,
  width = Inf
)

naive_strategy_latex <- knitr::kable(
  naive_strategy_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  align = rep("c", ncol(naive_strategy_table)),
  caption = paste(
    "Returns from naïve betting strategies evaluated at",
    "maximum available odds."
  ),
  label = "tab:naive-strategy-returns"
) %>%
  kableExtra::kable_styling(
    latex_options = "HOLD_position",
    position = "center"
  )

cat(naive_strategy_latex)

#Print results by seasons-strategy-league
naive_strategy_by_league_season <- naive_strategy_bets %>%
  group_by(
    LeagueName,
    SeasonStart,
    strategy
  ) %>%
  summarise(
    bets = n(),
    wins = sum(win),
    hit_rate = mean(win),
    average_odds = mean(selected_odds),
    total_profit = sum(unit_profit),
    roi = mean(unit_profit),
    .groups = "drop"
  ) %>%
  arrange(
    LeagueName,
    SeasonStart,
    strategy
  )

print(
  naive_strategy_by_league_season,
  n = Inf,
  width = Inf
)


#6.2 USED IN THESIS
#Favourite longshot calibration plot
# ============================================================
# FAVOURITE–LONGSHOT CALIBRATION: POOLED
# ============================================================
calibration_pooled <- calibration_long %>%
  mutate(
    probability_decile = ntile(implied_probability, 10)
  ) %>%
  group_by(probability_decile) %>%
  summarise(
    observations = n(),
    mean_implied_probability = mean(implied_probability),
    realised_frequency = mean(realised),
    calibration_gap = realised_frequency - mean_implied_probability,
    .groups = "drop"
  )

axis_limit <- ceiling(
  max(
    calibration_pooled$mean_implied_probability,
    calibration_pooled$realised_frequency
  ) * 10
) / 10

accent_col <- "#B4436C"

favourite_longshot_plot <- ggplot(
  calibration_pooled,
  aes(
    x = mean_implied_probability,
    y = realised_frequency
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_line(
    colour = accent_col,
    linewidth = 1.15
  ) +
  geom_point(
    colour = accent_col,
    size = 3
  ) +
  scale_x_continuous(
    breaks = seq(0, axis_limit, 0.1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    breaks = seq(0, axis_limit, 0.1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  coord_equal(
    xlim = c(0, axis_limit),
    ylim = c(0, axis_limit)
  ) +
  labs(
    x = "De-vigged implied probability",
    y = "Realised outcome frequency"
  ) +
  theme_classic(
    base_size = 13,
    base_family = "serif"
  ) +
  theme(
    panel.grid.major = element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.margin = margin(
      t = 10,
      r = 12,
      b = 6,
      l = 6
    )
  )

print(favourite_longshot_plot)

#Trying new bins for calibration plot
# Fixed-width calibration bins (5-percentage-point bins)
#Creates 3 plots, H / D / A
bin_width <- 0.05

favourite_longshot_binned <- calibration_long %>%
  mutate(
    prob_bin = cut(
      implied_probability,
      breaks = seq(0, 1, by = bin_width),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  group_by(outcome, prob_bin) %>%
  summarise(
    observations = n(),
    mean_implied_probability = mean(implied_probability),
    realised_frequency = mean(realised),
    calibration_gap = realised_frequency - mean_implied_probability,
    .groups = "drop"
  ) %>%
  filter(observations >= 30)   # drop bins too sparse to be meaningful

favourite_longshot_plot <- ggplot(
  favourite_longshot_binned,
  aes(x = mean_implied_probability, y = realised_frequency)
) +
  geom_abline(slope = 1, intercept = 0,
              colour = "grey55", linetype = "dashed", linewidth = 0.7) +
  geom_line(colour = accent_col, linewidth = 0.9) +
  geom_point(aes(size = observations), colour = accent_col) +
  facet_wrap(~ outcome) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_equal() +
  labs(x = "De-vigged implied probability",
       y = "Realised outcome frequency") +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

print(favourite_longshot_plot)

#one panel
favourite_longshot_plot <- ggplot(
  favourite_longshot_binned,
  aes(
    x = mean_implied_probability,
    y = realised_frequency
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_line(
    colour = accent_col,
    linewidth = 0.9
  ) +
  geom_point(
    aes(size = observations),
    colour = accent_col
  ) +
  scale_size_continuous(
    range = c(1, 4),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.10),
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.10),
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_equal() +
  labs(
    x = "De-vigged implied probability",
    y = "Realised outcome frequency"
  ) +
  theme_classic(
    base_size = 13,
    base_family = "serif"
  ) +
  theme(
    panel.grid.major = element_line(
      colour = "grey92",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    axis.line = element_line(
      linewidth = 0.4
    ),
    axis.ticks = element_line(
      linewidth = 0.4
    )
  )

print(favourite_longshot_plot)


favourite_longshot_binned %>%
  arrange(mean_implied_probability) %>%
  transmute(
    bin_prob   = scales::percent(mean_implied_probability, accuracy = 0.1), # mean implied prob in the bin
    realised   = scales::percent(realised_frequency, accuracy = 0.1),       # how often it actually happened
    gap_points = round(calibration_gap * 100, 2),   # <-- THE NUMBER: signed gap in percentage points
    n          = observations                        # matches in the bin (small = noisy tail)
  ) %>%
  print(n = Inf)


favourite_longshot_plot <- favourite_longshot_plot +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave(
  filename = "calibration.pdf",
  plot = favourite_longshot_plot,
  width = 7,
  height = 6.2,
  units = "in",
  device = cairo_pdf
)


print(favourite_longshot_plot)
getwd()

# Ensure the plotting data are ordered correctly
bookmaker_margin_plot_data <- bookmaker_margin_plot_data %>%
  mutate(
    is_pinnacle = bookmaker == "Pinnacle",
    bookmaker = forcats::fct_relevel(
      bookmaker,
      "Pinnacle",
      after = Inf
    )
  ) %>%
  arrange(bookmaker, SeasonStart)

# Bookmakers shown in grey
soft_books <- c(
  "Bet365",
  "Bwin",
  "Interwetten",
  "William Hill",
  "VC Bet"
)

# Highlight colour for Pinnacle
pinnacle_col <- "#B4436C"

# Locate the final Pinnacle observation for the direct label
pinnacle_endpoint <- bookmaker_margin_plot_data %>%
  filter(is_pinnacle) %>%
  arrange(SeasonStart) %>%
  slice_tail(n = 1)

# Recreate the figure
bookmaker_margin_plot <- ggplot(
  bookmaker_margin_plot_data,
  aes(
    x = SeasonStart,
    y = avg_margin,
    group = bookmaker
  )
) +
  
  # Remaining bookmakers: darker and thicker grey lines
  geom_line(
    data = ~ dplyr::filter(.x, !is_pinnacle),
    aes(linetype = bookmaker),
    colour = "grey25",
    linewidth = 0.85,
    na.rm = TRUE
  ) +
  
  geom_point(
    data = ~ dplyr::filter(.x, !is_pinnacle),
    aes(shape = bookmaker),
    colour = "grey25",
    size = 2.7,
    stroke = 0.8,
    na.rm = TRUE
  ) +
  
  # Pinnacle: thicker coloured line and larger points
  geom_line(
    data = ~ dplyr::filter(.x, is_pinnacle),
    colour = pinnacle_col,
    linewidth = 1.5,
    na.rm = TRUE
  ) +
  
  geom_point(
    data = ~ dplyr::filter(.x, is_pinnacle),
    colour = pinnacle_col,
    size = 3.3,
    na.rm = TRUE
  ) +
  
  # Distinguish the grey bookmakers by line type
  scale_linetype_manual(
    values = c(
      "Bet365"       = "solid",
      "Bwin"         = "dashed",
      "Interwetten"  = "dotted",
      "William Hill" = "dotdash",
      "VC Bet"       = "longdash"
    ),
    limits = soft_books
  ) +
  
  # Distinguish the grey bookmakers by marker shape
  scale_shape_manual(
    values = c(
      "Bet365"       = 16,
      "Bwin"         = 15,
      "Interwetten"  = 17,
      "William Hill" = 18,
      "VC Bet"       = 4
    ),
    limits = soft_books
  ) +
  
  # Season labels
  scale_x_continuous(
    breaks = season_axis$SeasonStart,
    labels = season_axis$SeasonLabel,
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  
  # Percentage scale
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = scales::breaks_width(0.01),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  
  # Direct label for Pinnacle
  annotate(
    "text",
    x = pinnacle_endpoint$SeasonStart,
    y = pinnacle_endpoint$avg_margin,
    label = "Pinnacle",
    colour = pinnacle_col,
    family = "serif",
    fontface = "bold",
    size = 4.8,
    hjust = -0.15
  ) +
  
  labs(
    x = NULL,
    y = "Average overround",
    linetype = NULL,
    shape = NULL
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(
    base_size = 15,
    base_family = "serif"
  ) +
  
  theme(
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 13,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 13,
      colour = "black"
    ),
    axis.title.y = element_text(
      size = 14,
      margin = margin(r = 8)
    ),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 13,
      colour = "black"
    ),
    legend.key.width = grid::unit(1.6, "cm"),
    legend.spacing.x = grid::unit(0.25, "cm"),
    
    plot.margin = margin(
      t = 10,
      r = 35,
      b = 5,
      l = 8
    )
  ) +
  
  guides(
    linetype = guide_legend(
      nrow = 1,
      byrow = TRUE,
      keywidth = grid::unit(1.6, "cm")
    ),
    shape = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  )

# Display the revised figure
print(bookmaker_margin_plot)