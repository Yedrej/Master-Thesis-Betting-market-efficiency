# STEP 03 — Build the modelling variables
# Reads:  analysis_base_constantinou.rds
# Writes: analysis_features.rds / .csv
# Thesis: Section 4.3 and Table 3. Rolling xG for and against, points per
#         game, promotion and early-season indicators. All lagged.

# ============================================================
# BUILD FINAL FEATURE DATASET
# Top-5 European leagues, 2014/15-2024/25
# ============================================================
# Important design choices:
#   - Pi-ratings are already constructed in "five-league-pipeline"
#   - Rolling features are calculated within team-season and reset at
#     the beginning of every season.
#   - Every rolling feature uses prior matches only.
#   - Partial early-season windows are retained by default and are
#     accompanied by prior-match counters.
#   - Raw current-match statistics are never used directly as
#     pre-match predictors.
# ============================================================

# ------------------------------------------------------------
# 0. Packages and settings
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(slider)
library(tibble)
library(stringr)
library(rlang)

# Project paths
project_dir <- Sys.getenv("THESIS_PROJECT_DIR", unset = ".")
analysis_dir <- file.path(project_dir, "analysis_test")
data_dir <- file.path(analysis_dir, "data")
documentation_dir <- file.path(analysis_dir, "documentation")

# Input files
analysis_input_path <- file.path(
  data_dir,
  "analysis_base_constantinou.rds"
)

# The all-season parent dataset is used only to identify promoted teams.
all_season_input_path <- file.path(
  project_dir, "processed", "football_data_all_clean.csv"   # 2009/10–2024/25
)

# Output files
feature_output_path <- file.path(
  data_dir,
  "analysis_features.rds"
)

feature_csv_output_path <- file.path(
  data_dir,
  "analysis_features.csv"
)

feature_legend_output_path <- file.path(
  documentation_dir,
  "analysis_features_variable_legend.csv"
)

feature_only_legend_output_path <- file.path(
  documentation_dir,
  "engineered_feature_legend.csv"
)

feature_checks_output_path <- file.path(
  documentation_dir,
  "feature_build_checks.csv"
)

# Feature settings
rolling_windows <- c(3L, 5L)
use_partial_windows <- TRUE
save_csv_copy <- TRUE

# Create output folders if required
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(documentation_dir, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 1. Load and validate the input data
# ------------------------------------------------------------
if (!file.exists(analysis_input_path)) {
  stop("Input file not found: ", analysis_input_path)
}

analysis_base <- readr::read_rds(analysis_input_path) %>%
  mutate(MatchDate = as.Date(MatchDate)) %>%
  arrange(LeagueKey, MatchDate, match_id_analysis)

required_variables <- c(
  "match_id_analysis",
  "LeagueKey",
  "LeagueName",
  "SeasonStart",
  "SeasonLabel",
  "MatchDate",
  "HomeTeam",
  "AwayTeam",
  "FTHG",
  "FTAG",
  "FTR",
  "HS",
  "AS",
  "HST",
  "AST",
  "home_xG",
  "away_xG",
  "pi_home_home_rating_pre",
  "pi_away_away_rating_pre",
  "pi_rating_diff_pre",
  "odds_avg_pre_home",
  "odds_avg_pre_draw",
  "odds_avg_pre_away"
)

missing_variables <- setdiff(required_variables, names(analysis_base))

if (length(missing_variables) > 0) {
  stop(
    "The input dataset is missing the following required variables: ",
    paste(missing_variables, collapse = ", ")
  )
}

if (anyDuplicated(analysis_base$match_id_analysis) > 0) {
  stop("match_id_analysis is not unique in analysis_base.")
}

input_rows <- nrow(analysis_base)
input_columns <- names(analysis_base)

# Use cleaned team names when available; otherwise use Football-Data names.
analysis_base <- analysis_base %>%
  mutate(
    home_team_id = if ("HomeTeam_clean" %in% names(.)) {
      coalesce(HomeTeam_clean, HomeTeam)
    } else {
      HomeTeam
    },
    away_team_id = if ("AwayTeam_clean" %in% names(.)) {
      coalesce(AwayTeam_clean, AwayTeam)
    } else {
      AwayTeam
    }
  )


# ------------------------------------------------------------
# 2. Add de-vigged market probabilities from average odds
# ------------------------------------------------------------
analysis_features <- analysis_base %>%
  mutate(
    mkt_pre_overround =
      1 / odds_avg_pre_home +
      1 / odds_avg_pre_draw +
      1 / odds_avg_pre_away,

    mkt_pre_margin = mkt_pre_overround - 1,

    mkt_pre_p_home =
      (1 / odds_avg_pre_home) / mkt_pre_overround,

    mkt_pre_p_draw =
      (1 / odds_avg_pre_draw) / mkt_pre_overround,

    mkt_pre_p_away =
      (1 / odds_avg_pre_away) / mkt_pre_overround
  )

# Tag the market-benchmark columns so the modelling stage can exclude them.
mkt_prob_cols <- c("mkt_pre_overround", "mkt_pre_margin",
                   "mkt_pre_p_home", "mkt_pre_p_draw", "mkt_pre_p_away")
attr(analysis_features, "market_benchmark_cols") <- mkt_prob_cols


# ------------------------------------------------------------
# 3. Identify promoted teams
# ------------------------------------------------------------
# A team is flagged as promoted when it appears in a league-season but
# was absent from the same league in the immediately preceding season.
# The all-season parent dataset is required so that promoted teams in
# 2014/15 can be identified using 2013/14.
if (!file.exists(all_season_input_path)) {
  warning(
    "All-season input file not found. Promoted-team flags will be NA: ",
    all_season_input_path
  )

  promoted_lookup <- analysis_features %>%
    distinct(LeagueKey, SeasonStart, team = home_team_id) %>%
    mutate(promoted = NA_integer_) %>%
    select(LeagueKey, SeasonStart, team, promoted)

} else {
  all_season_data <- readr::read_csv(
    all_season_input_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      MatchDate = as.Date(MatchDate)
    )
  
  if (min(all_season_data$SeasonStart, na.rm = TRUE) > 2013) {
    stop(
      "The all-season input must include 2013/14 so that promoted ",
      "teams can be identified for the 2014/15 analysis season."
    )
  }

  all_home_team <- if ("HomeTeam_clean" %in% names(all_season_data)) {
    coalesce(all_season_data$HomeTeam_clean, all_season_data$HomeTeam)
  } else {
    all_season_data$HomeTeam
  }

  all_away_team <- if ("AwayTeam_clean" %in% names(all_season_data)) {
    coalesce(all_season_data$AwayTeam_clean, all_season_data$AwayTeam)
  } else {
    all_season_data$AwayTeam
  }

  season_teams <- bind_rows(
    all_season_data %>%
      transmute(
        LeagueKey,
        SeasonStart,
        team = all_home_team
      ),
    all_season_data %>%
      transmute(
        LeagueKey,
        SeasonStart,
        team = all_away_team
      )
  ) %>%
    filter(!is.na(team)) %>%
    distinct(LeagueKey, SeasonStart, team)

  previous_season_teams <- season_teams %>%
    transmute(
      LeagueKey,
      SeasonStart = SeasonStart + 1L,
      team,
      present_previous_season = TRUE
    )

  first_observed_season <- season_teams %>%
    group_by(LeagueKey) %>%
    summarise(
      first_season = min(SeasonStart),
      .groups = "drop"
    )

  promoted_lookup <- season_teams %>%
    left_join(
      previous_season_teams,
      by = c("LeagueKey", "SeasonStart", "team")
    ) %>%
    left_join(first_observed_season, by = "LeagueKey") %>%
    mutate(
      promoted = case_when(
        SeasonStart == first_season ~ NA_integer_,
        is.na(present_previous_season) ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    select(LeagueKey, SeasonStart, team, promoted)
}

analysis_features <- analysis_features %>%
  left_join(
    promoted_lookup %>%
      rename(
        home_team_id = team,
        home_promoted = promoted
      ),
    by = c("LeagueKey", "SeasonStart", "home_team_id")
  ) %>%
  left_join(
    promoted_lookup %>%
      rename(
        away_team_id = team,
        away_promoted = promoted
      ),
    by = c("LeagueKey", "SeasonStart", "away_team_id")
  ) %>%
  mutate(
    any_promoted = case_when(
      is.na(home_promoted) | is.na(away_promoted) ~ NA_integer_,
      home_promoted == 1L | away_promoted == 1L ~ 1L,
      TRUE ~ 0L
    ),
    promoted_diff = home_promoted - away_promoted
  )


# ------------------------------------------------------------
# 4. Reshape matches to one row per team-match
# ------------------------------------------------------------
home_team_matches <- analysis_features %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    MatchDate,
    side = "home",
    team = home_team_id,
    opponent = away_team_id,
    goals_for = as.numeric(FTHG),
    goals_against = as.numeric(FTAG),
    xg_for = as.numeric(home_xG),
    xg_against = as.numeric(away_xG),
    shots_for = as.numeric(HS),
    shots_against = as.numeric(AS),
    sot_for = as.numeric(HST),
    sot_against = as.numeric(AST),
    points = case_when(
      FTR == "H" ~ 3,
      FTR == "D" ~ 1,
      FTR == "A" ~ 0,
      TRUE ~ NA_real_
    )
  )

away_team_matches <- analysis_features %>%
  transmute(
    match_id_analysis,
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    MatchDate,
    side = "away",
    team = away_team_id,
    opponent = home_team_id,
    goals_for = as.numeric(FTAG),
    goals_against = as.numeric(FTHG),
    xg_for = as.numeric(away_xG),
    xg_against = as.numeric(home_xG),
    shots_for = as.numeric(AS),
    shots_against = as.numeric(HS),
    sot_for = as.numeric(AST),
    sot_against = as.numeric(HST),
    points = case_when(
      FTR == "A" ~ 3,
      FTR == "D" ~ 1,
      FTR == "H" ~ 0,
      TRUE ~ NA_real_
    )
  )

team_matches <- bind_rows(home_team_matches, away_team_matches) %>%
  mutate(
    goal_diff = goals_for - goals_against,
    xg_diff = xg_for - xg_against,
    shot_diff = shots_for - shots_against,
    sot_diff = sot_for - sot_against
  ) %>%
  arrange(LeagueKey, SeasonStart, team, MatchDate, match_id_analysis) %>%
  group_by(LeagueKey, SeasonStart, team) %>%
  mutate(
    matches_played_season_pre = row_number() - 1L,
    rest_days = as.numeric(MatchDate - lag(MatchDate))
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 5. Create lagged rolling features
# ------------------------------------------------------------
safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

lagged_rolling_mean <- function(x, window) {
  slider::slide_dbl(
    dplyr::lag(x),
    safe_mean,
    .before = window - 1L,
    .complete = FALSE
  )
}

rolling_source_variables <- c(
  "xg_for",
  "xg_against",
  "xg_diff",
  "goals_for",
  "goals_against",
  "goal_diff",
  "points",
  "shots_for",
  "shots_against",
  "shot_diff",
  "sot_for",
  "sot_against",
  "sot_diff"
)

for (window in rolling_windows) {
  team_matches <- team_matches %>%
    group_by(LeagueKey, SeasonStart, team) %>%
    mutate(
      across(
        all_of(rolling_source_variables),
        ~ lagged_rolling_mean(.x, window),
        .names = paste0("roll_{.col}_", window)
      )
    ) %>%
    ungroup()

  # A rolling mean of points is points per game over the window.
  team_matches <- team_matches %>%
    rename(
      !!paste0("roll_ppg_", window) :=
        !!rlang::sym(paste0("roll_points_", window))
    )

  if (!use_partial_windows) {
    generated_columns <- c(
      paste0("roll_", setdiff(rolling_source_variables, "points"), "_", window),
      paste0("roll_ppg_", window)
    )

    team_matches <- team_matches %>%
      mutate(
        across(
          all_of(generated_columns),
          ~ if_else(
            matches_played_season_pre >= window,
            .x,
            NA_real_
          )
        )
      )
  }
}


# ------------------------------------------------------------
# 6. Join home-team and away-team pre-match features back
# ------------------------------------------------------------
rolling_columns <- names(team_matches)[
  stringr::str_detect(names(team_matches), "^roll_")
]

home_features <- team_matches %>%
  filter(side == "home") %>%
  select(
    match_id_analysis,
    matches_played_season_pre,
    rest_days,
    all_of(rolling_columns)
  ) %>%
  rename_with(
    ~ paste0("home_", .x),
    -match_id_analysis
  )

away_features <- team_matches %>%
  filter(side == "away") %>%
  select(
    match_id_analysis,
    matches_played_season_pre,
    rest_days,
    all_of(rolling_columns)
  ) %>%
  rename_with(
    ~ paste0("away_", .x),
    -match_id_analysis
  )

analysis_features <- analysis_features %>%
  left_join(home_features, by = "match_id_analysis") %>%
  left_join(away_features, by = "match_id_analysis") %>%
  mutate(
    rest_days_diff = home_rest_days - away_rest_days,
    min_matches_played_season_pre = pmin(
      home_matches_played_season_pre,
      away_matches_played_season_pre
    ),
    early_season_lt3 = as.integer(min_matches_played_season_pre < 3L),
    early_season_lt5 = as.integer(min_matches_played_season_pre < 5L)
  )


# ------------------------------------------------------------
# 7. Create home-minus-away matchup differentials
# ------------------------------------------------------------
matchup_metrics <- c(
  "xg_for",
  "xg_against",
  "xg_diff",
  "goals_for",
  "goals_against",
  "goal_diff",
  "ppg",
  "shots_for",
  "shots_against",
  "shot_diff",
  "sot_for",
  "sot_against",
  "sot_diff"
)

for (window in rolling_windows) {
  for (metric in matchup_metrics) {
    home_variable <- paste0("home_roll_", metric, "_", window)
    away_variable <- paste0("away_roll_", metric, "_", window)
    output_variable <- paste0("matchup_", metric, "_", window)

    analysis_features[[output_variable]] <-
      analysis_features[[home_variable]] -
      analysis_features[[away_variable]]
  }
}


# ------------------------------------------------------------
# 8. Add explicit league dummy variables
# ------------------------------------------------------------
league_codes <- sort(unique(analysis_features$LeagueKey))

for (league_code in league_codes) {
  dummy_name <- paste0("league_", league_code)
  analysis_features[[dummy_name]] <-
    as.integer(analysis_features$LeagueKey == league_code)
}


# ------------------------------------------------------------
# 9. Final ordering and integrity checks
# ------------------------------------------------------------
analysis_features <- analysis_features %>%
  arrange(LeagueKey, MatchDate, match_id_analysis) %>%
  select(-home_team_id, -away_team_id)

if (nrow(analysis_features) != input_rows) {
  stop(
    "Row count changed during feature construction: ",
    input_rows,
    " input rows versus ",
    nrow(analysis_features),
    " output rows."
  )
}

if (anyDuplicated(analysis_features$match_id_analysis) > 0) {
  stop("match_id_analysis is no longer unique after feature construction.")
}

# First match of every team-season must have no lagged rolling history.
first_match_check <- team_matches %>%
  filter(matches_played_season_pre == 0L) %>%
  summarise(
    first_team_season_rows = n(),
    nonmissing_roll_xg_diff_3 = sum(!is.na(roll_xg_diff_3)),
    nonmissing_roll_goal_diff_3 = sum(!is.na(roll_goal_diff_3)),
    nonmissing_roll_ppg_3 = sum(!is.na(roll_ppg_3))
  )

if (
  first_match_check$nonmissing_roll_xg_diff_3 > 0 |
  first_match_check$nonmissing_roll_goal_diff_3 > 0 |
  first_match_check$nonmissing_roll_ppg_3 > 0
) {
  stop("Look-ahead check failed: first team-season matches have rolling values.")
}

feature_build_checks <- tibble(
  check = c(
    "input_rows",
    "output_rows",
    "unique_match_ids",
    "first_team_season_rows",
    "first_rows_with_roll_xg_diff_3",
    "first_rows_with_roll_goal_diff_3",
    "first_rows_with_roll_ppg_3",
    "complete_matchup_xg_diff_3",
    "complete_matchup_xg_diff_5",
    "complete_rest_days_diff"
  ),
  value = c(
    input_rows,
    nrow(analysis_features),
    n_distinct(analysis_features$match_id_analysis),
    first_match_check$first_team_season_rows,
    first_match_check$nonmissing_roll_xg_diff_3,
    first_match_check$nonmissing_roll_goal_diff_3,
    first_match_check$nonmissing_roll_ppg_3,
    sum(!is.na(analysis_features$matchup_xg_diff_3)),
    sum(!is.na(analysis_features$matchup_xg_diff_5)),
    sum(!is.na(analysis_features$rest_days_diff))
  )
)


# ------------------------------------------------------------
# 10. Build feature documentation
# ------------------------------------------------------------
metric_descriptions <- c(
  xg_for = "expected goals for",
  xg_against = "expected goals against",
  xg_diff = "expected-goal difference",
  goals_for = "actual goals for",
  goals_against = "actual goals against",
  goal_diff = "actual goal difference",
  ppg = "points per game",
  shots_for = "shots for",
  shots_against = "shots against",
  shot_diff = "shot difference",
  sot_for = "shots on target for",
  sot_against = "shots on target against",
  sot_diff = "shots-on-target difference"
)

feature_legend <- tibble(
  variable = character(),
  source = character(),
  variable_group = character(),
  modelling_role = character(),
  description = character(),
  notes = character()
)

feature_legend <- bind_rows(
  feature_legend,
  tibble(
    variable = c(
      "mkt_pre_overround",
      "mkt_pre_margin",
      "mkt_pre_p_home",
      "mkt_pre_p_draw",
      "mkt_pre_p_away"
    ),
    source = "Derived from harmonised average pre-closing odds",
    variable_group = "Market benchmark variables",
    modelling_role = "Benchmark and descriptive variable; not automatically a forecasting-model predictor",
    description = c(
      "Sum of raw implied probabilities from average pre-closing home, draw and away odds.",
      "Average pre-closing market overround minus one.",
      "Proportionally de-vigged average-market probability of a home win.",
      "Proportionally de-vigged average-market probability of a draw.",
      "Proportionally de-vigged average-market probability of an away win."
    ),
    notes = "Constructed before any modelling; uses no match outcome information."
  ),
  tibble(
    variable = c(
      "home_promoted",
      "away_promoted",
      "any_promoted",
      "promoted_diff"
    ),
    source = "Derived from consecutive top-flight season participation",
    variable_group = "Promoted-team controls",
    modelling_role = "Supplementary structural control",
    description = c(
      "Indicator that the home team was absent from the same league in the preceding season.",
      "Indicator that the away team was absent from the same league in the preceding season.",
      "Indicator that at least one participating team is newly promoted.",
      "Home promoted indicator minus away promoted indicator."
    ),
    notes = "The all-season parent dataset is used to identify the preceding-season team set."
  ),
  tibble(
    variable = c(
      "home_matches_played_season_pre",
      "away_matches_played_season_pre",
      "min_matches_played_season_pre",
      "early_season_lt3",
      "early_season_lt5",
      "home_rest_days",
      "away_rest_days",
      "rest_days_diff"
    ),
    source = "Derived chronologically within team-season",
    variable_group = "Timing and early-season controls",
    modelling_role = "Supplementary control and rolling-window coverage indicator",
    description = c(
      "Number of matches already played by the home team in the current season before kickoff.",
      "Number of matches already played by the away team in the current season before kickoff.",
      "Minimum of the home and away prior same-season match counts.",
      "Indicator that at least one team has fewer than three prior same-season matches.",
      "Indicator that at least one team has fewer than five prior same-season matches.",
      "Home-team days since its previous same-season match.",
      "Away-team days since its previous same-season match.",
      "Home rest days minus away rest days."
    ),
    notes = "The first match of each team-season has missing rest days and missing rolling history."
  )
)

for (window in rolling_windows) {
  for (side in c("home", "away")) {
    for (metric in names(metric_descriptions)) {
      feature_legend <- bind_rows(
        feature_legend,
        tibble(
          variable = paste0(side, "_roll_", metric, "_", window),
          source = "Derived from prior same-season team matches",
          variable_group = "Lagged rolling team features",
          modelling_role = case_when(
            metric %in% c("xg_for", "xg_against", "xg_diff") ~ "Primary rolling-strength feature",
            metric %in% c("goals_for", "goals_against", "goal_diff") ~ "Secondary robustness feature",
            metric == "ppg" ~ "Supplementary form feature",
            TRUE ~ "Supplementary shot-based feature"
          ),
          description = paste0(
            stringr::str_to_sentence(side),
            " team's lagged mean ",
            metric_descriptions[[metric]],
            " over up to the previous ",
            window,
            " same-season matches."
          ),
          notes = if (use_partial_windows) {
            "Partial early-season windows are retained; use the prior-match counters to identify window depth."
          } else {
            "Available only after the team has completed the full rolling window."
          }
        )
      )
    }
  }

  for (metric in names(metric_descriptions)) {
    feature_legend <- bind_rows(
      feature_legend,
      tibble(
        variable = paste0("matchup_", metric, "_", window),
        source = "Derived from lagged home-team and away-team rolling features",
        variable_group = "Matchup rolling differentials",
        modelling_role = case_when(
          metric %in% c("xg_for", "xg_against", "xg_diff") ~ "Primary rolling-strength differential",
          metric %in% c("goals_for", "goals_against", "goal_diff") ~ "Secondary robustness differential",
          metric == "ppg" ~ "Supplementary form differential",
          TRUE ~ "Supplementary shot-based differential"
        ),
        description = paste0(
          "Home-team rolling ",
          metric_descriptions[[metric]],
          " minus away-team rolling ",
          metric_descriptions[[metric]],
          " over the ",
          window,
          "-match window."
        ),
        notes = "Pre-match and look-ahead safe."
      )
    )
  }
}

feature_legend <- bind_rows(
  feature_legend,
  tibble(
    variable = paste0("league_", league_codes),
    source = "Derived from LeagueKey",
    variable_group = "League fixed effects",
    modelling_role = "League fixed-effect dummy",
    description = paste0(
      "Indicator equal to one for league code ",
      league_codes,
      "."
    ),
    notes = "For regression models, omit one reference category when fitting an intercept."
  )
) %>%
  distinct(variable, .keep_all = TRUE) %>%
  mutate(
    r_type = vapply(
      variable,
      function(x) class(analysis_features[[x]])[1],
      character(1)
    ),
    missing_count = vapply(
      variable,
      function(x) sum(is.na(analysis_features[[x]])),
      integer(1)
    ),
    missing_share = missing_count / nrow(analysis_features),
    nonmissing_count = nrow(analysis_features) - missing_count
  ) %>%
  select(
    variable,
    r_type,
    source,
    variable_group,
    modelling_role,
    description,
    missing_count,
    missing_share,
    nonmissing_count,
    notes
  )

# Locate the existing analysis-base legend when available.
legend_candidates <- c(
  file.path(data_dir, "analysis_base_variable_legend.csv"),
  file.path(documentation_dir, "analysis_base_variable_legend.csv"),
  file.path(project_dir, "analysis_base_variable_legend.csv")
)

existing_legend_path <- legend_candidates[file.exists(legend_candidates)][1]

if (!is.na(existing_legend_path)) {
  base_legend <- readr::read_csv(
    existing_legend_path,
    show_col_types = FALSE
  )

  feature_legend_for_binding <- feature_legend %>%
    mutate(
      column_order = max(base_legend$column_order, na.rm = TRUE) + row_number()
    ) %>%
    select(all_of(names(base_legend)))

  full_feature_legend <- bind_rows(
    base_legend,
    feature_legend_for_binding
  )
} else {
  warning(
    "The existing analysis-base legend was not found. ",
    "Only the engineered-feature legend will be saved."
  )

  full_feature_legend <- feature_legend %>%
    mutate(column_order = row_number()) %>%
    select(
      column_order,
      everything()
    )
}


# ------------------------------------------------------------
# 11. Save outputs
# ------------------------------------------------------------
attr(analysis_features, "market_benchmark_cols") <- c(
  "mkt_pre_overround", "mkt_pre_margin",
  "mkt_pre_p_home", "mkt_pre_p_draw", "mkt_pre_p_away"
)
readr::write_rds(analysis_features, feature_output_path)


readr::write_rds(
  analysis_features,
  feature_output_path
)

if (save_csv_copy) {
  readr::write_csv(
    analysis_features,
    feature_csv_output_path
  )
}

readr::write_csv(
  feature_legend,
  feature_only_legend_output_path
)

readr::write_csv(
  full_feature_legend,
  feature_legend_output_path
)

readr::write_csv(
  feature_build_checks,
  feature_checks_output_path
)

print(feature_build_checks, n = Inf)
