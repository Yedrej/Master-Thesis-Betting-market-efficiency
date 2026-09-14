# STEP 02 — Download and build the base dataset
# Reads:  Football-Data and Understat, downloaded from source
# Writes: analysis_base_constantinou.rds / .csv
# Thesis: Section 4.1 and Table 2. Merges the two sources, validates the
#         merge, and constructs the pi-ratings.

# ============================================================
# CLEAN REPRODUCIBLE FIVE-LEAGUE FOOTBALL BETTING PIPELINE
# Seasons: 2009/10 to 2024/25
# Model window: 2014/15 to 2024/25
# Sources: Football-Data.co.uk and Understat via worldfootballR
# Purpose: Build final analysis_base with xG, odds, and corrected Constantinou-style Pi-ratings
# Author: Jedrzej Szumniak
# ============================================================

# ------------------------------------------------------------
# 00. Packages and settings
# ------------------------------------------------------------
required_packages <- c(
  "data.table", "dplyr", "purrr", "readr", "stringr", "lubridate", "tidyr", "tibble", "worldfootballR"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))
options(stringsAsFactors = FALSE, scipen = 999)

project_dir <- Sys.getenv("THESIS_PROJECT_DIR", unset = ".")
raw_fd_dir <- file.path(project_dir, "raw", "football_data")
raw_us_dir <- file.path(project_dir, "raw", "understat")
processed_dir <- file.path(project_dir, "processed")
logs_dir <- file.path(project_dir, "logs")
analysis_dir <- file.path(project_dir, "analysis_test")
analysis_data_dir <- file.path(analysis_dir, "data")
analysis_docs_dir <- file.path(analysis_dir, "docs")
analysis_logs_dir <- file.path(analysis_dir, "logs")

purrr::walk(
  c(project_dir, raw_fd_dir, raw_us_dir, processed_dir, logs_dir, analysis_dir, analysis_data_dir, analysis_docs_dir, analysis_logs_dir),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

season_start_years <- 2009:2024
understat_start_years <- 2014:2024
football_data_base_url <- "https://www.football-data.co.uk/mmz4281"

league_map <- tibble::tribble(
  ~LeagueKey, ~LeagueName,        ~FDDiv, ~USLeague,
  "ENG",      "Premier League",   "E0",   "EPL",
  "ESP",      "La Liga",          "SP1",  "La liga",
  "ITA",      "Serie A",          "I1",   "Serie A",
  "GER",      "Bundesliga",       "D1",   "Bundesliga",
  "FRA",      "Ligue 1",          "F1",   "Ligue 1"
)

# Corrected Constantinou/Fenton-style Pi parameters.
# No cold-start multiplier is used.
pi_params <- list(
  lambda = 0.045,
  gamma = 0.35
)

# ------------------------------------------------------------
# 01. Helper functions
# ------------------------------------------------------------
team_name_dictionary <- tibble::tribble(
  ~name_raw,                ~name_std,
  "Man United",             "Manchester United",
  "Man City",               "Manchester City",
  "Newcastle",              "Newcastle United",
  "Nott'm Forest",          "Nottingham Forest",
  "Wolves",                 "Wolverhampton Wanderers",
  "West Brom",              "West Bromwich Albion",
  "QPR",                    "Queens Park Rangers",
  "Ath Madrid",             "Atletico Madrid",
  "Ath Bilbao",             "Athletic Club",
  "Sociedad",               "Real Sociedad",
  "Celta",                  "Celta Vigo",
  "Espanol",                "Espanyol",
  "Sp Gijon",               "Sporting Gijon",
  "La Coruna",              "Deportivo La Coruna",
  "Dep La Coruna",          "Deportivo La Coruna",
  "Vallecano",              "Rayo Vallecano",
  "Dortmund",               "Borussia Dortmund",
  "M'gladbach",             "Borussia Monchengladbach",
  "Mgladbach",              "Borussia Monchengladbach",
  "Hertha",                 "Hertha Berlin",
  "Leverkusen",             "Bayer Leverkusen",
  "Ein Frankfurt",          "Eintracht Frankfurt",
  "Mainz",                  "Mainz 05",
  "FC Koln",                "FC Cologne",
  "Koln",                   "FC Cologne",
  "Hannover",               "Hannover 96",
  "Dusseldorf",             "Fortuna Dusseldorf",
  "St Pauli",               "St. Pauli",
  "Verona",                 "Hellas Verona",
  "Spal",                   "SPAL",
  "Paris SG",               "Paris Saint-Germain",
  "PSG",                    "Paris Saint-Germain",
  "St Etienne",             "Saint-Etienne",
  "Ajaccio GFCO",           "GFC Ajaccio",
  "Evian",                  "Evian Thonon Gaillard"
)

make_fd_season_code <- function(start_year) {
  sprintf("%02d%02d", start_year %% 100, (start_year + 1) %% 100)
}

make_season_label <- function(start_year) {
  paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100))
}

is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

safe_as_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))
safe_as_integer <- function(x) suppressWarnings(as.integer(as.character(x)))

standardise_team_name <- function(x) {
  x_clean <- stringr::str_squish(as.character(x))
  matched_idx <- match(x_clean, team_name_dictionary$name_raw)
  ifelse(!is.na(matched_idx), team_name_dictionary$name_std[matched_idx], x_clean)
}

# Final date parser: use actual year in Football-Data date where possible.
parse_fd_date_final <- function(date_raw, season_start) {
  date_chr <- stringr::str_squish(as.character(date_raw))
  date_chr <- stringr::str_replace_all(date_chr, "-", "/")

  iso_flag <- stringr::str_detect(as.character(date_raw), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  iso_date <- suppressWarnings(as.Date(as.character(date_raw)))

  dmy_parts <- stringr::str_match(date_chr, "^(\\d{1,2})/(\\d{1,2})/(\\d{2}|\\d{4})")
  day_part <- suppressWarnings(as.integer(dmy_parts[, 2]))
  month_part <- suppressWarnings(as.integer(dmy_parts[, 3]))
  year_token <- dmy_parts[, 4]
  year_part <- suppressWarnings(as.integer(year_token))

  year_part <- dplyr::case_when(
    is.na(year_part) ~ NA_integer_,
    nchar(year_token) == 2 & year_part <= 50 ~ 2000L + year_part,
    nchar(year_token) == 2 & year_part > 50 ~ 1900L + year_part,
    TRUE ~ year_part
  )

  parsed_from_raw <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", year_part, month_part, day_part)))

  fallback_year <- dplyr::if_else(month_part >= 8, as.integer(season_start), as.integer(season_start) + 1L)
  parsed_fallback <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", fallback_year, month_part, day_part)))

  parsed <- dplyr::coalesce(parsed_from_raw, iso_date, parsed_fallback)

  # If a parser-generated ISO date is outside the football season but the month/day are known,
  # fall back to football-season logic.
  parsed_year <- lubridate::year(parsed)
  bad_year <- !is.na(parsed_year) & (parsed_year < season_start | parsed_year > season_start + 1)
  parsed <- dplyr::if_else(bad_year, parsed_fallback, parsed)

  parsed
}

normalise_shot_side <- function(x) {
  x <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    x %in% c("h", "home") ~ "h",
    x %in% c("a", "away") ~ "a",
    TRUE ~ NA_character_
  )
}

harmonise_team_for_xg <- function(x) {
  x <- stringr::str_squish(as.character(x))
  dplyr::case_when(
    x %in% c("Betis", "Real Betis") ~ "Betis",
    x %in% c("Valladolid", "Real Valladolid") ~ "Valladolid",
    x %in% c("Huesca", "SD Huesca") ~ "Huesca",
    x %in% c("Paris Saint-Germain", "Paris Saint Germain") ~ "Paris Saint-Germain",
    x %in% c("Bastia", "SC Bastia") ~ "Bastia",
    x %in% c("Clermont", "Clermont Foot") ~ "Clermont",
    x %in% c("Borussia Monchengladbach", "Borussia M.Gladbach") ~ "Borussia Monchengladbach",
    x %in% c("Hamburg", "Hamburger SV") ~ "Hamburg",
    x %in% c("Stuttgart", "VfB Stuttgart") ~ "Stuttgart",
    x %in% c("RB Leipzig", "RasenBallsport Leipzig") ~ "RB Leipzig",
    x %in% c("Fortuna Dusseldorf", "Fortuna Duesseldorf") ~ "Fortuna Dusseldorf",
    x %in% c("Nurnberg", "Nuernberg") ~ "Nurnberg",
    x %in% c("Bielefeld", "Arminia Bielefeld") ~ "Bielefeld",
    x %in% c("Greuther Furth", "Greuther Fuerth") ~ "Greuther Furth",
    x %in% c("Heidenheim", "FC Heidenheim") ~ "Heidenheim",
    x %in% c("Milan", "AC Milan") ~ "Milan",
    x %in% c("SPAL", "SPAL 2013") ~ "SPAL",
    x %in% c("Parma", "Parma Calcio 1913") ~ "Parma",
    TRUE ~ x
  )
}

safe_read_csv_url <- function(url) {
  tryCatch(
    data.table::fread(url, showProgress = FALSE, encoding = "UTF-8"),
    error = function(e1) {
      tryCatch(
        readr::read_csv(url, show_col_types = FALSE) |> as.data.table(),
        error = function(e2) {
          message("FAILED TO DOWNLOAD: ", url)
          NULL
        }
      )
    }
  )
}

# Pi helper functions
pi_weight_error <- function(error_value) {
  sign(error_value) * 3 * log10(1 + abs(error_value))
}

pi_rating_to_goal_diff <- function(rating) {
  sign(rating) * (10^(abs(rating) / 3) - 1)
}

compute_pi_ratings <- function(matches_df, pi_params, cutoff_season_start = 2013L) {
  matches_df <- matches_df |>
    arrange(MatchDate, HomeTeam_clean, AwayTeam_clean) |>
    mutate(RowID = row_number())

  teams <- sort(unique(c(matches_df$HomeTeam_clean, matches_df$AwayTeam_clean)))

  state <- tibble::tibble(
    LeagueKey = matches_df$LeagueKey[1],
    LeagueName = matches_df$LeagueName[1],
    Team = teams,
    home_rating = 0,
    away_rating = 0,
    matches_played = 0L
  )

  results_list <- vector("list", nrow(matches_df))
  cutoff_snapshot <- NULL

  for (i in seq_len(nrow(matches_df))) {
    row_i <- matches_df[i, ]

    home_idx <- match(row_i$HomeTeam_clean, state$Team)
    away_idx <- match(row_i$AwayTeam_clean, state$Team)

    home_home_pre <- state$home_rating[home_idx]
    home_away_pre <- state$away_rating[home_idx]
    away_home_pre <- state$home_rating[away_idx]
    away_away_pre <- state$away_rating[away_idx]

    home_matches_pre <- state$matches_played[home_idx]
    away_matches_pre <- state$matches_played[away_idx]

    home_expected_gd_pre <- pi_rating_to_goal_diff(home_home_pre)
    away_expected_gd_pre <- pi_rating_to_goal_diff(away_away_pre)

    predicted_home_goal_diff <- home_expected_gd_pre - away_expected_gd_pre
    predicted_away_goal_diff <- -predicted_home_goal_diff

    observed_home_goal_diff <- as.numeric(row_i$FTHG) - as.numeric(row_i$FTAG)
    observed_away_goal_diff <- -observed_home_goal_diff

    error_home <- observed_home_goal_diff - predicted_home_goal_diff
    error_away <- observed_away_goal_diff - predicted_away_goal_diff

    update_home <- pi_weight_error(error_home) * pi_params$lambda
    update_away <- pi_weight_error(error_away) * pi_params$lambda

    home_home_post <- home_home_pre + update_home
    home_away_post <- home_away_pre + pi_params$gamma * (home_home_post - home_home_pre)

    away_away_post <- away_away_pre + update_away
    away_home_post <- away_home_pre + pi_params$gamma * (away_away_post - away_away_pre)

    state$home_rating[home_idx] <- home_home_post
    state$away_rating[home_idx] <- home_away_post
    state$away_rating[away_idx] <- away_away_post
    state$home_rating[away_idx] <- away_home_post

    state$matches_played[home_idx] <- state$matches_played[home_idx] + 1L
    state$matches_played[away_idx] <- state$matches_played[away_idx] + 1L

    results_list[[i]] <- tibble::tibble(
      pi_home_home_rating_pre = home_home_pre,
      pi_home_away_rating_pre = home_away_pre,
      pi_away_home_rating_pre = away_home_pre,
      pi_away_away_rating_pre = away_away_pre,
      pi_home_expected_gd_pre = home_expected_gd_pre,
      pi_away_expected_gd_pre = away_expected_gd_pre,
      pi_rating_diff_pre = predicted_home_goal_diff,
      pi_observed_goal_diff = observed_home_goal_diff,
      pi_error_home = error_home,
      pi_error_away = error_away,
      pi_home_matches_played_pre = home_matches_pre,
      pi_away_matches_played_pre = away_matches_pre,
      pi_home_home_rating_post = home_home_post,
      pi_home_away_rating_post = home_away_post,
      pi_away_home_rating_post = away_home_post,
      pi_away_away_rating_post = away_away_post
    )

    reached_cutoff <- row_i$SeasonStart == cutoff_season_start
    next_is_after_cutoff <- (i == nrow(matches_df)) || (matches_df$SeasonStart[i + 1] > cutoff_season_start)

    if (reached_cutoff && next_is_after_cutoff) {
      cutoff_snapshot <- state |>
        transmute(
          LeagueKey,
          LeagueName,
          Team,
          pi_home_rating = home_rating,
          pi_away_rating = away_rating,
          pi_matches_played = matches_played,
          SnapshotSeason = make_season_label(cutoff_season_start)
        )
    }
  }

  final_state <- state |>
    transmute(
      LeagueKey,
      LeagueName,
      Team,
      pi_home_rating = home_rating,
      pi_away_rating = away_rating,
      pi_matches_played = matches_played,
      SnapshotSeason = "Final sample end"
    )

  list(
    matches = bind_cols(matches_df, bind_rows(results_list)),
    cutoff_snapshot = cutoff_snapshot,
    final_state = final_state
  )
}

# ------------------------------------------------------------
# 02. Download and clean Football-Data
# ------------------------------------------------------------
download_fd_league <- function(LeagueKey, LeagueName, FDDiv) {
  message("Downloading Football-Data for ", LeagueName)
  purrr::map_dfr(season_start_years, function(start_year) {
    season_code <- make_fd_season_code(start_year)
    season_label <- make_season_label(start_year)
    url <- paste0(football_data_base_url, "/", season_code, "/", FDDiv, ".csv")
    dt <- safe_read_csv_url(url)
    Sys.sleep(0.25)
    if (is.null(dt)) return(tibble())
    dt |>
      as_tibble() |>
      mutate(
        LeagueKey = LeagueKey,
        LeagueName = LeagueName,
        DivCode = FDDiv,
        SeasonStart = start_year,
        SeasonCode = season_code,
        SeasonLabel = season_label,
        Source = "Football-Data.co.uk",
        SourceURL = url
      )
  })
}

fd_all_raw <- purrr::pmap_dfr(league_map[, c("LeagueKey", "LeagueName", "FDDiv")], download_fd_league)
readr::write_csv(fd_all_raw, file.path(raw_fd_dir, "football_data_all_raw.csv"))

fd_all_clean <- fd_all_raw |>
  mutate(
    MatchDate = parse_fd_date_final(Date, SeasonStart),
    HomeTeam_clean = standardise_team_name(HomeTeam),
    AwayTeam_clean = standardise_team_name(AwayTeam),
    FTHG = safe_as_integer(FTHG),
    FTAG = safe_as_integer(FTAG),
    FTR = as.character(FTR)
  ) |>
  filter(!(is_blank(Date) | is_blank(HomeTeam) | is_blank(AwayTeam))) |>
  filter(!is.na(MatchDate)) |>
  arrange(LeagueKey, SeasonStart, MatchDate, HomeTeam_clean, AwayTeam_clean)

# Date validation
date_problems <- fd_all_clean |>
  mutate(
    MatchYear = lubridate::year(MatchDate),
    DateProblem = is.na(MatchDate) | MatchYear < SeasonStart | MatchYear > SeasonStart + 1
  ) |>
  filter(DateProblem) |>
  select(LeagueKey, LeagueName, SeasonStart, SeasonLabel, Date, MatchDate, HomeTeam, AwayTeam)

readr::write_csv(date_problems, file.path(logs_dir, "football_data_date_problems.csv"))
if (nrow(date_problems) > 0) stop("Date cleaning failed. Inspect logs/football_data_date_problems.csv.")

fd_counts <- fd_all_clean |> count(LeagueKey, LeagueName, SeasonStart, SeasonLabel, name = "n_matches")
fd_duplicates <- fd_all_clean |> count(LeagueKey, SeasonStart, HomeTeam_clean, AwayTeam_clean, name = "n_rows") |> filter(n_rows > 1)
readr::write_csv(fd_all_clean, file.path(processed_dir, "football_data_all_clean.csv"))
readr::write_csv(fd_counts, file.path(logs_dir, "football_data_counts_by_league_season.csv"))
readr::write_csv(fd_duplicates, file.path(logs_dir, "football_data_duplicate_keys.csv"))

# ------------------------------------------------------------
# 03. Download Understat and aggregate xG
# ------------------------------------------------------------
download_understat_league <- function(LeagueKey, LeagueName, USLeague) {
  message("Downloading Understat shots for ", LeagueName)
  raw_shots <- worldfootballR::load_understat_league_shots(league = USLeague)
  raw_shots |>
    as_tibble() |>
    mutate(LeagueKey = LeagueKey, LeagueName = LeagueName, USLeague = USLeague)
}

us_shots_raw <- purrr::pmap_dfr(league_map[, c("LeagueKey", "LeagueName", "USLeague")], download_understat_league)
readr::write_csv(us_shots_raw, file.path(raw_us_dir, "understat_shots_all_raw.csv"))

us_shots_clean <- us_shots_raw |>
  mutate(
    SeasonStart = safe_as_integer(season),
    MatchDate_us = as.Date(lubridate::parse_date_time(as.character(date), orders = c("ymd HMS", "ymd HM", "ymd", "ymd HMS z"), quiet = TRUE)),
    xG = safe_as_numeric(xG),
    shot_side_raw = dplyr::coalesce(as.character(h_a), as.character(home_away)),
    shot_side = normalise_shot_side(shot_side_raw),
    HomeTeam_us = as.character(home_team),
    AwayTeam_us = as.character(away_team),
    HomeTeam_clean = standardise_team_name(home_team),
    AwayTeam_clean = standardise_team_name(away_team),
    FTHG_us = safe_as_integer(home_goals),
    FTAG_us = safe_as_integer(away_goals)
  ) |>
  filter(SeasonStart %in% understat_start_years)

us_match_xg_all <- us_shots_clean |>
  group_by(LeagueKey, LeagueName, SeasonStart, match_id, MatchDate_us, HomeTeam_us, AwayTeam_us, HomeTeam_clean, AwayTeam_clean, FTHG_us, FTAG_us) |>
  summarise(
    home_xG = sum(xG[shot_side == "h"], na.rm = TRUE),
    away_xG = sum(xG[shot_side == "a"], na.rm = TRUE),
    home_shots_us = sum(shot_side == "h", na.rm = TRUE),
    away_shots_us = sum(shot_side == "a", na.rm = TRUE),
    total_xG = sum(xG, na.rm = TRUE),
    missing_shot_side = sum(is.na(shot_side)),
    .groups = "drop"
  ) |>
  arrange(LeagueKey, SeasonStart, MatchDate_us, HomeTeam_clean, AwayTeam_clean)

us_counts <- us_match_xg_all |> count(LeagueKey, LeagueName, SeasonStart, name = "n_matches")
us_duplicates <- us_match_xg_all |> count(LeagueKey, SeasonStart, HomeTeam_clean, AwayTeam_clean, name = "n_rows") |> filter(n_rows > 1)
readr::write_csv(us_match_xg_all, file.path(processed_dir, "understat_match_xg_all.csv"))
readr::write_csv(us_counts, file.path(logs_dir, "understat_counts_by_league_season.csv"))
readr::write_csv(us_duplicates, file.path(logs_dir, "understat_duplicate_keys.csv"))

# ------------------------------------------------------------
# 04. Merge Football-Data with Understat xG
# ------------------------------------------------------------
fd_model <- fd_all_clean |>
  filter(SeasonStart %in% understat_start_years) |>
  mutate(
    HomeTeam_xg_key = harmonise_team_for_xg(HomeTeam_clean),
    AwayTeam_xg_key = harmonise_team_for_xg(AwayTeam_clean)
  )

us_match_xg_fixed <- us_match_xg_all |>
  mutate(
    HomeTeam_xg_key = harmonise_team_for_xg(HomeTeam_clean),
    AwayTeam_xg_key = harmonise_team_for_xg(AwayTeam_clean)
  ) |>
  select(
    LeagueKey, SeasonStart, HomeTeam_xg_key, AwayTeam_xg_key,
    MatchDate_us, match_id, HomeTeam_us, AwayTeam_us, FTHG_us, FTAG_us,
    home_xG, away_xG, home_shots_us, away_shots_us, total_xG, missing_shot_side
  )

final_data_xg_fixed_v2 <- fd_model |>
  left_join(
    us_match_xg_fixed,
    by = c("LeagueKey", "SeasonStart", "HomeTeam_xg_key", "AwayTeam_xg_key")
  ) |>
  mutate(
    date_diff_days = as.integer(MatchDate_us - MatchDate),
    score_match = case_when(
      is.na(FTHG_us) | is.na(FTAG_us) ~ NA,
      TRUE ~ FTHG == FTHG_us & FTAG == FTAG_us
    ),
    has_understat_xg = !is.na(home_xG) & !is.na(away_xG),
    date_warning = case_when(
      is.na(MatchDate_us) ~ "Missing Understat match",
      score_match == FALSE ~ "Score mismatch",
      abs(date_diff_days) > 2 ~ "Large date mismatch, same score",
      TRUE ~ "OK"
    )
  ) |>
  arrange(LeagueKey, SeasonStart, MatchDate, HomeTeam_clean, AwayTeam_clean)

merge_validation <- final_data_xg_fixed_v2 |>
  group_by(LeagueKey, LeagueName, SeasonStart, SeasonLabel) |>
  summarise(
    n_matches = n(),
    matched_xg = sum(has_understat_xg, na.rm = TRUE),
    missing_xg = sum(!has_understat_xg, na.rm = TRUE),
    score_mismatches = sum(score_match == FALSE, na.rm = TRUE),
    large_date_mismatches = sum(!is.na(date_diff_days) & abs(date_diff_days) > 2),
    .groups = "drop"
  )
readr::write_csv(final_data_xg_fixed_v2, file.path(processed_dir, "matches_merged_xg_fixed_model_window.csv"))
readr::write_csv(merge_validation, file.path(logs_dir, "merge_validation_xg_fixed.csv"))

excluded_matches <- final_data_xg_fixed_v2 |>
  filter(is.na(home_xG) | is.na(away_xG) | score_match == FALSE) |>
  mutate(
    exclusion_reason = case_when(
      is.na(home_xG) | is.na(away_xG) ~ "Missing Understat xG",
      score_match == FALSE ~ "Score mismatch between Football-Data and Understat",
      TRUE ~ "Other"
    )
  ) |>
  arrange(LeagueKey, SeasonStart, MatchDate, HomeTeam)
readr::write_csv(excluded_matches, file.path(analysis_logs_dir, "excluded_matches_strict_model_sample.csv"))


#EXLCUDE 1 OBSERVATION WITH NO ODDS
analysis_base %>%
  filter(
    is.na(odds_avg_pre_home) |
      is.na(odds_avg_pre_draw) |
      is.na(odds_avg_pre_away)
  ) %>%
  select(
    match_id_analysis,
    LeagueName,
    SeasonLabel,
    MatchDate,
    HomeTeam,
    AwayTeam,
    odds_avg_pre_home,
    odds_avg_pre_draw,
    odds_avg_pre_away
  )

analysis_base <- analysis_base %>%
  filter(
    !is.na(odds_avg_pre_home),
    !is.na(odds_avg_pre_draw),
    !is.na(odds_avg_pre_away)
  )

readr::write_rds(
  analysis_base,
  file.path(
    analysis_dir,
    "data",
    "analysis_base_constantinou.rds"
  )
)

readr::write_csv(
  analysis_base,
  file.path(
    analysis_dir,
    "data",
    "analysis_base_constantinou.csv"
  )
)

# ------------------------------------------------------------
# 05. Corrected Pi-ratings from all-season Football-Data
# ------------------------------------------------------------
matches_for_pi <- fd_all_clean |>
  filter(!is.na(FTHG), !is.na(FTAG)) |>
  arrange(LeagueKey, MatchDate, HomeTeam_clean, AwayTeam_clean)

pi_results_by_league <- split(matches_for_pi, matches_for_pi$LeagueKey) |>
  purrr::map(function(league_data) {
    compute_pi_ratings(matches_df = league_data, pi_params = pi_params, cutoff_season_start = 2013L)
  })

matches_with_pi_all_seasons <- purrr::map_dfr(pi_results_by_league, "matches")
pi_initial_ratings_2013_14 <- purrr::map(pi_results_by_league, "cutoff_snapshot") |> purrr::compact() |> bind_rows()
pi_final_ratings <- purrr::map_dfr(pi_results_by_league, "final_state")

if ("pi_effective_k" %in% names(matches_with_pi_all_seasons)) stop("Invalid old Pi variable pi_effective_k exists.")

pi_cols <- grep("^pi_", names(matches_with_pi_all_seasons), value = TRUE)
pi_for_join <- matches_with_pi_all_seasons |>
  select(LeagueKey, SeasonStart, MatchDate, HomeTeam_clean, AwayTeam_clean, all_of(pi_cols))

pi_duplicate_keys <- pi_for_join |>
  count(LeagueKey, SeasonStart, MatchDate, HomeTeam_clean, AwayTeam_clean, name = "n_rows") |>
  filter(n_rows > 1)
if (nrow(pi_duplicate_keys) > 0) {
  print(pi_duplicate_keys)
  stop("Duplicate Pi join keys found.")
}

final_data_pi_fixed <- final_data_xg_fixed_v2 |>
  select(-matches("^pi_")) |>
  left_join(pi_for_join, by = c("LeagueKey", "SeasonStart", "MatchDate", "HomeTeam_clean", "AwayTeam_clean"))

final_data_clean_final <- final_data_pi_fixed |>
  filter(!is.na(home_xG), !is.na(away_xG), score_match == TRUE)

# ------------------------------------------------------------
# 06. Odds harmonisation
# ------------------------------------------------------------
final_data_clean_final <- final_data_clean_final |>
  mutate(
    odds_avg_pre_home = if_else(SeasonStart <= 2018, BbAvH, AvgH),
    odds_avg_pre_draw = if_else(SeasonStart <= 2018, BbAvD, AvgD),
    odds_avg_pre_away = if_else(SeasonStart <= 2018, BbAvA, AvgA),
    odds_max_pre_home = if_else(SeasonStart <= 2018, BbMxH, MaxH),
    odds_max_pre_draw = if_else(SeasonStart <= 2018, BbMxD, MaxD),
    odds_max_pre_away = if_else(SeasonStart <= 2018, BbMxA, MaxA),
    odds_avg_close_home = AvgCH,
    odds_avg_close_draw = AvgCD,
    odds_avg_close_away = AvgCA,
    odds_max_close_home = MaxCH,
    odds_max_close_draw = MaxCD,
    odds_max_close_away = MaxCA,
    has_pre_avgmax_odds = !is.na(odds_avg_pre_home) & !is.na(odds_avg_pre_draw) & !is.na(odds_avg_pre_away) &
      !is.na(odds_max_pre_home) & !is.na(odds_max_pre_draw) & !is.na(odds_max_pre_away),
    has_market_avgmax_closing_odds = !is.na(odds_avg_close_home) & !is.na(odds_avg_close_draw) & !is.na(odds_avg_close_away) &
      !is.na(odds_max_close_home) & !is.na(odds_max_close_draw) & !is.na(odds_max_close_away)
  )

# ------------------------------------------------------------
# 07. Final quality checks
# ------------------------------------------------------------
final_checks <- final_data_clean_final |>
  summarise(
    rows = n(),
    first_season = min(SeasonStart, na.rm = TRUE),
    last_season = max(SeasonStart, na.rm = TRUE),
    missing_xG = sum(is.na(home_xG) | is.na(away_xG)),
    score_mismatches = sum(score_match == FALSE, na.rm = TRUE),
    missing_pi_diff = sum(is.na(pi_rating_diff_pre)),
    missing_pre_avgmax_odds = sum(!has_pre_avgmax_odds),
    missing_market_avgmax_closing_odds = sum(!has_market_avgmax_closing_odds),
    has_pi_effective_k = "pi_effective_k" %in% names(final_data_clean_final)
  )
print(final_checks)

if (final_checks$missing_xG != 0) stop("xG missingness found in final strict dataset.")
if (final_checks$score_mismatches != 0) stop("Score mismatch found in final strict dataset.")
if (final_checks$missing_pi_diff != 0) stop("Missing corrected Pi-ratings found.")
if (final_checks$has_pi_effective_k) stop("pi_effective_k still exists. Stop and inspect.")

pi_distribution <- final_data_clean_final |>
  summarise(
    min_pi = min(pi_rating_diff_pre, na.rm = TRUE),
    q01 = quantile(pi_rating_diff_pre, 0.01, na.rm = TRUE),
    q05 = quantile(pi_rating_diff_pre, 0.05, na.rm = TRUE),
    q25 = quantile(pi_rating_diff_pre, 0.25, na.rm = TRUE),
    median = median(pi_rating_diff_pre, na.rm = TRUE),
    q75 = quantile(pi_rating_diff_pre, 0.75, na.rm = TRUE),
    q95 = quantile(pi_rating_diff_pre, 0.95, na.rm = TRUE),
    q99 = quantile(pi_rating_diff_pre, 0.99, na.rm = TRUE),
    max_pi = max(pi_rating_diff_pre, na.rm = TRUE),
    n_abs_gt_3 = sum(abs(pi_rating_diff_pre) > 3, na.rm = TRUE),
    n_abs_gt_4 = sum(abs(pi_rating_diff_pre) > 4, na.rm = TRUE)
  )
print(pi_distribution)

readr::write_csv(final_checks, file.path(analysis_logs_dir, "final_checks_constantinou.csv"))
readr::write_csv(pi_distribution, file.path(analysis_logs_dir, "pi_distribution_constantinou.csv"))
readr::write_csv(pi_initial_ratings_2013_14, file.path(analysis_logs_dir, "pi_initial_ratings_2013_14_constantinou.csv"))
readr::write_csv(pi_final_ratings, file.path(analysis_logs_dir, "pi_final_ratings_constantinou.csv"))

# ------------------------------------------------------------
# 08. Build analysis_base
# ------------------------------------------------------------
remove_from_analysis <- c(
  "V71", "V72", "V73",
  "RowID", "InUnderstatWindow", "InBurnInWindow",
  "MatchDate_us", "HomeTeam_us", "AwayTeam_us", "FTHG_us", "FTAG_us",
  "date_diff_days", "score_match", "has_understat_xg", "HomeTeam_xg_key", "AwayTeam_xg_key",
  "covid_date_patch", "date_warning", "MatchDate_original", "MatchDate_old", "date_changed", "covid_date_error",
  "pi_observed_goal_diff", "pi_error_home", "pi_error_away",
  "pi_home_home_rating_post", "pi_home_away_rating_post", "pi_away_home_rating_post", "pi_away_away_rating_post"
)

analysis_base <- final_data_clean_final |>
  arrange(LeagueKey, SeasonStart, MatchDate, HomeTeam_clean, AwayTeam_clean) |>
  mutate(
    match_id_analysis = row_number(),
    match_key = paste(LeagueKey, SeasonStart, MatchDate, HomeTeam_clean, AwayTeam_clean, sep = "__")
  ) |>
  select(match_id_analysis, match_key, everything(), -any_of(remove_from_analysis))

analysis_checks <- analysis_base |>
  summarise(
    rows = n(),
    columns = ncol(analysis_base),
    missing_xG = sum(is.na(home_xG) | is.na(away_xG)),
    missing_pi = sum(is.na(pi_rating_diff_pre)),
    missing_pre_avgmax_odds = sum(!has_pre_avgmax_odds),
    missing_market_avgmax_closing_odds = sum(!has_market_avgmax_closing_odds),
    has_pi_effective_k = "pi_effective_k" %in% names(analysis_base)
  )
print(analysis_checks)
readr::write_csv(analysis_checks, file.path(analysis_logs_dir, "analysis_base_checks_constantinou.csv"))

# ============================================================
# STEP 8B: FINAL GENERAL SANITY CHECKS BEFORE SAVING
# ============================================================

# Create logs folder if it does not already exist
dir.create(file.path(analysis_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Overall dataset size and basic coverage
# ------------------------------------------------------------
final_overview_check <- analysis_base %>%
  summarise(
    rows = n(),
    columns = ncol(analysis_base),
    first_date = min(MatchDate, na.rm = TRUE),
    last_date = max(MatchDate, na.rm = TRUE),
    first_season = min(SeasonStart, na.rm = TRUE),
    last_season = max(SeasonStart, na.rm = TRUE),
    leagues = n_distinct(LeagueKey),
    missing_xG = sum(is.na(home_xG) | is.na(away_xG)),
    missing_pi = sum(is.na(pi_rating_diff_pre)),
    duplicate_match_keys = n() - n_distinct(match_key),
    has_pi_effective_k = "pi_effective_k" %in% names(analysis_base)
  )

print(final_overview_check)

# Hard stops for things that should never be true
if (final_overview_check$missing_xG != 0) stop("Missing xG found in analysis_base.")
if (final_overview_check$missing_pi != 0) stop("Missing Pi-rating values found in analysis_base.")
if (final_overview_check$duplicate_match_keys != 0) stop("Duplicate match_key values found.")
if (final_overview_check$has_pi_effective_k) stop("pi_effective_k still exists. Stop and inspect.")

# ------------------------------------------------------------
# 2. Match counts by league and season
# ------------------------------------------------------------

league_season_counts <- analysis_base %>%
  count(
    LeagueKey,
    LeagueName,
    SeasonStart,
    SeasonLabel,
    name = "n_matches"
  ) %>%
  arrange(LeagueKey, SeasonStart)

print(league_season_counts, n = Inf)

readr::write_csv(
  league_season_counts,
  file.path(analysis_dir, "logs", "check_match_counts_by_league_season.csv")
)

# ------------------------------------------------------------
# 3. Outcome distribution: home/draw/away wins
# ------------------------------------------------------------
outcome_distribution_overall <- analysis_base %>%
  count(FTR, name = "n") %>%
  mutate(share = n / sum(n)) %>%
  arrange(FTR)

print(outcome_distribution_overall)

outcome_distribution_by_league <- analysis_base %>%
  count(LeagueKey, LeagueName, FTR, name = "n") %>%
  group_by(LeagueKey, LeagueName) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  arrange(LeagueKey, FTR)

print(outcome_distribution_by_league, n = Inf)

outcome_distribution_by_league_season <- analysis_base %>%
  count(LeagueKey, LeagueName, SeasonStart, SeasonLabel, FTR, name = "n") %>%
  group_by(LeagueKey, LeagueName, SeasonStart, SeasonLabel) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  arrange(LeagueKey, SeasonStart, FTR)

print(outcome_distribution_by_league_season, n = Inf)

readr::write_csv(
  outcome_distribution_overall,
  file.path(analysis_dir, "logs", "check_outcome_distribution_overall.csv")
)

readr::write_csv(
  outcome_distribution_by_league,
  file.path(analysis_dir, "logs", "check_outcome_distribution_by_league.csv")
)

readr::write_csv(
  outcome_distribution_by_league_season,
  file.path(analysis_dir, "logs", "check_outcome_distribution_by_league_season.csv")
)

# ------------------------------------------------------------
# 4. xG summary checks
# ------------------------------------------------------------
xg_summary_by_league <- analysis_base %>%
  group_by(LeagueKey, LeagueName) %>%
  summarise(
    n_matches = n(),
    avg_home_xG = mean(home_xG, na.rm = TRUE),
    avg_away_xG = mean(away_xG, na.rm = TRUE),
    avg_total_xG = mean(total_xG, na.rm = TRUE),
    min_home_xG = min(home_xG, na.rm = TRUE),
    max_home_xG = max(home_xG, na.rm = TRUE),
    min_away_xG = min(away_xG, na.rm = TRUE),
    max_away_xG = max(away_xG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(LeagueKey)

print(xg_summary_by_league, n = Inf)

readr::write_csv(
  xg_summary_by_league,
  file.path(analysis_dir, "logs", "check_xg_summary_by_league.csv")
)

# ------------------------------------------------------------
# 5. Pi-rating distribution checks
# ------------------------------------------------------------
pi_distribution_check <- analysis_base %>%
  summarise(
    min_pi = min(pi_rating_diff_pre, na.rm = TRUE),
    q01 = quantile(pi_rating_diff_pre, 0.01, na.rm = TRUE),
    q05 = quantile(pi_rating_diff_pre, 0.05, na.rm = TRUE),
    q25 = quantile(pi_rating_diff_pre, 0.25, na.rm = TRUE),
    median = median(pi_rating_diff_pre, na.rm = TRUE),
    q75 = quantile(pi_rating_diff_pre, 0.75, na.rm = TRUE),
    q95 = quantile(pi_rating_diff_pre, 0.95, na.rm = TRUE),
    q99 = quantile(pi_rating_diff_pre, 0.99, na.rm = TRUE),
    max_pi = max(pi_rating_diff_pre, na.rm = TRUE),
    n_abs_gt_3 = sum(abs(pi_rating_diff_pre) > 3, na.rm = TRUE),
    n_abs_gt_4 = sum(abs(pi_rating_diff_pre) > 4, na.rm = TRUE)
  )

print(pi_distribution_check)

readr::write_csv(
  pi_distribution_check,
  file.path(analysis_dir, "logs", "check_pi_distribution.csv")
)

# Not a hard stop at >3, but >4 should be inspected carefully
if (pi_distribution_check$n_abs_gt_4 > 0) {
  warning("Some Pi-rating differences are above 4 in absolute value. Inspect extremes.")
}

pi_extremes_check <- analysis_base %>%
  arrange(desc(abs(pi_rating_diff_pre))) %>%
  select(
    LeagueKey,
    SeasonStart,
    MatchDate,
    HomeTeam,
    AwayTeam,
    FTHG,
    FTAG,
    FTR,
    pi_rating_diff_pre,
    pi_home_home_rating_pre,
    pi_away_away_rating_pre,
    pi_home_expected_gd_pre,
    pi_away_expected_gd_pre,
    pi_home_matches_played_pre,
    pi_away_matches_played_pre
  ) %>%
  head(60)

print(pi_extremes_check, n = 60)

readr::write_csv(
  pi_extremes_check,
  file.path(analysis_dir, "logs", "check_pi_extreme_matches.csv")
)

# ------------------------------------------------------------
# 6. Odds coverage checks
# ------------------------------------------------------------
# Works whether the flags are still named has_pre_odds / has_closing_odds
# or renamed to has_pre_avgmax_odds / has_market_avgmax_closing_odds.

odds_flag_pre <- if ("has_pre_avgmax_odds" %in% names(analysis_base)) {
  "has_pre_avgmax_odds"
} else {
  "has_pre_odds"
}

odds_flag_close <- if ("has_market_avgmax_closing_odds" %in% names(analysis_base)) {
  "has_market_avgmax_closing_odds"
} else {
  "has_closing_odds"
}

odds_coverage_by_season <- analysis_base %>%
  group_by(SeasonStart, SeasonLabel) %>%
  summarise(
    n_matches = n(),
    pre_avgmax_available = sum(.data[[odds_flag_pre]], na.rm = TRUE),
    pre_avgmax_missing = sum(!.data[[odds_flag_pre]], na.rm = TRUE),
    market_avgmax_closing_available = sum(.data[[odds_flag_close]], na.rm = TRUE),
    market_avgmax_closing_missing = sum(!.data[[odds_flag_close]], na.rm = TRUE),
    nonmissing_PSCH = sum(!is.na(PSCH)),
    nonmissing_PSCD = sum(!is.na(PSCD)),
    nonmissing_PSCA = sum(!is.na(PSCA)),
    .groups = "drop"
  ) %>%
  arrange(SeasonStart)

print(odds_coverage_by_season, n = Inf)

readr::write_csv(
  odds_coverage_by_season,
  file.path(analysis_dir, "logs", "check_odds_coverage_by_season.csv")
)

# ------------------------------------------------------------
# 7. Missingness by variable
# ------------------------------------------------------------
missingness_check <- tibble::tibble(
  variable = names(analysis_base),
  missing_n = sapply(analysis_base, function(x) sum(is.na(x))),
  missing_share = missing_n / nrow(analysis_base)
) %>%
  arrange(desc(missing_share), variable)

print(missingness_check, n = Inf)

readr::write_csv(
  missingness_check,
  file.path(analysis_dir, "logs", "check_variable_missingness.csv")
)

# ------------------------------------------------------------
# 8. Remove outdated bookies
# ------------------------------------------------------------
analysis_base <- analysis_base %>%
  select(-any_of(c(
    "BSA", "BSD", "BSH",
    "GBA", "GBD", "GBH",
    "SBA", "SBD", "SBH"
  )))

#EXLCUDE 1 OBSERVATION WITH NO ODDS
analysis_base %>%
  filter(
    is.na(odds_avg_pre_home) |
      is.na(odds_avg_pre_draw) |
      is.na(odds_avg_pre_away)
  ) %>%
  select(
    match_id_analysis,
    LeagueName,
    SeasonLabel,
    MatchDate,
    HomeTeam,
    AwayTeam,
    odds_avg_pre_home,
    odds_avg_pre_draw,
    odds_avg_pre_away
  )

analysis_base <- analysis_base %>%
  filter(
    !is.na(odds_avg_pre_home),
    !is.na(odds_avg_pre_draw),
    !is.na(odds_avg_pre_away)
  )

# ------------------------------------------------------------
# 09. Save outputs
# ------------------------------------------------------------
readr::write_rds(final_data_clean_final, file.path(analysis_data_dir, "final_data_clean_final_constantinou.rds"))
readr::write_csv(final_data_clean_final, file.path(analysis_data_dir, "final_data_clean_final_constantinou.csv"))
readr::write_rds(analysis_base, file.path(analysis_data_dir, "analysis_base_constantinou.rds"))
readr::write_csv(analysis_base, file.path(analysis_data_dir, "analysis_base_constantinou.csv"))
writeLines(capture.output(sessionInfo()), con = file.path(analysis_logs_dir, "session_info_constantinou.txt"))


