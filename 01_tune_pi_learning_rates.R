# STEP 01 — Pi-rating learning rates
# Reads:  Football-Data results, 1995/96-2008/09 (downloaded here)
# Writes: grid-search diagnostics only
# Thesis: Appendix B. Selects lambda = 0.045 and gamma = 0.35, which are
#         then used in step 02. Standalone; touches no analysis data.

# ============================================================
# STANDALONE PI-RATING LEARNING-RATE GRID SEARCH
# ============================================================
# Purpose:
#   Reproduce the Constantinou-Fenton grid search for lambda and gamma
#   without changing the existing data pipeline.
#
# Calibration design:
#   - 1995/96--1999/00: rating burn-in only
#   - 2000/01--2008/09: minimise squared goal-difference error
#   - 2009/10--2013/14 remains available exclusively for warming up the
#     ratings used in the thesis analysis
#   - No match from 2009/10 onward is used to select lambda or gamma
#
# The script downloads only Football-Data results and writes diagnostic
# output. It does not overwrite analysis_base or any existing pi-ratings.
# ============================================================

# ------------------------------------------------------------
# 0. Packages and settings
# ------------------------------------------------------------
required_packages <- c(
  "data.table", "dplyr", "purrr", "readr", "tibble", "tidyr"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0L) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))
options(stringsAsFactors = FALSE, scipen = 999)

# Change only this path if required.
project_dir <- Sys.getenv("THESIS_PROJECT_DIR", unset = ".")

diagnostic_dir <- file.path(
  project_dir,
  "analysis_test",
  "diagnostics",
  "pi_learning_rate_tuning"
)

raw_cache_dir <- file.path(diagnostic_dir, "raw_football_data")
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_cache_dir, recursive = TRUE, showWarnings = FALSE)

football_data_base_url <- "https://www.football-data.co.uk/mmz4281"

league_map <- tibble::tribble(
  ~LeagueKey, ~LeagueName,        ~FDDiv,
  "ENG",      "Premier League",   "E0",
  "ESP",      "La Liga",          "SP1",
  "ITA",      "Serie A",          "I1",
  "GER",      "Bundesliga",       "D1",
  "FRA",      "Ligue 1",          "F1"
)

# Same grid as Constantinou and Fenton (2013): 20 x 20 = 400 pairs.
lambda_grid <- seq(0.005, 0.100, by = 0.005)
gamma_grid <- seq(0.05, 1.00, by = 0.05)

published_lambda <- 0.035
published_gamma <- 0.70

download_seasons <- 1995:2008

scenario_table <- tibble::tribble(
  ~Scenario,                       ~StartSeason, ~EndSeason, ~TuneStart, ~TuneEnd,
  "PRE_SAMPLE_OPTIMISATION_2000_2008",     1995L,       2008L,      2000L,    2008L
)

# ------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------
make_fd_season_code <- function(start_year) {
  sprintf("%02d%02d", start_year %% 100, (start_year + 1L) %% 100)
}

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

standardise_team_name <- function(x) {
  x <- trimws(as.character(x))
  matched <- match(x, team_name_dictionary$name_raw)
  ifelse(
    is.na(matched),
    x,
    team_name_dictionary$name_std[matched]
  )
}

# Older Football-Data files occasionally contain rows with more fields than
# the header. fread(fill = TRUE) handles this while we retain only five fields.
download_fd_file <- function(league_key, league_name, div, start_year) {
  season_code <- make_fd_season_code(start_year)
  cache_path <- file.path(
    raw_cache_dir,
    paste0(league_key, "_", season_code, ".csv")
  )

  if (!file.exists(cache_path)) {
    url <- paste0(
      football_data_base_url,
      "/",
      season_code,
      "/",
      div,
      ".csv"
    )

    utils::download.file(
      url = url,
      destfile = cache_path,
      mode = "wb",
      quiet = TRUE
    )
  }

  dt <- data.table::fread(
    cache_path,
    fill = TRUE,
    select = c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG"),
    showProgress = FALSE,
    encoding = "Latin-1"
  )

  dt[, `:=`(
    LeagueKey = league_key,
    LeagueName = league_name,
    SeasonStart = as.integer(start_year)
  )]

  dt
}

download_league_history <- function(LeagueKey, LeagueName, FDDiv) {
  message("Downloading/reading ", LeagueName)

  purrr::map_dfr(
    download_seasons,
    ~ download_fd_file(
      league_key = LeagueKey,
      league_name = LeagueName,
      div = FDDiv,
      start_year = .x
    )
  )
}

parse_fd_date <- function(date_value) {
  date_value <- gsub("-", "/", trimws(as.character(date_value)))

  parsed <- suppressWarnings(as.Date(date_value, format = "%d/%m/%Y"))
  missing <- is.na(parsed)

  parsed[missing] <- suppressWarnings(
    as.Date(date_value[missing], format = "%d/%m/%y")
  )

  parsed
}

pi_weight_error <- function(error_value) {
  sign(error_value) * 3 * log10(1 + abs(error_value))
}

pi_rating_to_goal_diff <- function(rating) {
  sign(rating) * (10^(abs(rating) / 3) - 1)
}

# Convert a league history into integer/numeric vectors. This makes the grid
# search considerably faster than repeatedly indexing tibbles by team name.
prepare_league_sequence <- function(matches_df, start_season, end_season,
                                    tune_start, tune_end) {
  matches_df <- matches_df |>
    dplyr::filter(
      SeasonStart >= start_season,
      SeasonStart <= end_season,
      !is.na(MatchDate),
      !is.na(FTHG),
      !is.na(FTAG)
    ) |>
    dplyr::arrange(MatchDate, HomeTeam_clean, AwayTeam_clean)

  teams <- sort(unique(c(
    matches_df$HomeTeam_clean,
    matches_df$AwayTeam_clean
  )))

  list(
    home_id = match(matches_df$HomeTeam_clean, teams),
    away_id = match(matches_df$AwayTeam_clean, teams),
    observed_gd = as.numeric(matches_df$FTHG - matches_df$FTAG),
    tune_match = matches_df$SeasonStart >= tune_start &
      matches_df$SeasonStart <= tune_end,
    n_teams = length(teams)
  )
}

# This is the same venue-specific update used in Pasted text(26).txt.
evaluate_pi_pair <- function(sequence, lambda, gamma) {
  home_rating <- numeric(sequence$n_teams)
  away_rating <- numeric(sequence$n_teams)

  squared_error <- 0
  n_eval <- 0L

  for (i in seq_along(sequence$observed_gd)) {
    home_id <- sequence$home_id[i]
    away_id <- sequence$away_id[i]

    home_home_pre <- home_rating[home_id]
    home_away_pre <- away_rating[home_id]
    away_home_pre <- home_rating[away_id]
    away_away_pre <- away_rating[away_id]

    predicted_gd <-
      pi_rating_to_goal_diff(home_home_pre) -
      pi_rating_to_goal_diff(away_away_pre)

    error <- sequence$observed_gd[i] - predicted_gd

    if (sequence$tune_match[i]) {
      squared_error <- squared_error + error^2
      n_eval <- n_eval + 1L
    }

    update <- lambda * pi_weight_error(error)

    home_rating[home_id] <- home_home_pre + update
    away_rating[home_id] <- home_away_pre + gamma * update
    away_rating[away_id] <- away_away_pre - update
    home_rating[away_id] <- away_home_pre - gamma * update
  }

  c(
    sse = squared_error,
    n_matches = n_eval,
    mse = squared_error / n_eval
  )
}

# Reproduce the layout of Constantinou and Fenton's Appendix B table:
# gamma in descending rows, lambda in ascending columns, and four-decimal MSE.
make_mse_table <- function(grid_results, scenario_name, scope_name) {
  grid_results |>
    dplyr::filter(
      Scenario == scenario_name,
      Scope == scope_name
    ) |>
    dplyr::mutate(
      gamma_label = sprintf("%.2f", gamma),
      lambda_label = sprintf("%.3f", lambda),
      mse = sprintf("%.5f", mse)
    ) |>
    dplyr::select(gamma, gamma_label, lambda_label, mse) |>
    tidyr::pivot_wider(
      names_from = lambda_label,
      values_from = mse
    ) |>
    dplyr::arrange(dplyr::desc(gamma)) |>
    dplyr::select(-gamma) |>
    dplyr::rename(gamma = gamma_label)
}

write_mse_latex <- function(mse_table, best_result, scope_name, output_path) {
  lambda_labels <- names(mse_table)[-1]
  lambda_values <- as.numeric(lambda_labels)
  column_specification <- paste0(
    "c",
    paste(rep("r", length(lambda_labels)), collapse = "")
  )

  table_rows <- vapply(
    seq_len(nrow(mse_table)),
    function(row_index) {
      gamma_value <- as.numeric(mse_table$gamma[row_index])
      cells <- as.character(
        unlist(mse_table[row_index, -1], use.names = FALSE)
      )

      best_cell <-
        dplyr::near(gamma_value, best_result$best_gamma[[1]]) &
        dplyr::near(lambda_values, best_result$best_lambda[[1]])

      cells[best_cell] <- paste0("\\textbf{", cells[best_cell], "}")

      paste0(
        paste(c(sprintf("%.2f", gamma_value), cells), collapse = " & "),
        " \\\\"
      )
    },
    character(1)
  )

  scope_caption <- if (scope_name == "POOLED") {
    "the pooled five-league sample"
  } else {
    paste0("the ", scope_name, " sample")
  }

  safe_scope <- tolower(gsub("[^A-Za-z0-9]+", "-", scope_name))

  latex_lines <- c(
    "% Requires \\usepackage{booktabs}, \\usepackage{graphicx}, and \\usepackage{rotating}.",
    "\\begin{sidewaystable}[!htbp]",
    "\\centering",
    paste0(
      "\\caption{Mean squared goal-difference error by pi-rating learning parameters for ",
      scope_caption,
      "}"
    ),
    paste0("\\label{tab:pi-learning-rate-mse-", safe_scope, "}"),
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{2.5pt}",
    "\\resizebox{\\textheight}{!}{%",
    paste0("\\begin{tabular}{", column_specification, "}"),
    "\\toprule",
    paste0(
      paste(
        c("$\\gamma \\backslash \\lambda$", lambda_labels),
        collapse = " & "
      ),
      " \\\\"
    ),
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{sidewaystable}"
  )

  writeLines(latex_lines, con = output_path, useBytes = TRUE)
}

# ------------------------------------------------------------
# 2. Download and clean the required match histories
# ------------------------------------------------------------
all_matches <- purrr::pmap_dfr(
  league_map,
  download_league_history
) |>
  dplyr::mutate(
    MatchDate = parse_fd_date(Date),
    HomeTeam_clean = standardise_team_name(HomeTeam),
    AwayTeam_clean = standardise_team_name(AwayTeam),
    FTHG = suppressWarnings(as.numeric(FTHG)),
    FTAG = suppressWarnings(as.numeric(FTAG))
  ) |>
  dplyr::filter(
    !is.na(MatchDate),
    !is.na(FTHG),
    !is.na(FTAG)
  )

if (nrow(all_matches) == 0L) {
  stop("No valid Football-Data matches were downloaded.")
}

# ------------------------------------------------------------
# 3. Prepare the calibration sequence
# ------------------------------------------------------------
prepared_sequences <- list()

for (scenario_index in seq_len(nrow(scenario_table))) {
  scenario <- scenario_table[scenario_index, ]
  scenario_name <- scenario$Scenario[[1]]

  prepared_sequences[[scenario_name]] <- split(
    all_matches,
    all_matches$LeagueKey
  ) |>
    purrr::map(
      ~ prepare_league_sequence(
        matches_df = .x,
        start_season = scenario$StartSeason[[1]],
        end_season = scenario$EndSeason[[1]],
        tune_start = scenario$TuneStart[[1]],
        tune_end = scenario$TuneEnd[[1]]
      )
    )
}

# ------------------------------------------------------------
# 4. Run the 20 x 20 grid
# ------------------------------------------------------------
parameter_grid <- tidyr::expand_grid(
  lambda = lambda_grid,
  gamma = gamma_grid
)

grid_results <- vector(
  "list",
  nrow(parameter_grid) * length(prepared_sequences)
)

result_index <- 0L

for (scenario_name in names(prepared_sequences)) {
  message("Running scenario: ", scenario_name)
  scenario_sequences <- prepared_sequences[[scenario_name]]

  for (parameter_index in seq_len(nrow(parameter_grid))) {
    lambda_value <- parameter_grid$lambda[parameter_index]
    gamma_value <- parameter_grid$gamma[parameter_index]

    league_results <- purrr::imap_dfr(
      scenario_sequences,
      function(sequence, league_key) {
        metric <- evaluate_pi_pair(
          sequence = sequence,
          lambda = lambda_value,
          gamma = gamma_value
        )

        tibble::tibble(
          Scenario = scenario_name,
          Scope = league_key,
          lambda = lambda_value,
          gamma = gamma_value,
          sse = unname(metric["sse"]),
          n_matches = as.integer(unname(metric["n_matches"])),
          mse = unname(metric["mse"])
        )
      }
    )

    pooled_result <- league_results |>
      dplyr::summarise(
        Scenario = scenario_name,
        Scope = "POOLED",
        lambda = lambda_value,
        gamma = gamma_value,
        sse = sum(sse),
        n_matches = sum(n_matches),
        mse = sum(sse) / sum(n_matches)
      )

    result_index <- result_index + 1L
    grid_results[[result_index]] <- dplyr::bind_rows(
      pooled_result,
      league_results
    )

    if (parameter_index %% 25L == 0L) {
      message(
        "  Completed ",
        parameter_index,
        " of ",
        nrow(parameter_grid),
        " parameter pairs"
      )
    }
  }
}

grid_results <- dplyr::bind_rows(grid_results) |>
  dplyr::mutate(
    is_published_pair =
      dplyr::near(lambda, published_lambda) &
      dplyr::near(gamma, published_gamma)
  ) |>
  dplyr::arrange(Scenario, Scope, mse, lambda, gamma)

# ------------------------------------------------------------
# 5. Summarise the optimum and compare with published values
# ------------------------------------------------------------
best_results <- grid_results |>
  dplyr::group_by(Scenario, Scope) |>
  dplyr::slice_min(mse, n = 1L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    Scenario,
    Scope,
    best_lambda = lambda,
    best_gamma = gamma,
    best_mse = mse,
    n_matches
  )

published_results <- grid_results |>
  dplyr::filter(is_published_pair) |>
  dplyr::select(
    Scenario,
    Scope,
    published_lambda = lambda,
    published_gamma = gamma,
    published_mse = mse
  )

summary_results <- best_results |>
  dplyr::left_join(
    published_results,
    by = c("Scenario", "Scope")
  ) |>
  dplyr::mutate(
    published_mse_excess_pct =
      100 * (published_mse / best_mse - 1)
  ) |>
  dplyr::arrange(Scenario, dplyr::desc(Scope == "POOLED"), Scope)

top_20_results <- grid_results |>
  dplyr::group_by(Scenario, Scope) |>
  dplyr::slice_min(mse, n = 20L, with_ties = FALSE) |>
  dplyr::ungroup()

# Construct Constantinou-style 20 x 20 MSE tables. The pooled table is printed
# to the console; pooled and league-specific tables are also saved as CSV and
# ready-to-paste LaTeX files.
mse_table_directory <- file.path(diagnostic_dir, "mse_tables")
dir.create(mse_table_directory, recursive = TRUE, showWarnings = FALSE)

scenario_name <- unique(grid_results$Scenario)
scope_order <- c(
  "POOLED",
  sort(setdiff(unique(grid_results$Scope), "POOLED"))
)

mse_tables <- purrr::set_names(
  purrr::map(
    scope_order,
    ~ make_mse_table(
      grid_results = grid_results,
      scenario_name = scenario_name,
      scope_name = .x
    )
  ),
  scope_order
)

for (scope_name in scope_order) {
  safe_scope <- tolower(gsub("[^A-Za-z0-9]+", "_", scope_name))
  mse_table <- mse_tables[[scope_name]]
  best_result <- summary_results |>
    dplyr::filter(Scope == scope_name)

  readr::write_csv(
    mse_table,
    file.path(
      mse_table_directory,
      paste0("pi_learning_rate_mse_table_", safe_scope, ".csv")
    )
  )

  write_mse_latex(
    mse_table = mse_table,
    best_result = best_result,
    scope_name = scope_name,
    output_path = file.path(
      mse_table_directory,
      paste0("pi_learning_rate_mse_table_", safe_scope, ".tex")
    )
  )
}

# ------------------------------------------------------------
# 6. Save diagnostics only
# ------------------------------------------------------------
readr::write_csv(
  grid_results,
  file.path(diagnostic_dir, "pi_learning_rate_grid.csv")
)

readr::write_csv(
  summary_results,
  file.path(diagnostic_dir, "pi_learning_rate_summary.csv")
)

readr::write_csv(
  top_20_results,
  file.path(diagnostic_dir, "pi_learning_rate_top20.csv")
)

print(summary_results, n = Inf)
