BETTING MARKET EFFICIENCY IN EUROPEAN FOOTBALL
Replication code
=============================================

Code for an MSc thesis testing whether pre-closing bookmaker odds in the
top five European football leagues are efficient with respect to public
football fundamentals, and expected goals in particular.

Sample: 19,825 matches, 2014/15 to 2024/25. Written in R. No data files
are included; the scripts download from Football-Data.co.uk and
Understat directly.


SETUP
-----

Install the packages:

    install.packages(c("dplyr", "tidyr", "readr", "purrr",
                       "data.table", "ggplot2", "worldfootballR"))

Create a folder for the output data inside the repository:

    analysis_test/data/

Then set the project root at the start of each R session:

    Sys.setenv(THESIS_PROJECT_DIR = "/path/to/this/repository")

If you skip this, the scripts use the current working directory
instead, so opening R inside the repository folder also works.


THE SCRIPTS
-----------

Run them in order. Each one needs the output of the one before it.

01_tune_pi_learning_rates.R
    Grid search for the two pi-rating learning rates, using seasons
    before the analysis sample. Produces lambda = 0.045, gamma = 0.35.
    Standalone: downloads its own data and writes nothing to the
    analysis dataset.

02_pipeline.R
    Downloads Football-Data and Understat, aggregates the shot data to
    match level, harmonises team names, merges the two sources and
    validates the merge, then builds the pi-ratings.
    Writes analysis_base_constantinou.rds.

03_build_feature_dataset.R
    Builds the modelling variables: rolling expected goals created and
    conceded, rolling points per game, promotion indicators and the
    early-season indicator. Everything is lagged so a match never
    predicts itself.
    Writes analysis_features.rds and .csv.

04a_descriptive_suite.R
    Descriptive figures, including the bookmaker margin by season and
    the calibration of market probabilities by outcome.

04b_descriptive_analysis.R
    Descriptive tables: outcome rates by league, the market forecast
    against naive benchmarks, the five naive betting strategies, the
    random-betting Monte Carlo, and calibration in probability bins.

05_model_analysis.R
    The main analysis. Full-sample calibration tests, the three
    multinomial-logit models, the walk-forward forecasts across eight
    test seasons, forecast scores, and the betting results at average
    and maximum odds. Also the two robustness checks, on the final four
    seasons and on individual bookmaker prices.

06_devig_robustness.R
    Rebuilds the market probabilities under the Shin and power methods
    of removing the bookmaker margin and repeats the analysis under
    each.

07_cluster_bootstrap.R
    Cluster bootstrap at the league-season level, 10,000 replications.
    Produces the confidence intervals for forecast-score differences
    and for returns.

08_format_confidence_intervals.R
    Formats the estimates and intervals into the columns as they appear
    in the thesis tables.


ONE THING TO WATCH
------------------

Steps 05, 07 and 08 must be run in the same R session, in that order.
Step 05 leaves its results in memory rather than writing them to disk,
and the other two read those objects. Restarting R in between will
produce object-not-found errors.

Everything else can be run in a fresh session.


REPRODUCIBILITY
---------------

All random steps set a seed, so the numbers reproduce exactly when the
scripts are run in order from a clean session.

Confidence intervals are 95 per cent league-season cluster-bootstrap
percentile intervals over 10,000 replications. Coefficient standard
errors are conventional maximum-likelihood estimates.

The data was retrieved in July 2026. Both providers occasionally revise
historical files, so a later download may not reproduce the sample
exactly. Step 02 reports any matches dropped in validation.
