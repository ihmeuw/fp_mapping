source("source_for_setup.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 9,
    dag_has = "FAKE_HASH",
    node_name = "p07_diagnostic_plots",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = NA_integer_
  )
}

## load in the pipeline without loading in any objects from previous nodes
pipeline_preamble(inputs = inputs, names_to_load = character())

site <- 'none'
type <- 'none'
dir.create(paste0(pipeline$outputdir, '/diagnostic_plots/'))
indicator <- pipeline$indicator
indicator_group <-pipeline$indicator_group
run_date <- pipeline$run_date
Regions <- pipeline$eval_conf("Regions")
shapefile_version <- pipeline$config_list$modeling_shapefile_version
year_list <- pipeline$config_list$year_list
sf::sf_use_s2(FALSE)
options(ggrepel.max.overlaps = Inf)

## Covariate importance plots
if (as.logical(pipeline$config_list$use_stacking_covs)) {
  source("<<<< FILEPATH REDACTED >>>>/get_cov_weights.r")
  get_cov_weights(indicator, indicator_group, run_date, pipeline$eval_conf("Regions"), paste0(pipeline$outputdir, '/diagnostic_plots/'))
}

## Hyperparameters
source("<<<< FILEPATH REDACTED >>>>/plot_hyperparameters.r")
plot_hyperparameters(indicator, indicator_group, run_date, 0, 0, paste0(pipeline$outputdir, "/diagnostic_plots/hyperparameters.pdf"))

## Data-and-estimates maps
source("<<<< FILEPATH REDACTED >>>>/data_and_estimates_maps.r")
try(data_and_estimates_maps_simplified(indicator, indicator_group, run_date, 'BFA'))
try(data_and_estimates_maps_simplified(indicator, indicator_group, run_date, 'KEN'))
try(data_and_estimates_maps_simplified(indicator, indicator_group, run_date, 'NGA'))

## Levels and differences maps
source("<<<< FILEPATH REDACTED >>>>/map_model_results.r")
try(map_model_results(indicator, indicator_group, run_date, type = "mean", raked = F, plot_by_year = F, lvl_year_list = year_list, reg = "BFA"))
try(map_model_results(indicator, indicator_group, run_date, type = "mean", raked = F, plot_by_year = F, lvl_year_list = year_list, reg = "KEN"))
try(map_model_results(indicator, indicator_group, run_date, type = "mean", raked = F, plot_by_year = F, lvl_year_list = year_list, reg = "NGA"))

## Aggregated data-and-estimates plots
source("<<<< FILEPATH REDACTED >>>>/make_ts_plots.r")
try(make_ts_plots(indicator, indicator_group, run_date, Regions, paste0(pipeline$outputdir, '/diagnostic_plots/'), pipeline$config_list$modeling_shapefile_version, counts = F))

pipeline_postamble()
