source("source_for_setup.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 5,
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
dir.create(paste0("<<<< FILEPATH REDACTED >>>>", "/", name))
indicator <- pipeline$indicator
indicator_group <-pipeline$indicator_group
run_date <- pipeline$run_date
Regions <- pipeline$eval_conf("Regions")
shapefile_version <- pipeline$config_list$modeling_shapefile_version
year_list <- pipeline$config_list$year_list
sf::sf_use_s2(FALSE)
options(ggrepel.max.overlaps = Inf)

## Levels and differences maps
source(paste0("<<<< FILEPATH REDACTED >>>>/map_model_results.r"))
try(map_model_results(indicator, indicator_group, run_date, type = "mean", raked = F, plot_by_year = F, lvl_year_list = year_list, reg = "BFA"))
try(map_model_results(indicator, indicator_group, run_date, type = "mean", raked = F, plot_by_year = F, lvl_year_list = year_list, reg = "KEN"))
try(map_model_results(indicator, indicator_group, run_date, type = "mean", raked = F, plot_by_year = F, lvl_year_list = year_list, reg = "NGA"))

## Aggregated data-and-estimates plots
source(paste0("<<<< FILEPATH REDACTED >>>>/make_ts_plots.r"))
try(make_ts_plots(indicator, indicator_group, run_date, Regions, paste0("<<<< FILEPATH REDACTED >>>>", "/", name), pipeline$config_list$modeling_shapefile_version, counts = F))

pipeline_postamble()
