source("source_for_setup.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 20,
    dag_has = "FAKE_HASH",
    node_name = "p06_combine_postest",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = NA_integer_
  )
}

## load in the pipeline without loading in any objects from previous nodes
pipeline_preamble(inputs = inputs, names_to_load = character())

## Combine post est stuff across regions and save needed outputs
post_load_combine_save(regions = eval(parse(text = pipeline$config_list$Regions)),
                       summstats = eval(parse(text = pipeline$config_list$summstats)),
                       raked = c("unraked", if(pipeline$eval_conf("rake_countries")) "raked"),
                       ages = unique(pipeline$loopvars$age),
                       holdouts = unique(pipeline$loopvars$holdout),
                       rf_table = TRUE,
                       run_summ = TRUE,
                       indic = pipeline$indicator,
                       ig = pipeline$indicator_group,
                       run_date = pipeline$run_date,
                       sdir =  pipeline$sharedir,
                       proj = FALSE,
                       proj_folder = NULL)

## Clean up / delete unnecessary files
clean_after_postest(indicator             = pipeline$indicator,
                    indicator_group       = pipeline$indicator_group,
                    run_date              = pipeline$run_date,
                    strata                = eval(parse(text = pipeline$config_list$Regions)),
                    delete_region_rasters = F)


## validate all files are present for admin combination
raked <- unique(c(F, pipeline$eval_conf("rake_countries")))
counts_status <- check_admin_preds(regions = unique(pipeline$loopvars$region),
                                   raked = raked,
                                   indicator = pipeline$indicator,
                                   indicator_group = pipeline$indicator_group,
                                   run_date = pipeline$run_date)

## Combine region admin draws into a single file
combine_aggregation(rd =  pipeline$run_date,
                    indic =  pipeline$indicator,
                    ig =  pipeline$indicator_group,
                    ages = unique(pipeline$loopvars$age),
                    regions = unique(pipeline$loopvars$region),
                    holdouts = unique(pipeline$loopvars$holdout),
                    raked = raked,
                    counts = counts_status,
                    delete_region_files = F,
                    merge_hierarchy_list = T,
                    check_for_dupes = T)

## Make summary admin csvs for easy plotting from all-regions admin draws file
summarize_admins(ind = pipeline$indicator,
                 ig = pipeline$indicator_group,
                 run_date = pipeline$run_date,
                 summstats = c("mean", "lower", "upper", "cirange"),
                 raked = raked,
                 ad_levels = c(0, 1, 2),
                 counts = counts_status,
                 ages = unique(pipeline$loopvars$age),
                 holdouts = unique(pipeline$loopvars$holdout))


pipeline_postamble()
