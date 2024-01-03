source("source_for_setup.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 6,
    node_name = "j06_MBG_postest",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = 1
  )
}

## load in the pipeline without loading in any objects from previous
## nodes since all the necessary objects in this script are loaded as
## needed by the functions
pipeline_preamble(inputs = inputs)


## when running step-wise integration tests, need to temporarily change the pipeline outputdir
## to load in the cell draws and temp objects from the correct directory
if(pipeline$indicator == "int_testing" &
   endsWith(pipeline$run_date, "step")) {
  pipeline_temp_outputdir <- copy(pipeline$outputdir)
  pipeline$outputdir <- gsub("_([^_]+)_step", "", pipeline$outputdir)
}

print(Sys.time())

## this is where the postest_frax_script used to start

## NOTES: reg, age, holdout should all have been assigned to globals in 01_PM_Data_Prepping.R
## stratum, age, holdout are fixed to be reg, 0, 0 here until the remainder of the script is extended to other cases
stratum <- reg <- pipeline$loopvars[loopvar_index, region]
if (!exists("age")) age <- 0
if (!exists("holdout")) holdout <- 0
path_addin <- if(age != 0 | holdout != 0) paste0("_bin", age, "_", holdout) else ""

interval_mo <- 12

# Load objects from convenience temp file
temp_dir <- file.path(pipeline$outputdir, "temp_post_est/")
if(dir.exists(temp_dir)) {
  load(file.path(temp_dir, "post_est_temp_objs.RData"))
}
# Get the necessary variables out from the config object into global env
summstats <- eval(parse(text = pipeline$config_list$summstats))

# Print some settings to console
print(paste0("Indicator: ", pipeline$indicator))
print(paste0("Indicator group: ", pipeline$indicator_group))
print(paste0("Run date:", pipeline$run_date))
print(paste0("Stratum: ", stratum))
print(paste0("Pop measure: ", pipeline$config_list$pop_measure))
print(paste0("Pop release: ", pipeline$config_list$pop_release))
print(paste0("Summary stats: ", paste0(summstats, collapse = ", ")))
print(paste0("Metric space                       : ", pipeline$config_list$metric_space))
print(paste0("Subnational raking                 : ", pipeline$config_list$subnational_raking))
print(paste0("Countries not to rake at all       : ", pipeline$config_list$countries_not_to_rake))
print(paste0("Countries not to rake subnationally: ", pipeline$config_list$countries_not_to_subnat_rake))

## PREPARE RASTERS, ETC. ################################################################

# Load cell draws
message("Loading Data...")
load(paste0("<<<< FILEPATH REDACTED >>>>", "/",name, ".RData"))

if(pipeline$indicator == "int_testing" &
   endsWith(pipeline$run_date, "step")) {
  pipeline$outputdir <- pipeline_temp_outputdir
}

# Check if load was successful; stop if not
if (!exists("cell_pred")) {
  message(filename_rds)
  stop("Unable to load cell_pred object! Check to make sure that the relevant object exists.")
}

# Rake estimates
if (pipeline$config_list$rake_countries) {
  if (!exists("gbd")) {
    stop("rake_countries was specified as T in config, gbd raking targets must be provided.")
  }

  ## determine if a crosswalk is needed
  if (pipeline$config_list$modeling_shapefile_version == pipeline$config_list$raking_shapefile_version){
    crosswalk <- F
  }else{
    crosswalk <- T
  }

  # Assume linear raking unless specified as logit
  if (pipeline$config_list$rake_transform == "logit") rake_method <- "logit" else rake_method <- "linear"


  ##### Prep input data into raking:


  ## Get the simple and new_simple rasters prepped up for us
  print("Getting simple and prepped rasters")
  with_globals(new = list(modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
                          raking_shapefile_version = pipeline$config_list$raking_shapefile_version),
               raster_outputs <- prep_shapes_for_raking(
                 reg = reg,
                 modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
                 raking_shapefile_version = pipeline$config_list$raking_shapefile_version,
                 field = "loc_id",
                 raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor),
                 rake_subnational = as.logical(pipeline$config_list$subnational_raking),
                 countries_not_to_subnat_rake = pipeline$config_list$countries_not_to_subnat_rake)
  )

  ## Take out the objects from the list that actually matters to us:
  simple_raster <- raster_outputs[["simple_raster"]]
  new_simple_raster <- raster_outputs[["new_simple_raster"]]
  simple_polygon <- raster_outputs[["simple_polygon"]]
  pixel_id <- raster_outputs[["pixel_id"]]


  ##### Using fractional raking #####
  if (pipeline$config_list$metric_space == "rates") {
    print("Get GBD populations")
    with_globals(new = list(raking_shapefile_version = pipeline$config_list$raking_shapefile_version),
                 gbd_pops <- prep_gbd_pops_for_fraxrake(pop_measure = pipeline$config_list$pop_measure,
                                                        reg = reg,
                                                        year_list = pipeline$config_list$year_list,
                                                        gbd_round = 6,
                                                        decomp_step = "step5")
    )


    print("Using the rates raking and aggregation functions:")

    indicator <- pipeline$indicator
    run_date <- pipeline$run_date

    if (indicator == "mod_contra") {
      custom_population_function <- function(pop, any_contra) pop * any_contra
      custom_population_list <- list("any_contra" = paste0("<<<< FILEPATH REDACTED >>>>", "/",name, "_0.RData"))

    } else if (indicator == "need_contra") {
      custom_population_function <- function(pop, any_contra) pop * (1 - any_contra)
      custom_population_list <- list("any_contra" = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,"_0.RData"))

    } else if (indicator == "intent_use") {
      custom_population_function <- function(pop, any_contra, need_contra) pop * (1 - any_contra) * need_contra
      custom_population_list <- list("any_contra" = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,"_0.RData"),
                                     "need_contra" = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,"_0.RData"))

    } else if (indicator == "indic_demand") {
      custom_population_function <- function(pop, group_a) pop * (1 - group_a)
      custom_population_list <- list("group_a" = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,"_0.RData"))

    } else {
      custom_population_function <- NULL
      custom_population_list <- NULL
    }

    ## First, create all the fractional rake factors
    with_globals(new = list(modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
                            pop_release = pipeline$config_list$pop_release),
                 use_pop_draws <- fractional_rake_rates(
                   cell_pred = cell_pred,
                   simple_raster = simple_raster,
                   simple_polygon = simple_polygon,
                   pixel_id = pixel_id,
                   shapefile_version = pipeline$config_list$raking_shapefile_version,
                   reg = reg,
                   pop_measure = pipeline$config_list$pop_measure,
                   year_list = pipeline$config_list$year_list,
                   interval_mo = interval_mo,
                   rake_subnational = pipeline$config_list$subnational_raking,
                   age_group = age_group,
                   sex_id = sex_id,
                   sharedir = pipeline$sharedir,
                   run_date = pipeline$run_date,
                   indicator = pipeline$indicator,
                   gbd = gbd,
                   rake_method = rake_method,
                   gbd_pops = gbd_pops,
                   countries_not_to_rake = pipeline$config_list$countries_not_to_rake,
                   countries_not_to_subnat_rake = pipeline$config_list$countries_not_to_subnat_rake,
                   raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor),
                   custom_population_function = custom_population_function,
                   custom_population_list = custom_population_list
                 ))

    ## Now, create the raked cell pred files!
    with_globals(new = list(modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
                            pop_release = pipeline$config_list$pop_release),
                 outputs <- fractional_agg_rates(
                   cell_pred = cell_pred,
                   simple_raster = simple_raster,
                   simple_polygon = simple_polygon,
                   pixel_id = pixel_id,
                   shapefile_version = pipeline$config_list$raking_shapefile_version,
                   reg = reg,
                   pop_measure = pipeline$config_list$pop_measure,
                   year_list = pipeline$config_list$year_list,
                   interval_mo = interval_mo,
                   rake_subnational = pipeline$config_list$subnational_raking,
                   sharedir = pipeline$sharedir,
                   run_date = pipeline$run_date,
                   indicator = pipeline$indicator,
                   main_dir = pipeline$outputdir,
                   rake_method = rake_method,
                   age = age,
                   holdout = holdout,
                   countries_not_to_subnat_rake = pipeline$config_list$countries_not_to_subnat_rake,
                   raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor),
                   return_objects = TRUE,
                   use_pop_draws = use_pop_draws
                 ))

    ## Get the necessary outputs and rename the columns
    rf <- data.table(outputs[["rf"]])[, .(loc = location_id, year, start_point = mbg_prev, target = gbd_prev, raking_factor = rf)]
    raked_cell_pred <- outputs[["raked_cell_pred"]]


    ## Raked simple raster has been made above
    raked_simple_raster <- new_simple_raster

  } else if (pipeline$config_list$metric_space == "counts") {
    print("Using the counts raking and aggregation functions:")

    ## Rake counts
    with_globals(new = list(pop_release = pipeline$config_list$pop_release),
                 outputs <- fractionally_rake_counts(
                   count_cell_pred = data.table(cell_pred),
                   rake_to = gbd,
                   reg = reg,
                   year_list = pipeline$config_list$year_list,
                   rake_subnational = pipeline$config_list$subnational_raking,
                   countries_not_to_subnat_rake = pipeline$config_list$countries_not_to_subnat_rake,
                   countries_not_to_rake = pipeline$config_list$countries_not_to_rake,
                   simple_raster = simple_raster,
                   modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
                   raking_shapefile_version = pipeline$config_list$raking_shapefile_version,
                   raster_agg_factor = pipeline$config_list$raster_agg_factor
                 ))

    ## Get the necessary outputs
    rf <- outputs$raking_factors
    raked_cell_pred <- outputs$raked_cell_pred
    raked_simple_raster <- new_simple_raster

    ## Save out the aggregate files
    with_globals(new = list(run_date = pipeline$run_date,
                            main_dir = pipeline$outputdir),
                 raked_frax_counts_save(output_list = outputs, sharedir = pipeline$sharedir,
                                        indicator = pipeline$indicator,
                                        age = age, reg = reg, holdout = holdout))
  }
} else {
  rf <- NULL
  raked_cell_pred <- NULL

  # Define simple raster for mask
  simple_polygon_list <- load_simple_polygon(
    gaul_list = get_adm0_codes(reg,
                               shapefile_version = pipeline$config_list$modeling_shapefile_version
    ),
    buffer = 0.4, subset_only = FALSE,
    shapefile_version = pipeline$config_list$modeling_shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  simple_polygon <- simple_polygon_list[[2]]
  simple_raster <- build_simple_raster(extent_template = subset_shape,
                                       shapefile_version = pipeline$config_list$modeling_shapefile_version,
                                       region = reg,
                                       raster_agg_factor = pipeline$config_list$raster_agg_factor)
  rm(simple_polygon_list)
}


## SAVE THE RESULTS #####################################################################


message("Saving results...")

## save RF
with_globals(new = list(indicator_group = pipeline$indicator_group,
                        indicator = pipeline$indicator,
                        run_date = pipeline$run_date),
             save_post_est(rf, "csv", paste0(reg, "_rf", indic = pipeline$indicator)))

## save raked cell preds
save(raked_cell_pred, file = sprintf("%s/%s_raked_cell_draws_eb_bin%i_%s_%i.RData",
                                     pipeline$outputdir,
                                     pipeline$indicator,
                                     age, reg, holdout))

# make and save summaries

save_cell_pred_summary <- function(summstat, raked, ...) {
  message(paste0("Making summmary raster for: ", summstat, " (", raked, ")"))

  if (raked == "unraked") {
    cpred <- "cell_pred"
    mask_raster <- "simple_raster"
  }
  if (raked == "raked") {
    cpred <- "raked_cell_pred"
    mask_raster <- "raked_simple_raster"
  }
  if (raked == "raked_c") {
    cpred <- "raked_cell_pred_c"
    load(paste0(pipeline$sharedir, "/output/", pipeline$run_date, "/",
                pipeline$indicator, "_raked_c_cell_draws_eb_bin0_",
                reg, "_0.RData" ))
    mask_raster <- "raked_simple_raster"
  }
  ras <- make_cell_pred_summary(
    draw_level_cell_pred = get(cpred),
    mask = get(mask_raster),
    return_as_raster = TRUE,
    summary_stat = summstat,
    ...
  )
  save_post_est(ras, 'raster',
                paste0(reg, switch(raked, "", raked = "_raked", raked_c = "_raked_c"),
                       '_', summstat, '_raster'))
}

# Do this as lapply to not fill up memory in global env with big obs
if (!as.logical(pipeline$config_list$rake_countries)) {
  rake_list <- c("unraked")
} else {
  rake_list <- c("unraked", "raked")
}

summ_list <- expand.grid(summstats[summstats != "p_below"], rake_list)

lapply(1:nrow(summ_list), function(i) {
  summstat <- as.character(summ_list[i, 1])
  raked <- as.character(summ_list[i, 2])
  with_globals(new = list(indicator_group = pipeline$indicator_group,
                          indicator = pipeline$indicator,
                          run_date = pipeline$run_date),
               save_cell_pred_summary(summstat, raked))
})

## Can't pass additional params in the above framework, so will code by hand here
for (r in rake_list) {
  if ("p_below" %in% summstats) {
    with_globals(new = list(indicator_group = pipeline$indicator_group,
                            indicator = pipeline$indicator,
                            run_date = pipeline$run_date),
                 save_cell_pred_summary(summstat = "p_below", raked = r,
                                        value = 0.8, equal_to = F))
  }
}

# Write a file to mark done
write(NULL, file = paste0(pipeline$outputdir, "/fin_postest", path_addin))

# All done
message(paste0("Done with post-estimation and aggregation for ", stratum))


# Postamble ---------------------------------------------------------------


pipeline_postamble()

q("no")
