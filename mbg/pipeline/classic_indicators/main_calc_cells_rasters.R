if (rlang::is_interactive()) {
  if (rstudioapi::isAvailable()) {
    # https://stackoverflow.com/a/47045368
    this.file <- rstudioapi::getSourceEditorContext()$path
  } else {
    warning("sorry - we don't know how to determine the file path of this script in non-RStudio interactive sessions")
    this.file <- NULL
  }
} else {
  this.file <- rprojroot::thisfile()
}
if (is.null(this.file)) {
  warning("We expect all pipeline files to be in the same directory. We cannot determine what directory this file is in, so we are assuming your getwd() IS this directory. If it isn't things will fail.")
} else {
  setwd(dirname(this.file))
}

source("source_for_setup.R")

# Initial parameters - users are expected to change these values when setting up a pipeline for use ##################
user <- Sys.info()["user"]
ig <- "<<<<INDICATOR GROUP>>>>"
indic <- "<<<<INDICATOR>>>>"
config <- "<<<<CONFIGURATION CSV FILE>>>>"
covs   <- "<<<<COVARIATE LIST CSV FILE>>>>"
rd <- "<<<<RUN DATE>>>>"
# substitute your team-specific project here
PROJECT <- "<<<<PROJECT>>>>"
# substitute your team-specific queue here
QUEUE <- "<<<<QUEUE>>>>"

pipeline <- MBGPipeline$new(
  indicator_group = ig,
  indicator = indic,
  config_file = config,
  covs_file = covs
)
# Minor edit - skip lots of nonsense messages
suppressMessages(suppressWarnings(pipeline$setup_conf(
  push_to_global_env = FALSE,
  run_tests = FALSE
)))
pipeline$setup_rundate(rd, # others can't use your rundate dir
                       full_cleanup = TRUE)
pipeline$make_holdouts()
pipeline$create_loopvars()

save(pipeline, file = file.path(paste0("<<<< FILEPATH REDACTED >>>>", "/",name, ".RData")))
# Save fixed effects (covariate) configuration to run date directory
write.csv(pipeline$fixed_effects_config, file.path(paste0("<<<< FILEPATH REDACTED >>>>", "/",name, ".csv")))

# (3) Make MBG DAG ---------------------------------------------

dag <- MBGDag$new(
  save_dir = sprintf("%s/output/%s", pipeline$sharedir, pipeline$run_date),
  dag_hash = "FAKE_HASH")

predict.ids <- vector("integer")
postest.ids <- vector("integer")

sf::sf_use_s2(FALSE)
options(ggrepel.max.overlaps = Inf)

## set repo and indicator
any_contra <- "<<<< FILEPATH REDACTED >>>>"
mod_contra <- "<<<< FILEPATH REDACTED >>>>"
need_contra <- "<<<< FILEPATH REDACTED >>>>"
intent_use <- "<<<< FILEPATH REDACTED >>>>"
group_a <- "<<<< FILEPATH REDACTED >>>>"
group_b <- "<<<< FILEPATH REDACTED >>>>"
group_c <- "<<<< FILEPATH REDACTED >>>>"
group_d <- "<<<< FILEPATH REDACTED >>>>"
group_d <- "<<<< FILEPATH REDACTED >>>>"

## region
regs <- c("BFA","KEN","NGA")

for (reg in regs){
  ## combine cell preds
  message("Loading Data...")
  if (indicator == "group_a") {
    load(paste0(any_contra, name,"_0.RData"))
    cell_pred_any_contra <- copy(cell_pred)
    load(paste0(need_contra, name,"_0.RData"))
    cell_pred_need_contra <- copy(cell_pred)
    cell_pred <- (1 - cell_pred_any_contra) * (1 - cell_pred_need_contra)
  } else if (indicator == "group_b") {
    load(paste0(any_contra, name,"_0.RData"))
    cell_pred_any_contra <- copy(cell_pred)
    load(paste0(intent_use, name,"_0.RData"))
    cell_pred_intent_use <- copy(cell_pred)
    load(paste0(need_contra, name,"_0.RData"))
    cell_pred_need_contra <- copy(cell_pred)
    cell_pred <- (1 - cell_pred_any_contra) * cell_pred_need_contra * (1 - cell_pred_intent_use)
  } else if (indicator == "group_c") {
    load(paste0(any_contra, name,"_0.RData"))
    cell_pred_any_contra <- copy(cell_pred)
    load(paste0(intent_use, name,"_0.RData"))
    cell_pred_intent_use <- copy(cell_pred)
    load(paste0(need_contra, name,"_0.RData"))
    cell_pred_need_contra <- copy(cell_pred)
    cell_pred <- (1 - cell_pred_any_contra) * cell_pred_need_contra * cell_pred_intent_use
  } else if (indicator == "group_d") {
    load(paste0(any_contra, name,"_0.RData"))
    cell_pred_any_contra <- copy(cell_pred)
    load(paste0(mod_contra, nameg,"_0.RData"))
    cell_pred_mod_contra <- copy(cell_pred)
    cell_pred <- cell_pred_any_contra * (1 - cell_pred_mod_contra)
  } else if (indicator == "group_e") {
    load(paste0(any_contra, name,"_0.RData"))
    cell_pred_any_contra <- copy(cell_pred)
    load(paste0(mod_contra, nameg,"_0.RData"))
    cell_pred_mod_contra <- copy(cell_pred)
    cell_pred <- cell_pred_any_contra * cell_pred_mod_contra
  } else if (indicator == "indic_demand") {
    load(paste0(group_a, name,"_0.RData"))
    cell_pred_group_a <- copy(cell_pred)
    load(paste0(group_e, name,"_0.RData"))
    cell_pred_group_e <- copy(cell_pred)
    cell_pred <- cell_pred_group_e/(1-cell_pred_group_a)
  } else if (indicator == "indic_unmet") {
    load(paste0(group_b, name,"_0.RData"))
    cell_pred_group_b <- copy(cell_pred)
    load(paste0(group_c, name,"_0.RData"))
    cell_pred_group_c <- copy(cell_pred)
    load(paste0(group_d, name,"_0.RData"))
    cell_pred_group_d <- copy(cell_pred)
    cell_pred <- cell_pred_group_b+cell_pred_group_c+cell_pred_group_d
  }

  ## save cell preds ###########################################################################################
  save(cell_pred, file = paste0(
    "<<<< FILEPATH REDACTED >>>>", "/",name, "_0.RData"
  ))

  ## save the results ##########################################################################################
  message("Saving results...")
  load(paste0("<<<< FILEPATH REDACTED >>>>", "/",name, "_0.RData"))

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
    ras <- make_cell_pred_summary(
      draw_level_cell_pred = get(cpred),
      mask = get(mask_raster),
      return_as_raster = TRUE,
      summary_stat = summstat,
      ...
    )
    save_post_est(ras, 'raster',
                  paste0(reg, switch(raked, "", raked = "_raked"),
                         '_', summstat, '_raster'))
  }

  rake_list <- 'unraked'
  indicator_group <- pipeline$indicator_group
  indicator <- pipeline$indicator
  run_date <- pipeline$run_date
  summstats <- eval(parse(text = pipeline$config_list$summstats))

  # Load simple polygon template to model over
  gaul_list <- get_adm0_codes(
    reg,
    shapefile_version = pipeline$config_list$modeling_shapefile_version
  )
  simple_polygon_list <- load_simple_polygon(
    gaul_list = gaul_list, buffer = 1, tolerance = 0.4,
    shapefile_version = pipeline$config_list$modeling_shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  simple_polygon <- simple_polygon_list[[2]]

  simple_raster <- build_simple_raster(extent_template = subset_shape,
                                       shapefile_version = pipeline$config_list$modeling_shapefile_version,
                                       region = reg,
                                       raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor))


  ## save raster
  for (r in rake_list) {
    if ("mean" %in% summstats) {
      save_cell_pred_summary(
        summstat = "mean",
        raked = r
      )
    }
    if ("cirange" %in% summstats) {
      save_cell_pred_summary(
        summstat = "cirange",
        raked = r
      )
    }
    if ("lower" %in% summstats) {
      save_cell_pred_summary(
        summstat = "lower",
        raked = r
      )
    }
    if ("upper" %in% summstats) {
      save_cell_pred_summary(
        summstat = "upper",
        raked = r
      )
    }
    if ("cfb" %in% summstats) {
      save_cell_pred_summary(
        summstat = "cfb",
        raked = r
      )

    }
  }
}

for (i in 1:nrow(pipeline$loopvars)){
  postest.id <- dag$create_node(
    pipeline,
    base_name = "j06_MBG_postest",
    loopvar_index = i,
    jobscript = "06_PM_PostEstFrax.R",
    project = PROJECT,
    cores = 1,
    ram_gb = 200,
    runtime = "10:00:00",
    queue = QUEUE,
    singularity_version = pipeline$config_list$singularity_version
  )
  postest.ids <- c(postest.ids, postest.id)
}

# combine regions objects
combine.id <- dag$create_node(pipeline,
                              # "post job 06"
                              base_name = "p06_combine_postest",
                              jobscript = "p06_PM_CombinePostest.R",
                              project = PROJECT,
                              cores = 1,
                              ram_gb = 200,
                              runtime = "10:00:00",
                              queue = QUEUE,
                              hold_job = postest.ids,
                              singularity_version = pipeline$config_list$singularity_version)

diagnostic.id <- dag$create_node(pipeline,
                                 # "post job 06"
                                 base_name = "p07_diagnostic_plots",
                                 jobscript = "p07_PM_Diagnostic_Plots.R",
                                 project = PROJECT,
                                 cores = 1,
                                 ram_gb = 200,
                                 runtime = "10:00:00",
                                 queue = QUEUE,
                                 hold_job = combine.id,
                                 singularity_version = pipeline$config_list$singularity_version)


# Load GBD Estimates for this indicator which will be used in raking
source("<<<< FILEPATH REDACTED >>>>/get_covariate_estimates.R")
gbd <- get_covariate_estimates(covariate_id = 19,
                               release_id = 10,
                               status = 'best',
                               age_group_id = 22,
                               sex_id = 3,
                               year_id = pipeline$config_list$year_list
)

gbd <- gbd %>%
  select(location_id,year_id,mean_value) %>%
  filter(location_id %in% c(180,201,214))
colnames(gbd) <- c('name','year', 'mean')


# save objects to reload in postestfrax
prep_postest(
  indicator = pipeline$indicator,
  indicator_group = pipeline$indicator_group,
  run_date = pipeline$run_date,
  save_objs = c("gbd")
)
# # (5) Submit, save, and monitor the DAG -----------------------------------
#
#
# Look at DAG
print(dag$DAG)

# Save DAG
print(sprintf("Saving DAG to %s", dag$save_file))
dag$save()

# Submit everything in the DAG
dag$submit_jobs()

# save now that job numbers are known
dag$save()

# Monitor DAG - NULL will monitor all nodes, can also pass in a node_id to wait on
dag$wait_on_node(node_id = NULL)

# save again before exiting
dag$save()
