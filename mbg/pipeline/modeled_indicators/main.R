# Set the working directory to the directory this file is located in
# this will ensure all other source() calls on relative paths work
# in addition, qsubs submitted will automatically use the same working directory as this script
if (rlang::is_interactive()) {
  if (rstudioapi::isAvailable()) {
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

# (2) Prep Pipeline Object ------------------------------------------------

# Initial parameters - users are expected to change these values when setting up a pipeline for use
user <- Sys.info()["user"]
ig <- "<<<<INDICATOR GROUP>>>>"
indic <- "<<<<INDICATOR>>>>"
config <- "<<<<CONFIGURATION CSV FILE>>>>"
covs   <- "<<<<COVARIATE LIST CSV FILE>>>>"
rd <- "<<<<RUN DATE>>>>"
PROJECT <- "<<<<PROJECT>>>>"
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
write.csv(pipeline$fixed_effects_config, file.path(paste0("<<<< FILEPATH REDACTED >>>>", "/",name, ".csv")))

# (3) Make MBG DAG ---------------------------------------------

dag <- MBGDag$new(
  save_dir = sprintf("%s/output/%s", pipeline$sharedir, pipeline$run_date),
  dag_hash = "FAKE_HASH")

predict.ids <- vector("integer")
postest.ids <- vector("integer")

for (i in 1:nrow(pipeline$loopvars)) {
  prep.data.id <- dag$create_node(
    pipeline = pipeline,
    base_name = "j01_data_prep",
    loopvar_index = i,
    jobscript = "01_PM_Data_Prepping.R",
    project = PROJECT,
    cores = 1,
    ram_gb = 5, 
    runtime = "00:10:00",
    queue = QUEUE,
    singularity_version = pipeline$config_list$singularity_version
  )

  stack.id <- dag$create_node(
    pipeline,
    base_name = "j02_stacking",
    loopvar_index = i,
    jobscript = "02_PM_Stacking.R",
    project = PROJECT,
    cores = 1,
    ram_gb = 5,
    runtime = "00:60:00",
    queue = QUEUE,
    hold_job = "previous",
    singularity_version = pipeline$config_list$singularity_version
  )

  prep.inla.id <- dag$create_node(
    pipeline,
    base_name = "j03_prep_for_INLA",
    loopvar_index = i,
    jobscript = "03_PM_PrepForFitting.R",
    project = PROJECT,
    cores = 1,
    ram_gb = 5,
    runtime = "00:10:00",
    queue = QUEUE,
    hold_job = "previous",
    singularity_version = pipeline$config_list$singularity_version
  )

  fit.id <- dag$create_node(
    pipeline,
    base_name = "j04_MBG_fitting",
    loopvar_index = i,
    jobscript = "04_PM_MBGFitting.R",
    project = PROJECT,
    cores = 3,
    ram_gb = 50,
    runtime = "24:00:00",
    queue = QUEUE,
    hold_job = "previous",
    singularity_version = pipeline$config_list$singularity_version
  )

  predict.id <- dag$create_node(
    pipeline,
    base_name = "j05_MBG_predict",
    loopvar_index = i,
    jobscript = "05_PM_MBGPredict.R",
    project = PROJECT,
    cores = 1,
    ram_gb = 100,
    runtime = "2:00:00",
    queue = QUEUE,
    hold_job = "previous",
    singularity_version = pipeline$config_list$singularity_version
  )
  predict.ids <- c(predict.ids, predict.id)
  
  postest.id <- dag$create_node(
    pipeline,
    base_name = "j06_MBG_postest",
    loopvar_index = i,
    jobscript = "06_PM_PostEstFrax.R",
    project = PROJECT,
    cores = 1,
    ram_gb = 150,
    runtime = "6:00:00",
    queue = QUEUE,
    hold_job = "previous",
    singularity_version = pipeline$config_list$singularity_version
  )
  postest.ids <- c(postest.ids, postest.id)
}

# get a summary of model params
table.id <- dag$create_node(pipeline,
                            # "post job 05"
                            base_name = "p05_model_results_table",
                            jobscript = "p05_PM_CreateModelResultsTable.R",
                            project = PROJECT,
                            cores = 1,
                            ram_gb = 5, 
                            runtime = "00:10:00",
                            queue = QUEUE,
                            hold_job = predict.ids,
                            singularity_version = pipeline$config_list$singularity_version)

# combine regions objects
combine.id <- dag$create_node(pipeline,
                              # "post job 06"
                              base_name = "p06_combine_postest",
                              jobscript = "p06_PM_CombinePostest.R",
                              project = PROJECT,
                              cores = 1,
                              ram_gb = 10,
                              runtime = "01:00:00",
                              queue = QUEUE,
                              hold_job = postest.ids,
                              singularity_version = pipeline$config_list$singularity_version)

# make diagnostic plots
diagnostic.id <- dag$create_node(pipeline,
                                 # "post job 06"
                                 base_name = "p07_diagnostic_plots",
                                 jobscript = "p07_PM_Diagnostic_Plots.R",
                                 project = PROJECT,
                                 cores = 1,
                                 ram_gb = 10,
                                 runtime = "4:00:00",
                                 queue = QUEUE,
                                 hold_job = combine.id,
                                 singularity_version = pipeline$config_list$singularity_version)

# (4) Prepare necessary objects for postest -------------------------------

# Load GBD Estimates for this indicator which will be used in raking
source(paste0("<<<< FILEPATH REDACTED >>>>/get_covariate_estimates.R"))
gbd <- get_covariate_estimates(covariate_id = 19,
                               release_id = 10,
                               # location_id = pipeline$config_list$location_id,
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

# (5) Submit, save, and monitor the DAG -----------------------------------

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

# (6) Extra things one can do ---------------------------------------------

# Remove nodes with:
# dag$remove_node("stacking")

# One can submit single nodes and track them as follows:
# # Submit data_prep and update DAG
# dag$submit_jobs(nodename = "data_prep")
# dag$wait_on_node("data_prep")
# saveRDS(object = dag, file = dag$img_path)
