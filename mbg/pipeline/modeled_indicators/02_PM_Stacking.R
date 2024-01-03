source("source_for_setup.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 2,
    node_name = "j02_stacking",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = 1
  )
}
pipeline_preamble(inputs = inputs)


# Stacking ----------------------------------------------------------------


## If skipping to INLA, then just quit job
if (as.logical(pipeline$config_list$skiptoinla)) {
  print("Skipping to INLA")

  ## Save out environment
  mbg_save_nodeenv(
    node = nodename,
    ig = indicator_group,
    indic = indicator,
    rd = run_date,
    reg = reg,
    age = age,
    holdout = holdout,
    objs = ls()
  )

  ## Create output file and remove err file ##
  mbg_job_marker(type = "end", tmpdir = "~/mbgdir")

  q("no")
}


tic("Stacking - all") ## Start stacking reference timer

## Figure out which models we're going to use
child_model_names <- pipeline$config_list$stacked_fixed_effects %>%
  gsub(" ", "", .) %>%
  strsplit(., "+", fixed = T) %>%
  unlist()
message(paste0(
  "Child stackers included are: ",
  paste(child_model_names,
    collapse = " // "
  )
))

the_covs <- format_covariates(all_fixed_effects)

## copy the dataset to avoid unintended namespace conflicts
the_data <- copy(df)

## only use data where we know what age group or point
ag_data <- the_data[the_data$agg_weight != 1, ]
the_data <- the_data[the_data$agg_weight == 1, ]

set.seed(seed)
increment_seed(seed)

## shuffle the data into six folds
the_data <- the_data[base::sample(nrow(the_data)), ]
the_data[, fold_id := cut(
  seq(1, nrow(the_data)),
  breaks = as.numeric(
    pipeline$config_list$n_stack_folds
  ),
  labels = FALSE
)]

## add a row id column
the_data[, a_rowid := seq(1:nrow(the_data))]

## extract covariates to the points and subset data
## where its missing covariate values
cs_covs <- extract_covariates(the_data,
  all_cov_layers,
  id_col = "a_rowid",
  return_only_results = TRUE,
  centre_scale = TRUE,
  period_var = "year",
  period_map = period_map
)

# A check to see if any of the variables do not vary across the data.
# This could break model later so we check and update some objects
covchecklist <- check_for_cov_issues(
  cc = cs_covs,
  afe = all_fixed_effects,
  afeb = all_fixed_effects_brt,
  fe = pipeline$config_list$fixed_effects,
  check_pixelcount = pipeline$config_list$check_cov_pixelcount,
  check_pixelcount_thresh = ifelse(
    "pixelcount_thresh" %in% names(pipeline$config_list),
    pipeline$config_list$pixelcount_thresh, 0.95
  )
)
for (n in names(covchecklist)) {
  assign(n, covchecklist[[n]])
}

# Plot covariates as a simple diagnostic here
pdf(
  sprintf("%s/raw_covariates_%s.pdf", pipeline$outputdir, pipeline$get_path_addin(loopvar_index)),
  height = 12, width = 12
)
for (covname in names(all_cov_layers)) {
  plot(all_cov_layers[[covname]], main = covname, maxpixel = 24)
}
dev.off()

## Check for data where covariate extraction failed
rows_missing_covs <- nrow(the_data) - nrow(cs_covs[[1]])
if (rows_missing_covs > 0) {
  pct_missing_covs <- round((rows_missing_covs / nrow(the_data)) * 100, 2)
  warning(
    paste0(
      rows_missing_covs, " out of ", nrow(the_data), " rows of data ",
      "(", pct_missing_covs, "%) do not have corresponding ",
      "covariate values and will be dropped from child models..."
    )
  )
  if (rows_missing_covs / nrow(the_data) > 0.1) {
    stop(
      paste0(
        "Something has gone quite wrong: more than 10% of your data ",
        " does not have corresponding covariates.  You should investigate ",
        "this before proceeding."
      )
    )
  }
}

the_data <- merge(the_data, cs_covs[[1]], by = "a_rowid", all.x = F, all.y = F)

## store the centre scaling mapping
covs_cs_df <- cs_covs[[2]]

## this will drop rows with NA covariate values
the_data <- na.omit(the_data, c(pipeline$indicator, "N", the_covs))

## stop if this na omit demolished the whole dataset
if (nrow(the_data) == 0) {
  stop(paste0(
    "You have an empty df, make sure one of your",
    " covariates was not NA everywhere."
  ))
}


## Running Stackers
if (pipeline$config_list$use_stacking_covs) {
  message("Fitting Stackers")
  with_globals(
    new = list(
      gbm_tc = as.numeric(pipeline$config_list$gbm_tc),
      gbm_lr = as.numeric(pipeline$config_list$gbm_lr),
      gbm_bf = as.numeric(pipeline$config_list$gbm_bf)
    ),
    # Run the child stacker models
    child_model_run <- run_child_stackers(
      models = child_model_names,
      input_data = the_data,
      indicator = pipeline$indicator,
      indicator_family = pipeline$config_list$indicator_family,
      covariates = all_fixed_effects,
      covariates_brt = all_fixed_effects_brt,
      outputdir = pipeline$outputdir,
      reg = reg
    )
  )

  # Bind the list of predictions into a data frame
  child_mods_df <- do.call(cbind, lapply(child_model_run, function(x) x[[1]]))

  ## combine the children models with the_data
  the_data <- cbind(the_data, child_mods_df)

  ## Rename the child model objects into a named list
  child_model_objs <- setNames(
    lapply(child_model_run, function(x) x[[2]]),
    child_model_names
  )



  ## return the stacked rasters
  stacked_rasters <- make_stack_rasters(
    covariate_layers = all_cov_layers, # raster layers and bricks
    period = min(period_map[, period_id]):max(period_map[, period_id]),
    child_models = child_model_objs,
    indicator_family = pipeline$config_list$indicator_family,
    centre_scale_df = covs_cs_df,
    rd = pipeline$run_date,
    re = reg,
    ind_gp = pipeline$indicator_group,
    ho = as.numeric(holdout),
    ind = pipeline$indicator
  )

  ## plot stackers
  pdf(paste0(
    pipeline$outputdir,
    "/stacker_rasters",
    pipeline$get_path_addin(loopvar_index),
    ".pdf"
  ))
  for (i in 1:length(stacked_rasters)) {
    plot(stacked_rasters[[i]],
      main = names(stacked_rasters[[i]]),
      maxpixel = ncell(stacked_rasters[[i]])
    )
  }
  dev.off()

  message("Stacking is complete")
}


## add aggregate data back in, with stacking predictions from the full model
if (nrow(ag_data) > 0) {
  ag_data[, a_rowid := 1:.N + max(the_data$a_rowid)]
  if(pipeline$config_list$use_stacking_covs) {
    ag_stackers <- extract_covariates(ag_data,
                                      stacked_rasters,
                                      id_col              = "a_rowid",
                                      return_only_results = TRUE,
                                      centre_scale        = FALSE,
                                      period_var          = "year",
                                      period_map          = period_map)
    ag_stackers <- ag_stackers[, c("a_rowid", child_model_names, child_model_names), with = F]

    stacker_names <- c(paste0(child_model_names, "_full_pred"), paste0(child_model_names, "_cv_pred"))
    setnames(ag_stackers, c("a_rowid", stacker_names))

    ag_data <- merge(ag_data, ag_stackers, by = "a_rowid")

    if(any(is.na(ag_data[, ..stacker_names]))) {
      stop("There are NAs in predictions from stackers for aggregated data.")
    }
  } else {
    ag_covs <- extract_covariates(ag_data,
                                  all_cov_layers,
                                  id_col              = "a_rowid",
                                  return_only_results = TRUE,
                                  centre_scale        = FALSE,
                                  period_var          = 'year',
                                  period_map          = period_map)
    cov_names <- names(all_cov_layers)
    cs_ag_covs <- centreScale(ag_covs[, ..cov_names], df = covs_cs_df)
    ag_covs <- cbind(ag_covs[,- ..cov_names], cs_ag_covs)

    ag_data <- merge(ag_data, ag_covs, by = "a_rowid")

    if(as.logical(pipeline$config_list$use_raw_covs)) {
      if(any(is.na(ag_data[, ..cov_names]))) {
        stop("There are NAs in covariates for aggregated data.")
      }
    }
  }

  the_data <- rbind(the_data, ag_data, fill = T)
}

# Postamble ---------------------------------------------------------------

pipeline_postamble()
q("no")
