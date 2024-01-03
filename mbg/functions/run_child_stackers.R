#' @title Fitting child stackers
#' @description
#' Parallelization in stackers is a little bit complicated. Some of the stackers
#' rely on packages like "mgcv" and "glmnet" which have their own internal
#' parallelization which we have to make sure plays nice with the MKL.
#' \code{mclapply()} is used heavily in stackers as well which will hang with
#' multithreaded operations like OpenMP or MKL.
#'
#' Those stacking functions which have a "cores" argument have an "auto" option
#' (default) where the stacker function has code to properly determine how many
#' cores to give each operation based on how it is parallelized, how many cores
#' are available for the job based on slots, etc. It is highly recommended that
#' the "auto" option be used. Otherwise, you may pass in an integer argument for
#' the number of cores. The only value that is known to work generally on all
#' stackers is 1, or serial operation.
#'
#' @param models a vector of sub-models to run
#' @param input_data the data frame with the input data, defaulted to \code{the_data} from the global environment
#' @param indicator Indicator name e.g. "tr_had_diarrhea"
#' @param indicator_family Model family (e.g. "binomial")
#' @param covariates Covariates used in non-BRT child models
#' @param covariates_brt Covariates used in BRT (GBM or XGBOOST) child models
#' @param reg Region
#' @param outputdir Output directory
#' @param parallelize_stacking T/F, launch sub jobs to run all models in parallel?
#' @param pipeline Pipeline object from main lbd.mbg run scripts, default NULL. Only used if parallelize_stacking is TRUE.
#'
#' @return A list of sub-model predictions and model objects where the first element of each list member are the predictions,
#' and the second element the model statistics and summary
#'
#' @rdname run_child_stackers
#'
#' @importFrom raster extent crop mask
#'
#' @export
#'
#' @concept mbg
run_child_stackers <- function(models,
                               input_data = the_data,
                               indicator = use_global_if_missing("indicator"),
                               indicator_family = use_global_if_missing("indicator_family"),
                               covariates = all_fixed_effects,
                               covariates_brt = all_fixed_effects_brt,
                               reg = use_global_if_missing("reg"),
                               outputdir = use_global_if_missing("outputdir"),
                               parallelize_stacking = FALSE,
                               pipeline = NULL) {

  if (parallelize_stacking) {
    if (is.null(pipeline)) {
      stop("Pipeline object must be passed to run_child_stackers when running in parallel")
    }

    if(is.null(pipeline$config_list$project) | pipeline$config_list$project == "change_me") {
      stop("You must set the project field in your config to run stackers in parallel")
    }

    # temp folder for passing objects to subjobs and back
    parallel_sub_dir <- "<<<< FILEPATH REDACTED >>>>"
    if (!dir.exists(parallel_sub_dir)) dir.create(parallel_sub_dir)

    save(
      input_data, indicator, indicator_family, covariates, covariates_brt, outputdir,
      file = "<<<< FILEPATH REDACTED >>>>"
    )

    # Launch a subjob for each desired stacking model
    subjobs <- parallelize(
      user = Sys.info()["user"],
      memory = as.numeric(pipeline$config_list$parallelize_stacking_memory),
      script = "parallelize_stacking.R",
      proj = pipeline$config_list$project,
      ig = pipeline$indicator_group,
      indic = pipeline$indicator,
      rd = pipeline$run_date,
      expand_vars = models,
      save_objs = c("reg", "pipeline"),
      script_dir = glue::glue(getwd(), "/subscripts"),
      prefix = glue::glue("j02_parallelized_stacking_{reg}"),
      log_location = glue::glue("{outputdir}/"),
      queue = "long.q",
      run_time = pipeline$config_list$parallelize_stacking_runtime,
      threads = as.integer(pipeline$config_list$parallelize_stacking_threads),
      singularity = pipeline$config_list$singularity_version
    )

    message("Waiting on stacking subjobs to finish... ")
    job_ids <- subjobs[[1]]$jobid
    wait_on_jobs(job_ids)

    # validate jobs finished sucessfully
    stacker_filenames <- list.files(parallel_sub_dir, pattern = glue::glue("{reg}_stacker"))
    expected_names <- paste0(reg, "_stacker_", models, ".RDS")

    if (length(setdiff(expected_names, stacker_filenames)) > 0) {
      message("Not all expected stacker files were found - ")
      message("    ", paste0(setdiff(expected_names, stacker_filenames), collapse = ", "))
      stop()
    }

    # combine outputs (for object called child_models to match non-parallel version)
    stacker_paths <- "<<<< FILEPATH REDACTED >>>>"
    stackers <- lapply(stacker_paths, readRDS)

    # remove the extra top list level created by lapply (stackers is a list of lists)
    child_models <- unlist(stackers, recursive = F)

    # delete tmp file (save_objs) created by parallelize
    # gets the submission string of the first subjob (subjobs[[1]]$the_qsub[1]) as all
    # subjobs share a tmp object, then split it and take the second to last element to get the path
    message("Cleaning up after stacking subjobs...")
    parallelize_tmp_object <- tail(strsplit(subjobs[[1]]$the_qsub[1], " ")[[1]], 2)[1]
    unlink(parallelize_tmp_object)

  } else {

    if ("gam" %in% models) {
      tic("Stacking - GAM")
      gam_child <- fit_gam_child_model(
        df = input_data,
        model_name = "gam",
        fold_id_col = "fold_id",
        covariates = covariates,
        additional_terms = NULL,
        weight_column = "weight",
        bam = FALSE,
        spline_args = list(bs = "ts", k = 3),
        auto_model_select = TRUE,
        indicator = indicator,
        indicator_family = indicator_family,
        cores = "auto"
      )

      toc(log = T)
    }

    # Fit a GBM/BRT model
    if ("gbm" %in% models) {
      tic("Stacking - GBM")
      gbm_child <- fit_gbm_child_model(
        df = input_data,
        model_name = "gbm",
        fold_id_col = "fold_id",
        covariates = covariates_brt,
        weight_column = "weight",
        tc = as.numeric(gbm_tc),
        lr = as.numeric(gbm_lr),
        bf = as.numeric(gbm_bf),
        indicator = indicator,
        indicator_family = indicator_family,
        cores = "auto"
      )
      toc(log = T)
    }

    # Fit a BRT model with Xgboost (faster than GBM)
    if ("xgboost" %in% models) {
      tic("Stacking - Xgboost")

      if (!exists("xg_model_tune")) {
        message("xg_model_tune not found in config. Will be set to true, and model will be tuned with default grid search")
        xg_model_tune <- TRUE
        hyperparameter_filepath <- NULL
      }

      if (xg_model_tune == T & !exists("hyperparameter_filepath")) {
        message("Tuning xgboost on default grid search")
        hyperparameter_filepath <- NULL
      }
      xgboost_child <- fit_xgboost_child_model(
        df = input_data,
        covariates = covariates,
        weight_column = "weight",
        indicator = indicator,
        indicator_family = indicator_family,
        outputdir = outputdir,
        region = reg,
        xg_model_tune = xg_model_tune,
        hyperparameter_filepath = hyperparameter_filepath
      )
      toc(log = T)
    }

    # fit some nets
    # lasso
    if ("lasso" %in% models) {
      tic("Stacking - lasso")
      lasso_child <- fit_glmnet_child_model(
        df = input_data,
        model_name = "lasso",
        covariates = covariates,
        fold_id_col = "fold_id",
        additional_terms = NULL,
        indicator_family = indicator_family,
        indicator = indicator,
        alpha = 1,
        weight_column = "weight",
        cores = "auto"
      )
      toc(log = T)
    }

    # ridge
    if ("ridge" %in% models) {
      tic("Stacking - ridge")
      ridge_child <- fit_glmnet_child_model(
        df = input_data,
        model_name = "ridge",
        covariates = covariates,
        fold_id_col = "fold_id",
        additional_terms = NULL,
        indicator_family = indicator_family,
        indicator = indicator,
        alpha = 0,
        weight_column = "weight",
        cores = "auto"
      )
      toc(log = T)
    }

    # enet
    if ("enet" %in% models) {
      tic("Stacking - enet")
      enet_child <- fit_glmnet_child_model(
        df = input_data,
        model_name = "enet",
        covariates = covariates,
        fold_id_col = "fold_id",
        additional_terms = NULL,
        indicator_family = indicator_family,
        indicator = indicator,
        alpha = 0.5,
        weight_column = "weight",
        cores = "auto"
      )
      toc(log = T)
    }

    ## Get all child ones
    child_models <- lapply(paste0(models, "_child"), function(x) get(x))
  }

  ## Return the list of child stacker (pred and model)
  return(child_models)
}
