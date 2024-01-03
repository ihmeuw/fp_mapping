#' @title Fit Penalized Regression Child Model
#' @description Fit penalized regression (lasso, elastic net, ridge)
#'
#' @param df  data table with the outcome/indicator and some covariates already extracted
#' @param model_name what do you want to call this?
#' @param fold_id_col string, name of column that ids the fold
#' @param covariates a vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
#' @param additional_terms a vector or single character of column names to be included in the model fit
#' @param weight_column in df, is there a column that specifies the observation weights?
#' @param alpha Alpha parameter for glmnet calls. 1 = lasso, 0 = ridge
#' @param indicator name of column in data frame with outcome
#' @param indicator_family model family
#' @param parallel TRUE/FALSE to turn on/off parallelization
#'
#' @return df with predictions from all data and using holdouts
#'
#' @export
#'
#' @concept stacking
fit_glmnet_child_model <- function(df,
                                   model_name = "glmnet",
                                   fold_id_col = "fold_id",
                                   covariates = all_fixed_effects,
                                   additional_terms = NULL,
                                   weight_column = NULL,
                                   alpha = 1,
                                   indicator = indicator,
                                   indicator_family = "binomial",
                                   cores = "auto") {
  df <- copy(df)
  message("Fitting the Full GLMNET")

  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # glmnet has it's own internal parallelization that we don't have much control
  # over. It is either on or off and is parallel over each fold supplied by the
  # 'nfolds' argument to `cv.glmnet()`. Since we use the default of 10 for
  # 'nfolds' in `cv.glmnet()` in the `fit_glmnet()` function (i.e. we don't pass
  # in an 'nfolds' argument), we have to make sure we at least have 10 cores to
  # use before we turn the parallel option on. See this:
  # https://www.rdocumentation.org/packages/glmnet/versions/2.0-16/topics/cv.glmnet

  # Also, `cv.glmnet()` has caused multithreaded operations to hang similar to
  # `mclapply()`, so we set multithreaded operations to serial here
  set_serial_threads()
  if (cores == "auto") cores <- get_total_threads()
  if (cores >= 10) {
    parallel <- TRUE
    # start the cluster if parallel
    # Apparently, with Linux systems we don't need the `cl <- makeCluster(cores)`
    # step. Doing it that way will also work, but it is much slower.
    # Do it here, so we don't have to spin up/down the cluster with each
    # `fit_glmnet()` call.
    registerDoParallel(cores = cores)
  } else {
    parallel <- FALSE
  }

  # fit the full model
  full_model <- fit_glmnet(df[!is.na(get(indicator))],
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    alpha = alpha,
    indicator = indicator,
    indicator_family = indicator_family,
    parallel = parallel
  )

  full_model$model_name <- model_name

  # fit the child/cv rfs
  folds <- unique(df[, get(fold_id_col)])

  for (fff in folds) {
    # message(paste0('Fitting and Predicting Fold: ', fff))
    baby_model <- fit_glmnet(df[get(fold_id_col) != fff & !is.na(get(indicator)), ],
      covariates = covariates,
      additional_terms = additional_terms,
      weight_column = weight_column,
      alpha = alpha,
      indicator = indicator,
      indicator_family = indicator_family,
      parallel = parallel
    )

    new_data <- df[get(fold_id_col) == fff, the_covs, with = F]

    n_nd <- names(new_data)
    new_data <- as.matrix(new_data)
    names(new_data) <- n_nd

    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_model, newx = new_data, s = baby_model$cv_1se_lambda, type = "link")]
  }

  # stop the cluster just in case
  stopImplicitCluster()

  # Return to multithreading (if any):
  set_original_threads()

  # predict using full model fit earlier
  new_data <- df[, the_covs, with = F]

  n_nd <- names(new_data)
  new_data <- as.matrix(new_data)
  names(new_data) <- n_nd
  df[, paste0(model_name, "_full_pred") := predict(full_model, newx = new_data, s = full_model$cv_1se_lambda, type = "link")]

  # return a subset of the columns. Full pred denotes the fit from the full model. CV pred is the OOS stuff
  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)

  # if binomial, undo the logit
  if (indicator_family == "binomial") {
    df[, return_cols[1] := invlogit(get(return_cols[1]))]
    df[, return_cols[2] := invlogit(get(return_cols[2]))]
  }


  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
