#' @title Fit Generalized Boosted Regression Model Wrapper
#' @description Fit k+1 boosted regression tree models
#'
#' @param df  data table with the outcome/indicator and some covariates already extracted
#' @param model_name string indicating name of model to attach to the model fit object
#' @param fold_id_col in df, the column name that specifies which fold each observation/row belongs to
#' @param covariates a vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
#' @param additional_terms a vector or single character of column names to be included in the model fit
#' @param weight_column in df, is there a column that specifies the observation weights?
#' @param tc tree complexity
#' @param lr learning rate
#' @param bf bag fraction
#' @param indicator name of column in data frame with outcome
#' @param indicator_family model family
#' @param cores number of cores are available for use
#'
#' @return df with predictions from all data and using holdouts
#'
#' @export
#'
#' @concept stacking
fit_gbm_child_model <- function(df,
                                model_name = "gbm",
                                fold_id_col = "fold_id",
                                covariates = all_fixed_effects,
                                additional_terms = NULL, weight_column = NULL,
                                tc = 4,
                                lr = 0.005,
                                bf = 0.75,
                                indicator = use_global_if_missing("indicator"),
                                indicator_family = use_global_if_missing("indicator_family"),
                                cores = "auto") {
  # prevent df scoping
  df <- copy(df)

  # fit the baby trees in parallel
  folds <- unique(df[, get(fold_id_col)])

  message("Fitting baby gbm models in parallel")
  # Set multithreading to serial for `mclapply()`:
  set_serial_threads()
  # Determine appropriate number of cores to use in `mclapply()`
  if (cores == "auto") cores <- get_max_forked_threads(nobjs = length(folds))
  baby_models <- mclapply(folds, function(fff) {
    fit_gbm(
      df = df[get(fold_id_col) != fff & !is.na(get(indicator)), ],
      covariates = covariates,
      additional_terms = additional_terms,
      weight_column = weight_column,
      tc = tc,
      lr = lr,
      bf = bf,
      indicator = indicator,
      indicator_family = indicator_family,
      plot.main = F
    )
  }, mc.cores = cores)
  # Return to multithreading (if any):
  set_original_threads()

  for (fff in folds) {
    # use the data fit on K-1 of the folds to fit on the held out fold
    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_models[[fff]], df[get(fold_id_col) == fff, ], n.trees = baby_models[[fff]]$gbm.call$best.trees, type = "response")]
  }


  # fit GBM
  message("Fitting Full GBM")
  full_model <- fit_gbm(
    df = df[!is.na(get(indicator))],
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    tc = tc,
    lr = lr,
    bf = bf,
    indicator = indicator,
    indicator_family = indicator_family
  )


  # add a model name slot
  full_model$model_name <- model_name

  # predict the main BRT
  df[, paste0(model_name, "_full_pred") := predict(full_model, df, n.trees = full_model$gbm.call$best.trees, type = "response")]

  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)
  # print(return_cols)
  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
