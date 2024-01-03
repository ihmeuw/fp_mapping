#' @title Fit Additive Model Wrapper
#' @description Fit 1+k gams or bams
#'
#' @param df  data table with the outcome/indicator and some covariates already extracted. This is different than the gam_cov functions
#' @param model_name string indicating name of model to attach to the model fit object
#' @param fold_id_col in df, the column name that specifies which fold each observation/row belongs to
#' @param covariates a vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
#' @param additional_terms a vector or single character of column names to be included in the model fit
#' @param weight_column in df, is there a column that specifies the observation weights?
#' @param bam should bam be used rather than gam?
#' @param spline_args list with additional arguments to be sent to the spline call of gam/bam (see ?mgcv::s for options).
#' Default is for k (dimension of basis) to be 3 and bs (smoothing basis used) to be "ts" a type of
#' thin plate regression splines
#' @param auto_model_select if true, it overwrites BAM instructions to fit a GAM on smaller (N<2000) datasets-- helps with convergence
#' @param indicator name of column in data frame with outcome
#' @param indicator_family model family
#' @param cores number of cores are available for use
#'
#' @return df with predictions from all data and using holdouts
#'
#' @export
#'
#' @concept stacking
fit_gam_child_model <- function(df,
                                model_name = "gam",
                                fold_id_col = "fold_id",
                                covariates = all_fixed_effects,
                                additional_terms = NULL,
                                weight_column = NULL,
                                bam = F,
                                spline_args = list(bs = "ts", k = 3),
                                auto_model_select = T,
                                indicator = use_global_if_missing("indicator"),
                                indicator_family = "binomial",
                                cores = "auto") {
  # remove scoping surprises
  df <- copy(df)

  # start by fitting the full gam
  message("Fitting the Full GAM model")
  full_model <- fit_gam(df[!is.na(get(indicator))],
    covariates = covariates,
    additional_terms = additional_terms,
    weight_column = weight_column,
    bam = bam,
    spline_args = spline_args,
    auto_model_select = auto_model_select,
    indicator = indicator,
    indicator_family = indicator_family,
    cores = cores
  )

  # add a name to the gam object
  full_model$model_name <- model_name

  # fit the child/cv gams
  message("Fitting baby gams")
  # fit the child/cv rfs
  folds <- unique(df[, get(fold_id_col)])

  for (fff in folds) {
    baby_model <- fit_gam(
      df = df[get(fold_id_col) != fff & !is.na(get(indicator)), ],
      covariates = covariates,
      additional_terms = additional_terms,
      bam = bam,
      weight_column = weight_column,
      spline_args = spline_args,
      auto_model_select = auto_model_select,
      indicator = indicator,
      indicator_family = indicator_family,
      cores = cores
    )

    # fill in the data
    df[get(fold_id_col) == fff, paste0(model_name, "_cv_pred") := predict(baby_model, df[get(fold_id_col) == fff, ], type = "response")]
  }

  # predict using full model fit earlier
  df[, paste0(model_name, "_full_pred") := predict(full_model, df, type = "response")]

  # return a subset of the columns. Full pred denotes the fit from the full model. CV pred is the OOS stuff
  suffixes <- c("_full_pred", "_cv_pred")
  return_cols <- paste0(model_name, suffixes)
  # print(return_cols)
  # set up with for the return call
  return(setNames(list(df[, return_cols, with = F], full_model), c("dataset", paste0(model_name))))
}
