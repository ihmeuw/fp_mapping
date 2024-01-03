#' @title Fit Penalized Regression
#' @description Fit penalized regression (lasso, elastic net, ridge)
#'
#' @param df  data table with the outcome/indicator and some covariates already extracted
#' @param covariates a vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
#' @param additional_terms a vector or single character of column names to be included in the model fit
#' @param weight_column in df, is there a column that specifies the observation weights?
#' @param alpha Alpha parameter for glmnet calls. 1 = lasso, 0 = ridge
#' @param indicator name of column in data frame with outcome
#' @param indicator_family model family
#' @param parallel TRUE/FALSE to turn on/off parallelization
#'
#' @return model fit
#'
#' @export
#'
#' @concept stacking
fit_glmnet <- function(df,
                       covariates = all_fixed_effects,
                       additional_terms = NULL,
                       weight_column = NULL,
                       alpha = 1,
                       indicator,
                       indicator_family = "binomial",
                       parallel = FALSE) {
  df <- copy(df)

  # add additional terms if requested
  the_covs <- format_covariates(add_additional_terms(covariates, additional_terms))

  # format weights
  if (!is.null(weight_column)) {
    data_weights <- df[, get(weight_column)]
  } else {
    data_weights <- rep(1, nrow(df))
  }

  # create outcome object
  if (indicator_family == "binomial") {
    not_indicator <- df[, N] - df[, get(indicator)]
    response_var <- cbind(not_indicator, outcome = df[, get(indicator)])
  } else if (indicator_family == "poisson") {
    if (!is.null(offset)) {
      stop("fit_glmnet does not currently work for offset poissons")
    } else {
      response_var <- df[, get(indicator)]
    }
  } else {
    response_var <- df[, get(indicator)]
  }


  # create design matrix
  dm <- as.matrix(df[, the_covs, with = F])
  colnames(dm) <- the_covs

  # search for lambda
  # these models are run as gaussian because of the prior transformation.
  message(paste0("Fitting glmnet with alpha: ", alpha))

  cv_res <- glmnet::cv.glmnet(x = dm, y = response_var, family = indicator_family, alpha = alpha, weights = data_weights, parallel = parallel)

  # fit full model using selected lambdas
  model <- glmnet::glmnet(x = dm, y = response_var, family = indicator_family, lambda = cv_res$lambda, alpha = alpha, weights = data_weights)

  # preserve the cv_1se_lambda
  model$cv_1se_lambda <- cv_res$lambda.1se

  return(model)
}
