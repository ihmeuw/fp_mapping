#' @title Fit Generalized Boosted Regression Model
#' @description Fit a boosted regression tree model
#'
#' @param df  data table with the outcome/indicator and some covariates already extracted
#' @param covariates a vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
#' @param additional_terms a vector or single character of column names to be included in the model fit
#' @param weight_column in df, is there a column that specifies the observation weights?
#' @param tc tree complexity
#' @param lr learning rate
#' @param bf bag fraction
#' @param indicator name of column in data frame with outcome
#' @param indicator_family model family
#' @param plot.main Logical.
#'
#' @return model fit
#'
#' @export
#'
#' @concept stacking
fit_gbm <- function(df,
                    covariates = all_fixed_effects,
                    additional_terms = NULL,
                    weight_column = NULL,
                    tc = 4,
                    lr = .005,
                    bf = .75,
                    indicator,
                    indicator_family = "binomial",
                    plot.main = F) {
  # check to see if its a vector of characters or a pseudo-formula
  covariates <- format_covariates(add_additional_terms(covariates, additional_terms))

  df <- copy(df)

  # format weights
  if (!is.null(weight_column)) {
    df[, data_weight := get(weight_column)]
  } else {
    df[, data_weight := 1]
  }
  weight_column <- "data_weight" # specify the column


  # BRT function we use has no binomial. Use emperical logistic or poisson

  # set up poisson outcome structure for binomial and poisson data
  if (indicator_family %in% c("binomial", "poisson")) {
    indicator_family <- "poisson"
    offset <- log(df[, N])
    df[, pre_round := get(indicator)]
    # message('WARNING: For Poisson to work, need to round decimals in the response')
    df[, paste0(indicator) := round(pre_round, 0)] # round indicator to 0
  } else {
    offset <- NULL
  }

  # run the brts. The logic is similiar to the BRT covs (e.g. copy pasted). NOTE,
  # this will run a model for all periods at once. Brt_covs runs each year independently.
  # learning brt

  message(paste("Fitting GBM/BRT with tc:", tc, "lr:", lr, "bf:", bf))

  # throw a try-catch so if some years work it at least will return that,
  # if it doesnt it will try different things (like changing the learning rate. )
  mod <- try(
    dismo::gbm.step(
      data = as.data.frame(df),
      gbm.y = indicator,
      gbm.x = covariates,
      offset = offset,
      family = indicator_family,
      site.weights = df[, get(weight_column)],
      tree.complexity = tc,
      learning.rate = lr,
      bag.fraction = bf,
      silent = T,
      plot.main = F,
      plot.folds = F
    ),
    silent = TRUE
  )

  if (is.null(mod)) {
    message("First BRT attempt failed. Lowering Learning Rate by 1/10")
    mod <- try(
      dismo::gbm.step(
        data = as.data.frame(df),
        gbm.y = indicator,
        gbm.x = covariates,
        offset = offset,
        family = indicator_family,
        site.weights = df[, get(weight_column)],
        tree.complexity = tc,
        learning.rate = lr * .1,
        bag.fraction = bf,
        silent = T,
        plot.main = F,
        plot.folds = F
      )
    )
  }
  if (is.null(mod)) {
    message("Second BRT attempt failed. Lowering Original Learning rate by 1/1000 AGAIN")
    mod <- try(
      dismo::gbm.step(
        data = as.data.frame(df),
        gbm.y = indicator,
        gbm.x = covariates,
        offset = offset,
        family = indicator_family,
        site.weights = df[, get(weight_column)],
        tree.complexity = tc,
        learning.rate = lr * .001,
        bag.fraction = bf,
        silent = T,
        plot.main = F,
        plot.folds = F
      )
    )
  }
  if (is.null(mod)) {
    message("Third BRT attempt failed. Slow learn plus low tree complexity")
    mod <- try(
      dismo::gbm.step(
        data = as.data.frame(df),
        gbm.y = indicator,
        gbm.x = covariates,
        offset = offset,
        family = indicator_family,
        site.weights = df[, get(weight_column)],
        tree.complexity = 2,
        learning.rate = lr * .001,
        bag.fraction = bf,
        silent = T,
        plot.main = F,
        plot.folds = F
      )
    )
  }

  if (is.null(mod)) stop("ALL BRT ATTEMPTS FAILED")

  return(mod)
}
