#' @title Fit Additive Model
#' @description Fit a gam or bam
#'
#' @param df  data table with the outcome/indicator and some covariates already extracted. This is different than the gam_cov functions
#' @param covariates a vector of covariate names and/or formula in the style of all_fixed_effects (e.g. cov1 + cov2 + cov3)
#' @param additional_terms a vector or single character of column names to be included in the model fit
#' @param weight_column in df, is there a column that specifies the observation weights?
#' @param bam should bam be used rather than gam?
#' @param spline_args list with additional arguments to be sent to the spline call of gam/bam (see ?mgcv::s for options)
#' @param auto_model_select if true, it overwrites BAM instructions to fit a GAM on smaller (N<2000) datasets-- helps with convergence
#' @param indicator name of column in data frame with outcome
#' @param indicator_family model family
#' @param cores number of cores are available for use
#'
#' @return model fit
#'
#' @export
#'
#' @concept stacking
fit_gam <- function(df,
                    covariates = all_fixed_effects,
                    additional_terms = NULL,
                    weight_column = NULL,
                    bam = F,
                    spline_args = list(),
                    auto_model_select = F,
                    indicator,
                    indicator_family = "binomial",
                    cores = "auto") {
  # also requires seeg

  df <- copy(df) # in case data table scoping gets wonky

  # check to see if the gam formula is prespecified. if not, make it
  # check to see if its a vector of characters or a psudo-formula
  covariates <- format_covariates(covariates) # additional terms are handled below


  # remove binary vars from the splined versions and set as additional terms
  n_uniq_vals <- unlist(setNames(df[, lapply(covariates, function(x) uniqueN(get(x)))], covariates)) # this needs to return a named vector
  binary_vars <- names(n_uniq_vals[n_uniq_vals <= 2])
  covariates <- covariates[!covariates %in% binary_vars]

  additional_terms <- c(additional_terms, binary_vars)
  additional_terms <- additional_terms[!(is.null(additional_terms) | is.na(additional_terms))]

  # set response variable (stolen from GAM trans)
  if (indicator_family == "binomial") response <- cbind(events = df[, get(indicator)], trials = df[, N] - df[, get(indicator)])
  if (indicator_family == "gaussian") response <- cbind(outcome = df[, get(indicator)])


  # sort out the formula body
  f_bod <- paste(paste0("s(", covariates, ", ", parseArgsS(spline_args), ")"), collapse = " + ")
  gam_formula <- paste0("response ~ 1 + ", f_bod)
  if (length(additional_terms) > 0) gam_formula <- paste0(gam_formula, " + ", paste(additional_terms, collapse = " + "))

  # message(gam_formula)

  gam_formula <- as.formula(gam_formula) # final model formula

  # sort out weights
  # format weights
  if (!is.null(weight_column)) {
    df[, data_weight := get(weight_column)]
  } else {
    df[, data_weight := 1]
  }
  weight_column <- "data_weight"


  # The `gam` and `bam` functions in the mgcv package have their own internal
  # OpenMP parallelization which can also use the MKL. So, we'll use those cores
  # set aside for OpenMP here for the `gam` and `bam` internal parallelization,
  # namely OMP_NUM_THREADS:
  if (cores == "auto") cores <- get_omp_threads()

  # fit the gam
  message(paste0("Fitting GAM/BAM with spline args of: ", names(spline_args)[1], "=", spline_args[1], " ", names(spline_args)[2], "=", spline_args[2]))
  if (bam) {
    if (auto_model_select == T & nrow(df) < 2000) {
      model <- mgcv::gam(gam_formula, data = df, family = indicator_family, weights = df[, get(weight_column)], control = list(nthreads = as.numeric(cores)))
    } else {
      model <- mgcv::bam(gam_formula, data = df, family = indicator_family, weights = df[, get(weight_column)], nthreads = as.numeric(cores), discrete = T)
    }
  } else {
    model <- mgcv::gam(gam_formula, data = df, family = indicator_family, weights = df[, get(weight_column)], control = list(nthreads = as.numeric(cores)))
  }

  # return the gam object
  return(model)
}
