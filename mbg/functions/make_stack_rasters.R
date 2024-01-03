#' @title Produce Stack Rasters Wrapper
#' @description Create a stacked prediction for each period and child model. The
#' wrapper runs all (or a subset of periods) and formats the results. Also saves
#' the child_models.
#' @param covariate_layers list of raster objects that consititue the covs. Must share names with the DT that the models were fit on, Default: all_cov_layers
#' @param period  what periods should be used, Default: NULL (all periods-- 1:4)
#' @param child_models a list of model objects from the child models, Default: list()
#' @param indicator_family model family, Default: 'binomial'
#' @param rd run data
#' @param re region
#' @param ind_gp indicator group
#' @param ind indicator
#' @param ho holdout
#' @param centre_scale_df df that contains centering and scaling info, Default: NULL
#' @return List of raster objects. First object is a brick of stacked results.
#' Additional objects refer to the child models and periods
#' @export
#'
#' @concept stacking
make_stack_rasters <- function(covariate_layers = all_cov_layers,
                               period = NULL,
                               child_models = list(),
                               indicator_family = "binomial",
                               rd = run_date,
                               re = reg,
                               ind_gp = indicator_group,
                               ind = indicator,
                               ho = holdout,
                               centre_scale_df = NULL,
                               ...) {

  ## first, save child_models for use in get.cov.wts in post_estiamtion if not other places too
  save(child_models, file = "<<<< FILEPATH REDACTED >>>>")

  if (is.null(period)) {
    period <- 1:4
  }

  ## Create rasters from the child models
  res <- lapply(period, function(the_period) {
    produce_stack_rasters(
      covariate_layers = covariate_layers, # raster layers and bricks. Covariate rasters essentially
      period = the_period, # period of analysis. Fed in from the lapply
      child_models = child_models, # a list of model objects for the child learners
      indicator_family = indicator_family,
      centre_scale_df = centre_scale_df
    )
  })


  ## Prep the rasters to be ordered in the following order:
  ## raster_brick[[child_models]][[period]]
  ret_obj <- lapply(names(child_models), function(x_cn) {
    raster::brick(lapply(period, function(x_t) res[[x_t]][[x_cn]]))
  })

  ## Fix for single period (rename with suffix ".1")
  if (length(period) == 1) {
    for (i in 1:length(ret_obj)) {
      names(ret_obj[[i]]) <- paste0(names(child_models)[i], ".1")
    }
  }

  ## Set names of the output raster list
  names(ret_obj) <- names(child_models)

  # Make sure that the raster brick dimensions and names are all correct
  stopifnot(assertthat::are_equal(length(ret_obj), length(names(child_models))))

  j <- 0
  for (x_t in ret_obj) {
    j <- j + 1
    stopifnot(assertthat::are_equal(names(x_t), paste0(names(child_models)[j], ".", period)))
  }


  # return!
  return(ret_obj)
}
