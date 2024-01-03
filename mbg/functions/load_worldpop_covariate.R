#' @title Load worldpop covariate raster and return
#' @description Loads a covariate raster (worldpop by default, can be overridden) and returns.
#' @param template_raster the raster template which all returned data will match.
#' @param covariate the covariate to load. Defaults to "worldpop"
#' @param pop_measure the covariate measure to load.
#' @param pop_release the covariate measure release to use.
#' @param start_year the first year to locate data for. 
#' @param end_year the last year to locate data for.
#' @param interval the number of months between data readings. 
#' @examples
#' @export
#' @rdname load_worldpop_covariate
#' @return list with named value containing your covariate data as a raster.
#'
#' @concept covariate
load_worldpop_covariate <- function(template_raster,
                                    covariate = "worldpop",
                                    pop_measure,
                                    pop_release,
                                    start_year = min(year_list),
                                    end_year = max(year_list),
                                    interval = interval_mo) {
  worldpop_config <- data.table(
    covariate = c(covariate),
    measure = c(pop_measure),
    release = c(pop_release)
  )

  loader <- MbgStandardCovariateLoader$new(
    start_year = start_year,
    end_year = end_year,
    interval = interval,
    covariate_config = worldpop_config
  )
  return(loader$get_covariates(template_raster))
}
