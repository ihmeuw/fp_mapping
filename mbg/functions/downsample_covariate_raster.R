#' @title downsample_covariate_raster
#' @description Given an integer aggregration factor, downsample a covariate raster
#' @param cov_raster raster to be resampled to coarser resolution
#' @param simple_raster raster that has already been resampled, used as target
#' @param raster_agg_factor integer for scaling factor
#' @return rescaled coarser covariate raster
#' @details Note that this can be used for any pair of rasters.
#' @export
#'
#' @concept misc
downsample_covariate_raster <- function(cov_raster,
                                        simple_raster,
                                        raster_agg_factor) {
  message(sprintf(
    "\nDownsampling %s covariate raster by a factor of %d",
    strsplit(names(cov_raster)[1], "[.]")[[1]][1], # get first part of first name
    as.integer(raster_agg_factor)
  ))

  # Categorical covariates LOG1S = Boolean ,
  # assume if datatype is integer it is a classification, so not something you want to interpolate
  # finally test for Boolean raster that is not declared that way
  if ((dataType(cov_raster) == "LOG1S") |
    grepl("INT", dataType(cov_raster)) |
    isTRUE(.test_Boolean(cov_raster = cov_raster))) {
    downsampled_raster <- raster::resample(cov_raster,
      simple_raster,
      method = "ngb"
    )

    # Covariates requiring sum rather than mean during aggregation
  } else if (any(stringr::str_detect(names(cov_raster), "worldpop"))) {
    # raster::aggregate introduces NaN values for worldpop,
    # for an unknown reason; this line avoids the problem
    values(cov_raster)[is.na(values(cov_raster))] <- 0

    downsampled_raster <- suppressWarnings(raster::aggregate(cov_raster,
      fun = sum,
      as.integer(raster_agg_factor)
    ))

  } else {
    downsampled_raster <- raster::resample(cov_raster, simple_raster)
  }

  return(downsampled_raster)
}
