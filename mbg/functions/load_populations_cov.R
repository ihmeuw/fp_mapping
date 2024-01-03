#' @title Load Populations For Covariates
#' @description This gets the population in each cell which you need to convert the rates to counts for fractional raking
#' it can also get the stackers if you would like
#'
#' @param reg modeling region over which you are operating
#' @param pop_measure the world pop covariate measure used in your model
#' @param measure needs to be depricated, currently a flag that if set to 'prevalence' this function also grabs teh covatiate stakers
#' @param simple_polygon the simple polygon for your region
#' @param simple_raster the simple raster for your model
#' @param year_list The years you are modeling over, will be used to pull populations
#' @param interval_mo the number of months between time steps
#' @param outputdir currently not used, there is a commented out line of code which can save the population raster if you want
#' @param raster_agg_factor int. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.
#' No other resolutions are supported at this time. Set in the modelling config file
#'
#' @return used internally above, returns a data frame with the population in each cell-year so rates can be turned into counts
#' @return for fractional raking and aggregation
#' @export
#'
#' @concept fractional_raking
load_populations_cov <- function(reg,
                                 pop_measure,
                                 measure = "prevalence",
                                 simple_polygon,
                                 simple_raster,
                                 year_list,
                                 interval_mo,
                                 outputdir,
                                 pixel_id,
                                 raster_agg_factor) {
  message("Loading the world pop rasters for this region")
  pop_raster_annual <- load_worldpop_covariate(
    template_raster = simple_polygon,
    pop_measure = pop_measure,
    pop_release = pop_release,
    start_year = min(year_list),
    end_year = max(year_list),
    interval = as.numeric(interval_mo)
  )[[1]]

  ## downsample population raster if necessary
  if (as.integer(raster_agg_factor) > 1) {
    pop_raster_annual <- downsample_covariate_raster(
      cov_raster = pop_raster_annual,
      simple_raster = simple_raster,
      raster_agg_factor = as.integer(raster_agg_factor)
    )
  }

  ## extend and crop pop raster to ensure it matches the simple raster #not convinced this is totally needed
  pop <- pop_raster_annual
  pop <- raster::extend(pop, simple_raster, values = NA)
  pop <- raster::crop(pop, raster::extent(simple_raster))
  pop <- raster::setExtent(pop, simple_raster)
  pop <- raster::mask(pop, simple_raster)

  ## check to ensure the pop raster matches the simple raster in extent and resolution
  if (raster::extent(pop) != raster::extent(simple_raster)) {
    stop("population raster extent does not match simple raster")
  }
  if (any(round(res(pop), 8) != round(res(simple_raster), 8))) {
    stop("population raster resolution does not match simple raster")
  }
  # writeRaster(pop, paste0(outputdir, indicator,"_", reg, "_pop_rasters.tif"), format = "GTiff", overwrite = TRUE)

  message("loading the covariate stakers for this model")
  if (measure == "prevalence") {
    covs <- fetch_from_rdata("<<<< FILEPATH REDACTED >>>>")
    fes <- fetch_from_rdata("<<<< FILEPATH REDACTED >>>>")

    submodels <- trimws(strsplit(fes, "+", fixed = T)[[1]])
    covs <- covs[submodels]
    # make sure spatial extent is the same
    covs <- lapply(covs, function(x) invlogit(raster::crop(x, simple_raster)))
  } else {
    covs <- list()
  }

  # bring everything into one place
  covs$pop <- raster::crop(pop, simple_raster)
  covnames <- names(covs)

  # ensure the dimensions are the same
  for (ccc in covs) {
    stopifnot(dim(ccc)[1:2] == dim(simple_raster)[1:2])
  }

  message("converting the covariate stackers and pop data in to a data table")
  # convert to datables, reshape and stuff
  brick_to_dt <- function(bbb, pixel_id = pixel_id) {
    dt <- setDT(as.data.frame(bbb))
    dt[, pxid := .I] # probably uncessary

    # drop rows now in cellIdx
    dt <- dt[pixel_id, ]

    dt <- melt(dt, id.vars = "pxid", variable.factor = F)
    dt <- dt[, .(value)]
    return(dt)
  }
  covdt <- covnames
  for (iii in 1:length(covs)) {
    dt <- brick_to_dt(bbb = covs[[iii]], pixel_id = pixel_id)
    covdt[[iii]] <- dt
  }
  # covdt = lapply(covs, brick_to_dt(bbb = covs, pixel_id = pixel_id))
  covdt <- do.call(what = cbind, covdt)
  setnames(covdt, names(covs))

  # Add pixel_id, but make sure that its recycled explicitly as per data.table 1.12.2 guidelines
  covdt[, pixel_id := rep(pixel_id, times = nrow(covdt) / length(pixel_id))]

  # set pop to 0 when pop is na
  covdt[is.na(pop), pop := 0]

  # add year to covdt
  yyy <- as.vector(unlist(lapply(min(year_list):max(year_list), function(x) rep.int(x, times = length(pixel_id)))))
  covdt[, year := yyy]

  # free up a bit of space
  rm(covs)

  return(covdt)
}
