#' @title Load GBD Covariates
#' @param covs A character vector listing GBD covariates/outputs to extract. For covariates, this
#' should be indicated by covariate_name_short, while for outputs, this should be indicated by
#' acause. Usually fulfilled by gbd_fixed_effects.
#' @param measures A character vector coresponding to 'covs' listing the type of GBD quantity for
#' each item. Options are 'covariate' and 'output'. Usually fulfilled by gbd_fixed_effects_measures.
#' @param year_ids A numeric vector of year_ids. Usually fulfilled by year_list.
#' @param age_ids A string of age_ids. Usually fulfilled by gbd_fixed_effects_age.
#' @param template A raster layer of the buffered modelling area. usually it will be cov_layers[[1]][[1]].
#' If NULL, a default template is loaded using load_and_crop_covariates_annual()
#' @param use_subnationals Logical. If true, the function will replace admin0 with a subnational units
#' where possible. Use with caution because it's not been tested outside of Africa. It might not
#' work for countries with multiple levels of subnational units (e.g. UK or India).
#' @param simple_polygon simple_polygon object used for the modeling region. made in load_simple_polygon
#' @param interval_mo number of months in a time unit. usually 12 to correspond 'annual' year_loadxs
#' @param year_list vector of years. If NULL, defaults to global value "year_list".
#' @param modeling_shapefile_version String specifying the shapefile version date to be used
#' @return A list of covariates
#' @export
#'
#' @concept covariate
load_gbd_covariates <- function(covs, measures, year_ids, age_ids,
                                template, use_subnationals = F,
                                simple_polygon, interval_mo,
                                year_list = use_global_if_missing("year_list"),
                                modeling_shapefile_version = use_global_if_missing("modeling_shapefile_version")) {

  # check to see if the template is class raster, if not it is most
  # likely NULL since we pass in cov_layers[[1]][[1]] by default and
  # that is only created if we loaded in geospatial covs. otherwise,
  # we load in a geospatial cov to use as template
  if (class(template) != "RasterLayer") {
    message("Loading in raster template for GBD covs since template argument was not a RasterLayer")
    template <- load_and_crop_covariates_annual(
      covs = "evi",
      measures = "median",
      simple_polygon = simple_polygon,
      start_year = min(year_ids),
      end_year = max(year_ids),
      interval_mo = as.numeric(interval_mo)
    )[[1]][[1]]
  }

  # If we are not using subnationals, keep only national units; otherwise remove the parent units
  if (!use_subnationals) {
    # Read in admin 0 shapefile
    shape <- readOGR(paste0(get_admin_shapefile(0, version = modeling_shapefile_version)), integer64 = "warn.loss")
    # the downstream logic
    if (!("loc_id" %in% colnames(shape@data))) {
      loc_ids <- get_location_code_mapping(modeling_shapefile_version)
      shape <- merge(shape, loc_ids, by.x = "ADM0_CODE", by.y = "ADM_CODE")
    }
  } else {
    # Read in raking shapefile that include GBD subnationals
    shape <- readOGR(paste0(get_admin_shape_dir(version = modeling_shapefile_version), "lbd_standard_raking.shp"), integer64 = "warn.loss")
  }

  shape <- crop(shape, simple_polygon)
  # we must skip using the link_table as it is not relevant to this shapefile
  rast <- rasterize_check_coverage(shape, template, "loc_id", fun = "last", link_table = NULL)
  rast <- crop_set_mask(rast, template)

  # Check to make sure all of the relevant gauls are in the file
  if (!all(shape$loc_id %in% unique(rast))) {
    rast <- rasterize_check_coverage(shape[!shape$loc_id %in% unique(rast), ], rast, "loc_id", fun = "first", update = T)
    rast <- crop_set_mask(rast, template)
  }


  # Loop over requested covariates
  fetch_gbd_cov <- function(name, measure, rast) {

    # Load country-level results
    message("  Loading: ", name)
    gbd <- load_gbd_data(
      gbd_type = measure,
      gbd_name = name,
      loc_ids = unique(rast),
      measure_id = 5,
      age_group_id = age_ids,
      metric_id = 3,
      year_ids = year_ids,
      return_by_age_sex = "no",
      shapefile_version = modeling_shapefile_version,
      collapse_age_sex = TRUE,
      named_location_field = "location_id"
    )
    
    # Getting duplicated location when using with integration tests
    gbd <- unique(gbd)

    if (nrow(gbd) != nrow(unique(gbd[, list(name, year)]))) stop(paste0(name, "is not unique by location-year"))

    # For each gaul code and year, update the values
    blank <- brick(lapply(year_list, function(x) rast * NA))

    for (yyy in 1:length(year_ids)) {
      for (ggg in unique(rast)) {
        blank[[yyy]][which(raster::getValues(rast) == ggg)] <- gbd[name == ggg & year == year_list[yyy], mean]
      }
    }

    names(blank) <- rep(name, times = dim(blank)[3])

    return(blank)
  }

  all_gbd_layers <- lapply(1:length(covs), function(x) fetch_gbd_cov(covs[x], measures[x], rast))
  names(all_gbd_layers) <- covs

  return(all_gbd_layers)
}
