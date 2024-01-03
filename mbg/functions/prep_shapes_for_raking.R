#' @title Create rasters for raking and aggregation
#' @description This snippet of code is taken from \code{rake_cell_pred} so that
#' we can feed this into making summary statistics later on in the fractional raking code
#'
#' @param reg Region
#' @param modeling_shapefile_version Modeling shapefile version
#' @param raking_shapefile_version Raking shapefile version
#' @param field Location field. Default: "loc_id"
#' @param raster_agg_factor int. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.
#' No other resolutions are supported at this time. Set in the modelling config file
#' @param rake_subnational Boolean, do you want to rake subnationally or not? Set in the modelling config file
#' @param countries_not_to_subnat_rake A valid string for \code{\link{get_adm0_codes}}. Any countries that have subnational gbd targets that you do NOT want to rake subnationally. Set in the modelling config file.
#'
#' @return A list with objects: "simple_raster", "simple_polygon", "subset_shape", "new_simple_raster", "new_simple_polygon", "new_subset_shape"
#'
#' @export
#'
#' @concept fractional_raking
prep_shapes_for_raking <- function(reg,
                                   modeling_shapefile_version,
                                   raking_shapefile_version,
                                   field = "loc_id",
                                   raster_agg_factor,
                                   rake_subnational,
                                   countries_not_to_subnat_rake) {

  ## Load simple polygon template to model over (in GADM SPACE)
  gaul_list <- get_adm0_codes(reg, shapefile_version = raking_shapefile_version)
  simple_polygon_list <- load_simple_polygon(
    gaul_list = gaul_list, buffer = 0.4,
    shapefile_version = raking_shapefile_version
  )
  subset_shape <- simple_polygon_list[[1]]
  simple_polygon <- simple_polygon_list[[2]]

  ## Load list of raster inputs (pop and simple)
  simple_raster <- build_simple_raster(
    extent_template = subset_shape,
    shapefile_version = modeling_shapefile_version,
    region = reg,
    raster_agg_factor = raster_agg_factor
  )
  pixel_id <- seegSDM:::notMissingIdx(simple_raster)

  # load raking raster
  new_simple_raster <- build_raking_raster(
    extent_template = subset_shape,
    shapefile_version = raking_shapefile_version,
    region = reg,
    countries_not_to_subnat_rake = countries_not_to_subnat_rake,
    rake_subnational = rake_subnational,
    raster_agg_factor = raster_agg_factor
  )

  # get extents of original and simple raster to line up - extend and crop just in case
  new_simple_raster <- raster::extend(new_simple_raster, simple_raster, values = NA)
  new_simple_raster <- raster::crop(new_simple_raster, raster::extent(simple_raster))
  new_simple_raster <- raster::mask(new_simple_raster, simple_raster)

  ## Return a named list of things we want
  return(list(
    "simple_raster" = simple_raster,
    "simple_polygon" = simple_polygon,
    "new_simple_raster" = new_simple_raster,
    "pixel_id" = pixel_id
  ))
}
