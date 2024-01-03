#' @title Build a simple raster
#' @description Builds a raster version of the modeling domain. Each pixel is assigned a country code based
#' on the country with the largest area within the pixel. This calculation is facilitated by the link table,
#' see \code{\link{get_link_table_and_id_raster}}.
#'
#' @param template_raster A spatial object e.g \code{SpatialPolygonsDataFrame} or \code{raster} to define the
#' extent of the simple raster being created.
#' @param shapefile_version String. which shapefile version is used.
#' @param region A valid string for \code{\link{get_adm0_codes}}. The modeling region or countries being modeled.
#' @param raster_agg_factor int. Default 1. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.
#' No other resolutions are supported at this time.
#' @param field String, default ADM0_CODE. The column in the link table to insert into the simple raster. Standard options are
#' ADM0_CODE, ADM1_CODE, and ADM2_CODE for admin0/1/2 ownership respectively.
#'
#' @return simple raster object
#'
#' @details The simple raster build process is based off of link tables. A link table contains information about the area
#' fraction of each pixel owned by an admin 2 unit, and there is an admin 2 unit within each pixel that is designated as the
#' owner (see \code{\link{assign_pixel_owners}}). Each shapefile version has an associated link table which is created by
#' overlaying those shapefiles with a raster of the world.
#'
#' The simple raster is a key part of the mbg process because it is used to define pixel ownership (usually at the country level)
#' during modeling. This impacts the fitting and prediction of the model, especially for covariates and country random effects.
#'
build_simple_raster <- function(extent_template,
                                shapefile_version,
                                region,
                                raster_agg_factor = 1,
                                field = "ADM0_CODE") {

  # load in link table and id_raster
  link <- get_link_table_and_id_raster(shapefile_version, raster_agg_factor)
  link_table <- link$link_table
  id_raster <- link$id_raster

  # check that the given field is in the link table
  if (!field %in% names(link_table)) {
    msg <- paste(
      "Error: build_simple_raster called with field:", field,
      "which is not present in link_table. See function documentation for field options."
    )
    stop(msg)
  }

  # crop id_raster to template extent
  reg_id_raster <- crop(id_raster, raster::extent(extent_template), snap = "out")

  # for old link tables without a pixel owner column, run the function that assigns it in new link tables
  if (!"pixel_owner" %in% colnames(link_table)) {
    link_table <- link_table[pixel_id %in% unique(reg_id_raster)]
    link_table <- assign_pixel_owners(link_table)
  }

  adm0_list <- get_adm0_codes(region, shapefile_version = shapefile_version)

  # subset link table to ADM0s in region and to the owners of the pixels
  assigned_pixels <- link_table[ADM0_CODE %in% adm0_list & pixel_owner]

  # merge to get an NA or value for each pixel in the id_raster
  id_raster_table <- data.table(pixel_id = raster::extract(reg_id_raster, extent(reg_id_raster)))
  id_raster_table <- merge(id_raster_table, assigned_pixels, by = "pixel_id", all.x = T)

  # insert values from desired field into region id_raster
  simple_raster <- raster::setValues(reg_id_raster, id_raster_table[[field]])

  return(simple_raster)
}
