#' @title Build a raking raster
#' @description Builds a raster version of the modeling domain. The raking raster is similar to the
#' \code{\link{build_simple_raster}}, except it has GBD loc_id values rather than LBD admin codes.
#' If doing subnational raking, the corresponding subnational countries will have subnational loc_ids rather than
#' national ones. This raster is primarily used in non-fractional raking to group pixels to a GBD raking target.
#'
#' @param template_raster A spatial object e.g \code{SpatialPolygonsDataFrame} or \code{raster} to define the
#' extent of the simple raster being created.
#' @param shapefile_version String. which shapefile version is used.
#' @param region A valid string for \code{\link{get_adm0_codes}}. The modeling region or countries being modeled.
#' @param countries_not_to_subnat_rake A valid string for \code{\link{get_adm0_codes}}. Any countries that have subnational gbd targets that you do NOT want to rake subnationally. Set in the modelling config file.
#' @param subnational_rake Boolean, do you want to rake subnationally or not? Set in the modelling config file
#' @param raster_agg_factor int.. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.
#' No other resolutions are supported at this time. Set in the modelling config file
#'
#' @return raking raster object
#'
#' @export
#'
#' @concept prep
build_raking_raster <- function(extent_template,
                                shapefile_version,
                                region,
                                countries_not_to_subnat_rake,
                                rake_subnational,
                                raster_agg_factor) {

  # get table linking GBD loc ids and LBD standard admin codes
  raking_link_pieces <- standard_raking_link_pieces(shapefile_version, countries_not_to_subnat_rake)
  locid_lookup_table <- unique(raking_link_pieces$code_locid)

  if (rake_subnational == F) {
    message("You have chosen not to rake subnationally; make sure you are using national level raking factors from GBD\n")
    subnational_codes <- NULL
  } else {
    subnational_codes <- raking_link_pieces$admin_codes$ADM1_CODE
  }

  # load in link table and id raster
  link <- get_link_table_and_id_raster(shapefile_version, raster_agg_factor = raster_agg_factor)
  link_table <- link$link_table
  id_raster <- link$id_raster

  # the downstream logic
  if ("loc_id" %in% names(link_table)) {
    setnames(link_table, "loc_id", "gbd_loc_id")
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
  link_table <- link_table[ADM0_CODE %in% adm0_list & pixel_owner]

  # create ADM_CODE column with mixed ADM0 and subnational columns
  link_table[, ADM_CODE := ADM0_CODE]
  link_table <- link_table[ADM1_CODE %in% subnational_codes, ADM_CODE := ADM1_CODE]

  # merge on GBD loc ids
  gbd_link_table <- merge(link_table, locid_lookup_table, by.x = "ADM_CODE", by.y = "admin_code", all.x = T)
  gbd_link_table <- setorder(gbd_link_table, pixel_id)

  id_raster_table <- data.table(pixel_id = raster::extract(reg_id_raster, extent(reg_id_raster)))
  id_raster_table <- merge(id_raster_table, gbd_link_table, by = "pixel_id", all.x = T)

  # insert values from desired field into region id_raster
  raking_raster <- raster::setValues(reg_id_raster, id_raster_table$loc_id)

  return(raking_raster)
}
