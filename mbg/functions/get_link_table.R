#' @title Subset global link table to regional table
#'
#' @description Reads in the global link table and id raster and subsets using
#' a regional simple raster.
#'
#' @param simple_raster Simple raster used for modelling region
#' @param shapefile_version character. The release date of the admin shapefile to use e.g., "2019_10_10".
#' @param raster_agg_factor int. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.
#' No other resolutions are supported at this time. Set in the modelling config file
#'
#' @return list of 2 objects:
#' - a link table
#' - a vector of pixel ids used to link one year in a cell pred to the link table.
#'
#' @export
#'
#' @concept fractional_raking
get_link_table <- function(simple_raster, shapefile_version, raster_agg_factor) {

  # read in link objects
  link <- get_link_table_and_id_raster(shapefile_version, raster_agg_factor)
  global_link <- link$link_table
  global_raster <- link$id_raster

  # crop id_raster to simple_raster
  cropped_raster <- raster::crop(global_raster, simple_raster)
  # mask id_raster with simple_raster
  masked_raster <- raster::mask(cropped_raster, simple_raster)
  # extract id_raster
  pixel_ids <- raster::extract(masked_raster, raster::extent(masked_raster), na.rm = T)
  pixel_ids <- pixel_ids[!is.na(pixel_ids)]
  # subset link table by pixel ids from extract
  link_table <- global_link[ID %in% pixel_ids, ]
  # make sure list of pixel ids is same number of nonna cells in simple raster
  if (length(pixel_ids) != length(which(!is.na(getValues(simple_raster))))) {
    stop("The number of pixel_ids does not match the number of non-na values in simple raster. \nPart of the simple raster may be outside the extent of the global id raster.")
  }
  # return subsetted link table and list of pixel ids
  return(list("link_table" = link_table, "pixel_ids" = pixel_ids))
}
