#' @title Load and return the 5km or the 10km link table and id raster
#'
#' @description Loads the 5km or 10km link table and id raster files associated with an admin shapefile.
#' This function is a wrapper of the lt::get_link_table_and_id_raster function
#'
#' @param modeling_shapefile_version the administrative shapefile version. Either "current" or
#' a valid YYYY_MM_DD string.
#'
#' @param raster_agg_factor The aggregation factor used to create id raster. Currently, only
#' factors 1 and 2 are supported.
#'
#' @param pathonly Boolean that determines whether to return paths for .rds files or the files themselves.
#'
#' @return the list of paths of RDS files (link table and id raster) or
#' list of data.tables stored in the RDS files (link table and id raster).
#' @export
#'
#' @concept link_table
get_link_table_and_id_raster <- function(modeling_shapefile_version, raster_agg_factor = 1, pathonly = FALSE) {

  raster_agg_factor <- as.integer(raster_agg_factor)

  shapefile_path <- lsae.utils::get_admin_shapefile(admin_level = 2, version = modeling_shapefile_version)
  link_list <- lt::get_link_table_and_id_raster(shapefile_path, raster_agg_factor, pathonly)

  if(modeling_shapefile_version == "current") {
    shapefile_version_dt <- lubridate::parse_date_time("2019_10_12", orders="%Y_%m_%d", exact=TRUE)
  } else {
    shapefile_version_dt <- lubridate::parse_date_time(modeling_shapefile_version, orders="%Y_%m_%d", exact=TRUE)
  }
  id_raster_cutoff <- lubridate::parse_date_time("2019_10_11", orders="%Y_%m_%d", exact=TRUE)

  # early id rasters (up to 2019_10_10) were built off the stage 2 (LMICs) shapefile
  # rather than the world shapefile, so need to be read in from the file system rather
  # than generated through empty_world_raster()
  if (!is.na(shapefile_version_dt) && shapefile_version_dt < id_raster_cutoff) {
    if (raster_agg_factor == 1) {
      # This is for backward compatibility.
      # Assumes that either symlinks or older link table and id raster will be present.
      idr_file <- "lbd_standard_id_raster.rds"
    } else if (raster_agg_factor == 2) {
      idr_file <- "<<<< FILEPATH REDACTED >>>>"
    } else {
      stop(sprintf("Modeling for your raster aggregation factor: %i is not supported yet!", raster_agg_factor))
    }

    idr_path <- file.path(lsae.utils::get_admin_shape_dir(modeling_shapefile_version), idr_file)

    if (!file.exists(idr_path)) {
      stop(paste0("Expected id raster not present:\n", idr_path))
    }

    if (pathonly) {
      link_list$id_raster <- idr_path
    } else {
      link_list$id_raster <- readRDS(idr_path)
    }
  }

  return(link_list)
}
