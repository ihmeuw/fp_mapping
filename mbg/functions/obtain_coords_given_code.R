#' @title Obtain coordinates of pixels in a polygon
#'
#' @description Determines which pixel centroids fall inside the polygon of interest.
#'
#' @param sub_shp The polygon of interest
#' @param sub_raster The base raster cropped to the polygon of interest.
#' @param disaggregation_factor Integer. The number of cells the raster corresponding
#' to a polygon should be broken into. This can be useful if the polygon is very
#' small in area.
#' @param auto_disaggregate Logical. Should a disaggregation_factor of 5 (i.e. breaking
#' a 5km x 5km pixel to 1km x 1km) be applied if no pixel centroids fall inside the
#' polygon?
#' @param code The location code for the polygon
#'
#' @return Coordinates that fall into the polygon of interest.
#' @export
obtain_coords_given_code <- function(sub_shp, sub_raster, disaggregation_factor,
                                     auto_disaggregate, code) {
  # crop the global raster to the shapefile
  missing_codes <- c()

  # disaggregate raster to increase resolution
  dis_raster <- disaggregate(sub_raster, disaggregation_factor)

  # get centroids of raster
  centers <- coordinates(dis_raster)
  # convert centroids to spatialPoints object
  centers <- SpatialPoints(centers, crs(sub_shp))

  # overlay the centroid points with the polygon
  overlay <- centers[sub_shp, ]
  # pull out a matrix of coordinates from the overlayed object
  coords <- overlay@coords

  # if no raster cell centroids within polygon are found,
  # disaggregate raster by a factor of 5 and try again
  if (length(coords) == 0 & auto_disaggregate == T) {
    # disaggregate raster to increase resolution
    dis_raster <- disaggregate(dis_raster, 5)

    # get centroids of raster
    centers <- coordinates(dis_raster)
    # convert centroids to spatialPoints object
    centers <- SpatialPoints(centers, crs(sub_shp))

    # overlay the centroid points with the polygon
    overlay <- centers[sub_shp, ]
    # pull out a matrix of coordinates from the overlayed object
    coords <- overlay@coords
  }

  # if no matches, take centroid of polygon
  if (length(coords) == 0) {
    centroid <- coordinates(sub_shp)
    coords <- as.matrix(centroid)
    colnames(coords) <- c("x", "y")
    rownames(coords) <- NULL
    if (length(coords) == 0) {
      missing_codes <- c(missing_codes, code)
    }
  }

  # print out polygon codes that had no raster centroids
  if (length(missing_codes) > 0) {
    if (auto_disaggregate == T) {
      message("     After auto_disaggregate, no raster centroids found for these codes: ", paste(as.character(missing_codes), collapse = ", "))
    } else {
      message("     No raster centroids found for these codes: ", paste(as.character(missing_codes), collapse = ", "))
    }
  }

  return(coords)
}
