#' @title Return path to admin shapes directory
#'
#' @description
#' Returns path to the official LBD administrative shape file directory. This
#' actually includes non-shape data that is important for mapping, notably
#' the standard link table and standard id raster.
#'
#' @param version admin shapefile version to pull
#' @export
#'
#' @concept shapefile
get_admin_shape_dir <- function(version = "current") {
  return(file.path("<<<< FILEPATH REDACTED >>>>", version))
}
