#' @title Return path to world admin shapefile at specified admin level
#'
#' @description
#' Returns path to world admin shapefile given \code{admin_level}. stop()s if
#' no file exists at that admin_level. Defaults to returning the ".shp" file
#' path, but will substitute any other file \code{suffix} provided.
#'
#' @param admin_level Valid admin level we have a shapefile for. Current 0/1/2.
#' @param suffix '.shp' by default, provide any other suffix to e.g., get the .dbf file
#' associated with the admin shapefile.
#' @param type One of admin, raking, disputed_mask
#' @param version admin shapefile version to pull
#' @param raking boolean, default F. If TRUE pulls subnational raking shapefile.
#'
#' @examples
#' \dontrun{
#' get_admin_shapefile(2)
#' get_admin_shapefile(2, suffix = ".dbf")
#' }
#' @export
#'
#' @concept shapefile
get_admin_shapefile <- function(admin_level = 0, suffix = ".shp", type = "admin", version = "current", raking = F) {
  if (raking) type <- "raking" # backwards compatibility

  base_dir <- get_admin_shape_dir(version)

  if (type == "admin") {
    filename <- paste0("lbd_standard_admin_", admin_level, suffix)
  } else if (type == "raking") {
    filename <- paste0("lbd_standard_raking", suffix)
  } else if (type == "disputed_mask") {
    filename <- paste0("lbd_disputed_mask", suffix)
  } else {
    stop(paste("Unknown admin shapefile type '", type, "'"))
  }
  path <- file.path(base_dir, filename)

  if (!file.exists(path)) {
    warning(sprintf("Could not locate admin shapefile (%s)", path))
  }
  return(path)
}
