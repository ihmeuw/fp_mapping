#' @title Rasterize with border checks
#'
#' @description Rasterizing using a shapefile and a template raster, such that
#' we account for any pixels that are on the border of \code{field} units, which
#' could have been lost due to raster::rasterize only evaluating on centroids
#'
#' @param shapes SpatialPolygonDataFrame.. Input shapefile
#'
#' @param template_raster SpatialPolygonDataFrame.. The reference raster (usually WorldPop)
#'
#' @param field String The field with appropriate administrative unit information (usually ADM0_CODE)
#'
#' @param link_table String or data.table. If data.table it is used as-is. If String: either an absolute
#' file path to an RDS file OR a short name for the administrative shape file
#'
#' @param id_raster String or raster object. If link table is a data.table,
#' id_raster should be a raster RDS file. If link table is an absolute path,
#' id_raster should also be an absolute path. Otherwise if link_table is a
#' relative path, id_raster will be inferred.
#'
#' @return A raster with border and insides properly filled
#'
#' @details rasterize_check_coverage has three distinct use cases based off of the value of link_table
#'
#' 1. \code{link_table} is NULL. In this case rasterize_check_coverage will behave identically to raster::rasterize
#'
#' 2. \code{link_table} is a String referring to relase of admin shapefiles. In this case
#' \code{field} should be "ADM0_CODE", "ADM1_CODE" or "ADM2_CODE". This will load the lbd_standard_link.rds file,
#' from the related admin shapefile directory, aggregate area_fraction as necessary to match the level of \code{field},
#' and then apply those values to pixels in the space defined by \code{shapes}.
#'
#' 3. \link{link_table} is a data.table OR a String absolute path to a RDS file containing a data.table. This will use the
#' provided \code{link_table} to assign values to the result raster similarly to use case #2.
#'
#' Note that for both use cases 2 and 3 all pixel_id coordinates must be in the same raster space. This is currently the
#' area defined by cropping the world raster to the pixels occupied by stage 1 and stage 2 countries.
#'
#' @export
#'
#' @concept prep
rasterize_check_coverage <- function(shapes, template_raster, field, ..., link_table = modeling_shapefile_version, id_raster = NULL) {
  # backwards-compatible behavior - just call rasterize()
  if (is.null(link_table)) {
    return(raster::rasterize(shapes, template_raster, field = field, ...))
  }

  # Validate arguments
  is_admin_link_table <- FALSE
  if (is.data.table(link_table)) { # link table provided
    is_admin_link_table <- TRUE

    if (is.null(id_raster)) {
      stop("Link table given without ID raster.")
    }
  } else if (R.utils::isAbsolutePath(link_table)) { # absolute path to link table provided
    link_table <- readRDS(link_table)

    if (is.null(id_raster)) {
      stop("Link table path given without ID raster path.")
    } else {
      id_raster <- readRDS(id_raster)
    }
  } else if (lsae.utils::is_admin_version_string(link_table)) { # compute path to link table
    is_admin_link_table <- TRUE
    # load link table with pre-computed ownership percentages for each pixel cell
    link_list <- get_link_table_and_id_raster(modeling_shapefile_version = link_table)
    link_table <- link_list$link_table
    id_raster <- link_list$id_raster
  } else {
    stop("link_table argument was neither a data.table, an admin shapefile string, or an absolute path to a RDS file.")
  }

  if (!field %in% names(link_table)) {
    msg <- paste(
      "WARNING: rasterize_check_coverage called with field", field,
      "which is not present in link_table. Defaulting to raster::rasterize()"
    )
    message(msg)
    return(raster::rasterize(shapes, template_raster, field = field, ...))
  }

  # aggregate link table generically for admin 0/1/2
  # Note: we need `with=FALSE` because `field` is passed as a parameter (not a hard-coded string)
  table <- link_table[, c("pixel_id", field, "area_fraction"), with = FALSE]
  if (is_admin_link_table && field != "ADM2_CODE") {
    # sum rows; area_fraction now represents the total area coverage by ADM0/1_CODE instead of ADM2_CODE
    table <- table[, .(area_fraction = sum(area_fraction)), by = c("pixel_id", field)]
  }
  # subset table so that we have 1 entry per pixel_id - the value of `field` with the maximum
  # area_fraction value for that pixel_id
  # https://stackoverflow.com/a/24558696
  pixel_owner <- table[table[, .I[which.max(area_fraction)], by = pixel_id]$V1]
  pixel_owner <- pixel_owner[order(pixel_id)]

  # create reference pixel owner from id raster
  reference_pixel_owner <- id_raster
  raster::values(reference_pixel_owner) <- NA

  # subset to only those pixels owned by a shape we're interested in
  owned_pixels <- pixel_owner[pixel_owner[[field]] %in% shapes[[field]]]
  reference_pixel_owner[owned_pixels$pixel_id] <- owned_pixels[[field]]

  result <- raster::crop(reference_pixel_owner, template_raster, snap = "near")
  if (raster::ncell(result) != raster::ncell(template_raster)) {
    message <- paste(
      "Error in creating result raster. Should have created a raster of shape",
      paste(dim(result), collapse = ","),
      "but instead created a raster of shape",
      paste(dim(template_raster), collapse = ",")
    )
    stop(message)
  }
  return(result)
}
