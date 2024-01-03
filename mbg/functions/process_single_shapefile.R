#' @title Process single shapefile for polygon and potentially age aggregation
#'
#' @description Determines aggregation weights and points for polygon and potentially
#' age aggregated data.
#'
#' @param shape String that indicates the shapefile that should be read in.
#' @param shape_table Data table that has unique shapefiles, location codes,
#' years, and z_ag.
#' @param ref_raster The reference raster.
#' @param fast_shapefiles Dataframe with mapping from z values to worldpop age groups.
#' @param pop_release The population measure release to use.
#' @param interval_mo The number of months between time steps.
#' @param z_map Dataframe with mapping from z values to worldpop age groups.
#' @param disaggregation_factor Integer. The number of cells the raster corresponding
#' to a polygon should be broken into. This can be useful if the polygon is very
#' small in area.
#' @param auto_disaggregate Logical. Should a disaggregation_factor of 5 (i.e. breaking
#' a 5km x 5km pixel to 1km x 1km) be applied if no pixel centroids fall inside the
#' polygon?
#'
#' @return shape_table that has 4 additional columns: coordinates.long and
#' coordinates.lat (lists of the longitude and latitude that make up the polygon),
#' z (list of the values that make up z_ag), and agg_weight (list of the
#' aggregation weights for each point and z).
#' @export
process_single_shapefile <- function(shape, shape_table, ref_raster, fast_shapefiles,
                                     pop_release, interval_mo, z_map,
                                     disaggregation_factor, auto_disaggregate) {
  codes <- unique(shape_table[shapefile == shape]$location_code)
  # All coordinates are the same for a given shape and code
  if (fast_shapefiles) {
    shp <- fast_load_shapefile(shape)
  } else {
    shp <- rgdal::readOGR("<<<< FILEPATH REDACTED >>>>")
  }
  for (code in codes) {
    # subset the shapefile to the specific polygon
    sub_shp <- subset(shp, GAUL_CODE == code)
    sub_shp$GAUL_CODE <- as.character(sub_shp$GAUL_CODE)
    sub_raster <- crop(ref_raster, extent(sub_shp), snap = "out")
    coords <- obtain_coords_given_code(sub_shp, sub_raster, disaggregation_factor, auto_disaggregate, code)


    if (nrow(coords) != 0) {
      coords_sp <- SpatialPoints(coords, crs(sub_shp))
      ag_ages <- unique(shape_table[shapefile == shape & location_code == code]$z_ag)
      for (ag_age in ag_ages) {
        if (is.na(ag_age)) {
          years <- shape_table[shapefile == shape & location_code == code & is.na(z_ag)]$year
          ages_needed <- NA
        } else {
          years <- shape_table[shapefile == shape & location_code == code & z_ag == ag_age]$year
          ages_needed <- eval(parse(text = as.character(ag_age)))
        }

        for (year_needed in years) {
          # Get population values
          worldpop_cov_rasters <- lapply(ages_needed, function(age) {
            suppressMessages(load_worldpop_covariate(sub_raster,
              covariate = "worldpop",
              pop_measure = ifelse(is.na(age), "total", paste0("a", z_map$value[z_map$z == age], "t")),
              pop_release = pop_release,
              start_year = year_needed,
              end_year = year_needed,
              interval = as.numeric(interval_mo)
            )$worldpop)
          })
          worldpop_stack <- stack(worldpop_cov_rasters)
          pop_vals <- raster::extract(worldpop_stack, coords_sp)
          pop_vals <- data.frame(pop_vals)

          # If all population values are NA and/or 0 for any age group or total
          # Note: the cellnumber column should not have any 0s or NAs
          if (any(apply(is.na(pop_vals) | pop_vals == 0, 2, all))) {
            stop(paste0(
              "There are no population values at any of the pixel centroids for shape: ",
              shape, " and location code: ", code
            ))
            # If some population values are NA for any age group
          } else if (any(apply(is.na(pop_vals), 2, any))) {
            message(paste0(
              "There is at least one pixel centroid for shape: ",
              shape, " and location code: ", code, " that is missing population. Setting to 0."
            ))
            pop_vals[is.na(pop_vals)] <- 0 # assume there is no one living there
          }

          pop_vals <- as.matrix(pop_vals / sum(pop_vals)) # scale so that it sums to 1

          age_vals <- rep(ages_needed, each = nrow(pop_vals))
          pop_vals <- unlist(c(pop_vals)) # change to vector

          # add in population values as weights
          if (is.na(ag_age)) {
            shape_table[
              shapefile == shape & location_code == code & is.na(z_ag) & year == year_needed,
              c("coordinates.long", "coordinates.lat", "agg_weight", "z") :=
                list(
                  list(rep(coords[, 1], length(ages_needed))),
                  list(rep(coords[, 2], length(ages_needed))),
                  list(pop_vals), list(age_vals)
                )
            ]
          } else {
            shape_table[
              shapefile == shape & location_code == code & z_ag == ag_age & year == year_needed,
              c("coordinates.long", "coordinates.lat", "agg_weight", "z") :=
                list(
                  list(rep(coords[, 1], length(ages_needed))),
                  list(rep(coords[, 2], length(ages_needed))),
                  list(pop_vals), list(age_vals)
                )
            ]
          }
        }
      }
    } else { # no coordinates found
      shape_table[
        (shapefile == shape) & (location_code == code),
        c("coordinates.long", "coordinates.lat", "agg_weight", "z") :=
          list(list(NA), list(NA), list(NA), list(NA))
      ]
    }
  }
  return(shape_table)
}
