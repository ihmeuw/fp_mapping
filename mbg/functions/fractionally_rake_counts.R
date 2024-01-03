#
# In using 5x5 pixels, some pixels are split between several admin
# units. Fractional raking is a way of raking counts which takes
# this into consideration. For any pixel that is split between
# admin units, the fractional area of that admin unit within the
# pixel is multiplied by the count so that counts are properly
# attributed to their respective admin units.
#
# The key to fractional raking is a [link table] which has a row
# for each pixel-admin unit, and includes the gaul codes for admins
# 0-2, as well as the fractional area of the admin unit in that pixel
# (a pixel split between two admin 2 units would have two rows in the
# link table). There is a global link table generated from the stage 1
# and stage 2 simple raster, where every pixel gets a unique id.
# Practically, when using a link table, the global link table and raster
# are subsetted based on the simple raster of the region being raked
# using get_link_table()
#
# Build_link_polygon() and build_link_table() are functions used to
# create the link table, which will only need to be used when
# there is a change in the shapefile used to generate simple rasters.
#
# Many of the functions require the parameter overs, which is a vector
# of the column names of the draws, typically named V1 to Vndraws
# when the cell pred is converted to a data.frame or data.table.
#
#' @title Fractionally Rake Counts
#'
#' @description A function used to rake mbg count ouputs to GBD estimates using a fractional methodolgy. Counts in each pixel are assigned to admin units and split by area fraction for pixels with multiple admin units. Counts are then summed to ADM0 (except for those specified as subnational) and raking factors for those admin units are calculated. From there, the raking factors are applied to the linked cell pred, which is then processed in 2 ways: aggregated up to adm0/1/2, and deduplicated (the linked cell pred has 1 row for each pixel-adm unit-year which is reduced to 1 row per each pixel-year). N.B. If you rake subnationally and are missing raking targets for one or more subnationals, the final sum of counts will not match GBD at ADM0.
#'
#' @param count_cell_pred Cell pred matrix object with counts to be raked.
#' @param rake_to Df with `name`, `year`, and `mean` columns. Values in name must be gbd loc_ids.
#' @param reg character string - Region used to produce cell pred object.
#' @param year_list integer vector - vector of years
#' @param rake_subnational  If true, rakes to ADM1 units not in `countries_not_to_subnat_rake`
#' @param countries_not_to_subnat_rake default set in config, character vector of subnational country iso3 codes NOT to rake to
#' @param countries_not_to_rake default c("GUF", "ESH), french guiana and western sahara. Any country iso3s passed here will have raking factor set to 1.
#' @param simple_raster default `NULL`, option to pass in simple raster to function if it's been loaded already.
#' @param modeling_shapefile_version default `current`, string identifying version of of shapefile used in modeling
#' @param raking_shapefile_version default `current`, string identifying version of of shapefile to use in raking
#' @param raster_agg_factor int. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.

fractionally_rake_counts <- function(count_cell_pred,
                                     rake_to,
                                     reg,
                                     year_list,
                                     rake_subnational,
                                     countries_not_to_subnat_rake,
                                     countries_not_to_rake = c("ESH+GUF"),
                                     simple_raster = NULL,
                                     modeling_shapefile_version = "current",
                                     raking_shapefile_version = "current",
                                     raster_agg_factor) {
  # get draw column names
  ndraws <- ncol(count_cell_pred)
  overs <- paste0("V", 1:ndraws)

  ## load objects used in modeling
  if (is.null(simple_raster)) {
    ## get simple polygon and simple raster used to produce cell pred
    message("Loading Simple Polygon")
    simple_polygon <- load_simple_polygon(
      gaul_list = get_adm0_codes(reg,
        shapefile_version = modeling_shapefile_version
      ),
      buffer = 0.4,
      shapefile_version = modeling_shapefile_version
    )
    subset_shape <- simple_polygon[["subset_shape"]]

    message("Loading Simple Raster\n")
    simple_raster <- build_simple_raster(
      extent_template = subset_shape,
      shapefile_version = modeling_shapefile_version,
      region = reg,
      raster_agg_factor = raster_agg_factor
    )
  } else {
    message("Using Supplied Simple Raster")
  }

  message("Loading Link Table and Prepping Cell Pred for Raking")

  # get link table
  link_table_output <- get_link_table(simple_raster, raking_shapefile_version, raster_agg_factor)

  # format get_link_table outputs and add pixel ids to cell pred
  link <- link_table_output[["link_table"]]
  pixel_id <- link_table_output[["pixel_ids"]]
  pixel_id <- rep(pixel_id, times = length(year_list))
  year <- rep(year_list, each = (nrow(count_cell_pred) / length(year_list)))

  if (length(pixel_id) != nrow(count_cell_pred)) {
    message("Something has gone wrong with the matchup of pixels between the cell pred and link table-")
    message("Length of pixel vector: ", length(pixel_id))
    message("Rows in cell pred: ", nrow(count_cell_pred))
    message("This may be due to a mismatch between the cell pred and simple raster, or pixels in the simple raster not being in the link table.")
    stop()
  }

  id_count_cell <- cbind(count_cell_pred, pixel_id, year)

  # subset link table to countries in region -- fix for border issue with fractional raking
  link <- link[ADM0_CODE %in% unique(simple_raster), ]

  # build connector object between ADM codes and GBD loc ids
  connector <- data.table(get_gbd_locs(
    rake_subnational = rake_subnational,
    reg = reg,
    shapefile_version = raking_shapefile_version
  ))

  if (rake_subnational) {
    connector[, ADM_CODE := ADM0_CODE]
    connector[ADM1_CODE != 0, ADM_CODE := ADM1_CODE]
  }
  connector <- connector[, c("ADM_CODE", "location_id")]

  # add on ADM_CODE to raking factors so everything can be done in GADM codes rather than GBD loc ids
  rake_to <- merge(rake_to, connector, by.x = "name", by.y = "location_id", all.x = T)

  # set ADM_CODE which is used as the adm_level for raking
  # when subnational countries are provided, sets ADM_CODE to the ADM1_CODE
  # if countries_not_to_subnat_rake is set, will set it back to ADM0_CODE if necessary
  link[, ADM_CODE := ADM0_CODE]
  if (rake_subnational) {
    codes_not_to_subnat_rake <- get_adm0_codes(countries_not_to_subnat_rake,
      shapefile_version = raking_shapefile_version
    )
    link[ADM1_CODE %in% connector$ADM_CODE, ADM_CODE := ADM1_CODE]
    link[ADM0_CODE %in% codes_not_to_subnat_rake, ADM_CODE := ADM0_CODE]
  }

  # get raking factors
  message("\nCalculating Raking Factors")
  fractional_rf <- get_fractional_rf(id_count_cell, link, rake_to, reg, overs, raking_shapefile_version)

  if (nrow(fractional_rf[is.na(rf)]) > 0) {
    message("Warning: there are some admin units that are missing raking factors. This is likely due to a lack of raking targets. If these are subnational locations, your counts will not add up at the ADM0 level.")
    message("These locations (GADM code) are missing raking factors: ", paste(unique(fractional_rf[is.na(rf), ADM_CODE]), collapse = ", "))
  }

  # getting unraked draws - merge cell pred and link table
  unraked_linked_cell_pred <- merge(link, id_count_cell, by.x = "ID", by.y = "pixel_id", all.y = T, allow.cartesian = T)
  # multiply counts by area fraction
  unraked_linked_cell_pred[, (overs) := .SD * as.numeric(area_fraction), .SDcols = overs]
  # aggregate unraked draws to adm0/1/2
  unraked_aggregate_count_list <- aggregate_counts(unraked_linked_cell_pred, overs, raked = F)

  # setting rf for non-raking countries
  fractional_rf[ADM0_CODE %in% get_adm0_codes(countries_not_to_rake, shapefile_version = raking_shapefile_version), rf := 1]

  # apply raking factors
  message("\nApplying Raking Factors")
  raked_counts_linked_cell_pred <- apply_fractional_rf(id_count_cell, link, fractional_rf, overs)

  # deduplicate linked cell pred that has multiple rows per pixel
  message("Aggregating...\n")
  deduped_counts_cell_pred <- dedupe_linked_cell_pred(raked_counts_linked_cell_pred, overs)

  # aggregate deaths to Adm 0-2
  raked_aggregate_count_list <- aggregate_counts(raked_counts_linked_cell_pred, overs, raked = T)

  # compile outputs
  output_list <- list()
  output_list[["raked_cell_pred"]] <- deduped_counts_cell_pred
  output_list <- c(output_list, unraked_aggregate_count_list, raked_aggregate_count_list)
  output_list[["raking_factors"]] <- fractional_rf

  return(output_list)
}
