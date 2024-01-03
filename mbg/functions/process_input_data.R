#' @title Process input data
#'
#' @description After data has been loaded, this function will take any aggregated
#' data, determine components (e.g., points in the polygon or age groups within
#' the larger age aggregation) and the associated weights. Additionally, adds
#' the pixel_id (the pixel that the long/lat falls into).
#'
#' @param df The data frame load_input_data().
#' @param pop_release The population measure release to use.
#' @param interval_mo The number of months between time steps.
#' @param modeling_shapefile_version String identifying version of of shapefile used in modeling.
#' @param poly_ag Logical that indicates whether the new point-polygon method
#' should be used.
#' @param zcol String that indicates the name of the z column if present.
#' @param zcol_ag String that indicates the name of the aggregated z column if
#' present.
#' @param zcol_ag_id String that indicates the name of the aggregated z column
#' id if present.
#' @param z_map Dataframe with mapping from z values to worldpop age groups.
#' @param z_ag_mat Matrix that contains user-supplied aggregation weights for
#' aggregated z data (those that are specified in zcol_ag_id).
#' @param shapefile_col String indicating name of shapefile column.
#' @param location_code_col String indicating name of location code column.
#' @param fast_shapefiles Logical. Should fast shapefile loading be used?
#' @param disaggregation_factor Integer. The number of cells the raster corresponding
#' to a polygon should be broken into. This can be useful if the polygon is very
#' small in area.
#' @param auto_disaggregate Logical. Should a disaggregation_factor of 5 (i.e. breaking
#' a 5km x 5km pixel to 1km x 1km) be applied if no pixel centroids fall inside the
#' polygon?
#'
#' @return A data frame with the non aggregated data plus for the aggregated data,
#' multiple rows "disaggregated". The original columns will be present plus the
#' following additional columns: row_id, agg_weight, first_entry, and pixel_id.
#' @export
process_input_data <- function(df,
                               pop_release,
                               interval_mo,
                               modeling_shapefile_version,
                               poly_ag = FALSE,
                               zcol = "z_column_default_blank",
                               zcol_ag = NULL,
                               zcol_ag_id = NULL,
                               z_map = NULL,
                               z_ag_mat = NULL,
                               shapefile_col = "shapefile",
                               location_code_col = "location_code",
                               fast_shapefiles = T,
                               disaggregation_factor = 1,
                               auto_disaggregate = T) {
  ## Inputs: df: df with i rows prior to polygon resampling. Should have
  ##             shapefile, location code, zcol (if used), zcol_ag (if used),
  ##             year, other data columns
  ## Outputs: uncollapsed_df: df that has the following columns:
  ##            - ID (i unique values) corresponding to which row of the
  ##              collapsed data the uncollapsed data corresponds to
  ##              NOTE: for aggregated data, there will be multiple rows in
  ##              the uncollapsed data that correspond to the same original
  ##              data
  ##            - long/lat: for point data this is what was originally provided
  ##              For polygon data, this is the centroids of pixels that fall
  ##              within the polygon
  ##            - z (only if original df has z): For aggregated z data, this
  ##              is broken down into the relevant zs that make up the aggregation
  ##            - t: orignal df time
  ##            - agg_weight: 1 for non z-aggregated point data. For polygon
  ##              data this is based on (total) population at the given time.
  ##              For z aggregated this is based on population at given time
  ##              and age group at location OR can be user-specified. For
  ##              polygon and z-aggregated this is based on population and age
  ##              group at the given time


  # input validations
  if (!is.numeric(disaggregation_factor) || disaggregation_factor < 1) {
    stop("disaggregation factor must be a numeric value greater than or equal to 1")
  }
  if (!shapefile_col %in% names(df) & poly_ag) {
    stop("shapefile_col: ", shapefile_col, " is not a column name in df")
  }
  if (!location_code_col %in% names(df) & poly_ag) {
    stop("location_code_col: ", location_code_col, " is not a column name in df")
  }

  # Check if any polygon data has latitude information
  if ("latitude" %in% names(df) & poly_ag) {
    if (any(!is.na(df$latitude[df$point == 0]))) {
      stop("Some of the polygon data already has longitude and latitude.")
    }
  }

  df <- data.table(df)

  # get ordering so that df can be reordered at end
  df$row_id <- 1:nrow(df)

  # pull pop mask for reference raster
  mask_folder <- fp_list$map_root
  ref_raster <- raster("<<<< FILEPATH REDACTED >>>>")
  ref_raster <- setValues(ref_raster, rep(1, length(ref_raster)))

  # rename zcol so it doesn't get overwritten when we add in disaggregated zs
  if (zcol != "z_column_default_blank") {
    setnames(df, zcol, "z_tmp")
  }

  # rename zcol_ag for easy referencing
  if (!is.null(zcol_ag)) {
    setnames(df, zcol_ag, "z_ag")
  } else {
    df$z_ag <- NA
  }
  # rename zcol_ag_id for easy referencing
  if (!is.null(zcol_ag_id)) {
    setnames(df, zcol_ag_id, "z_ag_id")
  } else {
    df$z_ag_id <- NA
  }

  if (poly_ag) { # when using new polygon aggregation method
    setnames(df, c(shapefile_col, location_code_col), c("shapefile", "location_code"))

    # save classes for type conversion at the end
    shapefile_class <- class(df$shapefile)
    location_code_class <- class(df$location_code)

    # shapefile and location code need to be characters to work
    df <- cast_column(df, "shapefile", "character")
    df <- cast_column(df, "location_code", "character")

    ## Step 1. Process the polygon data
    #          (includes polygon-single z and polygon-aggregated z)
    shape_table <- unique(df[, .(shapefile, location_code, point, z_ag, year)])
    shape_table[shapefile == "", shapefile := NA]

    # Verify there are shapefiles and location codes for all polygon data
    if (any(is.na(shape_table[point == 0, .(shapefile, location_code)]))) {
      stop("There is missing shapefile and/or location code information for some of the polygon data")
    }

    # Subset to just polygon data
    shape_table <- shape_table[!is.na(shapefile) & !is.na(location_code)]
    setorder(shape_table, shapefile, location_code)

    if (nrow(shape_table) > 0) {
      message("Working on polygon data")

      for (shape in unique(shape_table$shapefile)) {
        message(paste0("Working on shape: ", shape))
        shape_table <- process_single_shapefile(
          shape, shape_table, ref_raster,
          fast_shapefiles, pop_release,
          interval_mo, z_map,
          disaggregation_factor,
          auto_disaggregate
        )
      }
      # add the new data back onto the datatable
      df <- merge(df, shape_table, by = c("shapefile", "location_code", "point", "z_ag", "year"), all.x = T)
    }
  }


  ## Step 2. Age only data
  age_table <- unique(df[, .(country, latitude, longitude, z_ag, z_ag_id, year)])
  age_table <- age_table[!is.na(z_ag) & !is.na(latitude)]

  if (nrow(age_table) > 0) {
    message("Working on age aggregated data")

    if (!is.null(z_ag_mat)) {
      age_table_mat <- age_table[!is.na(z_ag_id), ] # only rows that will use z_ag_mat
      age_table <- age_table[is.na(z_ag_id), ] # rows that will not use z_ag_mat
      for (i in 1:length(age_table_mat$z_ag)) {
        id <- age_table_mat$z_ag_id[i]
        ages_needed <- eval(parse(text = as.character(age_table_mat$z_ag[i])))
        weight.tmp <- as.numeric(z_ag_mat[id, ages_needed])
        weight.tmp <- weight.tmp / sum(weight.tmp) # ensure sum to 1
        age_vals <- ages_needed
        age_table_mat[i, c("agg_weight", "z") := list(list(weight.tmp), list(age_vals))]
      }
      # add the new data back onto the datatable
      df[age_table_mat,
        on = c("country", "latitude", "longitude", "z_ag", "z_ag_id", "year"),
        c("z", "agg_weight") := list(i.z, i.agg_weight)
      ]
    }
    if (nrow(age_table) > 0) { # will need to use worldpop data
      age_table <- unique(age_table[, .(country, latitude, longitude, z_ag, year)])
      for (reg in unique(age_table$country)) {
        adm0_list <- get_adm0_codes(reg, shapefile_version = modeling_shapefile_version)
        simple_polygon_list <- load_simple_polygon(
          gaul_list = adm0_list, buffer = 1, tolerance = 0.4,
          shapefile_version = modeling_shapefile_version
        )
        simple_polygon <- simple_polygon_list[[2]]
        age_gps <- unique(age_table[country == reg]$z_ag)
        for (ages in age_gps) {
          age_table <- process_single_z_gp(ages, reg, age_table, z_map,
            simple_polygon,
            pop_release, interval_mo,
            type = "age"
          )
        }
      }
      # add the new data back onto the datatable
      df[age_table,
        on = c("country", "latitude", "longitude", "z_ag", "year"),
        c("z", "agg_weight") := list(i.z, i.agg_weight)
      ]
    }
  }


  ## reorder df
  df <- df[order(row_id), ]


  cols_include <- !names(df) %in% c(
    "latitude", "longitude", "z_tmp", "z_ag",
    "z_ag_id", "coordinates.long", "coordinates.lat",
    "agg_weight", "z"
  )
  ## Step 3. Create uncollapsed df
  message("Finalizing...")
  uncollapsed_df <- lapply(1:nrow(df), function(i) {
    df_row <- df[i, ]

    agg_weight <- df_row$agg_weight[[1]]
    if (is.null(agg_weight[1])) { # point data
      agg_weight <- 1
    }

    if (all(is.na(agg_weight))) { # if the population were all 0s --> weights are NaN
      message(paste0("Observation ", i, " had NA weights... dropping..."))
      to_drop <- T
    } else {
      to_drop <- F
    }

    # remove entries that have agg_weight of 0 since they do not contribute
    # agg weights with NAs will cause problems here but this should only happen
    #   if all are NAs and therefore will be dropped due to code above
    idx_nonzero_aw <- agg_weight > 0 | is.na(agg_weight)
    agg_weight <- agg_weight[idx_nonzero_aw]

    if (!is.na(df_row$latitude)) {
      latitude <- df_row$latitude
      longitude <- df_row$longitude
    } else {
      latitude <- df_row$coordinates.lat[[1]][idx_nonzero_aw]
      longitude <- df_row$coordinates.long[[1]][idx_nonzero_aw]
    }
    if (!is.na(df_row$z_ag)) {
      z <- df_row$z[[1]][idx_nonzero_aw]
    } else {
      if (zcol != "z_column_default_blank") {
        z <- df_row$z_tmp
      } else {
        z <- NA
      }
    }

    first_entry <- c(1, rep(0, sum(idx_nonzero_aw) - 1)) # useful for not plotting duplicates
    cbind(
      df_row[, ..cols_include], longitude, latitude,
      z, agg_weight, first_entry, to_drop
    )
  })

  uncollapsed_df <- do.call("rbind", uncollapsed_df)

  uncollapsed_df <- uncollapsed_df[!uncollapsed_df$to_drop, ]

  uncollapsed_df$pixel_id <- raster::extract(ref_raster,
    cbind(
      uncollapsed_df$longitude,
      uncollapsed_df$latitude
    ),
    cellnumbers = T
  )[, "cells"]

  if (zcol != "z_column_default_blank") {
    setnames(uncollapsed_df, "z", zcol) # return to original name
  }

  return(data.table(uncollapsed_df))
}
