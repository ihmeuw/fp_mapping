#' @title Load, Combine Regions, and Save Post-Estimation Objects
#'
#' @description Run after Raking/Aggregation to combine objects across modeling regions. Function specifically combines rasters and raking factors.
#'
#' @param regions default `strata`, character vector of modelling regions to combine
#' @param summstats default c("mean", "cirange", "upper", "lower"). Which summary rasters to combine
#' @param raked default c("raked", "unraked"). Combine raked/unraked rasters?
#' @param ages: single value or vector of ages
#' @param holdouts: vector of holdouts used, e.g. just 0 or c(1,2,3,4,5,0)
#' @param rf_table default TRUE. Combine raking factors?
#' @param run_summ defualt TRUE. Make a run summary graph? See \code{\link{graph_run_summary}}
#' @param indic default `indicator`. Indicator for model run
#' @param ig default `indicator_group`. Indicator_group for model run
#' @param run_date default `run_date`. run date for model run
#' @param sdir default `sharedir`
#' @param proj default FALSE. If T, Combine projected rasters
#' @param proj_folder default NULL. Unused.
#'
#' @return None
#'
#' @export
#'
#' @concept post_estimation
post_load_combine_save <- function(regions = strata,
                                   summstats = c("mean", "cirange", "upper", "lower"),
                                   raked = c("raked", "unraked"),
                                   ages = 0,
                                   holdouts = 0,
                                   rf_table = TRUE,
                                   run_summ = TRUE,
                                   indic = indicator,
                                   ig = indicator_group,
                                   run_date = run_date,
                                   sdir = sharedir,
                                   proj = FALSE,
                                   proj_folder = NULL) {
  message(paste0("indic: ", indic))
  message(paste0("ig: ", ig))

  rake_addin <- character()
  if ("unraked" %in% raked) {
    lookup_dir <- "<<<< FILEPATH REDACTED >>>>"
    ur <- length(grep(paste0(indic, ".*unraked.*raster.tif"), list.files(lookup_dir)))
    if (proj) ur <- length(grep(paste0(indic, ".*unraked_PROJ.*raster_PROJ.tif"), list.files(lookup_dir)))
    if (ur > 0) rake_addin <- c(rake_addin, unraked = "_unraked")
    if (ur == 0) rake_addin <- c(rake_addin, unraked = "")
  }

  if ("raked" %in% raked) {
    rake_addin <- c(rake_addin, raked = "_raked")
  }
  
  # If there is only one region, there is no need for combining. Copy the reg raster/rf objects
  # to match the respective combined region outputs
  if(length(regions) == 1){
    for (age in ages) {
      for (holdout in holdouts) {
        if(age != 0 | holdout != 0) {
          path_addin <- sprintf("_bin%s_%s", age, holdout)
        } else {
          path_addin <- ""
        }
        for (ss in summstats) {
          for (rake in rake_addin) {
            reg_raster_path <- 
              file.path(
                "<<<< FILEPATH REDACTED >>>>"
              )
            copy_raster_path <- 
              file.path(
                "<<<< FILEPATH REDACTED >>>>"
              )
            file.copy(reg_raster_path, copy_raster_path)
          }
        }
        reg_rf_path <- 
          file.path(
            "<<<< FILEPATH REDACTED >>>>"
          )
        copy_rf_path <-
          file.path(
            "<<<< FILEPATH REDACTED >>>>"
          )
        file.copy(reg_rf_path, copy_rf_path)
      }
    }
    return()
  }
  
  # loop through and combine all rasters
  message("\nCombining rasters...")
  for (age in ages) {
    for (holdout in holdouts) {
      if(age != 0 | holdout != 0) {
        path_addin <- sprintf("_bin%s_%s", age, holdout)
      } else {
        path_addin <- ""
      }
      for (rake in rake_addin) {
        message(names(rake_addin)[which(rake_addin == rake)])
        rr <- rake
        for (ss in summstats) {
          message(paste0("  ", ss))
          rlist <- list()
          for (reg in regions) {
            message(paste0("    ", reg, " age: ", age, " holdout: ", holdout))
            rlist[[reg]] <-
              brick(ifelse(proj,
                file.path(
                  "<<<< FILEPATH REDACTED >>>>"
                ),
                file.path(
                  "<<<< FILEPATH REDACTED >>>>"
                )
              ))
          }
          if (length(rlist) > 1) rlist <- do.call(raster::merge, unname(rlist)) else rlist <- rlist[[1]]
          if (ss == "cirange") ssname <- "range" else ssname <- ss # naming convention
          save_post_est(
            rlist, "raster",
            ifelse(!proj,
              paste0(ssname, rr, "_raster", path_addin),
              paste0(ssname, rr, "_raster_PROJ")
            ),
            indic,
            ig,
            run_date
          )
        }
      }
    }
  }

  # do rf also
  if (rf_table) {
    message("RF table")
    for (age in ages) {
      for (holdout in holdouts) {
        if(age != 0 | holdout != 0) {
          path_addin <- sprintf("_bin%s_%s", age, holdout)
        } else {
          path_addin <- ""
        }
        rflist <- list()
        for (reg in regions) {
          rflist[[reg]] <-
            if (proj) {
              read.csv(file.path(
                "<<<< FILEPATH REDACTED >>>>"
              ))
            } else {
              read.csv(file.path(
                "<<<< FILEPATH REDACTED >>>>"
              ))
            }
        }
        if (!proj) {
          save_post_est(do.call(rbind.fill, rflist), "csv", sprintf("rf%s", path_addin), 
                        indic, ig, run_date)
        } else {
          save_post_est(do.call(rbind.fill, rflist), "csv", "rf_PROJ", indic, ig, run_date)
        }
      }
    }
  }

  # make a run summary graph
  if (run_summ) {
    graph_run_summary(
      run_date = run_date,
      indicator_group = ig,
      indicator = indic
    )
  }
}
