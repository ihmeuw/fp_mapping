#' @title Cleanup After Postestimation
#'
#' @description Remove `temp_post_est` directory and region rasters after \code{\link{post_load_combine_save}} runs.
#'
#' @param indicator indicator of model run
#' @param indicator_group indicator_group of model run
#' @param run_date run_date of model run
#' @param strata if `delete_region_rasters == T`, a vector of region names to delete.
#' @param delete_region_rasters default F. If T, remove region rasters.
#'
#' @return None
#'
#' @export
#'
#' @concept post_estimation
clean_after_postest <- function(indicator,
                                indicator_group,
                                run_date,
                                strata,
                                delete_region_rasters = F) {

  # Delete intermediate files that we no longer need
  main_dir <- "<<<< FILEPATH REDACTED >>>>"
  temp_dir <- "<<<< FILEPATH REDACTED >>>>"

  # Deleting - be careful!
  unlink(temp_dir, recursive = T)

  # If desired, insert code here to delete other temporary objects (eg. region-specific rasters)
  grep_string <- paste0(indicator, "_(", paste(strata, collapse = "|"), ").*_raster.tif")
  region_rasters <- grep(grep_string, list.files(main_dir), value = T) %>%
    paste0(main_dir, .)
  if (delete_region_rasters == T) unlink(region_rasters)
}
