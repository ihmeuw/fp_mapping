#' @title Prep Objects for Postestimation
#'
#' @description Save out a temporary .RData file that gets read in during postestimation. Primarily used to pass through GBD raking targets from the model launch script to the postestimation script. File gets saved in the model run folder in `/temp_post_est/post_est_temp_objs.RData`.
#'
#' @param indicator indicator for model run
#' @param indicator_group indicator_group for model run
#' @param run_date run_date for model run
#' @param save_objs vector of names of objects to save e.g. c("gbd"). Can pass any objects that exist in the global environment to this arg. Recommended not to pass through config arguments, because it can overwrite values accessed through the config.
#'
#' @return None
#' @export
#'
#' @concept post_estimation
prep_postest <- function(indicator,
                         indicator_group,
                         run_date,
                         save_objs) {

  # Save a list of objects in a standard location for parallel scripts to pull from
  main_dir <- "<<<< FILEPATH REDACTED >>>>"
  temp_dir <- "<<<< FILEPATH REDACTED >>>>"
  temp_file <- paste0(temp_dir, "post_est_temp_objs.RData")
  dir.create(temp_dir, showWarnings = F)
  save(list = save_objs, file = temp_file)
}
