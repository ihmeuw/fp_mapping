#' @title Check admin preds
#' @description Function to check whether all of the files for summarizing and combining admin files
#' are available. These files are created by `post_load_combine_save`. They are region-specific
#' (one file per region), either raked/unraked, and either counts or non-counts.
#'
#' The function returns a boolean vector, either F or c(F, T), depending on the existence of counts
#' files. This is necessary because there is no easy way to tell from the config whether or not
#' these files exist and need to be aggregated. If there are any missing files given the expected,
#' the function will error with a list of the missing files.
#'
#' @param regions vector of regions modeled
#' @param raked Raked (T), unraked (F), or both (`c(T,F)`)? Helps determine which files to look for.
#' @param indicator indicator
#' @param indicator_group indicator_group
#' @param run_date model run date
#'
#' @return boolean vector (F) if no counts, c(F,T) if counts exist
#' @export
#'
#' @concept post_estimation
check_admin_preds <- function(regions,
                              raked,
                              indicator,
                              indicator_group,
                              run_date) {
  dir_to_search <- "<<<< FILEPATH REDACTED >>>>"

  # Convert raked to character
  rr <- character()
  if (T %in% raked) rr <- c(rr, "raked")
  if (F %in% raked) rr <- c(rr, "unraked")

  # get a list of all of the expected non-count admin draws files
  files_to_check <- "<<<< FILEPATH REDACTED >>>>"

  file_exists <- file.exists(files_to_check)

  if (!all(file_exists)) {
    stop(paste0("The following admin draws files were expected to exist based on regions and rake status but were not found: ", paste0(files_to_check[!file_exists], collapse = ", ")))
  }

  # get a list of all of the expected counts files. There is no easy way to verify whether these
  # files will exist through the config alone
  counts_to_check <- "<<<< FILEPATH REDACTED >>>>"

  count_exists <- file.exists(counts_to_check)

  # count status is used in the summarizing and combine functions for admin preds
  # if counts exists, both counts and non-counts (can be rates from frax rake or other from nonfrax)
  # will be aggregated. If counts don't exist, only aggregate the non-counts
  if (all(count_exists)) {
    count_status <- c(F, T)
  } else if (any(count_exists)) {
    stop(paste0("The following admin count draws files were expected to exist based on regions and rake status but were not found: ", paste0(counts_to_check[!count_exists], collapse = ", ")))
  } else {
    count_status <- c(F)
  }

  return(count_status)
}
