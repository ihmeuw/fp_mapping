#' @title Set up log location
#'
#' @description Determines error/output file path and optionally ensures dirs exist.
#'
#' @param log_location the location of the log files 
#'
#' @param user the name of the user.
#'
#' @param indicator the indicator.
#'
#' @param run_date string run date in YYYY_MM_DD_HH_MM_SS format.
#'
#' @param make_dirs whether to make the directories. 
#'
#' @param combine_logs whether or not to combine the output and error logs. 
#'
#' @export
#'
#' @concept submission
setup_log_location <- function(log_location, user, indicator, indicator_group, run_date, make_dirs = TRUE, combine_logs = T) {
  if (log_location == "sgeoutput") {
    log_root <- get_sge_output_dir(user)
  } else if (log_location == "sharedir") {
    log_root <- get_model_output_dir(indicator_group, indicator, run_date)
  } else {
    log_root <- log_location
  }
  output <- path_join(log_root, "output")
  err <- ifelse(combine_logs, "combine_logs", path_join(log_root, "errors"))
  if (make_dirs) {
    dir.create(output, showWarnings = FALSE)
    if (!combine_logs) {
      dir.create(err, showWarnings = FALSE)
    }
  }
  
  # slurm needs to have job names specified directly
  # %x.o%j means <job_name>.o<job_id>
  output <- paste0(output, "/%x.o%j")
  if (err != "combine_logs") {
    err <- paste0(err, "/%x.e%j")
  }
  
  return(c(output, err))
}
