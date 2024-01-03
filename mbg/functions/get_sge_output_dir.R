#' @title SGE output directory
#'
#' @description return the directory where sgeoutput is usually kept.
#'
#' @param user the name of the user.
#'
#' @return the root directory for sge error/output logs.
#'
#' @export
#'
#' @concept submission
get_sge_output_dir <- function(user) {
  path_join(SGE_OUTPUT_ROOT, user)
}
