#' @title Format covariate string
#' @description Parse covariates if it is in a formula format
#' @param covariates Covariates
#' @return Vector of names of covariates
#' @export
#'
#' @concept stacking
format_covariates <- function(covariates) {
  if (length(covariates) == 1) {
    # split back into parts
    covariates <- unlist(tstrsplit(covariates, "\\+"))
  }
  covariates <- trimws(covariates)
  return(covariates)
}
