#' @title Update covariate layers
#' @description function to update the cov layers option. Wrapper around `append`.
#'
#' @param original_layers Original covariate layers
#' @param new_layers Covariate layers to add
#'
#' @return updated cov layers
#' @export
#'
#' @concept categorical_variable
update_cov_layers <- function(original_layers, new_layers) {
  return(append(original_layers, new_layers))
}
