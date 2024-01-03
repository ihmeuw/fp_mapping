#' @title Get All Rows in a Stratum
#' @description Creates a vector with the rows in the data that correspond to
#' a particular combination of strata
#'
#' @param data full dataset to split into test/train
#' @param strata datafame with one row containing the values for a partiuclar
#' combination of strata (columns are the strata)
#'
#' @return vector of rows in data that have that combination of strata
#' @export
#'
#' @concept holdout
get_strat_rows <- function(data = data,
                           strata,
                           ...) {

  ## this function returns all the rows in a strata

  if (length(strata) < 1) {
    message("Need to identify some strata!")
    stop()
  }

  ## loop through and intersect all rows we want
  good_rows <- data[, colnames(strata)[1]] == strata[[1]]

  if (length(strata) > 1) {
    for (c in 2:ncol(strata)) {
      tmp_rows <- data[, colnames(strata)[c]] == strata[[c]]
      good_rows <- good_rows * tmp_rows ## intersect them
    }
  }

  good_rows <- which(good_rows == 1)

  return(good_rows)
}
