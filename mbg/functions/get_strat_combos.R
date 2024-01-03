#' @title Get All Combinations of Stratifying Columns
#' @description Creates a data frame of all combination of stratifying columns.
#' E.g. if gender and age_bin vectors are specified this function will return a
#' list with all unique combos of age_bin and gender
#'
#' @param data full dataset to split into test/train
#' @param strat_cols vector of columns to stratify by
#'
#' @return data frame containing one row for each combination of strata
#' @export
#'
#' @concept holdout
get_strat_combos <- function(data = data,
                             strat_cols = strat_cols,
                             ...) {

  ## get all unique items from  each column
  unique_list <- list(NULL)
  for (i in 1:length(strat_cols)) {
    unique_list[[i]] <- sort(unique(data[, strat_cols[i]]))
  }

  ## make a dataframe of all combos and return it
  all_combos <- expand.grid(unique_list)
  colnames(all_combos) <- strat_cols
  return(all_combos)
}
