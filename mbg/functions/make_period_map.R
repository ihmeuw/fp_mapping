#' @title Make map of period indices
#'
#' @description Make map of period indices to run any time periods in your data (like annual instead of 5-year). Each period passed through to `modeling_periods` gets a corresponding `period_id` in the output df.
#' 
#' @param modeling_periods a numeric vector of years 
#'
#' @return a data.table with columns `data_period` and `period_id` mapping each year passed to the function to a unique id. 
#' 
#' @examples
#' \dontrun{
#' 
#' make_period_map(c(2000, 2005, 2010, 2015))
#'     
#'     data_period period_id
#' 1:        2000         1
#' 2:        2005         2
#' 3:        2010         3
#' 4:        2015         4
#' 
#' }
#' @export
#'
#' @concept prep
make_period_map <- function(modeling_periods) {
  ## Make map of period indices to run any time periods in your data (like annual instead of 5-year)
  data_period <- sort(modeling_periods)
  period_ids <- seq(data_period)
  period_map <- as.data.table(data_period)
  period_map <- period_map[, period_id := period_ids]
  return(period_map)
}
