#' @title Get GBD sex_id for the given WorldPop measure
#' @description This function will return the sex_id by parsing the final character of standard worldpop measures
#' which end in either "m", "f", or "t", representing male, female, or both sexes. Also accepts "total" and "wocba" measures.
#'
#' @param pop_measure The WorldPop measure, e.g. \code{a0004t}.
#'
#' @return The \code{sex_id} for the associated \code{pop_measure} input.
#' @export
get_sex_id_from_worldpop <- function(pop_measure) {

  # define sex group for worldpop measures that don't match the standard format
  sex_dict <- list(
    "total" = 3,
    "wocba" = 2
  )

  if (pop_measure %in% names(sex_dict)) {
    return(unname(unlist(sex_dict[pop_measure])))
  }

  if (endsWith(pop_measure, "m")) {
    return(1)
  } else if (endsWith(pop_measure, "f")) {
    return(2)
  } else if (endsWith(pop_measure, "t")) {
    return(3)
  } else {
    message("could not identify GBD sex_id from worldpop, using 3 (both sexes) as default")
    return(3)
  }
}
