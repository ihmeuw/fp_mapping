#' @title Get GBD population for use in fractional raking
#' @description Get GBD population for use in fractional raking
#'
#' @param pop_measure WorldPop measure
#' @param reg Region
#' @param year_list the years to get population for
#'
#' @return A data.table with \code{c('location_id', 'year_id', 'sex_id', 'run_id', 'age_group_id', 'population')}
#' @note the \code{age_group_id} returned is equal to the value of \code{pop_measure}
#'
#' @export
#'
#' @concept fractional_raking

prep_gbd_pops_for_fraxrake <- function(pop_measure = "a0004t", reg, year_list, sex_id = NULL, ...) {

  # read in arguments from ... into named list format
  arglist <- get_args_from_ellipsis(...)

  if (class(year_list) == "character") year_list <- eval(parse(text = year_list))

  ## Get age group ID from pop_measure
  age_GBD <- get_age_group_from_worldpop(pop_measure)

  ## Get location ID from GBD
  loc_GBD <- unique(c(get_gbd_locs(reg)$location_id, get_gbd_locs(reg, rake_subnational = FALSE)$location_id))

  if (is.null(sex_id)) {
    sex_GBD <- get_sex_id_from_worldpop(pop_measure)
    message(paste0("Inferred sex_id ", sex_GBD, " from pop_measure ", pop_measure, " for pulling gbd population"))
    message("If you would like to use a different sex_id, please use the sex_id arg in pre_gbd_pops_for_fraxrake")
  } else {
    sex_GBD <- sex_id
  }

  source(paste0(CC_ENV_DIR, "/get_population.R"))
  arglist$age_group_id = age_GBD
  arglist$location_id = loc_GBD
  arglist$sex_id = sex_GBD
  arglist$year_id = year_list

  gbd_pops <- do.call(get_population, arglist)

  ## Reduce to a single age group:
  gbd_pops <- gbd_pops[, .(population = sum(population)), by = c("location_id", "year_id", "sex_id", "run_id")]
  gbd_pops[, age_group_id := pop_measure]

  return(gbd_pops)
}
