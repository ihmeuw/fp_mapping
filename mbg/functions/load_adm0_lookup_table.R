## GET ADMIN CODES AND RELATED FUNCTIONS
#' @title Load the GAUL lookup table
#'
#' @description Loads the most recent version of the lookup table that links
#' ADM0 codes with other identifiers such as GBD location IDs, MBG modeling
#' regions, and ISO codes, among others.
#'
#' @return Returns a data.table of the ADM0 lookup table.
#'
#' @export
#'
#' @concept prep
load_adm0_lookup_table <- function() {
  lookup_table <- load_stage_list()
  # Set ISO codes as lowercase for easy lookup
  lookup_table$iso3 <- tolower(lookup_table$iso3)
  return(lookup_table)
}
