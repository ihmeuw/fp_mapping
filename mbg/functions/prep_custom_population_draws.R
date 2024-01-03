#' @title Prep custom populations
#' @description A function that takes in list of paths to cell pred objects that will be used in
#' a custom population calculation, prepares them, then runs the custom population function.
#' The preparation of these cell preds mirrors the main cell pred in `fractional_rake_rates.R`
#' and the custom cell pred objects are expanded via link table to match.
#'
#' @param custom_population_function a function for calculating the pop object. The first argument
#' to the function must be `pop` and the following arguments the objects used in the function.
#' These arguments must be in the same order as the objects in the custom_population_list. The
#' result of this function must be a matrix of the same dimensions as the main cell pred object.
#' @param custom_population_list a named list of filepaths to cell_pred objects that will be used
#' by the `custom_population_function`. These can be RData, RDS, or csvs.
#' @param cell_pred the main cell pred, passed through from `fractional_rake_rates.R`
#'
#' @note **The remaining params are context passed through from `fractional_rake_rates.R`**
#'
#' @param cell_ids
#' @param pixel_id
#' @param covdt
#' @param link
#' @param ndraws
#' @param sharedir
#' @param custom_output_folder
#' @param stratum
#' @param indicator
#' @param run_date
#'
#' @return The cell pred object with custom population draws as columns named P1, P2, etc.
#' to mirror draws V1, V2.
#'
#' @export
#'
#' @concept fractional_raking
prep_custom_population_draws <- function(custom_population_function,
                                         custom_population_list,
                                         cell_pred,
                                         cell_ids,
                                         pixel_id,
                                         covdt,
                                         link,
                                         ndraws,
                                         sharedir,
                                         custom_output_folder,
                                         stratum,
                                         indicator,
                                         run_date) {

  message("loading custom population objects")
  # load in custom population list objects into a named list
  loaded_list <- lapply(seq_along(custom_population_list),
                        function(i) safe_load_custom_population(names(custom_population_list)[[i]],
                                                                custom_population_list[[i]]))

  # running seq_along in lapply above removes names from list, adding them back in
  names(loaded_list) <- names(custom_population_list)

  message("prepping custom population objects")
  # Add on metadata to objects
  loaded_list <- lapply(loaded_list,
                        function(x) prep_cell_pred(cell_pred = x,
                                                   cell_ids = cell_ids,
                                                   pixel_id = pixel_id,
                                                   covdt = covdt))

  # merge link table on to expand
  loaded_list <- lapply(loaded_list,
                        function(x) merge(link, x,
                                          by.x = "ID",
                                          by.y = "cell_id",
                                          allow.cartesian = TRUE))

  # convert to matrix to prepare for custom function call
  overs <- paste0("V", 1:ndraws)
  loaded_list <- lapply(loaded_list,
                        function(x) as.matrix(x[, overs, with = F]))

  raked_gbd_pop <- cell_pred[["pop_raked"]]

  # this will load in the list into the function environment, assigning using the list names
  list2env(loaded_list, envir=environment())

  message("Applying custom population function")
  custom_pop_matrix <- do.call(custom_population_function, c(list(pop = raked_gbd_pop), loaded_list))

  # convert to datatable and set colnames to P1, P2, etc
  custom_pop_dt <- as.data.table(custom_pop_matrix)
  setnames(custom_pop_dt, paste0("P", 1:ndraws))

  # check that population matrix dimensions matches expanded cell pred draws
  if(!all.equal(dim(custom_pop_dt), dim(cell_pred[, paste0("V", 1:ndraws), with = F]))) {
    stop("Something happened during the custom population creation, population matrix does not match cell pred.")
  }

  message("Saving out custom population and merging onto cell pred")
  # save out custom populations for reference and for aggregation function
  if (!is.null(custom_output_folder)) {
    saveRDS(custom_pop_dt, file = "<<<< FILEPATH REDACTED >>>>")
  } else {
    saveRDS(custom_pop_dt, file = "<<<< FILEPATH REDACTED >>>>")
  }

  cell_pred <- cbind(cell_pred, custom_pop_dt)
  cell_pred$pop_raked <- NULL

  return(cell_pred)
}
