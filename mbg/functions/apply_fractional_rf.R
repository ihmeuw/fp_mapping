#' @title Apply Fractional Raking Factors
#'
#' @description Apply raking factors considering cases where a pixel is in
#' multiple admin units for deaths or cases
#'
#' @param cell_pred a cell pred with deaths/cases
#' @param link link table generated by `build_link_table()`
#' @param fractional_rf a data.table generated by `get_fractional_rf`
#' @param overs a vector of the column names in the cell pred corresponding to draws
#' (typically V1:Vndraws)
#'
#' @return a linked cell pred with the rf applied
#'
#' @export
#'
#' @concept fractional_raking
apply_fractional_rf <- function(cell_pred, link, fractional_rf, overs) {
  # generate id for rows in cell pred (merges below mess up ordering)
  cell_pred[, cell_pred_id := .I]
  # merge cell pred and link file
  linked_cell_pred <- merge(link, cell_pred, by.x = "ID", by.y = "pixel_id", all.y = T, allow.cartesian = T)
  # merge linked cell pred with raking factors
  linked_cell_pred <- merge(linked_cell_pred, fractional_rf, by.x = c("ADM_CODE", "year", "ADM0_CODE"), by.y = c("ADM_CODE", "year", "ADM0_CODE"), all.x = T)
  # converting from "units" type to double, truncates decimal otherwise
  linked_cell_pred$area_fraction <- as.double(linked_cell_pred$area_fraction)
  # calculate deaths on all draws
  raked_cell_pred <- linked_cell_pred[, (overs) := lapply(overs, function(x) get(x) * rf * area_fraction)]

  # sort on cell_pred_id
  setorder(raked_cell_pred, cell_pred_id)

  return(raked_cell_pred)
}
