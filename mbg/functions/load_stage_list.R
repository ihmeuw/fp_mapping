#' @title Returns the stage reference list
#'
#' @description Loads from J drive in lbd_core code and from a pre-saved data file in the
#' lbd.mbg package.
#'
#' @export
#'
#' @concept setup
load_stage_list <- function() {
  if (.in.package()) {
    data("stage_master_list")
    stage_master_list
  } else {
    data.table::fread(file.path("<<<< FILEPATH REDACTED >>>>"))
  }
}
