#' @title Save image of node in the MBG DAG
#'
#' @description Save model data for each node in DAG
#'
#' @param node The name of the node in the DAG
#'
#' @param indicator_group the indicator group
#'
#' @param indicator the indicator
#'
#' @param run_date string run date in YYYY_MM_DD_HH_MM_SS format.
#'
#' @param age the age for the model which uses this data.
#'
#' @param region str region name.
#'
#' @param holdout numeric holdout value (0 for no holdout).
#'
#' @param objs Objects to save out.
#'
#' @return the path to save the file, but the file itself is saved out.
#'
#' @export
#'
#' @concept pipeline
mbg_save_nodeenv <- function(node,
                             ig,
                             indic,
                             rd,
                             reg,
                             age,
                             holdout,
                             objs = ls(envir = .GlobalEnv)) {

  ## Create RData path
  path_str <- "<<<< FILEPATH REDACTED >>>>"

  ## Create absolute path
  img_path <- "<<<< FILEPATH REDACTED >>>>"

  print("The following objects will be saved out in this node's environment:")
  print(objs)

  ## Remove the nodename constant from the environment and then save out
  ## Otherwise, loading this will override the command line argument "nodename"
  ## from the script!
  save(list = setdiff(objs, "nodename"), file = img_path, envir = .GlobalEnv)
  print(paste0("Image saved to ", img_path))
  return(img_path)
}
