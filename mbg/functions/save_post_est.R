#' @title Save Postestimation objects
#' @description Takes in objects created during postestimation and saves them in a standard format in accordance with their object type.
#'
#' @param x any R object
#' @param filetype either 'rdata', 'raster', or 'csv'. The type of file to save `x` as.
#' @param filename The name of the file to be saved. Is prefixed by `indic_`
#' @param indic indicator name, default indicator.
#' @param indicator_group indicator_group name, default indicator_group in global scope
#' @param run_date model run date, default run_date in global scope
#'
#' @return None
#' @export
#'
#' @concept post_estimation
save_post_est <- function(x,
                          filetype,
                          filename,
                          indic = use_global_if_missing("indicator"),
                          indicator_group = use_global_if_missing("indicator_group"),
                          run_date = use_global_if_missing("run_date")) {
  ### Saves post-estimation output files in the proper directory
  ## Inputs:
  ## Outputs:

  output_dir <- "<<<< FILEPATH REDACTED >>>>"


  dir.create(output_dir, showWarnings = FALSE)

  filetype <- tolower(filetype)
  if (!filetype %in% c("rdata", "raster", "csv")) {
    stop("filetype argument has to be either rdata or raster or csv")
  }


  if (filetype == "raster") {
    writeRaster(
      x,
      file = paste0(output_dir, "/", indic, "_", filename),
      format = "GTiff",
      overwrite = TRUE
    )
  }

  if (filetype == "rdata") {
    save(
      x,
      file = paste0(output_dir, "/", indic, "_", filename, ".RData"),
      compress = TRUE
    )
  }

  if (filetype == "csv") {
    write.csv(
      x,
      file = paste0(output_dir, "/", indic, "_", filename, ".csv")
    )
  }
}
