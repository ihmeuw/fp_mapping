#' @title Save predictions from MBG model
#' @description Function that saves results and mean raster, sd raster, model fit,
#' cell predictions/draws, and data into output directory
#'
#' @param config config
#' @param time_stamp Logical. True saves in run_date folder otherwise scratch
#' @param run_date run date
#' @param mean_ras mean_ras (raster containing mean predictions)
#' @param sd_ras sd_ras (raster containing sd of predictions)
#' @param res_fit model result from \code{fit_mbg}
#' @param cell_pred cell_pred (matrix of all prediction draws)
#' @param df data frame with observations
#' @param pathaddin additional thing to add to path, quite often: \code{paste0("_bin", age, "_", reg, "_", holdout)}
#' @return None
#' @export
#'
#' @concept mbg
save_mbg_preds <- function(config,
                           time_stamp,
                           run_date,
                           mean_ras,
                           sd_ras,
                           res_fit,
                           cell_pred,
                           df,
                           pathaddin = "") {
  if (time_stamp == TRUE) output_dir <- "<<<< FILEPATH REDACTED >>>>"
  if (time_stamp == FALSE) output_dir <- "<<<< FILEPATH REDACTED >>>>"
  dir.create(output_dir, showWarnings = FALSE)

  if (!is.null(mean_ras)) {
    writeRaster(
      mean_ras,
      file = "<<<< FILEPATH REDACTED >>>>",
      overwrite = TRUE
    )
  }

  if (!is.null(sd_ras)) {
    # latent sd
    writeRaster(
      sd_ras,
      file = "<<<< FILEPATH REDACTED >>>>",
      overwrite = TRUE
    )
  }

  # save model
  save(res_fit,
    file = "<<<< FILEPATH REDACTED >>>>"
  )
  # save draws (with compression) to recombine later
  save(
    cell_pred,
    file = "<<<< FILEPATH REDACTED >>>>",
    compress = TRUE
  )
  # save training data
  write.csv(
    df,
    file = "<<<< FILEPATH REDACTED >>>>",
    row.names = FALSE
  )

  # Write a an empty file to indicate done with this parallel script
  write(NULL, file = "<<<< FILEPATH REDACTED >>>>")
}
