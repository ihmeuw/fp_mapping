#' @title Make cell pred summary
#' @description Takes in raked or raw draw-level estimates and makes a stat summary vector or raster brick as a result. Summarization occurs across columns (e.g. a row with values 1 and 3 will be summarized as 2).
#'
#' @param draw_level_cell_pred raked or raw draw-level estimates
#' @param mask default simple_raster, if creating a raster brick (`return_as_raster=T`), masks output using this raster. Number of rows in draw_level_cell_pred must be a multiple of number of non-na pixels in the mask.
#' @param return_as_raster default T. If T, returns a raster brick split into years determined by taking the number of rows in the `draw_level_cell_pred` and dividing by the number of non-na pixels in the `mask`. If F, returns a vector of summary statistics of length `nrow(draw_level_cell_pred)`
#' @param summary_stat summary statistic to apply. See `Summary statistics` family of functions.
#' @param ... any other arguments to pass to the `summary_stats` functions
#'            (note that currently will be passed to all functions)
#'
#' @return a stat summary vector or raster brick
#' @export
#'
#' @concept post_estimation
make_cell_pred_summary <- function(draw_level_cell_pred,
                                   mask = simple_raster,
                                   return_as_raster = TRUE,
                                   summary_stat = "mean",
                                   ...) {
  ### Takes in raked or raw draw-level estimates and makes stat summary rasters
  ## Inputs:
  # draw_level_cell_pred: Cells by Draws matrix which is output from predict_mbg() or from rake_predictions()
  # mask: Should be the simple_raster
  # return_as_raster: If TRUE returns as raster, else as table
  # summary_stat: ie mean, cirange, quantile, sd
  ## Outputs: Summary table or raster of the cell_pred table put in

  # make summary
  summ <- apply(draw_level_cell_pred, 1, summary_stat, ...)

  # put it in a raster
  if (return_as_raster) {
    yrs <- dim(draw_level_cell_pred)[1] / length(cellIdx(mask))
    message(sprintf("Making a RasterBrick with %i layers", yrs))
    summ <- insertRaster(mask, matrix(summ, ncol = yrs))
  }


  return(summ)
}
