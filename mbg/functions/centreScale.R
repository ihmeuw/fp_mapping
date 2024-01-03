#' @title Apply center/scaling to matrix
#' @description Use mean and sd from \code{\link{getCentreScale}} to center and scale values in x. Can uncenter/unscale if inverse = T
#'
#' @param x a data.frame, data.table, or matrix with named columns
#' @param df a data.frame with mean and sd values for each column in x. If NULL, will run getCentreScale on x. Use getCentreScale directly and pass result in here to exclude specific columns from centering/scaling/
#' @param inverse boolean, default FALSE. If TRUE, reverse scaling and centering
#'
#' @return x with columns centered and scaled
#' @export
#'
#' @concept misc
centreScale <- function(x, df, inverse = FALSE) {
  # apply pre-calculated centreing/scaling to matrix x,
  # with fixed dataframe of means/sds df
  # or uncentre/unscale if inverse = TRUE

  # get the centreing/scaling dataframe if not available
  if (is.null(df)) {
    df <- getCentreScale(x)
  }

  # get index to match up values with column names
  names <- colnames(x)
  idx <- match(names, df$name)

  if (any(is.na(idx))) {
    stop("could not match up column names with the values in df")
  }

  df <- df[idx, ]

  # apply transformations
  if (!inverse) {
    # move to standard normal

    # centre
    x <- sweep(x, MARGIN = 2, STATS = df$mean, FUN = "-")
    # scale
    x <- sweep(x, MARGIN = 2, STATS = df$sd, FUN = "/")
  } else {
    # inverse case, move from standard normal to original

    # unscale
    x <- sweep(x, MARGIN = 2, STATS = df$sd, FUN = "*")
    # uncentre
    x <- sweep(x, MARGIN = 2, STATS = df$mean, FUN = "+")
  }

  return(x)
}
