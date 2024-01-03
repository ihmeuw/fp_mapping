# Inspired by set_envvar in the withr package
# https://github.com/r-lib/withr/blob/main/R/env.R
set_globals <- function(globals) {
  if (length(globals) == 0) {
    return()
  }

  # validate that globals is a named list and all values have names.
  stopifnot(is.list(globals))
  g_names <- names(globals)
  if (is.null(g_names) || any(g_names == "")) {
    stop("All new globals must be explicitly named")
  }

  # Define old.values will be passed to this function as globals *after* of the
  # with code block. When these values are applied to set_globals the inverse
  # operation should occur.
  old.values <- mget(g_names, envir = .GlobalEnv, ifnotfound = NA)

  # with_globals details notes that NA values indicate "unset this"
  remove(list = g_names[is.na(globals)], envir = .GlobalEnv)

  for (name in g_names[!is.na(globals)]) {
    .GlobalEnv[[name]] <- globals[[name]]
  }

  return(old.values)
}

#' With (Temporary) Globals
#'
#' Temporarily change global variables
#'
#' @param new named list of values to assign to global environment.
#'
#' @details if `NA` is used those global values will be unset.
#'
with_globals <- withr::with_(set_globals)
