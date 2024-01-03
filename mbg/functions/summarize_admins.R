#' @title summarize_admins
#' @description Function to summarize admin_pred objects
#'
#' This is a wrapper for `make_admin_pred_summary()`
#'
#' @param ind indicator
#' @param ig indicator_group
#' @param summstats Summary statistics (functions) to compute.
#' Order will be the order they are written in csv
#' This is passed to `make_admin_pred_summary()`
#' @param raked Raked (T), unraked (F), or both (`c(T,F)`)?
#' @param ad_levels Admin levels to summarize (0, 1, and 2 by default)
#' @param file_addin String to add to end of output admin summary filenames
#' @param counts For fractional raking, counts pred (T), rates/other (F), or both (`c(T,F)`)?
#' @param ages: single value or vector of ages
#' @param holdouts: vector of holdouts used, e.g. just 0 or c(1,2,3,4,5,0)
#' @examples
#' \dontrun{
#' summarize_admins(
#'   summstats = c("mean", "lower", "upper", "cirange"),
#'   ad_levels = c(0, 1, 2),
#'   raked = c(T, F)
#' )
#' }
#' @rdname summarize_admins
#' @export
#'
#' @concept post_estimation
summarize_admins <- function(ind = indicator,
                             ig = indicator_group,
                             run_date = run_date,
                             summstats = c("mean", "lower", "upper", "cirange"),
                             raked = c(T, F),
                             ad_levels = c(0, 1, 2),
                             file_addin = NULL,
                             counts = F,
                             ages = 0,
                             holdouts = 0,
                             ...) {
  sharedir <- "<<<< FILEPATH REDACTED >>>>"
  input_dir <- "<<<< FILEPATH REDACTED >>>>"
  output_dir <-"<<<< FILEPATH REDACTED >>>>"
  dir.create(output_dir, recursive = T, showWarnings = F)

  # Convert raked to character
  rr <- character()
  if (T %in% raked) rr <- c(rr, "raked")
  if (F %in% raked) rr <- c(rr, "unraked")

  # If file_addin present, use it
  if (!is.null(file_addin)) file_addin <- paste0("_", file_addin)
  if (is.null(file_addin)) file_addin <- ""

  # Summarize and save admin preds
  for (rake in rr) {
    for (count in counts) {
      for (age in ages) {
        for (holdout in holdouts) {
          load("<<<< FILEPATH REDACTED >>>>")
          sp_hierarchy_list <- mutate_if(sp_hierarchy_list, is.factor, as.character)
          sp_hierarchy_list <- mutate_at(sp_hierarchy_list, grep("_CODE", names(sp_hierarchy_list), value = T), as.numeric)
    
          for (ad in ad_levels) {
            message(paste0("Summarizing ", ind, ": admin ", ad, " (", rake, ", ", ifelse(count, "counts", "rates"), ")"))
            message(paste0("    age: ", age, " holdout: ", holdout))
            ad_summary_table <- make_admin_pred_summary(
              admin_pred = get(paste0("admin_", ad)),
              sp_hierarchy_list,
              summary_stats = summstats,
              ...
            )
            path_addin <- if(age != 0 | holdout != 0) sprintf("_bin%s_%s", age, holdout) else ""
            fwrite(ad_summary_table,
              file = "<<<< FILEPATH REDACTED >>>>"
            )
          }
        }
      }
    }
  }
}
