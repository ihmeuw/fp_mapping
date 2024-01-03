#' @title Save outputs from fractional counts raking
#'
#' @description Save outputs from fractional count raking
#' coming out of \code{fractionally_rake_counts} call.
#'
#' @param output_list The list of outputs from \code{fractionally_rake_counts} call.
#' @param sharedir Output to directory
#' @param indicator Indicator
#' @param age Age
#' @param reg Region
#'
#' @param holdout Holdout
#'
#' @rdname raked_frax_counts_save
#' @export
#'
#' @concept fractional_raking
raked_frax_counts_save <- function(output_list, sharedir, indicator, age, reg, holdout) {

  ## Save raking factors
  fwrite(output_list[["raking_factors"]],
    file = "<<<< FILEPATH REDACTED >>>>"
  )

  # Save raked rates cell_pred object (which is equal to counts values!!)

  #### Need to unpack it ####
  raked_cell_pred <- output_list[["raked_cell_pred"]]

  save(raked_cell_pred,
    file = "<<<< FILEPATH REDACTED >>>>"
  )

  ## Save raked counts aggregations
  raked_adm0_draws <- output_list[["raked_adm0_draws"]]
  raked_adm1_draws <- output_list[["raked_adm1_draws"]]
  raked_adm2_draws <- output_list[["raked_adm2_draws"]]
  save(raked_adm0_draws, raked_adm1_draws, raked_adm2_draws,
    file = "<<<< FILEPATH REDACTED >>>>"
  )

  ## save unraked counts aggregations
  unraked_adm0_draws <- output_list[["unraked_adm0_draws"]]
  unraked_adm1_draws <- output_list[["unraked_adm1_draws"]]
  unraked_adm2_draws <- output_list[["unraked_adm2_draws"]]

  save(unraked_adm0_draws, unraked_adm1_draws, unraked_adm2_draws,
    file = "<<<< FILEPATH REDACTED >>>>"
  )


  return(0)
}
