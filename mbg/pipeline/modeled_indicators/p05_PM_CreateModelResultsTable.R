source("source_for_setup.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 19,
    dag_has = "FAKE_HASH",
    node_name = "create_model_results_table",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = NA_integer_
  )
}

# New API optionally provide inputs
pipeline_preamble(inputs = inputs)
# this job uses inputs from previous jobs (1 per loopvar)
input_data_nodes <- dag$get_direct_ancestor_ids(node_id)


# clean_model_results_table code

sharedir <- paste0("<<<< FILEPATH REDACTED >>>>",
  pipeline$indicator_group,
  pipeline$indicator,
  pipeline$run_date
)


# model_fit_table code
use_gp <- pipeline$eval_conf("use_gp")
mods <- list()
# grab formatted model fit objects
for (i in seq_along(input_data_nodes)) {
  ancestor.id <- input_data_nodes[i]
  object.store <- ObjectStore$from_pipeline_and_node_id(pipeline, ancestor.id)
  # populate information necessary
  suppressWarnings(object.store$load_objects(c("input_data", "reg", "age", "holdout")))

  message(sprintf("%i of %i regions %s, age %i, holdout %i", i, length(input_data_nodes), reg, age, holdout))

  # Get SPDE
  spde <- input_data[[2]]
  rm(list = "input_data")

  # Now get & transform model fit
  message("::::loading in INLA fit\n")
  # this file only contains the res_fit object
  f <- paste0(sharedir, pipeline$indicator, "_model_eb_bin", age, "_", reg, "_", holdout, ".RData")

  if (!file.exists(f)) {
    message(sprintf("FAILED TO OPEN INLA fit %s", f))
  } else {
    load(f)
  }

  model.name <- sprintf("%s_%s_%s", reg, age, holdout)
  if (pipeline$eval_conf("fit_with_tmb")) {
    mods[[model.name]] <- fitted_param_table_tmb(
      model_fit = res_fit,
      cov_constraints = covariate_constraint_vectorize(
        pipeline$config_list$fixed_effects,
        pipeline$config_list$gbd_fixed_effects,
        pipeline$config_list$fixed_effects_constraints,
        pipeline$config_list$gbd_fixed_effects_constraints
      )
    )
    # reg, age, and holdout here are coming from the global env
    mods[[model.name]][, region := reg ]
    mods[[model.name]][, age := age ]
    mods[[model.name]][, holdout := holdout ]
  } else {
    ## columns we'll show return
    keep.cols <- c("0.025quant", "0.5quant", "0.975quant")

    ## other hyperparmas
    hyps <- summary(res_fit)$hyperpar[-(1:2), keep.cols] ## first two rows are
    ## theta1/range, theta2/sd

    if (use_gp) {
      if (pipeline$eval_conf("spde_prior")$type == "pc") {
        ## extract values from the fit directly
        range <- res_fit$summary.hyperpar[1, keep.cols]
        nom.var <- res_fit$summary.hyperpar[2, keep.cols]^2
      } else {
        ## now we extract what we need from the fit to get transformed spatial params
        res.field <- INLA::inla.spde2.result(res_fit, "space", spde, do.transf = TRUE)

        ## nominal range at 0.025, 0.5, 0.975 quantiles
        range <- INLA::inla.qmarginal(c(0.025, 0.5, 0.975), res.field$marginals.range.nominal[[1]])
        nom.var <- INLA::inla.qmarginal(c(0.025, 0.5, 0.975), res.field$marginals.variance.nominal[[1]])
      }
      spat.hyps <- rbind(range, nom.var)
      rownames(spat.hyps) <- c("Nominal Range", "Nominal Variance")
      colnames(spat.hyps) <- keep.cols
    }

    ## fixed effects from coefs_sum1
    if (as.logical(pipeline$config_list$coefs_sum1) && as.logical(pipeline$config_list$use_stacking_covs)) {
      fixed.sum1 <- res_fit$summary.random$covar
      fixed.sum1$ID <- NULL
      rownames(fixed.sum1) <- strsplit(pipeline$config_list$stacked_fixed_effects, " + ", fixed = T)[[1]]
      fixed.sum1 <- fixed.sum1[, keep.cols]
    } else {
      fixed.sum1 <- NULL
    }

    ## all other coefs (e.g. intercept and raw covs)
    fixed <- summary(res_fit)$fixed
    if (is.null(nrow(fixed))) {
      fixed <- matrix(fixed, ncol = length(fixed)) ## in the event of one row, convert numeric back to data.frame
      rownames(fixed) <- rownames(summary(res_fit)$fixed)
      colnames(fixed) <- colnames(summary(res_fit)$fixed)
    }
    fixed <- fixed[, keep.cols]

    ## combine the two types of 'fixed' results
    fixed <- rbind(fixed, fixed.sum1)

    ## combine them all and just keep three quantiles
    all.model.res <- rbind(
      fixed,
      if (use_gp) {
        spat.hyps
      } else {
        NULL
      },
      hyps
    )

    ## rename GPRandom rho for time
    all.model.res <- as.data.table(all.model.res, keep.rownames = T)
    setnames(all.model.res, "rn", "parameter")
    if (use_gp) all.model.res[parameter == "GroupRho for space", parameter := "GPRandom rho for time"]
    # END model_fit_table code

    # column additions done after model_fit_table
    all.model.res[, region := reg]
    all.model.res[, age := age]
    all.model.res[, holdout := holdout]

    mods[[model.name]] <- all.model.res
  }
}


all_mods <- rbindlist(mods)
colorder <- c(
  "region", "age", "holdout",
  names(all_mods)[!(names(all_mods) %in% c("region", "age", "holdout"))]
)
setcolorder(all_mods, colorder)

write.csv(all_mods, paste0("<<<< FILEPATH REDACTED >>>>", "/",name, ".csv"),
  row.names = F
)


pipeline_postamble()
message("Finished CreateModelResultsTable")
q("no")
