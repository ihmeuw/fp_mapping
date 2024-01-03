source("source_for_setup.R")
source("<<<< FILEPATH REDACTED >>>>/predict_mbg.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 5,
    node_name = "j05_MBG_predict",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = 1
  )
}

pipeline_preamble(inputs = inputs)

path_addin <- pipeline$get_path_addin(loopvar_index)

# Predict MBG -------------------------------------------------------------

## Run predict_mbg on chunks of 50 samples (to avoid memory issues)
message("Making predictions in 50 draw chunks.")

max_chunk <- 1000
samples <- as.numeric(pipeline$config_list$samples)

## Create vector of chunk sizes
chunks <- rep(max_chunk, samples %/% max_chunk)
if (samples %% max_chunk > 0) chunks <- c(chunks, samples %% max_chunk)
pm <- lapply(chunks, function(samp) {
  if (pipeline$config_list$fit_with_tmb == FALSE) {
    predict_mbg(
      res_fit = model_fit,
      cs_df = cs_df,
      mesh_s = mesh_s,
      mesh_t = mesh_t,
      cov_list = cov_list,
      samples = samp,
      simple_raster = simple_raster,
      transform = pipeline$config_list$transform,
      coefs.sum1 = pipeline$config_list$coefs_sum1,
      yl = pipeline$config_list$year_list,
      nperiod = length(pipeline$config_list$year_list),
      pred_gp = pipeline$config_list$use_gp,
      shapefile_version = pipeline$config_list$modeling_shapefile_version,
      seed = seed,
      no_nugget_predict = pipeline$config_list$no_nugget_predict,
      use_space_only_gp = as.logical(pipeline$config_list$use_space_only_gp),
      use_time_only_gmrf = as.logical(pipeline$config_list$use_time_only_gmrf),
      use_timebyctry_res = as.logical(pipeline$config_list$use_timebyctry_res),
      use_timebyadm1 = as.logical(pipeline$config_list$use_timebyadm1),
      use_timebyadm2 = as.logical(pipeline$config_list$use_timebyadm2),
      ind = pipeline$indicator,
      indg = pipeline$indicator_group,
      rd = pipeline$run_date,
      region = reg,
      simple_raster_subnats = simple_raster2,
      subnat_country_to_get = pipeline$config_list$subnat_country_to_get,
      simple_raster_timebyadm1 = simple_raster3_adm1,
      simple_raster_timebyadm2 = simple_raster3_adm2,
      adm1_codes = adm1_codes,
      adm2_codes = adm2_codes
    )[[3]]
  } else {
    predict_mbg_tmb(
      samples = samp,
      seed = seed,
      tmb_input_stack = input_data,
      model_fit_object = model_fit,
      fes = all_fixed_effects,
      sr = simple_raster,
      yl = pipeline$config_list$year_list,
      zl = pipeline$config_list$z_list,
      covs_list = cov_list,
      clamp_covs = as.logical(pipeline$config_list$clamp_covs),
      cov_constraints = cov_constraints,
      use_full_interacting_effect = as.logical(pipeline$config_list$use_gp),
      use_space_only_gp = as.logical(pipeline$config_list$use_space_only_gp),
      use_time_only_gmrf = as.logical(pipeline$config_list$use_time_only_gmrf),
      use_age_only_gmrf = as.logical(pipeline$config_list$use_age_only_gmrf),
      use_timebyctry_res = as.logical(pipeline$config_list$use_timebyctry_res),
      coefs.sum1 = pipeline$config_list$coefs_sum1,
      no_nugget_predict = as.logical(pipeline$config_list$no_nugget_predict)
    )
  }
})



# Finish Up ---------------------------------------------------------------



# if z dimension has more than one level, then save each z as a different indicator
if (length(pipeline$config_list$z_list) > 1) {

  # reorder pm list, right now its z within each chunk. rbind all z's together
  for (z in pipeline$config_list$z_list) { # z_list must be integers starting with 1
    if (length(chunks) > 1) {
      for (ch in 2:length(chunks)) {
        pm[[1]][[z]] <- cbind(pm[[1]][[z]], pm[[ch]][[z]])
      }
    }
  }
  pm <- pm[[1]] # pm is now a list of cell_preds by z


  # loop over z and save as an indicator each one
  orig_indic <- indicator ## indicator from argparse

  message("Wrapping up")

  for (z in z_list) {
    cptmp <- pm[[z]]

    indicator <- sprintf("%s_%s%i", orig_indic, zcol, z) # new indicator name
    pathaddin <- paste0("_bin", z, "_", reg, "_", holdout) # new pathaddin
    outputdir <- paste0("<<<< FILEPATH REDACTED >>>>","/",
      indicator_group, indicator, run_date
    ) # new outputdir
    dir.create(outputdir)
    message(sprintf("New indicator: %s", indicator))

    # make a mean raster
    mean_ras <- insertRaster(
      simple_raster,
      matrix(rowMeans(cptmp),
        ncol = max(period_map$period)
      )
    )
    sd_ras <- insertRaster(
      simple_raster,
      matrix(rowSds(cptmp), ncol = max(period_map$period))
    )

    # save z specific objects
    writeRaster(
      mean_ras,
      file = paste0("<<<< FILEPATH REDACTED >>>>", "/",name),
      overwrite = TRUE
    )

    save(
      cptmp,
      file = paste0(
        outputdir, "/",
        indicator, "_cell_draws_eb", pathaddin, ".RData"
      ),
      compress = TRUE
    )

    pdf(paste0("<<<< FILEPATH REDACTED >>>>", "/",name,".pdf"))
    plot(mean_ras, main = "mean", maxpixel = 1e6)
    plot(sd_ras, main = "sd", maxpixel = 1e6)
    dev.off()

    rm(cptmp)
  }

  ## Reset the constants back
  indicator <- orig_indic

  # save training data
  write.csv(
    df,
    file = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,".csv"),
    row.names = FALSE
  )

  message("done saving indicator-specific outputs by z")
} else {
  ## if no z columns (most peoples cases) ##
  ## Make cell preds and a mean raster
  cell_pred <- do.call(cbind, pm)
  mean_ras <- insertRaster(
    simple_raster,
    matrix(rowMeans(cell_pred),
      ncol = max(period_map$period)
    )
  )



  message("Wrapping up")
  with_globals(
    new = list(
      indicator = pipeline$indicator,
      indicator_group = pipeline$indicator_group
    ),
    save_mbg_preds(
      config = pipeline$config,
      time_stamp = TRUE,
      run_date = pipeline$run_date,
      mean_ras = mean_ras,
      sd_ras = NULL,
      res_fit = model_fit,
      cell_pred = cell_pred,
      df = df,
      pathaddin = path_addin
    )
  )


  # plot the mean raster
  pdf(
      paste0("<<<< FILEPATH REDACTED >>>>", "/",name,".pdf")
    )

  plot(mean_ras, maxpixel = 1e6)
  dev.off()
}


# Postamble ---------------------------------------------------------------

pipeline_postamble()
q("no")
