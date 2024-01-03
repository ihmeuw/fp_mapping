source("source_for_setup.R")
source("<<<< FILEPATH REDACTED >>>>/build_mbg_formula_with_priors.R")
source("<<<< FILEPATH REDACTED >>>>/build_mbg_data_stack.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 4,
    node_name = "j04_MBG_fitting",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = 1
  )
}

pipeline_preamble(inputs = inputs)


# Prep For MBG ------------------------------------------------------------


# Bound GBM to 0-1 if desired
gbm_bounded_0_1 <- TRUE
if (exists("gbm_bounded_0_1")) {
  if (as.logical(gbm_bounded_0_1) == T & "gbm" %in% names(cov_list)) {
    message("Truncating GBM values >= 1 to 0.999 and <= 0 to 1e-4")
    values(cov_list[["gbm"]])[values(
      cov_list[["gbm"]]
    ) >= 1 & !is.na(values(cov_list[["gbm"]]))] <- 0.999
    values(cov_list[["gbm"]])[values(
      cov_list[["gbm"]]
    ) <= 0 & !is.na(values(cov_list[["gbm"]]))] <- 1e-4
    gbm_cols <- grep(paste0("(gbm)(.*_pred)"),
      names(df),
      value = T
    )
    replace_one <- function(x) {
      x[x >= 1 & !is.na(x)] <- 0.999
      return(x)
    }
    replace_zero <- function(x) {
      x[x <= 0 & !is.na(x)] <- 1e-4
      return(x)
    }
    df[, (gbm_cols) := lapply(.SD, replace_one), .SDcols = gbm_cols]
    df[, (gbm_cols) := lapply(.SD, replace_zero), .SDcols = gbm_cols]
  }
}

## convert stackers to transform space, if desired
## NOTE: we do this here to ensure that the stacker rasters are
## saved in prevalence/untransformed space
## this is useful for diagnostics and other code that was built
## expecting the untransformed rasters
if (
  as.logical(pipeline$config_list$stackers_in_transform_space)
  && as.logical(pipeline$config_list$use_stacking_covs)
  && pipeline$config_list$indicator_family == "binomial") {
  message("Converting stackers to logit space")

  ## transform the rasters
  for (ii in child_model_names) {

    ## Preserve variable names in the raster first
    tmp_rastvar <- names(cov_list[[ii]])

    ## Logit
    cov_list[[ii]] <- logit(cov_list[[ii]])

    ## Reassign names
    names(cov_list[[ii]]) <- tmp_rastvar
    rm(tmp_rastvar)
  }

  ## transform the stacker values that are in df
  stacker_col_regexp <- sprintf(
    "(%s)(.*_pred)", paste(child_model_names, collapse = "|")
  )
  stacker_cols <- grep(stacker_col_regexp, names(df), value = TRUE)
  df[, (stacker_cols) := lapply(.SD, logit), .SDcols = stacker_cols]
}

# Prep Objects For MBG ----------------------------------------------------

## for stacking, overwrite the columns matching the model_names
## so that we can trick inla into being our stacker
if (pipeline$config_list$use_stacking_covs) {
  df[, paste0(child_model_names) := lapply(
    child_model_names, function(x) get(paste0(x, "_cv_pred"))
  )]
}

## Generate MBG formula for INLA call (will run but not used by TMB)
mbg_formula <- build_mbg_formula_with_priors(
  fixed_effects = all_fixed_effects,
  add_nugget = as.logical(pipeline$config_list$use_inla_nugget),
  nugget_prior = pipeline$config_list$nugget_prior,
  add_ctry_res = as.logical(pipeline$config_list$use_country_res),
  ctry_re_prior = pipeline$config_list$ctry_re_prior,
  temporal_model_type = pipeline$config_list$temporal_model_type,
  temporal_model_theta1_prior = pipeline$config_list$rho_prior,
  no_gp = !as.logical(pipeline$config_list$use_gp),
  coefs.sum1 = as.logical(pipeline$config_list$coefs_sum1),
  use_space_only_gp = as.logical(pipeline$config_list$use_space_only_gp),
  use_time_only_gmrf = as.logical(pipeline$config_list$use_time_only_gmrf),
  time_only_gmrf_type = pipeline$config_list$time_only_gmrf_type,
  use_timebyadm1 = pipeline$config_list$use_timebyadm1,
  use_timebyadm2 = pipeline$config_list$use_timebyadm2,
  timebyadm_type = pipeline$config_list$timebyadm_type,
  adj_mat_adm1 = adj_mat_adm1,
  adj_mat_adm2 = adj_mat_adm2,
  subnat_RE = as.logical(pipeline$config_list$use_subnat_res),
  subnat_country_to_get = pipeline$config_list$subnat_country_to_get,
  subnat_re_prior = pipeline$config_list$subnat_re_prior,
  timebycountry_RE = as.logical(pipeline$config_list$use_timebyctry_res),
  adm0_list = gaul_list,
  use_nid_res = as.logical(pipeline$config_list$use_nid_res),
  nid_re_prior = pipeline$config_list$nid_re_prior
)

## If needed, add fake data to make sure INLA estimates all years
missing_years <- base::setdiff(
  pipeline$config_list$year_list,
  df$year
)

print("Missing years: ")
print(missing_years)

# For INLA we need to add data for missing time points to ensure we get predictions
#  for all relevant time points. The 0 observations do not contribute to the
#  model fitting but they prevent INLA from auto-removing
#  random effects that (conditionally) have no data impacting their fit
if(pipeline$config_list$use_timebyctry_res) {
  ## If we are using a time only effect by country then we need to make sure
  ##  all year effects are estimated for each country.
  df$adm0code <- gaul_convert(df$country)
  for(adm0_code in gaul_list) {
    dfsub <- df[df$adm0code == adm0_code, ]
    missing_years <- setdiff(pipeline$config_list$year_list, dfsub$year)
    if (length(missing_years) > 0) {
      fake_data <- dfsub[1:length(missing_years), ]
      fake_data[, year := missing_years]
      fake_data[, c(pipeline$indicator, 'N', 'weight') := 0]
      fake_data[, period := NULL]
      fake_data <- merge(fake_data, period_map)
      df <- rbind(df, fake_data)
    }
  }
} else {
  ## If not, we only need to make sure we have an observation for each missing
  ##  year (country irrelevant)
  missing_years <- setdiff(pipeline$config_list$year_list, df$year)
  if (length(missing_years) > 0) {
    fake_data <- df[1:length(missing_years), ]
    fake_data[, year := missing_years]
    fake_data[, c(pipeline$indicator, 'N', 'weight') := 0]
    fake_data[, period := NULL]
    fake_data <- merge(fake_data, period_map)
    df <- rbind(df, fake_data)
  }
}
# get covariate constraints for data stack
cov_constraints <- covariate_constraint_vectorize(
  pipeline$config_list$fixed_effects,
  pipeline$config_list$gbd_fixed_effects,
  pipeline$config_list$fixed_effects_constraints,
  pipeline$config_list$gbd_fixed_effects_constraints
)

## Create SPDE INLA stack
## note that merge (if using TMB) will return data in a different
## but internally consistent) order, just different than df
df[, N := round(N)]
df[, (pipeline$indicator) := get(pipeline$indicator) %>% round]


with_globals(
  new = list(indicator = pipeline$indicator),
  input_data <- build_mbg_data_stack(
    df = df,
    fixed_effects = all_fixed_effects,
    mesh_s = mesh_s,
    # mest_t not currently implemented with tmb
    mesh_t = mesh_t,
    # raw covs will get center scaled here though (see notes above)
    exclude_cs = child_model_names,
    # prior for Matern GP
    spde_prior = pipeline$config_list$spde_prior,
    #  sum-to-1 not currenlty implemented tmb
    coefs.sum1 = pipeline$config_list$coefs_sum1,
    use_ctry_res = pipeline$config_list$use_country_res,
    use_subnat_res = pipeline$config_list$use_subnat_res,
    # nugget implemented with tmb
    use_nugget = as.logical(pipeline$config_list$use_inla_nugget),
    yl = pipeline$config_list$year_list,
    # if zl is not zero and tmb==TRUE,
    # it will trigger 3rd kronecker and fixed effects
    zl = pipeline$config_list$z_list,
    # zcol must not be null if z_list is present
    zcol = pipeline$config_list$zcol,
    scale_gaussian_variance_N = pipeline$config_list$scale_gaussian_variance_N,
    tmb = pipeline$config_list$fit_with_tmb,
    cov_constraints = cov_constraints,
    use_gp = as.logical(pipeline$config_list$use_gp),
    use_space_only_gp = as.logical(pipeline$config_list$use_space_only_gp),
    use_time_only_gmrf = as.logical(pipeline$config_list$use_time_only_gmrf),
    use_timebyadm1 = as.logical(pipeline$config_list$use_timebyadm1),
    use_timebyadm2 = as.logical(pipeline$config_list$use_timebyadm2),
    shapefile_version = pipeline$config_list$modeling_shapefile_version,
    st_gp_int_zero = as.logical(pipeline$config_list$st_gp_int_zero),
    s_gp_int_zero = as.logical(pipeline$config_list$s_gp_int_zero),
    use_age_only_gmrf = as.logical(pipeline$config_list$use_age_only_gmrf),
    use_nid_res = as.logical(pipeline$config_list$use_nid_res),
    use_timebyctry_res = as.logical(pipeline$config_list$use_timebyctry_res),
    adm0_list = gaul_list,
    seed = seed,
    # ... args (config args to be passed to build_mbg_data_stack_tmb)
    indicator_family = pipeline$config_list$indicator_family,
    nugget_prior = pipeline$config_list$nugget_prior,
    ctry_re_prior = pipeline$config_list$ctry_re_prior,
    nid_re_prior = pipeline$config_list$nid_re_prior,
    use_s2_mesh = pipeline$config_list$use_s2_mesh,
    mesh_s_max_edge = pipeline$config_list$mesh_s_max_edge,
    mesh_s_offset = pipeline$config_list$mesh_s_offset,
    s2_mesh_params = pipeline$config_list$s2_mesh_params
  )
)

## combine all the inputs, other than cs_df
## these are not used if you are using TMB
stacked_input  <- input_data[[1]]
spde           <- input_data[[2]] ## used for space-time gp
cs_df          <- input_data[[3]]
spde.sp        <- input_data[[4]] ## used for space only (time stationary) gp

## Generate other inputs necessary
outcome <- df[[pipeline$indicator]] # N+_i - event obs in cluster
N <- df$N # N_i - total obs in cluster
weights <- df$weight

## catch in case there is no weight column
if (is.null(weights)) {
  weights <- rep(1, nrow(df))
}

set.seed(seed)
increment_seed(seed)

# Fit The MBG -------------------------------------------------------------

if (!as.logical(pipeline$config_list$skipinla)) {
  if (!as.logical(pipeline$config_list$fit_with_tmb)) {
    message("Fitting model with R-INLA")

    model_fit <- fit_mbg(
      indicator_family = pipeline$config_list$indicator_family,
      stack.obs = stacked_input,
      spde = spde,
      cov = outcome,
      N = N,
      int_prior_mn = pipeline$config_list$intercept_prior,
      f_mbg = mbg_formula,
      run_date = pipeline$run_date,
      keep_inla_files = pipeline$config_list$keep_inla_files,
      cores = Sys.getenv("SGE_HGR_fthread"),
      wgts = weights,
      intstrat = pipeline$config_list$intstrat,
      fe_sd_prior = 1 / 9, ## this actually sets precision!. prec=1/9 -> sd=3
      verbose_output = TRUE,
      omp_strat = pipeline$config_list$omp_strat,
      blas_cores = as.integer(pipeline$config_list$blas_cores),
      pardiso_license = pipeline$config_list$pardiso_license
    )

  } else {
    message("Fitting model with TMB")
    message(sprintf(
      "%s Data points and %s mesh nodes",
      nrow(df),
      length(input_data$Parameters$Epsilon_stz)
    ))

    # save RDS file of input data for replication
    saveRDS(
      object = input_data, ## save this here in case predict dies
      file = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,
        pipeline$indicator_group, pipeline$indicator, pipeline$run_date,
        ifelse(
          pipeline$config_list$fit_with_tmb, "tmb", "inla"
        ),
        reg, holdout, age
      )
    )
    # run the model
    model_fit <- fit_mbg_tmb(
      cpp_template = pipeline$config_list$tmb_template_path,
      tmb_input_stack = input_data,
      control_list = list(
        trace = 1,
        eval.max = 500, iter.max = 300, abs.tol = 1e-20
      ),
      optimizer = "nlminb",
      ADmap_list = NULL,
      sparse_ordering = as.logical(pipeline$config_list$sparse_ordering),
      seed = seed
    )

    # clamping
    clamp_covs <- pipeline$config_list$clamp_covs
  }

  saveRDS(
    object = model_fit, ## save this here in case predict dies
    file = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,
      pipeline$indicator_group, pipeline$indicator, pipeline$run_date,
      ifelse(
        pipeline$config_list$fit_with_tmb, "tmb", "inla"
      ),
      reg, holdout, age
    )
  )
} else {
  ## skipped fitting INLA so just load model and move to predict
  model_fit <- readRDS(
    file = paste0("<<<< FILEPATH REDACTED >>>>", "/",name,
      indicator_group, indicator, run_date,
      ifelse(
        pipeline$config_list$fit_with_tmb, "tmb", "inla"
      ), reg, holdout, age
    )
  )
}

# Postamble ---------------------------------------------------------------

pipeline_postamble()
q("no")
