source("source_for_setup.R")
source("<<<< FILEPATH REDACTED >>>>/load_input_data_subnational.R")

## Setup preambles
user <- Sys.info()["user"]
inputs <- if (interactive()) { # otherwise NULL
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 1,
    node_name = "j01_data_prep",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = 1
  )
}
pipeline_preamble(inputs = inputs)

# Populate global variables tied to loopvar_index.
assign("reg", pipeline$loopvars[loopvar_index, region])
assign("age", pipeline$loopvars[loopvar_index, age])
assign("holdout", pipeline$loopvars[loopvar_index, holdout])
assign("seed", pipeline$config_list$seed)

# Prep MBG inputs/Load Data -----------------------------------------------

## If skipping to INLA, then just quit job
if (as.logical(pipeline$config_list$skiptoinla)) {
  print("Skipping to INLA")

  ## Save out environment
  mbg_save_nodeenv(
    node = nodename,
    ig = pipeline$indicator_group,
    indic = pipeline$indicator,
    rd = pipeline$run_date,
    reg = reg,
    age = age,
    holdout = holdout,
    objs = ls()
  )

  ## Create output file and remove err file ##
  mbg_job_marker(type = "end", tmpdir = "~/mbgdir")

  q("no")
}

## Load simple polygon template to model over
gaul_list <- get_adm0_codes(
  reg,
  shapefile_version = pipeline$config_list$modeling_shapefile_version
)
simple_polygon_list <- load_simple_polygon(
  gaul_list = gaul_list, buffer = 0.2, tolerance = 0.2,
  shapefile_version = pipeline$config_list$modeling_shapefile_version
)
subset_shape <- simple_polygon_list[[1]]
simple_polygon <- simple_polygon_list[[2]]

simple_raster <- build_simple_raster(extent_template = subset_shape,
                                     shapefile_version = pipeline$config_list$modeling_shapefile_version,
                                     region = reg,
                                     raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor))

## Load input data based on stratification and holdout,
## OR pull in data as normal and run with the whole dataset if holdout == 0.
## For holdouts, we have depreciated val level,
## so each val level must be recorded in a different run date
if (holdout != 0) {
  message(paste0(
    "Holdout != 0 so loading holdout data only from holdout ", holdout
  ))
  message(
    "Please be sure you have a list object called stratum_ho in your environment."
  )
  ## if stratifies by age then make sure loads correctly
  if (age != 0) {
    df <- as.data.table(
      pipline$stratum_ho[[paste("region", reg, "_age", age, sep = "__")]]
    )
  } else {
    df <- as.data.table(
      pipeline$stratum_ho[[paste("region", reg, sep = "__")]]
    )
  }
  df <- df[fold != holdout, ]
  df$first_entry <- 1
  df$agg_weight <- 1
} else {
  message("Holdout == 0 so loading in full dataset using load_input_data()")
  with_globals(
    new = list(
      run_date = pipeline$run_date,
      modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version
    ),
    df <- load_input_data(
      indicator = gsub(paste0("_age", age), "", pipeline$indicator),
      indicator_group = pipeline$indicator_group,
      agebin = age,
      removeyemen = FALSE,
      date = pipeline$run_date,
      pathaddin = pipeline$get_path_addin(loopvar_index),
      years = pipeline$config_list$yearload,
      withtag = as.logical(pipeline$config_list$withtag),
      datatag = pipeline$config_list$datatag,
      use_share = as.logical(pipeline$config_list$use_share),
      yl = pipeline$config_list$year_list,
      poly_ag = pipeline$config_list$poly_ag,
      # converts text value e.g., "NULL" into proper scalar e.g., NULL
      zcol_ag = pipeline$eval_conf("zcol_ag"),
      region = reg
    )
  )
  df <- process_input_data(
    df,
    pop_release                = pipeline$config_list$pop_release,
    interval_mo                = pipeline$config_list$interval_mo,
    modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
    poly_ag                    = as.logical(pipeline$config_list$poly_ag),
    zcol                       = pipeline$config_list$zcol,
    zcol_ag                    = pipeline$eval_conf("zcol_ag"),
    zcol_ag_id                 = pipeline$eval_conf("zcol_ag_id"),
    z_map                      = pipeline$eval_conf("z_map"),
    z_ag_mat                   = pipeline$eval_conf("z_ag_mat")
  )
}

# Get admin codes for subnational REs
if (as.logical(pipeline$config_list$use_subnat_res)) {

  # load in adm1 simple raster
  simple_raster2 <- build_simple_raster(extent_template = simple_raster,
                                        shapefile_version = pipeline$config_list$modeling_shapefile_version,
                                        reg = reg,
                                        raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor),
                                        field = "ADM1_CODE")
  # get admin 0 codes for entire region
  region_adm0s <- get_adm0_codes(
    reg,
    shapefile_version = pipeline$config_list$modeling_shapefile_version
  )

  # remove countries from simple_raster2 if specified in subnat_country_to_get
  if ("all" %in% pipeline$config_list$subnat_country_to_get) {
    countries_to_get_subnat_res <- region_adm0s
  } else {
    re_country_adm0s <- get_adm0_codes(
      pipeline$config_list$subnat_country_to_get,
      shapefile_version = pipeline$config_list$modeling_shapefile_version
    )
    countries_to_get_subnat_res <- intersect(re_country_adm0s, region_adm0s)

    # make a mask by removing target countries from simple raster
    simple_mask <- copy(simple_raster)
    countries_to_remove <- na.omit(setdiff(unique(simple_mask), countries_to_get_subnat_res))
    simple_mask[simple_mask %in% countries_to_remove] <- NA

    simple_raster2 <- raster::mask(simple_raster2, simple_mask)
  }

  # load and subset standard admin1 shape to countries with subnational REs
  subnat_full_shp <- readRDS(get_admin_shapefile(
    admin_level = 1,
    raking = FALSE,
    suffix = ".rds",
    version = pipeline$config_list$modeling_shapefile_version
  ))

  subnat_shapefile <- raster::subset(
    subnat_full_shp,
    ADM0_CODE %in% countries_to_get_subnat_res
  )

  ## Merge ADM0/1 codes to df
  adm1_subset_lox <- sp::over(sp::SpatialPoints(
    df[, .(long = longitude, lat = latitude)],
    sp::CRS(proj4string(subnat_shapefile))
  ), subnat_shapefile)
  df[, subnat_re_ADM1_CODE := as.numeric(as.character(adm1_subset_lox$ADM1_CODE))]
  df[, subnat_re_ADM0_CODE := as.numeric(as.character(adm1_subset_lox$ADM0_CODE))]

  # create new ADM1 columns for each country in subnat_country_to_get so data can be fit separately
  for (i in 1:length(unique(na.omit(df$subnat_re_ADM0_CODE)))) {
    df[subnat_re_ADM0_CODE == unique(na.omit(df$subnat_re_ADM0_CODE))[i], (paste0("SUBNAT", i)) := subnat_re_ADM1_CODE]
  }
} else {
  simple_raster2 <- NULL
}

# Get admin codes for subnational time effects
if (as.logical(pipeline$config_list$use_timebyadm1) | as.logical(pipeline$config_list$use_timebyadm2) | as.logical(pipeline$config_list$nid_res_by_adm1)) {

  # load in adm2 simple raster
  simple_raster3_adm1 <- build_simple_raster(extent_template = subset_shape,
                                             shapefile_version = pipeline$config_list$modeling_shapefile_version,
                                             reg = reg,
                                             raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor),
                                             field = "ADM1_CODE")
  stopifnot(length(simple_raster) == length(simple_raster3_adm1))

  simple_raster3_adm2 <- build_simple_raster(extent_template = subset_shape,
                                             shapefile_version = pipeline$config_list$modeling_shapefile_version,
                                             reg = reg,
                                             raster_agg_factor = as.integer(pipeline$config_list$raster_agg_factor),
                                             field = "ADM2_CODE")
  stopifnot(length(simple_raster) == length(simple_raster3_adm2))

  # get admin 0 codes for entire region
  region_adm0s <- get_adm0_codes(
    reg,
    shapefile_version = pipeline$config_list$modeling_shapefile_version
  )

  # load and subset standard admin1 shape to countries with subnational REs
  subnat_full_shp <- readRDS(get_admin_shapefile(
    admin_level = 2,
    raking = FALSE,
    suffix = ".rds",
    version = pipeline$config_list$modeling_shapefile_version
  ))

  subnat_shapefile <- raster::subset(
    subnat_full_shp,
    ADM0_CODE %in% region_adm0s
  )

  # get the full list of adm1 and adm2 codes, ordered
  adm1_codes <- sort(unique(as.integer(as.character(subnat_shapefile$ADM1_CODE))))
  adm2_codes <- sort(unique(as.integer(as.character(subnat_shapefile$ADM2_CODE))))

  ## Merge ADM0/1/2 codes to df
  adm_subset_lox <- sp::over(sp::SpatialPoints(df[, .(long = longitude, lat = latitude)],
                                               CRS(proj4string(subnat_shapefile))),
                             subnat_shapefile)

  adm1_subset_lox <- SpatialPoints(coords = as.matrix(df[, .(longitude, latitude)]),
                                    proj4string = CRS(proj4string(simple_raster3_adm1)))
  adm1_subset_lox <- raster::extract(simple_raster3_adm1, adm1_subset_lox)

  adm2_subset_lox <- SpatialPoints(coords = as.matrix(df[, .(longitude, latitude)]),
                                   proj4string = CRS(proj4string(simple_raster3_adm2)))
  adm2_subset_lox <- raster::extract(simple_raster3_adm2, adm2_subset_lox)

  # use value extracted from shapefile, if available, and from the raster if not
  # this is to help with cases where a point falls in a river or along a coastline and so according
  # to the shapefile does not belong to any admin
  df[, timeby_ADM1_CODE := ifelse(is.na(adm_subset_lox$ADM1_CODE),
                                  as.integer(as.character(adm1_subset_lox)),
                                  as.integer(as.character(adm_subset_lox$ADM1_CODE)))]
  df[, timeby_ADM2_CODE := ifelse(is.na(adm_subset_lox$ADM2_CODE),
                                  as.integer(as.character(adm2_subset_lox)),
                                  as.integer(as.character(adm_subset_lox$ADM2_CODE)))]

  df[, timeby_ADM1_CODE := as.integer(factor(timeby_ADM1_CODE, levels = adm1_codes))]
  df[, timeby_ADM2_CODE := as.integer(factor(timeby_ADM2_CODE, levels = adm2_codes))]

  # this drops small amounts of data outside the country borders
  if (sum(is.na(df$timeby_ADM1_CODE)) >= 10) {
    stop("Missing timeby_ADM1_CODE for 10+ points")
  } else if (sum(is.na(df$timeby_ADM1_CODE)) > 0) {
    warning(paste("Dropping", sum(is.na(df$timeby_ADM1_CODE)), "points with missing timeby_ADM1_CODE"))
    df <- df[!is.na(timeby_ADM1_CODE), ]
  }

  # setup adm2 adjacency graphs for besag effects
  if (pipeline$config_list$timebyadm_type == "besag") {
    subnat_shapefile_sf <- st_as_sf(subnat_shapefile)
    subnat_shapefile_sf <- st_make_valid(subnat_shapefile_sf)
    subnat_shapefile_sf$ADM2_CODE <- as.integer(as.character(subnat_shapefile_sf$ADM2_CODE))
    subnat_shapefile_sf <- subnat_shapefile_sf[order(subnat_shapefile_sf$ADM2_CODE),]
    adj_list_adm2 <- st_intersects(subnat_shapefile_sf, subnat_shapefile_sf)
    adj_mat_adm2 <- as(as.matrix(adj_list_adm2), "dgTMatrix")

    connections <- as.data.table(st_coordinates(st_centroid(subnat_shapefile_sf)))
    connections[, ID := 1:.N]
    connections[, tmp := 1]
    connections <- merge(connections, connections, by = "tmp", allow.cartesian = T)
    connections <- merge(connections, as.data.frame(adj_list_adm2), by.x = c("ID.x", "ID.y"), by.y = c("row.id", "col.id"))

    pdf(paste0(pipeline$outputdir, "/", reg, "_adm2_graph.pdf"))
    gg <- ggplot() +
      geom_sf(data = subnat_shapefile_sf, fill = NA, lwd = 0.1) +
      geom_segment(data = connections, aes(x = X.x, xend = X.y, y = Y.x, yend = Y.y), col = "blue") +
      geom_sf(data = st_centroid(subnat_shapefile_sf), col = "red")
    print(gg)
    dev.off()

  # setup adm1 adjacency graphs for besag effects
    subnat_shapefile_sf <- aggregate(subnat_shapefile_sf, by = list(subnat_shapefile_sf$ADM1_CODE), mean)
    subnat_shapefile_sf <- st_make_valid(subnat_shapefile_sf)
    subnat_shapefile_sf$ADM1_CODE <- as.integer(as.character(subnat_shapefile_sf$Group.1))
    subnat_shapefile_sf <- subnat_shapefile_sf[order(subnat_shapefile_sf$ADM1_CODE),]
    adj_list_adm1 <- st_intersects(subnat_shapefile_sf, subnat_shapefile_sf)
    adj_mat_adm1 <- as(as.matrix(adj_list_adm1), "dgTMatrix")

    connections <- as.data.table(st_coordinates(st_centroid(subnat_shapefile_sf)))
    connections[, ID := 1:.N]
    connections[, tmp := 1]
    connections <- merge(connections, connections, by = "tmp", allow.cartesian = T)
    connections <- merge(connections, as.data.frame(adj_list_adm1), by.x = c("ID.x", "ID.y"), by.y = c("row.id", "col.id"))

    pdf(paste0(pipeline$outputdir, "/", reg, "_adm1_graph.pdf"))
    gg <- ggplot() +
      geom_sf(data = subnat_shapefile_sf, fill = NA, lwd = 0.1) +
      geom_segment(data = connections, aes(x = X.x, xend = X.y, y = Y.x, yend = Y.y), col = "blue") +
      geom_sf(data = st_centroid(subnat_shapefile_sf), col = "red")
    print(gg)
    dev.off()

    rm(subnat_shapefile_sf, connections, gg)

  } else {
    adj_mat_adm1 <- NULL
    adj_mat_adm2 <- NULL
  }

} else {
  simple_raster3_adm1 <- NULL
  simple_raster3_adm2 <- NULL
  adj_mat_adm1 <- NULL
  adj_mat_adm2 <- NULL
  adm1_codes <- NULL
  adm2_codes <- NULL
}

## If using an adm1-specific NID random effect, change the nid column to combine with adm1
if (as.logical(pipeline$config_list$nid_res_by_adm1)) {
  df[, nid := paste(nid, timeby_ADM1_CODE)]
}

## if testing, we only keep test_pct of observations
if (pipeline$eval_conf("test")) {
  test_pct <- as.numeric(pipeline$config_list$test_pct)

  message(paste0(
    "Test option was set on and the test_pct argument was found at ",
    test_pct,
    "% \n\n                 ... keeping only ",
    round(nrow(df) * (test_pct / 100), 0), " random rows of data."
  ))
  set.seed(seed)
  increment_seed(seed)
  df <- df[sample(nrow(df), round(nrow(df) * (test_pct / 100), 0)), ]

  message("Also, making it so we only take 100 draws")
  samples <- 100
}

## for u5m in particular,  make sure indicator
## is properly named here, wont affect others
df[[pipeline$indicator]] <- df[[gsub(paste0("_age", age), "", pipeline$indicator)]]

## if there is another weight column, multiply it with weight now
if (pipeline$config_list$other_weight != "") {
  message(paste0(
    "Multiplying weight and ",
    pipeline$config_list$other_weight
  ))
  df[["weight"]] <- df[["weight"]] * df[[other_weight]]
}

## Some built in data checks that cause known problems later on
if (
  pipeline$config_list$indicator_family == "binomial" & any(
    df[, get(pipeline$indicator)] / df$N > 1
  )) {
  stop("You have binomial data where k > N. Check your data before proceeding")
}
if (any(df[["weight"]] %in% c(Inf, -Inf) | any(is.na(df[["weight"]])))) {
  stop(
    "You have illegal weights (NA,Inf,-Inf). Check your data before proceeding"
  )
}

# Pull Covariates ---------------------------------------------------------

## Define modeling space. In years only for now.
if (pipeline$config_list$yearload == "annual") {
  period_map <-
    make_period_map(
      modeling_periods = c(
        min(
          pipeline$config_list$year_list
        ):max(
          pipeline$config_list$year_list
        )
      )
    )
}
if (pipeline$config_list$yearload == "five-year") {
  period_map <-
    make_period_map(
      modeling_periods = seq(
        min(
          pipeline$config_list$year_list
        ), max(
          pipeline$config_list$year_list
        ),
        by = 5
      )
    )
}

## Make placeholders for covariates
cov_layers <- gbd_cov_layers <- NULL

## Pull all covariate bricks/layers
if (nrow(pipeline$fixed_effects_config) > 0) {
  message("Grabbing raster covariate layers")
  loader <- MbgStandardCovariateLoader$new(
    start_year = min(pipeline$config_list$year_list),
    end_year = max(pipeline$config_list$year_list),
    interval = as.numeric(pipeline$config_list$interval_mo),
    covariate_config = pipeline$fixed_effects_config
  )
  cov_layers <- loader$get_covariates(simple_polygon)
}

## Pull country level gbd covariates
if (nchar(pipeline$config_list$gbd_fixed_effects) > 0) {
  message("Grabbing GBD covariates")

  with_globals(
    new = list(modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version),
    gbd_cov_layers <- load_gbd_covariates(
      covs = trim(
        strsplit(pipeline$config_list$gbd_fixed_effects, "\\+")[[1]]
      ),
      measures = trim(
        strsplit(
          pipeline$config_list$gbd_fixed_effects_measures, "\\+"
        )[[1]]
      ),
      year_ids = pipeline$config_list$year_list,
      age_ids = pipeline$config_list$gbd_fixed_effects_age,
      template = cov_layers[[1]][[1]],
      modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version,
      simple_polygon = simple_polygon,
      interval_mo = pipeline$config_list$interval_mo,
      year_list = pipeline$config_list$year_list
    )
  )
}

## Combine all covariates
all_cov_layers <- c(cov_layers, gbd_cov_layers)

## Aggregate covariate raster by raster_agg factor. Using previously downsampled simple raster
## to accommodate resampling of categorical covariates.
if (as.integer(pipeline$config_list$raster_agg_factor) > 1) {

  # in order to ensure that all downsampled covariates have the same extents, make a template
  # from a world raster and extent of the simple polygon. Simple raster cannot be used as a template
  # for extent/resolution because the covariates need a buffer zone and are thus larger
  extent_template <- empty_world_raster()
  extent_template <- raster::aggregate(extent_template, fact = as.integer(pipeline$config_list$raster_agg_factor))
  extent_template <- raster::crop(extent_template, extent(simple_polygon))

  for (idx in 1:length(all_cov_layers)) {
    all_cov_layers[[idx]] <- downsample_covariate_raster(cov_raster = all_cov_layers[[idx]],
                                                         simple_raster = extent_template,
                                                         raster_agg_factor = pipeline$config_list$raster_agg_factor)
  }
  rm(idx)
}

## regenerate all fixed effects equation from the cov layers
all_fixed_effects <- paste(names(all_cov_layers), collapse = " + ")

## Make stacker-specific formulas where applicable
all_fixed_effects_brt <- all_fixed_effects

## Set Up Country Fixed Effects
if (pipeline$config_list$use_child_country_fes == TRUE | pipeline$config_list$use_inla_country_fes == TRUE) {
  message("Setting up country fixed effects")
  fe_gaul_list <- unique(c(
    gaul_convert(unique(df[, country]),
      shapefile_version =
        pipeline$config_list$modeling_shapefile_version
    ),
    gaul_list
  ))
  fe_template <- cov_layers[[1]][[1]]
  simple_polygon_list <- load_simple_polygon(
    gaul_list = fe_gaul_list,
    buffer = 0.4,
    subset_only = TRUE,
    shapefile_version = pipeline$config_list$modeling_shapefile_version
  )
  fe_subset_shape <- simple_polygon_list[[1]]
  admin_code_raster <- rasterize_check_coverage(
    fe_subset_shape, fe_template,
    field = "ADM0_CODE",
    link_table = pipeline$config_list$modeling_shapefile_version
  )
  admin_code_raster <- setNames(admin_code_raster, "gaul_code")
  admin_code_raster <- create_categorical_raster(admin_code_raster)

  ## update covlayers and add country fixed effects to the formula object
  all_cov_layers <- update_cov_layers(all_cov_layers, admin_code_raster)
  all_fixed_effects_cfes <- paste(all_fixed_effects,
    paste(names(admin_code_raster)[1:length(names(admin_code_raster))],
      collapse = " + "
    ),
    sep = " + "
  )

  ## update specific stacker formulas
  ## For now we just want country effects in BRT
  all_fixed_effects_brt <- all_fixed_effects_cfes
} else {
  admin_code_raster <- NULL
}

## Add these to the fixed effects if we want them in stacking
if (pipeline$config_list$use_child_country_fes == TRUE) {
  gaul_fes <- paste(
    names(admin_code_raster)[2:length(names(admin_code_raster))],
    collapse = " + "
  )
  all_fixed_effects <- paste(all_fixed_effects, gaul_fes, sep = " + ")
}

# Postamble ---------------------------------------------------------------

pipeline_postamble()
q("no")
