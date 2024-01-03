source("<<<< FILEPATH REDACTED >>>>/subnational_ts_plots.R")
source("<<<< FILEPATH REDACTED >>>>/time_series_data_xwalk.R")
source("<<<< FILEPATH REDACTED >>>>/most_recent_date.R")

make_ts_plots <- function(indicator, indicator_group, run_date, Regions, output_dir, shapefile_version = 'current', counts = F) {
  version = shapefile_version

  #Set up for counts file pulls and naming
  if(counts == T) cts <- 'c_' else cts <- ''

  # set Regions names
  region_names <- c(BFA = 'Burkina Faso', KEN = 'Kenya', NGA = 'Nigeria')[Regions]
  region_names[is.na(region_names)] <- Regions[is.na(region_names)]

  ## Get full indicator name

  indic <- c("indic_contra", "group_e", "group_d", "indic_unmet", "indic_demand", "indic_intent",
             "any_contra", "mod_contra", "need_contra", "intent_use")
  name <- c("Contraceptive prevalence", "Modern contraceptive prevalence", "Traditional contraceptive prevalence",
            "Unmet need for modern methods of contraception", "Demand satisfied with modern methods", "Intention to use contraception",
            "Any contraception use", "Modern contraception use", "Need for contraception", "Intention to use contraception")
  for (x in 1:length(indic)){
    if (indicator == indic[x]) full_name <- name[x]
  }

  # load collapsed input data
  in_dir <- "<<<< FILEPATH REDACTED >>>>"

  collapse_version <- most_recent_date("<<<< FILEPATH REDACTED >>>>")

  if(indicator %in% c("any_contra","mod_contra","need_contra","intent_use")){
    input_data <- readRDS(paste0("<<<< FILEPATH REDACTED >>>>/", indicator, "_data_resampled.rds"))
    flagss <- unique(input_data[,c("nid", "flag")]) %>% setnames("nid", "svy_id")
    ## create new dataset with pre-xwalked values
    input_pre <- input_data[, -c(7,9)]
    setnames(input_pre, c("value_original","N_original"), c(eval(indicator),"N"))
    input_data <- input_data[, -c(14,15)]
    classic <- FALSE
  } else{
    input_data <- readRDS(paste0("<<<< FILEPATH REDACTED >>>>/", indicator, "_data_resampled.rds"))
    flagss <- unique(input_data[,c("nid", "flag")]) %>% setnames("nid", "svy_id")
    classic <- TRUE
  }

  input_data[, nid := as.double(nid)]
  if (classic==F) input_pre[, nid := as.double(nid)]

  input_data[nid %in% input_data[, uniqueN(year), 'nid'][V1 > 2, nid], nid := 10000*nid + year]
  if (classic==F) input_pre[nid %in% input_pre[, uniqueN(year), 'nid'][V1 > 2, nid], nid := 10000*nid + year]


  # get aggregated estimates
  admin_data <- input_aggregate_admin(indicator, indicator_group, input_data = input_data,
                                      regions = Regions, shapefile_version = shapefile_version)


  if (classic==F) admin_data_pre <- input_aggregate_admin(indicator, indicator_group, input_data = input_pre,
                                      regions = Regions, shapefile_version = shapefile_version)


  # reset the polygon variable based on original input data (the function tries to guess based on
  # resampling weights, but gets this wrong if these are used to adjust N rather than as analytic
  # weights)
  input_data_poly <- input_data[, list(polygon = as.numeric(mean(point) < 1)), 'nid,country']
  input_data_poly[, polygon := as.factor(polygon)]
  input_data_poly <- merge(input_data_poly, get_location_code_mapping(shapefile_version = shapefile_version), by.x = 'country', by.y = 'ihme_lc_id')
  input_data_poly <- input_data_poly[, list(svy_id = nid, ADM0_CODE = ADM_CODE, polygon)]

  admin_data <- lapply(admin_data, function(d) {
    d[, polygon := NULL]
    d <- merge(d, input_data_poly)
    return(d)
  })

  if (classic==F)  { ##
    input_data_pre_poly <- input_pre[, list(polygon = as.numeric(mean(point) < 1)), 'nid,country']
    input_data_pre_poly[, polygon := as.factor(polygon)]
    input_data_pre_poly <- merge(input_data_pre_poly, get_location_code_mapping(shapefile_version = shapefile_version), by.x = 'country', by.y = 'ihme_lc_id')
    input_data_pre_poly <- input_data_pre_poly[, list(svy_id = nid, ADM0_CODE = ADM_CODE, polygon)]

    admin_data_pre <- lapply(admin_data_pre, function(d) {
      d[, polygon := NULL]
      d <- merge(d, input_data_pre_poly)
      return(d)
    })

    setnames(admin_data_pre$ad0, c("outcome","N"), c("outcome_pre","N_pre"))
    setnames(admin_data_pre$ad1, c("outcome","N"), c("outcome_pre","N_pre"))
    setnames(admin_data_pre$ad2, c("outcome","N"), c("outcome_pre","N_pre"))
    admin_data$ad0 <- merge(admin_data$ad0, admin_data_pre$ad0, by=c("svy_id","ADM0_CODE","source","point","ADM0_NAME","year","polygon"))
    admin_data$ad1 <- merge(admin_data$ad1, admin_data_pre$ad1, by=c("svy_id","ADM0_CODE","source","point","ADM0_NAME", "ADM1_NAME","ADM1_CODE","year","polygon"))
    admin_data$ad2 <- merge(admin_data$ad2, admin_data_pre$ad2, by=c("svy_id","ADM0_CODE","source","point","ADM0_NAME", "ADM1_NAME","ADM1_CODE","ADM2_NAME","ADM2_CODE","year","polygon"))
    admin_data$ad0 <- merge(admin_data$ad0, flagss, by=c("svy_id"))
    admin_data$ad1 <- merge(admin_data$ad1, flagss, by=c("svy_id"))
    admin_data$ad2 <- merge(admin_data$ad2, flagss, by=c("svy_id"))
  } else {
    ## Add in variables even if not captured, so plotting code works
    admin_data$ad0[,outcome_pre := outcome][,N_pre := N]
    admin_data$ad1[,outcome_pre := outcome][,N_pre := N]
    admin_data$ad2[,outcome_pre := outcome][,N_pre := N]
    admin_data$ad0 <- merge(admin_data$ad0, flagss, by=c("svy_id"))
    admin_data$ad1 <- merge(admin_data$ad1, flagss, by=c("svy_id"))
    admin_data$ad2 <- merge(admin_data$ad2, flagss, by=c("svy_id"))
  }

  # get aggregated preds
  admin_preds <- lapply(0:2, function(d) {
    fread(paste0("<<<< FILEPATH REDACTED >>>>/","unraked_summary.csv"))
  })
  names(admin_preds) <- paste0('ad', 0:2)

  # run subnational_ts_plots() function
  subnational_ts_plots(ad0_df = admin_preds$ad0, ad1_df = admin_preds$ad1, ad2_df = admin_preds$ad2,
                       ad0_data = admin_data$ad0, ad1_data = admin_data$ad1, ad2_data = admin_data$ad2,
                       ind_title = full_name, highisbad = T,
                       ad0_map_regions = Regions, ad0_map_region_titles = region_names,
                       out_dir = output_dir, plot_data = T, verbose = T, shapefile_version = shapefile_version)

  return(paste('Plots saved to:', output_dir))
}
