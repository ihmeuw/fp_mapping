require(data.table)
require(raster)
require(rgeos)
require(maptools)
require(plyr)
require(boot)
require(ggplot2)
require(RColorBrewer)
require(grid)
require(gridExtra)
require(rgdal)
require(sf)

load_data_and_estimates <- function(indicator, indicator_group, run_date, rgn, shapefile_version = shapefile_version) {

  ## load arguments from the config
  output_dir <- '<<<< FILEPATH REDACTED >>>>'
  model_dir <- '<<<< FILEPATH REDACTED >>>>'

  config <- fread(paste0(output_dir, 'config.csv'))
  year_list <- eval(parse(text = config[V1 == 'year_list', V2]))
  regions <- eval(parse(text = config[V1 == 'Regions', V2]))
  use_stacking_covs <- eval(parse(text = config[V1 == 'use_stacking_covs', V2]))
  child_model_names <- c("gam","ridge","gbm")

  ## load stackers & predictions
  if (use_stacking_covs) {
    stackers <- lapply(regions, function(reg) {
      # load(paste0(model_dir, run_date, '_bin0_', reg, '_0.RData'))
      # load(paste0(model_dir, indicator, '_bin0_', reg, '_0.RData'))
      # load(dag_path)
      # ## desired objects come from the 03_PM_PrepForFitting script
      node_table <- dag$get_node_id_table(job_str = "03", region = reg, indicator = indicator)

      to_load<- c("df", "simple_raster", "mesh_s", "mesh_t", "cov_list",
                  "child_model_names", "all_fixed_effects", "period_map", "simple_raster2")
      node_table$obj_store[[1]]$load_objects(names = to_load)
      cov_list[child_model_names]
    })
    print(stackers)
    stackers <- lapply(child_model_names, function(mod) {
      if (length(regions) == 1) stackers[[1]][[mod]] else do.call(merge, lapply(stackers, function(s) s[[mod]]))
    })
  }

  unraked <- brick(paste0(output_dir, indicator, '_mean_raster.tif'))
  # raked <- brick(paste0(output_dir, indicator, '_mean_raked_raster.tif'))

  if (use_stacking_covs) {
    pred <- c(stackers, unraked)
    names(pred) <- c(child_model_names, 'unraked')
    rm(stackers, unraked)
  } else {
    pred <- c(unraked)
    names(pred) <- c('unraked')
    rm(unraked)
  }

  ## load the input data and doctor NIDs for better grouping when plotting
  source("<<<< FILEPATH REDACTED >>>>/prep_functions.R")
  locs <- get_location_code_mapping(shapefile_version = shapefile_version)
  locs <- locs[ADM_CODE %in% get_adm0_codes(regions, shapefile_version = shapefile_version), list(name = loc_name, country = ihme_lc_id, ADM0_CODE = ADM_CODE)]
  data <- fread(paste0("<<<< FILEPATH REDACTED >>>>", "/", "input_data_bin0_", rgn, "_0.csv"))

  data <- merge(data, locs, by = 'country')

  data <- data[, list(ADM0_CODE, iso3 = country, country = name, nid, source, year, latitude, longitude, N = N*weight, indicator = get(indicator)/N, point)]
  names(data)[names(data) == 'indicator'] <- indicator
  setnames(data, "point", "type")
  data[, nid2 := paste0(nid, '_', ADM0_CODE)]
  data[, type := ifelse(type == 1, "Point data", "Polygon data")]
  data[, type := factor(type, levels = intersect(c("Point data", "Polygon data"), unique(type)))]

  ## extracted fitted values and calculate residuals
  for (p in names(pred)) {
    data[, paste0('pred_', p) := raster::extract(pred[[p]][[which(year_list == year[1])]], data.frame(longitude, latitude)), by = 'year']
    data[, paste0('resid_', p) := get(paste0('pred_', p)) - get(indicator)]
  }

  ## load the admin0 shape file
  admin0 <- readRDS(get_admin_shapefile(
    admin_level = 0,
    raking = FALSE,
    suffix = ".rds",
    version = pipeline$config_list$modeling_shapefile_version
  ))
  admin0 <- admin0[admin0@data$ADM0_CODE %in% data$ADM0_CODE, ]

  # return data, predictions, and admin0 shape file
  all <- list(data = data, pred = pred, admin0 = admin0)
  return(all)
}

map_predictions_with_data <- function(data, pred, poly, limits) {
  p <- ggplot() +
    geom_raster(data = pred, aes(x = x, y = y, fill = value)) +
    annotation_map(poly, fill = 'NA', color = 'black', size = 0.05) +
    geom_point(data = data, aes(x = longitude, y = latitude, fill = get(indicator)), stroke = 0.1, shape = 21) +
    scale_fill_gradientn(colors = brewer.pal(9, 'RdPu'), limits = limits, na.value = brewer.pal(9, 'RdPu')[9], name = 'Prev.') +
    coord_equal(xlim = pred[, range(x)], ylim = pred[, range(y)]) +
    labs(x = '', y = '') +
    guides(fill = guide_colorbar(nbin = 100, barwidth = 0.25)) +
    theme_bw(base_size = 8) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  if ('model' %in% names(data)) p <- p + facet_grid(~ model)
  return(p)
}

map_residuals <- function(data, pred, poly, limits) {
  p <- ggplot() +
    annotation_map(poly, fill = 'NA', color = 'black', size = 0.05) +
    geom_point(data = data, aes(x = longitude, y = latitude, fill = resid), stroke = 0.1, shape = 21) +
    scale_fill_gradientn(colors = brewer.pal(11, 'BrBG')[c(1:4, 6, 8:11)],
                         values = c(0, 0.25, .4, .45, .5, .55, .6, 0.75, 1), limits = limits, name = 'Resid. ') +
    coord_equal(xlim = pred[, range(x)], ylim = pred[, range(y)]) +
    labs(x = '', y = '') +
    guides(fill = guide_colorbar(nbin = 100, barwidth = 0.25)) +
    theme_bw(base_size = 8) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  if ('model' %in% names(data)) p <- p + facet_grid(~ model)
  return(p)
}

scatter_plot_data_and_estimates <- function(data, limits) {
  # calc correlation
  if (data[!is.na(pred), .N] > 0) {
    cor <- data[!is.na(pred),]
    if (!'model' %in% names(cor)) cor[, model := NA]
    names(cor)[names(cor) == indicator] <- 'indicator'
    names(cor)[names(cor) == paste0(indicator, '_orig')] <- 'indicator_orig'
    cor <- cor[, list(cor = cor(indicator, pred), cor_orig = cor(indicator_orig, pred)), by = 'model']
    cor[, c('cor', 'cor_orig') := lapply(.SD, format, digits = 2, nsmall = 2), .SDcols = c('cor', 'cor_orig')]
  } else {
    cor <- NULL
  }

  # make initial plot
  p <- ggplot() +
    geom_point(data = data, aes(x = get(indicator), y = pred), stroke = 0.25, shape = 21) +
    geom_abline(intercept = 0, slope = 1) +
    coord_equal(xlim = limits, ylim = limits) +
    labs(x = 'Data', y = 'Estimate') +
    theme_bw(base_size = 8)

  # add correlation, if available
  if (!is.null(cor)) p <- p + geom_text(data = cor, aes(label = cor), x = limits[2], y = 0, hjust = 1)

  if ('model' %in% names(data)) p <- p + facet_grid(~ model)

  return(p)
}

data_and_estimates_maps <- function(indicator, indicator_group, run_date, rgn) {

  ## Load data
  message('Load data...')
  all <- load_data_and_estimates(indicator, indicator_group, run_date, rgn, shapefile_version = shapefile_version)
  data <- all$data
  pred <- all$pred
  admin0 <- all$admin0

  ## Make plots by source/year
  message('Make plots...')
  output_dir <- "<<<< FILEPATH REDACTED >>>>"
  dir.create(paste0(output_dir, 'diagnostic_plots/'), showWarnings = F)
  pdf(paste0(output_dir, 'diagnostic_plots/data_and_estimates_maps_', rgn,'.pdf'), width = 14, height = 8)

  for (this_country in data[order(country), unique(ADM0_CODE)]) {
    message(paste0('  ', data[ADM0_CODE == this_country, country[1]]))

    ## subset data & predictions to just this country
    country_poly <- admin0[admin0@data$ADM0_CODE == this_country, ]
    suppressMessages(country_poly_df <- fortify(country_poly))

    country_pred <- rbindlist(lapply(names(pred), function(mod) {
      temp <- mask(crop(pred[[mod]], extent(country_poly)), country_poly)
      temp <- data.table(rasterToPoints(temp))
      temp <- melt(temp, id.vars = c('x', 'y'), variable.name = 'year')
      temp[, year := as.numeric(year) + min(year_list) - 1]
      temp[, model := mod]
      temp
    }))
    country_pred[, model := factor(model, levels = names(pred))]

    country_data <- melt(data[ADM0_CODE == this_country, ],
                         id.vars = c('ADM0_CODE', 'country', 'nid', 'nid2', 'source', 'site', 'year',
                                     'latitude', 'longitude', 'N', paste0(indicator, '_orig'), indicator, 'type'),
                         measure.vars = patterns('pred_', 'resid_'), value.name = c('pred', 'resid'),
                         variable.name = 'model')
    levels(country_data$model) <- names(pred)

    ## loop over NIDs/years in this country
    for (this_nid in country_data[order(type, year, source, nid), unique(nid2)]) {

      ## subset relevant data & predictions to just this nid
      source_data <- country_data[nid2 == this_nid, ]
      source_years <-  source_data[, sort(unique(year))]
      if (length(source_years) == 1) {
        source_pred <- country_pred[year == source_years, ]
      } else {
        source_pred <- country_pred[year %in% source_years, list(value = mean(value)), by = 'x,y,model']
      }

      ## map all predictions with data
      limits <- c(0, round_any(source_pred[, quantile(value, 0.99, na.rm = T)], 0.01, ceiling))
      p1 <- map_predictions_with_data(source_data, source_pred, country_poly_df, limits)

      ## map all residuals
      limits <- c(-1, 1) * round_any(source_data[, max(abs(resid), na.rm = T)], 0.01, ceiling)
      p2 <- map_residuals(source_data, source_pred, country_poly_df, limits)

      ## scatter plots of data & estimates
      limits <- c(0, source_data[, max(pred, get(indicator), get(paste0(indicator, '_orig')), na.rm = T)])
      p3 <- scatter_plot_data_and_estimates(source_data, limits)

      ## combine all plots
      title <- source_data[, paste0(country, ' - ', paste(source_years, collapse = '-'), ' ', source, '\ntype: ', type, '; nid: ', nid)][1]
      grid.arrange(p1, p2, p3, ncol = 1, top = textGrob(title, gp = gpar(fontsize = 12)))

      rm(source_years, source_data, source_pred)
    }

    rm(country_poly, country_poly_df, country_data, country_pred)
  }
  dev.off()

  return(paste0('Plots saved: ', output_dir, 'diagnostic_plots/data_and_estimates_maps_', rgn, '.pdf'))
}

data_and_estimates_maps_simplified <- function(indicator, indicator_group, run_date, rgn) {

  ## Load data
  message('Load data...')
  all <- load_data_and_estimates(indicator, indicator_group, run_date, rgn, shapefile_version = shapefile_version)
  data <- all$data
  pred <- all$pred[['unraked']] # keep unraked predictions only
  admin0 <- all$admin0

  ## Make plots by country
  message('Make plots...')
  output_dir <- "<<<< FILEPATH REDACTED >>>>"
  dir.create(paste0(output_dir, 'diagnostic_plots/'), showWarnings = F)
  pdf(paste0(output_dir, 'diagnostic_plots/data_and_estimates_maps_simplified_', rgn,'.pdf'), width = 14, height = 8)

  for (this_country in data[order(country), unique(ADM0_CODE)]) {
    message(paste0('  ', data[ADM0_CODE == this_country, country[1]]))

    ## subset data & predictions to just this country
    country_poly <- admin0[admin0@data$ADM0_CODE == this_country, ]
    suppressMessages(country_poly_df <- fortify(country_poly))

    country_pred <- mask(crop(pred, extent(country_poly)), country_poly)
    country_pred <- data.table(rasterToPoints(country_pred))
    country_pred <- melt(country_pred, id.vars = c('x', 'y'), variable.name = 'year')
    country_pred[, year := as.numeric(year) + min(year_list) - 1]
    if (indicator != "male_circumcision") {
      country_data_names <- c('country', 'nid', 'nid2', 'source', 'site', 'year', 'latitude', 'longitude',
                              'N', 'type', paste0(indicator, '_orig'), indicator, 'pred_unraked', 'resid_unraked')
    } else {
      country_data_names <- c('country', 'nid', 'nid2', 'source', 'year', 'latitude', 'longitude',
                              'N', 'type', paste0(indicator, '_orig'), indicator, 'pred_unraked', 'resid_unraked')
    }
    country_data <- data[ADM0_CODE == this_country, country_data_names, with=FALSE]
    names(country_data)[names(country_data) == 'pred_unraked'] <- 'pred'
    names(country_data)[names(country_data) == 'resid_unraked'] <- 'resid'


    ## loop over NIDs/years in this country
    plots <- lapply(country_data[order(type, year, source, nid), unique(nid2)], function(this_nid) {

      ## subset relevant data & predictions to just this nid
      source_data <- country_data[nid2 == this_nid, ]
      source_years <-  source_data[, sort(unique(year))]
      if (length(source_years) == 1) source_pred <- country_pred[year == source_years, ] else source_pred <- country_pred[year %in% source_years, list(value = mean(value)), by = 'x,y']

      ## map all predictions with data
      limits <- c(0, round_any(country_pred[, quantile(value, 0.99, na.rm = T)], 0.01, ceiling))
      p1 <- map_predictions_with_data(source_data, source_pred, country_poly_df, limits = limits)

      ## scatter plots of data & estimates
      limits <- c(0, country_data[, max(pred, get(indicator), get(paste0(indicator, '_orig')), na.rm = T)])
      p3 <- scatter_plot_data_and_estimates(source_data, limits)

      ## combine all plots
      title <- source_data[, paste0(paste(source_years, collapse = '-'), ' ', source, '\ntype: ', type, '; nid: ', nid)][1]
      arrangeGrob(p1, p3, nrow = 1, top = textGrob(title, gp = gpar(fontsize = 8)))
    })

    ## plot
    groups <- split(plots, rep(1:ceiling(length(plots)/9), each = 9, length.out = length(plots)))
    for (g in groups) {
      grid.arrange(grobs = g, layout_matrix = matrix(1:9, nrow = 3, byrow = T),
                   top = textGrob(data[ADM0_CODE == this_country, country[1]], gp = gpar(fontsize = 12)))
    }
    rm(country_poly, country_poly_df, country_data, country_pred, groups)
  }
  dev.off()

  return(paste0('Plots saved: ', output_dir, 'diagnostic_plots/data_and_estimates_maps_simplified_', rgn, '.pdf'))
}
