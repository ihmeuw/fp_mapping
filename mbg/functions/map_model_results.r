####################################################################################################
## Description:   Make maps of estimates in Africa for specified year_list and year pairs.
##
## Inputs:        mean rasters
##                adm0 and adm1 estimates
##                country outlines, lakes, and population mask
##
## Output:        PDF of maps ("<<<< FILEPATH REDACTED >>>>/[indicator]_[unraked/raked]_mean.pdf")
####################################################################################################

require(data.table)
require(rgeos)
require(rgdal)
require(maptools)
require(ggplot2)
require(RColorBrewer)
require(scales)
require(gridExtra)
require(raster)
require(dplyr)

## load_map_annotations ----------------------------------------------------------------------------
load_map_annotations <- function(reg) {

  ## Base shapefile (country outlines)
  adm0 <- shapefile(get_admin_shapefile(0, version = shapefile_version))
  adm0 <- as(adm0, "sf")
  adm0 <- adm0[adm0$ADM0_CODE %in% get_adm0_codes(reg), ]

  ## Lakes
  lakes <- raster("<<<< FILEPATH REDACTED >>>>/lakes_all_2_reg.tif")
  lakes <- raster::mask(lakes, adm0)
  lakes <- data.table(rasterToPoints(lakes))
  setnames(lakes, c("long", "lat", "lakes"))

  ## Population mask
  mask <- raster("<<<< FILEPATH REDACTED >>>>/mask_master_reg.tif")
  mask <- raster::mask(mask, adm0)
  mask <- data.table(rasterToPoints(mask))
  setnames(mask, c("long", "lat", "mask"))

  return(list(adm0 = adm0, lakes = lakes, mask = mask))
}

## load_map_results --------------------------------------------------------------------------------
load_map_results <- function(indicator, indicator_group, run_date, reg, type, raked, year_list, geo_levels) {

  ## set the input directory
  maps_path <- '<<<< FILEPATH REDACTED >>>>'

  ## raster estimates
  if ("raster" %in% geo_levels) {
    raster <- brick(paste0(maps_path, "/", indicator, "_", reg, if (raked) "_raked_" else "_", type, "_raster.tif"))
    raster <- data.table(rasterToPoints(raster))
    raster <- melt(raster, id.vars = c("x", "y"), value.name = "outcome", variable.name = "year")
    raster[, year := as.integer(year) + pipeline$config_list$year_list[1] - 1]
    raster <- raster[year %in% year_list, ]
    setnames(raster, c("x", "y"), c("long", "lat"))
    setkey(raster, long, lat, year)
  }

  ## admin1 estimates and shape file
  if ("admin1" %in% geo_levels) {
    pred <- fread(paste0(maps_path, "/pred_derivatives/admin_summaries/", indicator, "_admin_1_", if (raked) "raked" else "unraked", "_summary.csv"))
    pred <- pred[region %in% reg & year %in% year_list, c("ADM1_CODE", "year", type), with = F]
    setnames(pred, type, "outcome")

    admin1 <- shapefile(get_admin_shapefile(1, version = shapefile_version))
    admin1 <- as(admin1, "sf")
    admin1$ADM1_CODE <- as.integer(as.character(admin1$ADM1_CODE))
    admin1 <- merge(admin1, pred, by = "ADM1_CODE")
  }

  ## admin2 estimates and shape file
  if ("admin2" %in% geo_levels) {
    pred <- fread(paste0(maps_path, "/pred_derivatives/admin_summaries/", indicator, "_admin_2_", if (raked) "raked" else "unraked", "_summary.csv"))
    pred <- pred[region %in% reg & year %in% year_list, c("ADM2_CODE", "year", type), with = F]
    setnames(pred, type, "outcome")

    admin2 <- shapefile(get_admin_shapefile(2, version = shapefile_version))
    admin2 <- as(admin2, "sf")
    admin2$ADM2_CODE <- as.integer(as.character(admin2$ADM2_CODE))
    admin2 <- merge(admin2, pred, by = "ADM2_CODE")
  }

  ## combine and return all estimates
  pred <- mget(geo_levels)
  return(pred)
}

## calc_diff_map -----------------------------------------------------------------------------------
calc_diff_map <- function(pred, diff_year_list) {
  diff <- lapply(names(pred), function(g) {
    rbindlist(lapply(diff_year_list, function(y) {
      temp <- pred[[g]][year %in% y, ]
      temp <- temp[, list(outcome = outcome[year == y[2]] - outcome[year == y[1]]), by=setdiff(names(temp), c("outcome", "year"))]
      temp[, year_list := paste(y, collapse="-")]
      temp
    }))
  })
  names(diff) <- names(pred)
  return(diff)
}

## plot_map ----------------------------------------------------------------------------------------
plot_map <- function(map_data, annotations, title, limits, legend_colors, legend_title) {

  ## Enforce limits
  start_range <- range(map_data$outcome, na.rm = T)
  map_data$plot_var <- pmax(limits[1], pmin(limits[2], map_data$outcome))

  ## Create breaks
  breaks <- pretty(limits, 5)
  if (limits[1] < 0 & limits[2] > 0) breaks <- sort(unique(c(0, breaks)))
  if (breaks[length(breaks)] > limits[2]) breaks[length(breaks)] <- limits[2]

  ## Create labels
  labels <- format(breaks, nsmall = 2)
  if (min(limits) >= 0) divider <- "-" else divider <- " to "
  if (start_range[1] < min(breaks)) {
    labels[1] <- paste0(format(floor(100*start_range[1])/100, nsmall=2), divider, labels[1])
  }
  if (start_range[2] > max(breaks)) {
    labels[length(labels)] <- paste0(labels[length(labels)], divider, format(ceiling(100*start_range[2])/100, nsmall=2))
  }

  ## Plot the base map (this is what shows in places with no estimates and no mask)
  gg <- ggplot() +
    geom_sf(data = annotations$adm0, fill = "gray90", color = "gray90")

  ## Plot predictions
  if (class(map_data)[1] == "data.table") {
    gg <- gg + geom_raster(data = map_data, aes(x = long, y = lat, fill = plot_var))
  } else {
    gg <- gg + geom_sf(data = map_data, aes(fill = plot_var), color = "transparent")
  }

  ## Plot mask, lakes, and adm boarders
  gg <- gg +
    annotate(geom = "raster", x = annotations$mask$long, y = annotations$mask$lat, fill = "gray70") +
    annotate(geom = "raster", x = annotations$lakes$long, y = annotations$lakes$lat, fill = "lightblue") +
    geom_sf(data = annotations$adm0, color = "black", fill = NA, lwd = 0.1) +
    labs(x = "", y = "", title = title) +
    guides(fill = guide_colorbar(barwidth = 1.3, barheight = 11.5))

  ## Scales
  gg <- gg +
    scale_fill_gradientn(colors = legend_colors, limits = range(breaks),
                         breaks = breaks, labels = labels, name = legend_title)

  ## Themes & coords
  gg <- gg +
    theme_classic(base_size = 10) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.title = element_text(family = "Helvetica", hjust = 0.5, size = 10),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.background = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank()) +
    coord_sf(expand = F)

  return(gg)
}

## map_model_results -------------------------------------------------------------------------------
map_model_results <- function(indicator,
                              indicator_group,
                              run_date,
                              reg,
                              type = "mean",
                              raked = FALSE,
                              lvl_year_list = c(2000, 2010, 2018, 2020),
                              lvl_colors = c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177"),
                              lvl_limits = c(0.01, 0.99),
                              diff_year_list = list(c(2000, 2019), c(2000, 2020)),
                              diff_colors = c("#003C30", "#01665E", "#35978F", "#80CDC1", "#F6E8C3", "#DFC27D", "#BF812D", "#8C510A", "#543005"),
                              diff_limits = c(-0.10, 0.10),
                              include_diff = TRUE,
                              limits_type = "quantile",
                              geo_levels = c("raster", "admin1", "admin2"),
                              plot_by_year = TRUE,
                              plot_combined = TRUE,
                              file_type = "pdf") {

  ## Quick argument checks
  if (!limits_type %in% c("absolute", "quantile")) stop("limits_type must be 'absolute' or 'quantile'")
  if (length(lvl_limits) != 2 | length(diff_limits) != 2) stop("lvl_limits & diff_limits must both be length 2")
  if (sum(!geo_levels %in% c("raster", "admin1", "admin2")) > 0) stop("geo_levels can only include 'raster', 'admin1', and 'admin2'")
  if (!file_type %in% c("pdf", "png")) stop("file_type must be 'pdf' or 'png'")
  if (!type %in% c("mean", "cirange", "cfb")) stop("type must be 'mean', 'cirange', or 'cfb'")

  ## Create output directory
  out_dir <- paste0("<<<< FILEPATH REDACTED >>>>/results_maps/")
  dir.create(out_dir, showWarnings = F)

  ## Load data
  message("Loading data")
  annotations <- load_map_annotations(reg)
  pred <- load_map_results(indicator, indicator_group, run_date, reg, type, raked,
                           unique(c(lvl_year_list, unlist(diff_year_list))), geo_levels)

  ## Make level maps
  message("Make levels maps")
  if (limits_type == "quantile") {
    lvl_limits <- quantile(unlist(lapply(pred, function(x) x$outcome)), probs = lvl_limits, na.rm = T)
    lvl_limits <- c(plyr::round_any(lvl_limits[1], 0.01, floor), plyr::round_any(lvl_limits[2], 0.01, ceiling))
  }

  legend_title <- c(mean = "Prev.", cirange = "UI range", cfb = "CFB")[type]

  for (g in names(pred)) {
    message(paste0("...", g))

    # make maps and plot by year
    plot_list <- lapply(lvl_year_list, function(y) {
      message(paste0("......", y))
      map_data <- pred[[g]]
      map_data <- map_data[map_data$year == y, ]
      gg <- plot_map(map_data = map_data, annotations = annotations, title = y,
                     legend_title = legend_title, limits = lvl_limits, legend_colors = lvl_colors)

      if (plot_by_year) {
        file_name <- paste0(out_dir, indicator, "_", type, "_", if (raked) "raked" else "unraked", "_",  g, "_", y, "_", reg, ".", file_type)
        if (file_type == "pdf") pdf(file_name, height = 14, width = 14)
        if (file_type == "png") png(file_name, height = 13, width = 14, units = "in", res = 1200)
        plot(gg)
        dev.off()
      }

      return(gg)
    })

    # plot combined
    if (plot_combined & length(lvl_year_list) > 1) {
      message("......combined")
      file_name <- paste0(out_dir, indicator, "_", type, "_", if (raked) "raked" else "unraked", "_",  g, "_combined_", reg, ".", file_type)
      gg <- ggpubr::ggarrange(plotlist = plot_list, common.legend = T, legend = "right")
      ggsave(file_name, plot = gg, dev = file_type, width = 16, height = 9, units = "in")
    }

    rm(plot_list)
  }

  return("Maps saved!")
}
