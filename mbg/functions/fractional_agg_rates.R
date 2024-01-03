#' Fractional aggregation of a rates cell pred
#' @title Fractional aggregation of a rates cell pred
#' @description This function:
#' \enumerate{
#' \item Takes a rates cell_pred
#' \item Links it to a fractional raking and aggregation link table
#' \item Rakes the rates based on the raking factors above
#' \item Saves raked rates cell_pred
#' \item Adds population per fractional cell
#' \item Rakes that population to ensure matching to GBD using pop raking factors
#' \item Uses that population to create a counts cell_pred
#' \item Saves that raked counts cell_pred
#' \item Aggregates the cell_pred to ADM 0, 1, 2 levels
#' \item Saves aggregations at ADM 0,1,2 levels, raked and unraked, rates and counts
#' }
#' @param cell_pred cell_pred object from mbg models.  Each cell must have a rate in it.  This needs to be unraked
#' @param simple_raster the simple raster that the cell_pred is based on
#' @param simple_polygon the simple polygon that the cell_pred is based on
#' @param pixel_id list of the pixels in the simple raster that have non na values
#' @param shapefile_version which shapefile geographies are being used
#' @param reg the modeling region
#' @param pop_measure the worldpop agegroup on which the model is built
#' @param year_list the modeled years
#' @param use_intermediate_years Boolean to indicate whether or not to rake to intermediate years. Default: TRUE
#' @param interval_mo the time in months between the modeled years
#' @param rake_subnational a logical value indicating the use of subnational raking targets or not
#' @param sharedir Path to sharedir
#' @param run_date model run date string
#' @param indicator modeled indicator
#' @param main_dir Path to main_dir
#' @param age age holdout group, assumed to be 0 but left flexible
#' @param holdout n fold hold out group, assumed to be 0 but left flexible
#' @param countries_not_to_subnat_rake character vector of iso3 codes for countries not to subnationally rake
#' @param raster_agg_factor int. 1 corresponds to 5km raster resolution, 2 corresponds to 10km resolution.
#' No other resolutions are supported at this time. Set in the modelling config file
#' @param return_objects Return cell pred and raking factors into environments? Default: FALSE
#' @param custom_output_folder Get the rake factors from this folder, and also save the Rdata files here. Default: NULL
#' @param rake_method if set to "logit" creates raking factors in logit space, otherwise assumes linear raking
#' @param use_pop_draws Boolean, use custom population draws to aggregate? Returned by
#' `fractional_rake_rates`, see `custom_population_function` documentation there for more info.
#'
#' @return automatically saves unraked and raked versions admin aggregations of both rates and counts,
#' as well as raked cell_pred objects of both rates and counts. If desired, one can also return raked_cell_pred
#' and the raking factors into environment.
#'
#' @note the raked cell_pred objects cannot be used to create raked aggregation
#' @note you need to start with an unraked cell_pred, link it, rake it, then aggregate it.
#' @export
#'
#' @concept fractional_raking
fractional_agg_rates <- function(cell_pred = cell_pred,
                                 simple_raster = simple_raster,
                                 simple_polygon = simple_polygon,
                                 pixel_id = pixel_id,
                                 shapefile_version = shapefile_version,
                                 reg = reg,
                                 pop_measure = pop_measure,
                                 year_list = year_list,
                                 use_intermediate_years = TRUE,
                                 interval_mo = interval_mo,
                                 rake_subnational = rake_subnational,
                                 sharedir = sharedir,
                                 run_date = run_date,
                                 indicator = indicator,
                                 main_dir = main_dir,
                                 rake_method = "linear",
                                 age = 0,
                                 holdout = 0,
                                 return_objects = FALSE,
                                 countries_not_to_subnat_rake = countries_not_to_subnat_rake,
                                 raster_agg_factor,
                                 custom_output_folder = NULL,
                                 use_pop_draws = FALSE) {
  # setting a reference for the number of draws
  ndraws <- ncol(cell_pred)
  region <- reg

  if (!(rake_method %in% c("linear", "logit"))) {
    stop("rake_method arg to fractional_agg_rates must be either 'linear' or 'logit")
  }
  # load the cell id to admin units link

  link_table <- get_link_table(simple_raster, shapefile_version = shapefile_version, raster_agg_factor)

  # collect and load the population data
  covdt <- load_populations_cov(reg, pop_measure, measure = "count", simple_polygon, simple_raster, year_list, interval_mo, pixel_id = pixel_id, raster_agg_factor = raster_agg_factor)

  if (!use_intermediate_years) {
    print("Subsetting to supplied years only")
    covdt <- covdt[year %in% year_list]
  }

  # Prepping the cell_pred and link table to be linked and then merging them
  link <- prep_link_table(
    link_table = link_table,
    simple_raster = simple_raster,
    pixel_id = pixel_id
  )

  cell_ids <- link_table[[2]]

  # getting the connector for sub-national raking
  connector <- get_gbd_locs(
    rake_subnational = rake_subnational,
    reg = reg,
    shapefile_version = shapefile_version
  )

  # getting the connector for sub-national raking
  nat_connector <- get_gbd_locs(
    rake_subnational = F,
    reg = reg,
    shapefile_version = shapefile_version
  )

  # merge the connector on to the link table
  link <- sub_nat_link_merge(
    rake_subnational,
    link,
    connector,
    nat_connector,
    countries_not_to_subnat_rake
  )

  # set cell pred as a data table, and rename things
  cell_pred <- prep_cell_pred(
    cell_pred = cell_pred,
    cell_ids = cell_ids,
    pixel_id = pixel_id,
    covdt = covdt
  )

  # merge on the link
  cell_pred <- merge(link, cell_pred, by.x = "ID", by.y = "cell_id", allow.cartesian = TRUE)


  # adding the raking factors and scaling the populations

  message("adding raking factors")
  # convert to fractional population
  cell_pred <- cell_pred[, pop := pop * area_fraction]

  scalars <- read.csv(file = ifelse(is.null(custom_output_folder),
    paste0(sharedir, "<<<< FILEPATH REDACTED >>>>"),
    paste0(custom_output_folder,"<<<< FILEPATH REDACTED >>>>")
  ))

  # add back to the cell_pred as a population rf
  cell_pred[, mergeI := .I]
  cell_pred <- merge(cell_pred, scalars, by = c("location_id", "year"))
  setorder(cell_pred, mergeI)
  cell_pred[, mergeI := NULL]

  ## load the raking factors
  path_addin <- if(age != 0 | holdout != 0) paste0("_bin", age, "_", holdout) else ""
  fractional_rf <- read.csv(file = ifelse(is.null(custom_output_folder),
    paste0(sharedir, "<<<< FILEPATH REDACTED >>>>"),
    paste0(custom_output_folder, "<<<< FILEPATH REDACTED >>>>")
  ))

  ## merge them onto the cell_pred
  cell_pred[, mergeI := .I]
  cell_pred <- merge(cell_pred, fractional_rf, by = c("location_id", "year"))
  setorder(cell_pred, mergeI)
  cell_pred[, mergeI := NULL]


  # creating a raked rates cell_pred (this happens first because once we go to counts in the cell_pred we can't do back without loading a fresh copy)
  message("creating a raked rates cell_pred")
  # rake rates
  overs <- paste0("V", 1:ndraws)

  if (rake_method == "linear") {
    cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) * rf)]
  } else {
    cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) invlogit(logit(get(x)) + rf))]
  }

  # multiply the cell_pred by the area fraction for the dedupe function (so that each cell will add to 1 and the constituent rates are weighted by area)
  cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) * area_fraction)]

  raked_cell_pred <- dedupe_linked_cell_pred(cell_pred, overs)

  # Save raked rates cell_pred object
  save(raked_cell_pred, file = ifelse(is.null(custom_output_folder),
    paste0(
      sharedir,"<<<< FILEPATH REDACTED >>>>"
    ),
    paste0(
      custom_output_folder, "<<<< FILEPATH REDACTED >>>>"
    )
  ))


  # SPACE!!!!!
  if (!return_objects) {
    raked_cell_pred <- NULL
    gc()
  }

  # un do the area fraction (so that you can use this cell pred again)
  overs <- paste0("V", 1:ndraws)
  cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) / area_fraction)]

  # creating a raked counts cell_pred
  message("creating a raked counts cell_pred")
  # rake fractional populations
  cell_pred$pop_raked <- 0
  cell_pred <- cell_pred[, pop_raked := pop * pop_scalar]
  cell_pred$pop <- NULL

  if(use_pop_draws) {
    if (!is.null(custom_output_folder)) {
      custom_pop_dt <- readRDS(paste0(custom_output_folder,"<<<< FILEPATH REDACTED >>>>"))
    } else {
      custom_pop_dt <- readRDS(paste0(sharedir,"<<<< FILEPATH REDACTED >>>>"))
    }

    cell_pred <- cbind(cell_pred, custom_pop_dt)
    cell_pred$pop_raked <- NULL
  }

  message("converting from prevalence to counts")
  # set the variables to aggregate
  overs <- paste0("V", 1:ndraws)
  if(use_pop_draws) {
    # convert to counts, e.g. V1 * P1, v2 * P2
    cell_pred <- cell_pred[, (overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) * get(paste0("P", x)))]
    # NA out population draws where the pixel value is NA (to prevent weirdness with denominators)
    cell_pred[is.na(V1), (paste0("P", 1:ndraws)) := NA]
  } else {
    cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) * pop_raked)]
    # NA out population where the pixel value is NA (to prevent weirdness with denominators)
    cell_pred <- cell_pred[is.na(V1), pop_raked := NA]
  }

  raked_cell_pred_c <- dedupe_linked_cell_pred(cell_pred, overs)
  save(raked_cell_pred_c, file = ifelse(is.null(custom_output_folder),
    paste0(
      sharedir,"<<<< FILEPATH REDACTED >>>>"
    ), paste0(
      custom_output_folder,"<<<< FILEPATH REDACTED >>>>"
    )
  ))

  # SPACE
  raked_cell_pred_c <- NULL

  # creating a raked counts aggregations
  message("creating a raked counts aggregations")

  if(use_pop_draws) {
    draw_overs <- paste0("V", 1:ndraws)
    pop_overs <- paste0("P", 1:ndraws)

    admin_2 <- cell_pred[, lapply(c(draw_overs, pop_overs),
                                  function(x) sum(get(x), na.rm = T)),
                         by = c("year", "ADM2_CODE", "ADM0_CODE")]
    admin_1 <- cell_pred[, lapply(c(draw_overs, pop_overs),
                                  function(x) sum(get(x), na.rm = T)),
                         by = c("year", "ADM1_CODE", "ADM0_CODE")]
    admin_0 <- cell_pred[, lapply(c(draw_overs, pop_overs),
                                  function(x) sum(get(x), na.rm = T)),
                         by = c("year", "ADM0_CODE")]
    setnames(admin_2, c("year", "ADM2_CODE", "ADM0_CODE", draw_overs, pop_overs))
    setnames(admin_1, c("year", "ADM1_CODE", "ADM0_CODE", draw_overs, pop_overs))
    setnames(admin_0, c("year", "ADM0_CODE", draw_overs, pop_overs))

    # cleaning raked admin draws in count space
    admin_1$ADM0_CODE <- NULL
    admin_2$ADM0_CODE <- NULL

  } else {
    # do calculations!
    admin_2 <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "ADM2_CODE", "ADM0_CODE")]
    admin_1 <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "ADM1_CODE", "ADM0_CODE")]
    admin_0 <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "ADM0_CODE")]


    setnames(admin_2, grep("V[0-9]", names(admin_2), value = T), c(overs, "pop_raked"))
    setnames(admin_1, grep("V[0-9]", names(admin_1), value = T), c(overs, "pop_raked"))
    setnames(admin_0, grep("V[0-9]", names(admin_0), value = T), c(overs, "pop_raked"))

    # cleaning raked admin draws in count space
    admin_0$pop <- admin_0$pop_raked
    admin_0$pop_raked <- NULL
    admin_1$ADM0_CODE <- NULL
    admin_1$pop <- admin_1$pop_raked
    admin_1$pop_raked <- NULL
    admin_2$ADM0_CODE <- NULL
    admin_2$pop <- admin_2$pop_raked
    admin_2$pop_raked <- NULL
  }

  # create the spatial hierarchy
  sp_hierarchy_list <- unique(link[ADM0_CODE %in% unique(admin_0[, ADM0_CODE]), .(ADM0_CODE, ADM1_CODE, ADM2_CODE, ADM0_NAME, ADM1_NAME, ADM2_NAME, region)])
  sp_hierarchy_list[, region := reg]

  ## save raked counts aggregations
  save(admin_0, admin_1, admin_2, sp_hierarchy_list,
    file = ifelse(is.null(custom_output_folder),
      paste0(main_dir,"<<<< FILEPATH REDACTED >>>>"),
      paste0(custom_output_folder,"<<<< FILEPATH REDACTED >>>>")
    )
  )

  # creating raked rates aggregations (you can work back at the admin level because there are no admin's with pop = 0)
  message("creating a raked rates aggregations")

  if(use_pop_draws) {
    admin_0 <- admin_0[, (draw_overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) / get(paste0("P", x)))]
    admin_1 <- admin_1[, (draw_overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) / get(paste0("P", x)))]
    admin_2 <- admin_2[, (draw_overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) / get(paste0("P", x)))]
  } else {
    # convert back to rates/prevalence
    overs <- paste0("V", 1:ndraws)
    admin_0 <- admin_0[, (overs) := lapply(overs, function(x) get(x) / pop)]
    admin_1 <- admin_1[, (overs) := lapply(overs, function(x) get(x) / pop)]
    admin_2 <- admin_2[, (overs) := lapply(overs, function(x) get(x) / pop)]
  }

  ## save raked rates aggregations
  save(admin_0, admin_1, admin_2, sp_hierarchy_list,
    file = ifelse(is.null(custom_output_folder),
      paste0(main_dir,"<<<< FILEPATH REDACTED >>>>"),
      paste0(custom_output_folder,"<<<< FILEPATH REDACTED >>>>")
    )
  )


  # creating unraked counts aggregations
  message("creating a unraked counts aggregations")

  # unrake all draws
  overs <- paste0("V", 1:ndraws)
  cell_pred <- cell_pred[, (overs) := lapply(overs, function(x) get(x) / rf)]

  if(use_pop_draws) {
    admin_2 <- cell_pred[, lapply(c(draw_overs, pop_overs),
                                  function(x) sum(get(x), na.rm = T)),
                         by = c("year", "ADM2_CODE", "ADM0_CODE")]
    admin_1 <- cell_pred[, lapply(c(draw_overs, pop_overs),
                                  function(x) sum(get(x), na.rm = T)),
                         by = c("year", "ADM1_CODE", "ADM0_CODE")]
    admin_0 <- cell_pred[, lapply(c(draw_overs, pop_overs),
                                  function(x) sum(get(x), na.rm = T)),
                         by = c("year", "ADM0_CODE")]
    setnames(admin_2, c("year", "ADM2_CODE", "ADM0_CODE", draw_overs, pop_overs))
    setnames(admin_1, c("year", "ADM1_CODE", "ADM0_CODE", draw_overs, pop_overs))
    setnames(admin_0, c("year", "ADM0_CODE", draw_overs, pop_overs))

    # cleaning raked admin draws in count space
    admin_1$ADM0_CODE <- NULL
    admin_2$ADM0_CODE <- NULL
  } else {

    ## Create unraked counts agregation
    admin_2 <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "ADM2_CODE", "ADM0_CODE")]
    admin_1 <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "ADM1_CODE", "ADM0_CODE")]
    admin_0 <- cell_pred[, lapply(c(overs, "pop_raked"), function(x) sum(get(x), na.rm = T)), by = c("year", "ADM0_CODE")]

    setnames(admin_2, grep("V[0-9]", names(admin_2), value = T), c(overs, "pop_raked"))
    setnames(admin_1, grep("V[0-9]", names(admin_1), value = T), c(overs, "pop_raked"))
    setnames(admin_0, grep("V[0-9]", names(admin_0), value = T), c(overs, "pop_raked"))

    admin_0$pop <- admin_0$pop_raked
    admin_0$pop_raked <- NULL
    admin_1$pop <- admin_1$pop_raked
    admin_1$pop_raked <- NULL
    admin_1$ADM0_CODE <- NULL
    admin_2$pop <- admin_2$pop_raked
    admin_2$pop_raked <- NULL
    admin_2$ADM0_CODE <- NULL
  }

  gc()

  ## save unraked counts aggregations
  save(admin_0, admin_1, admin_2, sp_hierarchy_list,
    file = ifelse(is.null(custom_output_folder),
      paste0(main_dir,"<<<< FILEPATH REDACTED >>>>"),
      paste0(custom_output_folder,"<<<< FILEPATH REDACTED >>>>")
    )
  )

  # creating unraked rates aggregations
  message("creating a unraked rates aggregations")

  # convert back to rates
  if(use_pop_draws) {
    admin_0 <- admin_0[, (draw_overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) / get(paste0("P", x)))]
    admin_1 <- admin_1[, (draw_overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) / get(paste0("P", x)))]
    admin_2 <- admin_2[, (draw_overs) := lapply(1:ndraws, function(x) get(paste0("V", x)) / get(paste0("P", x)))]
  } else {
    overs <- paste0("V", 1:ndraws)
    admin_0 <- admin_0[, (overs) := lapply(overs, function(x) get(x) / pop)]
    admin_1 <- admin_1[, (overs) := lapply(overs, function(x) get(x) / pop)]
    admin_2 <- admin_2[, (overs) := lapply(overs, function(x) get(x) / pop)]
  }

  ## save unraked rates aggregations
  save(admin_0, admin_1, admin_2, sp_hierarchy_list,
    file = ifelse(is.null(custom_output_folder),
      file.path(
        main_dir,
        sprintf(
          "%s_unraked_admin_draws_eb_bin%i_%s_%i.RData",
          indicator, age, reg, holdout
        )
      ),
      file.path(
        custom_output_folder,
        sprintf(
          "%s_unraked_admin_draws_eb_bin%i_%s_%i.RData",
          indicator, age, reg, holdout
        )
      )
    )
  )

  ## Return cell pred and raking factors if desired
  if (return_objects) {
    output_list <- list()
    output_list[["rf"]] <- data.table(fractional_rf)
    output_list[["raked_cell_pred"]] <- raked_cell_pred
    return(output_list)
  }
}
