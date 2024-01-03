#' @title Load Input Data
#'
#' @description Load in input data at the start of the modelling process. Runs a number of checks and cleanup on data depending on args. Saves out a copy of this processed data in the run date folder.
#'
#' @param indicator indicator for modelling run
#' @param indicator_group indicator_group for modelling run.
#' @param simple Single polygon that defines boundaries of the entire area you want to model over.
#' @param agebin Integer, default 0. 
#' @param removeyemem Boolean default F. 
#' @param pathaddin character default "". Additional information to add to the filename of the saved out data. 
#' @param withdate Boolean default F.
#' @param date character default "".
#' @param years character default 'five_year'.
#' @param range integer default 5. Not used.
#' @param update_run_date boolean default F. If False, increments run_date until a new run date is found. if T, will save input data in place.
#' @param withtag boolean default F. 
#' @param datatag character default "". 
#' @param use_share boolean default F. 
#' @param yl numeric vector.
#' @param region character, default NULL.
#' @param poly_ag Boolean. Set in the config and used in polygon integration models. If T, only data where point == 0 and poly_ag is T will be kept
#' @param zcol_ag Boolean. If T runs a check to ensure that the input data has non-aggregated data.
#'
#' @return cleaned data.table of input_data.
#' @export
#'
#' @concept prep
load_input_data <- function(indicator, indicator_group = use_global_if_missing("indicator_group"), simple = NULL, agebin = 0, removeyemen = FALSE, pathaddin = "",
                            withdate = FALSE, date = "", years = "five_year", range = 5, update_run_date = FALSE,
                            withtag = FALSE, datatag = "", use_share = FALSE, yl = year_list, region = NULL,
                            poly_ag = use_global_if_missing("poly_ag"),
                            zcol_ag = use_global_if_missing("zcol_ag")) {
  ## Load input data from required location
  #   Arguments:
  #     indicator = Specific outcome to be modeled within indicator category, i.e. "edu_0"
  #     simple    = Single polygon that defines boundaries of the entire area you want to model over.
  #   Returns: Input data subset to modeling area.

  # Ensure str_match loaded
  str_match <- stringr::str_match

  if (withdate && date != "") {
    rd <- date
  } else {
    rd <- run_date
  }

  # Load input data by indicator
  if (!is.null(fp_list$input_data_root)) {
    load_dir <- fp_list$input_data_root
  } else {
    # This uses the geospatial root rather than mbg root so that the input data can stay in place
    # when the model outputs get moved to a different directory
    if (use_share) {
      load_dir <- "<<<< FILEPATH REDACTED >>>>"
    } else {
      load_dir <- "<<<< FILEPATH REDACTED >>>>"
    }
  }

  if (!withdate & !withtag) filename <- "<<<< FILEPATH REDACTED >>>>"
  if (withtag) filename <- "<<<< FILEPATH REDACTED >>>>"
  if (withdate) filename <- "<<<< FILEPATH REDACTED >>>>"

  # try to see if an RDS exists, if so use that, if not use a csv
  if (file.exists(paste0(filename, ".RDS"))) {
    message("READING INPUT DATA FROM RDS FILE")
    d <- readRDS(paste0(filename, ".RDS"))
  } else if (file.exists(paste0(filename, ".csv"))) {
    message("READING INPUT DATA FROM CSV FILE")
    d <- read.csv(paste0(filename, ".csv"))
  } else {
    stop(paste0(
      "Attempt to read input data from ", filename, ".RDS (or .csv) failed. ",
      "Check to ensure that an .RDS or .csv file exists at that location. "
    ))
  }

  d$latitude <- as.numeric(as.character(d$latitude))
  d$longitude <- as.numeric(as.character(d$longitude))
  message(nrow(d))

  # Remove odd long/lats for point data (when not using polygon resampling)
  if (!"point" %in% names(d)) {
    # assume only point data
    d$point <- 1
  }

  d <- d[d$latitude <= 90 | (d$point == 0 & as.logical(poly_ag)), ]
  d <- d[d$latitude >= -90 | (d$point == 0 & as.logical(poly_ag)), ]
  d <- d[d$longitude <= 180 | (d$point == 0 & as.logical(poly_ag)), ]
  d <- d[d$longitude >= -180 | (d$point == 0 & as.logical(poly_ag)), ]
  d <- subset(d, !is.na(latitude) | (d$point == 0 & as.logical(poly_ag)))
  d <- subset(d, latitude != 0 | (d$point == 0 & as.logical(poly_ag)))
  message(nrow(d))

  # Check for necessary columns
  if (!(indicator %in% names(d))) stop(paste0("Your input data does not contain a column for your indicator: ", indicator))

  d <- as.data.table(d)

  # Change all "country" assignments to national level (in case subnational in the input data)
  if (nrow(d[grepl("[A-Z]*_[.]*", country), ]) > 0) {
    subnat_countries <- unique(d[grepl("[A-Z]*_[.]*", country), country])
    warning(paste0(
      "Changing subnational to national country codes for the following: ",
      paste0(subnat_countries, collapse = ",")
    ))
    d[grepl("[A-Z]*_[.]*", country), country := str_match(country, "([A-Z]*)_[.]*")[, 2]]
  }
  
  d$keep <- F

  # Subset by modeling shapefile or region
  if (!is.null(simple) && is.null(region)) {
    d$rowid <- 1:nrow(d)
    dpoint <- d[d$point == 1 | !as.logical(poly_ag), c("rowid", "longitude", "latitude")]
    coordinates(dpoint) <- c("longitude", "latitude")
    proj4string(dpoint) <- proj4string(simple)
    dpoint$keep <- !is.na(over(dpoint, as(simple, "SpatialPolygons")))
    d$keep[dpoint$rowid] <- dpoint$keep
  } else if (is.null(simple) && !is.null(region)) {
    adm0_list <- get_adm0_codes(region, shapefile_version = modeling_shapefile_version)
    # get GAUL to iso mapping
    loc_codes <- get_location_code_mapping(shapefile_version = modeling_shapefile_version)
    loc_codes <- loc_codes[ADM_CODE %in% adm0_list, ]
    regs <- loc_codes$ihme_lc_id
    d[country %in% regs, "keep"] <- T
  } else if (!is.null(simple) && !is.null(region)) {
    stop("Subsetting data by both 'simple' (points that overlay simple polygon of modeling region)
         and 'region' (points in countries in region) not supported. Please use argument or the other
         in load_input_data.")
  } else if (is.null(simple) && is.null(region)) {
    warning("Missing region information. Will keep all polygon data")
    d$keep <- T
  } 
  
  # if poly_ag (include polygons in model) is F, remove all non-point data from data
  if(!as.logical(poly_ag)) {
    d[point != 1, "keep"] <- F
  }

  message(paste0(round(mean(d$keep), 2) * 100, "% of input data in specified template"))
  d <- d[d$keep, ]

  if (agebin != 0) d <- d[age %in% agebin, ]
  if (removeyemen) d <- d[country != "Yemen" & country != "YEM", ]

  # remap any years as needed
  if (years == "five_year") {
    d <- d[year >= 1998 & year <= 2002, year := 2000]
    d <- d[year >= 2003 & year <= 2007, year := 2005]
    d <- d[year >= 2008 & year <= 2012, year := 2010]
    d <- d[year >= 2013 & year <= 2017, year := 2015]
  }

  if (nrow(subset(d, year < min(yl))) > 0) {
    warning(paste0("Dropping all data before min(year_list) = ", min(yl), "..."))
    d <- subset(d, year >= min(yl))
  }
  if (nrow(subset(d, year > max(yl))) > 0) {
    warning(paste0("Dropping all data after max(year_list) = ", max(yl), "..."))
    d <- subset(d, year <= max(yl))
  }

  # add in a weight column if it does not exist
  if (!"weight" %in% names(d)) {
    warning("A 'weight' column does not exists so one is added with all 1s.")
    d[, weight := 1]
  }

  # creaste a weighted SS to base QTs on
  if (sum(c("N", "weight") %in% colnames(d)) == 2) d[, weighted_n := N * weight]

  # check that there is some non aggregate data
  if (!is.null(zcol_ag)) {
    if (all((d$point == 0 & as.logical(poly_ag)) | !is.na(d[[zcol_ag]]))) {
      stop("There is only aggregate data. Currently the pipeline does not support modeling without some disaggregated data.")
    }
  } else {
    if (all(d$point == 0) && as.logical(poly_ag)) {
      stop("There is only aggregate data. Currently the pipeline does not support modeling without some disaggregated data.")
    }
  }

  # Save a copy
  if (update_run_date == TRUE) {
    if (dir.exists("<<<< FILEPATH REDACTED >>>>") == TRUE) {
      existing_dir <- "<<<< FILEPATH REDACTED >>>>"
      new_try <- existing_dir
      index <- 0
      while (dir.exists(new_try)) {
        index <- index + 1
        new_try <- paste0(existing_dir, "_", index)
      }
      run_date <- paste0(run_date, "_", index)
      dir.create(new_try, showWarnings = FALSE)
      run_date_dir <- new_try
    }
    if (dir.exists("<<<< FILEPATH REDACTED >>>>") == FALSE) {
      run_date_dir <- "<<<< FILEPATH REDACTED >>>>"
      dir.create(run_date_dir, showWarnings = FALSE)
    }
    write.csv(d, file = "<<<< FILEPATH REDACTED >>>>")
    return(list(d, run_date))
  }

  if (update_run_date == FALSE) {
    if (agebin == 0) {
      run_date_dir <- "<<<< FILEPATH REDACTED >>>>"
      dir.create(run_date_dir, showWarnings = FALSE)
      write.csv(d, file = "<<<< FILEPATH REDACTED >>>>")
    } else {
      run_date_dir <- "<<<< FILEPATH REDACTED >>>>"
      dir.create(run_date_dir, showWarnings = FALSE)
      write.csv(d, file = "<<<< FILEPATH REDACTED >>>>")
    }
    return(d)
  }
}
