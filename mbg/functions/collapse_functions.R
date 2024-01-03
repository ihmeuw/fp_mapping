####################################################################################################
## Description: Common functions used by multiple collapse codes
####################################################################################################

############################################################
## Subset geomatched data functions for covariates
#############################################################
subset_geomatched_contraception <- function(geo_data){

  # Drop unneeded variables and remove annoying stata attributes
  geo_data <- geo_data[, list(nid, country, survey_series, survey_name, survey_module, strata, psu, point, shapefile, location_code, latitude, longitude, #year,
                              sex_id, age_year, int_year, int_month, pweight, hh_id, line_id, geospatial_id, mar_restricted, contra_currmar_only,
                              contra_evermar_only, currmar_only, evermar_only, missing_fecund, missing_desire, missing_desire_timing,
                              missing_desire_later, no_preg, no_ppa, cv_subgeo, ihme_loc_id, year_start,
                              file_path, age_group, curr_cohabit, any_contra, mod_contra, mod_prop,unmet_prop)]

  # rename variables
  setnames(geo_data, c("survey_series"), c("source"))

  # fix variable class
  geo_data[, year_start := as.numeric(year_start)]
  geo_data[, point := as.numeric(point)]
  geo_data[, latitude := as.numeric(latitude)]
  geo_data[, longitude := as.numeric(longitude)]

  # subset to ages 15-49 (and only surveys with this full range)
  geo_data <- geo_data[between(age_year, 15, 49)]
  drop <- geo_data[, as.list(range(age_year)), nid][V1 != 15 | V2 != 49, nid]
  geo_data <- geo_data[!nid %in% drop]

  # drop observations with missing contraception response or individual weight
  pct_missing_contra <-
    geo_data %>%
    filter(is.na(any_contra) | is.na(pweight)) %>%
    nrow() %>%
    divide_by(nrow(geo_data)) %>%
    multiply_by(100) %>% round(1)
  message(paste0(pct_missing_contra, "% of data are missing contraception indicator or weight response"))
  geo_data <- geo_data[!is.na(any_contra) & !is.na(pweight)]

  # drop observations with missing need response or individual weight
  pct_missing_need <-
    geo_data %>%
    filter(is.na(unmet_prop) | is.na(pweight)) %>%
    nrow() %>%
    divide_by(nrow(geo_data)) %>%
    multiply_by(100) %>% round(1)
  message(paste0(pct_missing_need, "% of data are missing unmet need indicator or weight response"))
  geo_data <- geo_data[!is.na(unmet_prop) & !is.na(pweight)]

  # Drop data missing information on point or polygon
  pct_missing <-
    geo_data %>%
    filter(is.na(point)) %>%
    nrow() %>%
    divide_by(nrow(geo_data)) %>%
    multiply_by(100) %>% round(2)
  message(paste0(pct_missing, "% of data are missing point indicator"))
  geo_data <- geo_data[!is.na(point)]

  # drop point data with missing latitude/longitude
  pct_missing <-
    geo_data %>%
    filter(point == 1, is.na(latitude) | is.na(longitude)) %>%
    nrow() %>%
    divide_by(nrow(geo_data %>% filter(point == 1))) %>%
    multiply_by(100) %>% round(2)
  message(paste0(pct_missing, "% of point data are missing either latitude or longitude and are being dropped"))

  # drop polygon data with missing shapefile/location code
  pct_missing <-
    geo_data %>%
    filter(point == 0, is.na(shapefile) | is.na(location_code)) %>%
    nrow() %>%
    divide_by(nrow(geo_data %>% filter(point == 0))) %>%
    multiply_by(100) %>% round(2)
  message(paste0(pct_missing, "% of polygon data is missing either shapefile or location codes and are being dropped"))

  # drop point data with missing latitude/longitude
  geo_data <- geo_data[(!is.na(latitude) & !is.na(longitude)) | point == 0]

  # drop polygon data with missing shapefile/location code
  geo_data <- geo_data[(!is.na(shapefile) & !is.na(location_code)) | point == 1]
  return(geo_data)

}
