####################################################################################################
## Description: Collapse geomatched point and polygon data
# THIS IS A COLLAPSE ONLY VERSION THAT IS CALLED FROM ANOTHER SCRIPT
# DIFFERENCES ARE DEPENDENT ON WHETHER DATA IS COMPLETE OR COUNTERFACTUAL
####################################################################################################

## Setup -------------------------------------------------------------------------------------------
print('Collapsing data...')

# shapefile version
shapefile_version <- "current"
modeling_shapefile_version <- "current"

## subset, and format data -------------------------------------------------------------------

# check age/sex 
stopifnot(min(data$age_year, na.rm = T) == 15)
stopifnot(max(data$age_year, na.rm = T) == 49)
stopifnot(sum(data$sex_id != 2) == 0)

# drop rows where pweight is missing
drop <- data[, list(year = min(int_year), miss_N = sum(is.na(pweight)), 
                    miss_pct = round(100 * mean(is.na(pweight)), 2)), 
             by = "country,nid,survey_name"]
drop[miss_N > 0, ][order(miss_pct), ]

data <- data[!is.na(pweight), ]

# drop rows where location information is missing
data[point == 1, miss_loc := is.na(latitude) | is.na(longitude)]
data[point == 0, miss_loc := is.na(shapefile) | is.na(location_code)]

drop <- data[, list(year = min(int_year), miss_N = sum(miss_loc), 
                    miss_pct = round(100 * mean(miss_loc), 2)), 
             by = "country,nid,survey_name"]
drop[miss_N > 0, ][order(miss_pct), ]

data <- data[miss_loc == F, ]

# drop rows where contraceptive use is missing
drop <- data[, list(year = min(int_year), miss_N = sum(is.na(any_contra)), 
                    miss_pct = round(100 * mean(is.na(any_contra)), 2)), 
             by = "country,nid,survey_name"]
drop[miss_N > 0, ][order(miss_pct), ]

data <- data[!is.na(any_contra), ]
stopifnot(data[is.na(mod_contra), .N] == 0)
stopifnot(data[is.na(trad_contra), .N] == 0)

# remove factors
data[, country := as.character(country)]
data[, survey_name := as.character(survey_name)]
data[, shapefile := as.character(shapefile)]

# format survey_name
data[substr(survey_name, 1, 3) == country, survey_name := substr(survey_name, 5, 100)]
data[survey_name == "JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020", survey_name := "PMA2020"]
data[survey_name == "JHSPH_PERFORMANCE_FOR_ACTION_PMA", survey_name := "PMA"]

## Collapse all data (polygon and point) -----------------------------------------------------------

## specify collapse level based on when code is being run
by_vars <- ifelse(!exists("cf"),
                  "nid,country,survey_name,admin_1_id,point,shapefile,location_code,latitude,longitude", ## non-counterfactual extractions, collapse by mapping level geometry
                  "nid,country,survey_name,admin_1_id") ## counterfactual extractions, collapse by admin_1

# Contraceptive prevalence
any_contra_data <- copy(data)
any_contra_data <- any_contra_data[, list(year = floor(median(int_year, na.rm = T)),
                                          value = weighted.mean(any_contra, pweight), 
                                          N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                          N_obs = .N,
                                          sum_of_sample_weights = sum(pweight)),
                                   by = by_vars]
any_contra_data[, variable := "any_contra"]

# Modern Contraceptive prevalence (among any_contra == 1)
mod_contra_data <- data[any_contra == 1, ]
mod_contra_data <- mod_contra_data[, list(year = floor(median(int_year, na.rm = T)),
                                          value = weighted.mean(mod_contra, pweight), 
                                          N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                          N_obs = .N,
                                          sum_of_sample_weights = sum(pweight)),
                                   by = by_vars]
mod_contra_data[, variable := "mod_contra"]

# Need for contraception (among any_contra == 0)
need_data <- data[any_contra == 0 & !is.na(need_contra), ] 
need_data <- need_data[, list(year = floor(median(int_year, na.rm = T)),
                              value = weighted.mean(need_contra, pweight), 
                              N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                              N_obs = .N,
                              sum_of_sample_weights = sum(pweight)),
                       by = by_vars]
need_data[, variable := "need_contra"]

# Intention to use contraception in the future (among any_contra == 0 & need == 1)
intent_data <- data[ !is.na(intent_to_use), ]
intent_data <- intent_data[any_contra == 0 & need_contra == 1 &  intent_to_use != -1, ]
intent_data <- intent_data[, list(year = floor(median(int_year, na.rm = T)),
                                  value = weighted.mean(intent_to_use, pweight), 
                                  N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                  N_obs = .N,
                                  sum_of_sample_weights = sum(pweight)),
                           by = by_vars]
intent_data[, variable := "intent_use"]

# combine
all <- rbind(any_contra_data, mod_contra_data, need_data, intent_data)
sapply(all, function(x) mean(is.na(x)))

