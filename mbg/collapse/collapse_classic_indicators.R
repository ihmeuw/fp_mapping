####################################################################################################
## Collapse for classic indicators:
## modern contraception prevalence, traditional contraceptive prevalence, contraception prevalence, demand satisfied, unmet need, intent to use
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

# fill in implied 0s for unmarried/partnered women on surveys that restrict to married/partnered women 
# use the 'curr_cohabit' variable, which is a combination of the questions on cohabitation and marital
# status, depending on what is available in a given survey. 
data[flag_currmar_contra == 1, list(mean(is.na(any_contra)), .N), keyby = curr_cohabit] 
data[flag_currmar_contra == 1 & curr_cohabit == 0, 
     c("trad_contra", "mod_contra", "any_contra") := list(0, 0, 0)]

data[flag_currmar_need == 1, list(mean(is.na(need_contra)), .N), keyby = curr_cohabit] 
data[flag_currmar_need == 1 & curr_cohabit == 0 & is.na(need_contra), need_contra := 0]

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

data[intent_to_use == -1, intent_to_use := 0]
data[, no_need := 1-need_contra]
data[, no_intent := 1-intent_to_use]
data[(mod_contra==1 | trad_contra==1) & no_need==1, no_need := 0][(mod_contra==1 | trad_contra==1) & need_contra==0, need_contra := 1]
data[(mod_contra==1 | trad_contra==1) & intent_to_use==1, intent_to_use := 0][(mod_contra==1 | trad_contra==1) & no_intent==0, no_intent := 1]
data[no_need==1 & intent_to_use==1, intent_to_use:= 0][no_need==1 & no_intent==0, no_intent:= 1]

# Contraceptive prevalence
indic_contra_prev <- copy(data)
indic_contra_prev <- indic_contra_prev[, list(year = floor(median(int_year, na.rm = T)),
                                          value = weighted.mean(mod_contra, pweight,na.rm=T) + weighted.mean(trad_contra, pweight,na.rm=T),
                                          N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                          N_obs = .N,
                                          sum_of_sample_weights = sum(pweight)),
                                   by = by_vars]
indic_contra_prev[, variable := "indic_contra"]

# Modern Contraceptive prevalence
indic_contra_mod <- copy(data) 
indic_contra_mod <- indic_contra_mod[, list(year = floor(median(int_year, na.rm = T)),
                                          value = weighted.mean(mod_contra, pweight,na.rm=T),
                                          N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                          N_obs = .N,
                                          sum_of_sample_weights = sum(pweight)),
                                   by = by_vars]
indic_contra_mod[, variable := "group_e"]


# Traditional Contraceptive prevalence
indic_contra_trad <- copy(data) 
indic_contra_trad <- indic_contra_trad[, list(year = floor(median(int_year, na.rm = T)),
                                          value = weighted.mean(trad_contra, pweight,na.rm=T),
                                          N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                          N_obs = .N,
                                          sum_of_sample_weights = sum(pweight)),
                                   by = by_vars]
indic_contra_trad[, variable := "group_d"]

# demand satisfied (among need == 1) 
indic_satisf <- subset(data, need_contra==1) 
indic_satisf <- indic_satisf[, list(year = floor(median(int_year, na.rm = T)),
                              value = weighted.mean(mod_contra, pweight, na.rm=T),
                              N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                              N_obs = .N,
                              sum_of_sample_weights = sum(pweight)),
                       by = by_vars]
indic_satisf[, variable := "indic_demand"]


# unmet need 
indic_unmet <-  copy(data) 
indic_unmet <- indic_unmet[, list(year = floor(median(int_year, na.rm = T)),
                          value = 1- round(weighted.mean(mod_contra, pweight, na.rm=T) + weighted.mean(no_need, pweight, na.rm=T),7),  
                          N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                          N_obs = .N,
                          sum_of_sample_weights = sum(pweight)),
                   by = by_vars]
indic_unmet[, variable := "indic_unmet"]

# intent to use (among any_contra == 0 & need == 1) 
indic_intent <- subset(data, any_contra==0 & need_contra==1 & flag_intent==1) 
indic_intent <- indic_intent[, list(year = floor(median(int_year, na.rm = T)),
                                  value = weighted.mean(intent_to_use, pweight, na.rm=T),
                                  N = sum(pweight) ^ 2 / sum(pweight ^ 2), # Kish approximation
                                  N_obs = .N,
                                  sum_of_sample_weights = sum(pweight)),
                           by = by_vars]
indic_intent[, variable := "indic_intent"]

# combine
all <- rbind(indic_contra_prev, indic_contra_mod, indic_contra_trad, indic_satisf,indic_unmet,indic_intent)
sapply(all, function(x) mean(is.na(x)))
