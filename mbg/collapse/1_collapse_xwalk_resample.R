####################################################################################################
## Prepare data for model
## Description: 1. Collapse geomatched point and polygon data
##              2. Crosswalk 
##              3. Resample polygons
####################################################################################################

## Setup -------------------------------------------------------------------------------------------

rm(list = ls())

## Set up -- run normal collapse (FALSE) or collapse for classic indicators (TRUE)?
'%ni%' <- Negate('%in%')

# data paths
in_dir <- "<<<< FILEPATH REDACTED >>>>"
out_dir <- "<<<< FILEPATH REDACTED >>>>"

# archive dates
geomatch_version <- dir(paste0(in_dir), pattern = "^gem_fp")
geomatch_version <- gsub("(^.*)([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})(.*$)", "\\2", 
                         geomatch_version, perl = T)

collapse_version <- format(Sys.time(), "%Y_%m_%d")
dir.create(paste0(out_dir, collapse_version))

last_collapse_version <- dir(out_dir, pattern = "[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}")
last_collapse_version <- last_collapse_version[last_collapse_version != collapse_version]
last_collapse_version <- tail(sort(last_collapse_version), 1)

# shapefile version
shapefile_version <- "current"
modeling_shapefile_version <- "current"
 
# load libraries & functions
library(pacman)
pacman::p_load(rhdf5,tidyverse,msm,openxlsx,sf,ggpubr,ggrepel)

source("<<<< FILEPATH REDACTED >>>>/most_recent_date.R")
library(lbd.mbg, lib.loc = lbd.loader::pkg_loc("lbd.mbg"))

## Collapse data -------------------------------------------------------------------

# load geomatched and processed data
df <- readRDS(paste0(in_dir, "gem_fp_", geomatch_version, ".RDS"))

## Collapse classic indicators
data <- copy(df)
source("<<<< FILEPATH REDACTED >>>>/collapse_classic_indicators.R")

  ## merge with report-only data and exclude outliers
  type <- "classic"
  source("<<<< FILEPATH REDACTED >>>>/1b_mergeon_reports.R")

  ## save pre-resample
  collapse_version <- most_recent_date("<<<< FILEPATH REDACTED >>>>")
  saveRDS(all, file = paste0(out_dir, collapse_version, "/classic_variables_pre_resample.rds"))
  all_classic <- copy(all) ## rename so not overwritten

## Call collapse code for normal indicators
data <- copy(df)
source("<<<< FILEPATH REDACTED >>>>/1a_collapse.R")
 
  ## merge with report-only data and exclude outliers
  type <- "normal"
  source("<<<< FILEPATH REDACTED >>>>/1b_mergeon_reports.R")
  saveRDS(all, file = paste0(out_dir, collapse_version, "/all_variables_pre_xwalk.rds"))
  
  ## Crosswalk data
  source("<<<< FILEPATH REDACTED >>>>/1c_crosswalk.R")
  
  ## save pre-resample
  saveRDS(all, file = paste0(out_dir, collapse_version, "/all_variables_pre_resample.rds"))


## Resample the polygon data ---------------------------------------------------------
resample_function <- function(data, method) {
  # convert proportions to counts 
  data[, value := value * N]
  if(method == "normal") data[, value_original := value_original * N_original]
  
  # separate point and poly data
  pt_data   <- data[!is.na(latitude) & !is.na(longitude)]
  poly_data <- data[is.na(latitude) & is.na(longitude)]
  
  # run polygon resampling on poly data
  resampled_poly_data <- resample_polygons(data = poly_data,
                                           indic = "value",
                                           cores = 2,     
                                           pull_poly_method = "fast",
                                           seed = 98121,
                                           shapefile_version = shapefile_version)
    
  
  # recombine point and poly data
  pt_data[, weight := 1]
  pt_data[, pseudocluster := FALSE]
  final <- rbind(pt_data, resampled_poly_data)
  
  # use resampling weights to down-weight N and hiv_test, then reset to 1. This is as an 
  # alternative to using the resampling weights as analytic weights in INLA.
  final[, N := N * weight]
  if(method == "normal")  final[, N_original := N_original * weight]
  final[, value := value * weight]
  if(method == "normal")  final[, value_original := value_original * weight]
  final[, sum_of_sample_weights := sum_of_sample_weights * weight]
  final[, weight := 1]
  
  ## Save collapsed and resampled data -----------------------------------------------------------------------------
  
  # subset and rename variables
  if(method == "normal")  final <- final[, list(nid, country, source = survey_name, variable, year, latitude, longitude, N, N_obs, value, weight, point, sum_of_sample_weights, flag, value_original, N_original)]
  if(method == "classic") final <- final[, list(nid, country, source = survey_name, variable, year, latitude, longitude, N, N_obs, value, weight, point, sum_of_sample_weights, flag)]
  
  setkey(final, nid, country, source, variable, year, latitude, longitude, N, value)
  
  # add dates of geomatched data (geomatched_version) & collapsed data (collapse_version)
  final$geomatch_version <- geomatch_version
  final$collapse_version <- collapse_version
  
  # write files
  for (indic in unique(final$variable)) {
    tmp <- final[variable == indic, ]
    tmp[, variable := NULL]
    setnames(tmp, "value", indic)
    
    if(method == "normal") file2 <-  paste0(out_dir, collapse_version, "/", indic, "_data_resampled.rds")
    if(method == "classic") file2 <-  paste0(out_dir, collapse_version, "/classic_", indic, "_data_resampled.rds")
    saveRDS(tmp, file = file2)
  }
}
  
## resample normal indicators
resample_function(all, "normal")

## resample classic indicators
resample_function(all_classic, "classic")
