############################################################################################################
## Compile gold standard contraception surveys in one folder. Then, for every combination
##          of possible missing variables perform a counterfactual re-extraction of the gold-standard
##          surveys to inform our crosswalk of non-gold-standard surveys
###########################################################################################################


## SET-UP --------------------------------------------------------------------

# clear memory
rm(list=ls())

# load libraries
pacman::p_load(data.table,magrittr,ggplot2,haven,stringr,parallel,dplyr, plyr)

# in/out
team <- "<<<< TEAM NAME REDACTED >>>>"
input_dir <- "<<<< FILEPATH REDACTED >>>>"
temp_dir <- "<<<< FILEPATH REDACTED >>>>"
output_dir <- "<<<< FILEPATH REDACTED >>>>"
final_dir_root <- "<<<< FILEPATH REDACTED >>>>"

# settings
calc_script <- file.path("<<<< FILEPATH REDACTED >>>>/indicator_calc.R")
file_prep <- FALSE ## do files need to be identified as gold standard and saved to appropriate folder first?
cf_prep <- TRUE ## do files need to be checked for need counterfactual extraction first?

cf <- TRUE ## flag that this is for counterfactual extractions -- for collapse code
Sys.umask(mode = "0002")
cores.provided <- 10

# create the opposite of %in%
'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month 
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# create function to count number of digits in a number
nDigits <- function(x) nchar( trunc( abs(x) ) )

# create function to return unique characters in a string
uniqchars <- function(x) unique(strsplit(x, "")[[1]])

# find most recent version
processed_version <- most_recent_date(input_dir)
ubcov_newest <- paste0(team, "_", processed_version, ".RDS")

# read in data
df_all <- as.data.table(readRDS(file.path(input_dir, ubcov_newest)))

## MAKE CF UPDATES AS NEEDED -----------------------------------------------

## MICS KEN 2008 desire asked, but only to women using contraception and pregnant women
df_all[nid==7401, need_no_desire := NA] 

## IDENTIFY GOLD STANDARD DATA -----------------------------------------------

if (file_prep) {
  
  # compile folder of all gold standard surveys to prepare for counterfactual re-extractions
  compile_gold_data <- function(survey) {
    
    df <- subset(df_all, nid == survey)
    
    track_gold <- data.table(unique(df[, c("nid", "survey_name")]), gold_need = as.numeric(NA), gold_contra = as.numeric(NA), gold_intent = as.numeric(NA))
    ## contra gold standard
    if (df$contra_currmar_only== 0 | is.na(df$contra_currmar_only)) track_gold[nid==survey, gold_contra:=1]
    
    name <- unique(paste0(df$nid, "_", df$country,"_", df$survey_series, "_",  df$year_start))
    print(name)
    
    ##remove columns that have all NA
    df[need_sex == 0, need_sex := NA]
    df[need_no_desire == 0, need_no_desire := NA]
    df[need_infertile == 0, need_infertile := NA]
    df[need_nomens == 0, need_nomens := NA]
    df[need_nochild == 0, need_nochild := NA]
    df[need_preg == 0, need_preg := NA]
    df[need_ppa == 0, need_ppa := NA]
    
    df <- df[,which(unlist(lapply(df, function(x)!all(is.na(x))))),with=F]
    
    # check whether survey is gold standard (not marked as missing any necessary variables for prop_unmet)
    if (all(c("need_sex", "need_no_desire", "need_infertile", "need_nomens", "need_nochild", "need_preg", "need_ppa") %in% names(df)) &
                  "current_contra" %in% names(df)) {
      
      print(paste0("GOLD STANDARD : ", name))

      # convert string variables with accents into non-accented characters
      df[,current_contra := gsub("\x82|\xe9", "e", current_contra)]
      df[,current_contra := gsub("\\U3e33663c", "o", current_contra)]
      df[,current_contra := gsub("\xf3|\xf4", "o", current_contra)]
      df[,current_contra := gsub("\\U3e64653c", "i", current_contra)]
      df[,current_contra := gsub("\xed", "i", current_contra)]
      df[,current_contra := str_replace_all(iconv(current_contra, from="latin1", to="ASCII//TRANSLIT"), "'", "")]
      if ("reason_no_contra" %in% names(df)) {
        df[,reason_no_contra := gsub("\x82|\xe9", "e", reason_no_contra)]
        df[,reason_no_contra := gsub("\\U3e33663c", "o", reason_no_contra)]
        df[,reason_no_contra := gsub("\xf3|\xf4", "o", reason_no_contra)]
        df[,reason_no_contra := gsub("\\U3e64653c", "i", reason_no_contra)]
        df[,reason_no_contra := gsub("\xed", "i", reason_no_contra)]
        df[,reason_no_contra := str_replace_all(iconv(reason_no_contra, from="latin1", to="ASCII//TRANSLIT"), "'", "")]
      }
      
      # # save gold standard files to their own folder
      write.csv(df, paste0(temp_dir, name, ".csv"), row.names = F)
      
      track_gold[nid==survey, gold_need:=1]
      if ("intent_use_gold" %in% names(df)) track_gold[nid==survey, gold_intent:=1]
      return(track_gold)
      
    } 
    return(track_gold)
  }

  # parallelize the consolidation of gold standard surveys
    nid_run <- as.list(unique(df_all$nid))
    
    # actually launch extraction
    track_gold <- mclapply(nid_run, compile_gold_data, mc.cores = cores.provided)
    track_gold <- rbindlist(track_gold, fill=T)
    
    module_date <- format(Sys.Date(), "%Y_%m_%d")
    
    ## combine all gold CSVs into RDS file
    counterfac_temp <- list.files(temp_dir, pattern = ".csv", full.names = T)
    all <- rbindlist(lapply(counterfac_temp, read.csv), fill = T)
    saveRDS(all, file=paste0("<<<< FILEPATH REDACTED >>>>/gold_standard_", module_date, ".RDS"))
    
    ##collapse gold
    data <- copy(all)
    source("<<<< FILEPATH REDACTED >>>>/1a_collapse.R")
    saveRDS(all, file=paste0("<<<< FILEPATH REDACTED >>>>/collapsed_gold_standard_", module_date, ".RDS"))
    
    write.csv(track_gold, paste0("<<<< FILEPATH REDACTED >>>>/gold_standard_surveys_", module_date, ".csv"), row.names = FALSE)  
  
}


## IDENTIFY COUNTERFACTUALS NEEDED  -----------------------------------------------
if (cf_prep) { 
  source(file.path("<<<< FILEPATH REDACTED >>>>/list_counterfac.R"))
}

## PREFORM COUNTERFACTUAL EXTRACTIONS ----------------------------------------

# Read in list of all gold standard surveys
gold_name <- list.files(output_dir, pattern = ".csv", full.names=T)
gold_nid <- read.csv(gold_name) %>% subset(gold_need==1) 
gold_nid <- unique(gold_nid$nid)

counterfac_name <- list.files("<<<< FILEPATH REDACTED >>>>", pattern = ".RDS", full.names=T)
df_counterfac <- readRDS(counterfac_name)

## KENYA DHS 2014 fix
# administered two versions of the survey and the short version does not contain all of the necessary questions to determine need at the gold standard level
# extravar_cont_3 was extracted to  capture the version type: 1 = long, 2= short
df_counterfac[nid==157057 & extravar_cont_3 == 2, nid := 999999]
df_counterfac[nid == 999999, no_ppa := 1][nid == 999999, missing_desire := 1]

df_counterfac[nid==7401, missing_desire := 1] ## desire questions were only asked to women using contraception methods

# Read in counterfactual extractions needed for this data. 
counterfactuals <- read.csv("<<<< FILEPATH REDACTED >>>>/counterfacs_to_run.csv", stringsAsFactors=FALSE)
counterfactuals <- subset(counterfactuals, !is.na(N))[1:3] ##[1] ## Only keep if 1+ surveys need the extraction
counterfactuals <- subset(counterfactuals, !((none == "none" & cohabit=="nochild") | (none == "none" & is.na(cohabit)))) ## remove counterfactuals that would not capture anything

# loop through each counterfactual and preform re-extractions
for (i in 1:nrow(counterfactuals)) {
  counterfactual <- counterfactuals[i,1:2]
  name <- ifelse(!is.na(counterfactual[2]), paste(counterfactual[1], counterfactual[2], sep="_"), counterfactual[1])
  if (name == "none_contra") name <- "contra"
  print(paste0("COUNTERFACTUAL EXTRACTION FOR ", name))

  # set up variables corresponding to which counterfactual extraction this is (correspond to gateways in contraception_ubcov_calc.R)
  
  counterfac_missing_sex <- ifelse(grepl("sex", counterfactual[1]), 1, 0)
  counterfac_missing_infertile <- ifelse(grepl("infertile", counterfactual[1]), 1, 0)
  counterfac_missing_nomens <- ifelse(grepl("nomens", counterfactual[1]), 1, 0)
  counterfac_missing_nochild <- ifelse(grepl("nochild", counterfactual[1]), 1, 0)
  counterfac_missing_desire <- ifelse(grepl("desire", counterfactual[1]) & !grepl("_later|_timing", counterfactual[1]), 1, 0)
  counterfac_missing_desire_timing <- ifelse(grepl("desire_timing", counterfactual[1]), 1, 0)
  counterfac_missing_desire_later <- ifelse(grepl("desire_later", counterfactual[1]), 1, 0)
  counterfac_no_preg <- ifelse(grepl("preg", counterfactual[1]), 1, 0)
  counterfac_no_ppa <- ifelse(grepl("ppa", counterfactual[1]), 1, 0)
  counterfac_intent_12m <- ifelse(grepl("intent", counterfactual[1]), 1, 0)
  
  counterfac_cohab_sex <- ifelse(grepl("sex", counterfactual[2]), 1, 0)
  counterfac_cohab_contra <- ifelse(grepl("contra", counterfactual[2]), 1, 0)
  counterfac_cohab_infertile <- ifelse(grepl("infertile", counterfactual[2]), 1, 0)
  counterfac_cohab_nomens <- ifelse(grepl("nomens", counterfactual[2]), 1, 0)
  counterfac_cohab_contra <- ifelse(grepl("contra", counterfactual[2]), 1, 0)
  counterfac_cohab_desire <- ifelse(grepl("desire", counterfactual[2]) & !grepl("_later|_timing", counterfactual[2]), 1, 0)
  counterfac_cohab_desire_timing <- ifelse(grepl("desire_timing", counterfactual[2]), 1, 0)
  counterfac_cohab_desire_later <- ifelse(grepl("desire_later", counterfactual[2]), 1, 0)
  counterfac_cohab_preg <- ifelse(grepl("preg", counterfactual[2]), 1, 0)
  counterfac_cohab_ppa <- ifelse(grepl("ppa", counterfactual[2]), 1, 0)
  
  # # create the folder for saving the output from this counterfactual extraction
  final_output_dir <- file.path("<<<< FILEPATH REDACTED >>>>", name)
  dir.create(file.path("<<<< FILEPATH REDACTED >>>>"), recursive = T, showWarnings = F)

  # re-extract every gold-standard survey according to this counterfactual scenario
  reextract_survey <- function(survey) {
    print(survey)
    
    # read in survey
    df <- subset(df_counterfac, nid==survey)
    
    ##remove columns that have all NA
    df <- df[,which(unlist(lapply(df, function(x)!all(is.na(x))))),with=F]
    
    # by default, study-level covariates are false unless otherwise specified
    if ("mar_restricted" %ni% names(df)) df[,mar_restricted := 0]
    if ("contra_currmar_only" %ni% names(df)) df[,contra_currmar_only := 0]
    if ("contra_evermar_only" %ni% names(df)) df[,contra_evermar_only := 0]
    if ("currmar_only" %ni% names(df)) df[,currmar_only := 0]
    if ("evermar_only" %ni% names(df)) df[,evermar_only := 0]
    if ("missing_fecund" %ni% names(df)) df[,missing_fecund := 0]
    if ("missing_desire" %ni% names(df)) df[,missing_desire := 0]
    if ("missing_desire_timing" %ni% names(df)) df[,missing_desire_timing := 0]
    if ("missing_desire_later" %ni% names(df)) df[,missing_desire_later := 0]
    if ("no_preg" %ni% names(df)) df[,no_preg := 0]
    if ("no_ppa" %ni% names(df)) df[,no_ppa := 0]
    if ("intent_12m_only" %ni% names(df)) df[,intent_12m_only := 0]
    if ("cv_subgeo" %ni% names(df)) df[,cv_subgeo := 0]
    
    # source script containing survey fixes before starting the need for family planning algorithm
    source(file.path("<<<< FILEPATH REDACTED >>>>/ubcov_survey_fixes.R"), local = T)

    # run topic-specific code
    source("<<<< FILEPATH REDACTED >>>>/prep_ubcov_data_calc.R", local = T)

    # write output file
    name2 <- unique(paste0(df$nid, "_", df$country,"_", df$survey_series, "_",  df$year_start))
    write.csv(df, paste0("<<<< FILEPATH REDACTED >>>>", name2, ".csv"), row.names = F)
    
   }

  # preform counterfactual extractions for each counterfactual topic 
  mclapply(gold_nid, reextract_survey, mc.cores = cores.provided) %>% invisible
  
  ## combine all gold CSVs into RDS file
  counterfac_temp <- list.files("<<<< FILEPATH REDACTED >>>>", pattern = ".csv", full.names = T)
  all_data <- rbindlist(lapply(counterfac_temp, read.csv), fill = T)
  module_date <- format(Sys.Date(), "%Y_%m_%d")
  
  ## save all_women version
  saveRDS(all_data, file=paste0("<<<< FILEPATH REDACTED >>>>", "/",name, "_", module_date, ".RDS"))
  
  ## pass all_women version through collapse code
  data <- copy(all_data)
  source(paste0("<<<< FILEPATH REDACTED >>>>", "/mbg/1a_collapse.R"))
  
  ## save all_women version
  saveRDS(all, file=paste0("<<<< FILEPATH REDACTED >>>>", "/collapsed_",name, "_", module_date, ".RDS"))
  
  
}
