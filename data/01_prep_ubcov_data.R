############################################################################################################
## Launches extraction and indicator construction code for contraception ubcov (non-counterfactual) outputs
###########################################################################################################

## SET-UP --------------------------------------------------------------------

# clear memory
rm(list=ls())

# settings
cores.provided <- 20
pacman::p_load(data.table,magrittr,ggplot2,haven,stringr,parallel,dplyr, plyr) 

# in/out
team <-"<<<< TEAM NAME REDACTED >>>>"
in.dir <- "<<<< FILEPATH REDACTED >>>>"
out.dir <- "<<<< FILEPATH REDACTED >>>>"
temp.dir <- "<<<< FILEPATH REDACTED >>>>"

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# create function to convert CMC date to year and month 
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

# create function to count number of digits in a number
nDigits <- function(x) nchar( trunc( abs(x) ) )

# create function to return unique characters in a string
uniqchars <- function(x) unique(strsplit(x, "")[[1]])

# demographics
all_age_map <- fread("<<<< FILEPATH REDACTED >>>>/age_mapping.csv")
age_ids <- get_ids(table = "age_group")
age_ids <- age_ids[age_group_id %in% seq(8,14)]
age_ids[,age_group := gsub(" to ", "-", age_group_name)]
age_ids[,age_group_name := NULL]
age_ids <- all_age_map[age_group %in% age_ids$age_group]

# set values for gateways corresponding to counterfactual extractions (turn them all off for normal extraction)
counterfac_missing_sex <- 0 
counterfac_missing_infertile <- 0 
counterfac_missing_nomens <- 0 
counterfac_missing_nochild <- 0
counterfac_missing_desire <- 0 
counterfac_missing_desire_timing <- 0 
counterfac_missing_desire_later <- 0 
counterfac_no_preg <- 0 
counterfac_no_ppa <- 0 
counterfac_intent_12m <- 0
counterfac_cohab_sex <- 0
counterfac_cohab_contra <- 0 
counterfac_cohab_infertile <- 0
counterfac_cohab_nomens <- 0
counterfac_cohab_desire <- 0 
counterfac_cohab_desire_timing <- 0
counterfac_cohab_desire_later <- 0
counterfac_cohab_preg <- 0 
counterfac_cohab_ppa <- 0 

# find most recent version
geomatched_version <- most_recent_date(in.dir)
ubcov_newest <- paste0(team, "_", geomatched_version, ".RDS")

# read in data
df_all <- as.data.table(readRDS(file.path(in.dir, ubcov_newest)))

## KENYA DHS 2014 fix
# administered two versions of the survey and the short version does not contain all of the necessary questions to determine need at the gold standard level
# extravar_cont_3 was extracted to  capture the version type: 1 = long, 2= short
df_all[nid==157057 & extravar_cont_3 == 2, nid := 999999]
df_all[nid == 999999, no_ppa := 1][nid == 999999, missing_desire := 1] ## flag missing data for this short survey

## EXTRACTION FUNCTION -------------------------------------------------------

# define function to run extractions
extract_survey <- function(survey, input_dir) {
  
  df <- subset(df_all, nid == survey)
  print(unique(paste(df$survey_series, df$year_start)))
  
  if(length(unique(df$survey_series))==2) df[is.na(unique(survey_series)[2]), survey_series := unique(survey_series)[1]]
  
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

  # convert string variables with accents into non-accented characters
  df[,current_contra := gsub("\x82|\xe9","e",current_contra)]
  df[,current_contra := gsub("\\U3e33663c","o",current_contra)]
  df[,current_contra := gsub("\xf3|\xf4","o",current_contra)]
  df[,current_contra := gsub("\\U3e64653c","i",current_contra)]
  df[,current_contra := gsub("\xed","i",current_contra)]
  df[,current_contra := str_replace_all(iconv(current_contra, to="ASCII//TRANSLIT"),"'","")]
  if ("reason_no_contra" %in% names(df)) {
    df[,reason_no_contra := gsub("\x82|\xe9","e",reason_no_contra)]
    df[,reason_no_contra := gsub("\\U3e33663c","o",reason_no_contra)]
    df[,reason_no_contra := gsub("\xf3|\xf4","o",reason_no_contra)]
    df[,reason_no_contra := gsub("\\U3e64653c","i",reason_no_contra)]
    df[,reason_no_contra := gsub("\xed","i",reason_no_contra)]
    df[,reason_no_contra := str_replace_all(iconv(reason_no_contra, to="ASCII//TRANSLIT"),"'","")]
  }
 
  # source script containing survey fixes before starting the need for family planning algorithm
  source(file.path("<<<< FILEPATH REDACTED >>>>/ubcov_survey_fixes.R"), local = T)
  
  # merge on appropriate age groups for the collapse code (but restrict if survey itself does not cover full age range)
  df[,age_year := floor(age_year)]
  df <- merge(df, age_ids, by = "age_year", allow.cartesian = T)

  # run actual extraction calculation script
  source(file.path("<<<< FILEPATH REDACTED >>>>/indicator_calc.R"), local = T) 

  ## write output file ("mapped data")
  write.csv(df, file.path(temp.dir, paste0(survey, ".csv")), row.names = F) #folder, 
}

## RUN EXTRACTIONS -----------------------------------------------------------

nid_run <- as.list(unique(df_all$nid))
# actually launch extraction
mclapply(nid_run, extract_survey, input_dir = in.dir, mc.cores = cores.provided)

## COMBINE EXTRACTIONS -----------------------------------------------------------

ubcov_csv <- list.files(temp.dir, pattern = ".csv", full.names = T)

#bind all extracted surveys together
csvs <- rbindlist(lapply(ubcov_csv, read.csv), fill = T)

module_date <- format(Sys.Date(), "%Y_%m_%d")
saveRDS(csvs, file=paste0(out.dir, "/", team, "_", module_date, ".RDS"))

