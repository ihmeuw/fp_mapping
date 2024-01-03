############################################################################################################
## Custom code to process UbCov outputs; survey-specific and general fixes
###########################################################################################################

## FUNCTIONS ------------------------------------------------------------------

# convert CMC date to year and month
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12)
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)

get_cmc <- function(year, month){
  month <- ifelse(month >12, 1, month)
  year <- ifelse(year >2050, NA, year)
  year <- ifelse(year <2000, substr(year,3,4), year)
  year <- as.numeric(year)
  cmc <- ifelse(year <2000, (year * 12) + month, ((year-1900) * 12) + month)
  cmc <- as.integer(cmc)
  return(cmc)
}

## SURVEY-SPECIFIC FIXES ------------------------------------------------------------------

# impute interview_month and interview_year if provided in basic codebook
if ("int_month" %in% names(df)) df[, interview_month := int_month]
if ("int_year" %in% names(df)) df[, interview_year := int_year]

## convert last_birth_date to month and year if in CMC format
if (unique(df$nid) %in% c(409558,132739,43126,42958,200685,281744)) {
  df[!is.na(last_birth_date), last_birth_month := get_cmc_month(last_birth_date)]
  df[!is.na(last_birth_date), last_birth_year := get_cmc_year(last_birth_date)]
}

## Convert PMA NGA 2014 values to match other rounds
if (unique(df$nid) == 286022) {

  df <- df %>%
    mutate_at(c("last_menses_unit"), funs(recode(., `1`=-99, `2`=1, `3`=2, `4`=3, `5`=4, `6`=5, `7`=6, `8`=7, .default = NaN))) %>%
    mutate_at(c("last_sex_unit","desire_unit", "desire_gate","extravar_cont_5"), funs(recode(., `1`=-99, `2`=-88, `3`=1, `4`=2, `5`=3, `6`=4, `7`=5, .default = NaN))) %>%
    mutate_at(c("extravar_cont_3","intent_gate"), funs(recode(., `1`=-99, `2`=-88, `3`=0, `4`=1, .default = NaN)))
  df <- as.data.table(df)
}

## PMA2020 and PMA updates
if (grepl("pma", tolower(unique(df$survey_name)))) {

  if ("first_cohabit_year" %in% names(df)) {
    ## Convert first_cohabit_year into numeric
    if (any(grepl(",", df$first_cohabit_year))) df[first_cohabit_year != "",first_cohabit_date := as.Date(first_cohabit_year, format="%b %d, %Y")]
    if (any(grepl("-", df$first_cohabit_year))) df[first_cohabit_year != "",first_cohabit_date := as.Date(first_cohabit_year, format="%Y-%m-%d")]
    df[, first_cohabit_month:= month(first_cohabit_date)]
    df[, first_cohabit_year:= year(first_cohabit_date)][first_cohabit_year>=2030, first_cohabit_year:= NA]
  }
  if ("last_birth_date" %in% names(df)) {
    ## Convert recent birth date from string to CMC
    if (any(grepl(",", df$last_birth_date))) df[last_birth_date != "",last_birth_date2 := as.Date(last_birth_date, format="%b %d, %Y")]
    if (any(grepl("-", df$last_birth_date))) df[last_birth_date != "",last_birth_date2 := as.Date(last_birth_date, format="%Y-%m-%d")]
    if ("last_birth_date2" %in% names(df)) {
      df[, last_birth_month:= month(last_birth_date2)]
      df[, last_birth_year:= year(last_birth_date2)][last_birth_year>=2030, last_birth_year:= NA]
      df[, last_birth_date := get_cmc(last_birth_year, last_birth_month)]
      df$last_birth_date2 <- NULL
    } else {
      df[!is.na(last_birth_date), last_birth_month := get_cmc_month(last_birth_date)]
      df[!is.na(last_birth_date), last_birth_year := get_cmc_year(last_birth_date)][last_birth_year>=2030, last_birth_year:= NA]
    }
  }
  if ("recent_cohabit_start_date" %in% names(df)) {
    ## Convert recent cohabit date from string to CMC
    if (any(grepl(",", df$recent_cohabit_start_date))) df[recent_cohabit_start_date != "",recent_cohabit_start_date2 := as.Date(recent_cohabit_start_date, format="%b %d, %Y")]
    if (any(grepl("-", df$recent_cohabit_start_date))) df[recent_cohabit_start_date != "",recent_cohabit_start_date2 := as.Date(recent_cohabit_start_date, format="%Y-%m-%d")]
    if ("recent_cohabit_start_date2" %in% names(df)) {
      df[, recent_cohabit_start_date := get_cmc(year(recent_cohabit_start_date2), month(recent_cohabit_start_date2))]
      df$recent_cohabit_start_date2 <- NULL
    }
  }
  if ("interview_date" %in% names(df)) {
    ## Convert interview date from string to CMC
    if (any(grepl(",", df$interview_date))) df[interview_date != "",interview_date2 := as.Date(interview_date, format="%b %d, %Y")]
    if (any(grepl("-", df$interview_date))) df[interview_date != "",interview_date2 := as.Date(interview_date, format="%Y-%m-%d")]
    if ("interview_date2" %in% names(df)) {
      df[, int_month := month(interview_date2)][, interview_month := int_month]
      df[, int_year := year(interview_date2)][int_year>=2030, int_year:= NA][, interview_year := int_year]
      df[, interview_date := get_cmc(year(interview_date2), month(interview_date2))]
      df$interview_date2 <- NULL
    } else {
      df[, int_month := get_cmc_month(interview_date)][, interview_month := int_month]
      df[, int_year := get_cmc_year(interview_date)][int_year>=2030, int_year:= NA][, interview_year := int_year]
    }
  }

  ## Get additional infertile information
  df[desire_unit == 4, desire_children_infertile := 1]

  ## Combine desire_unit and pregnant version into 1 variable
  if ("extravar_cont_5" %in% names(df)) df[!is.na(extravar_cont_5) & is.na(desire_unit), desire_unit := extravar_cont_5]

  ### PMA BFA 2018 one woman marked as male. This person is included in PMA calculations for indicators and completed the women's questionnaire.
  if (unique(df$nid)==407829)  df[sex_id==1 & extravar_cont_1==1, sex_id := 2]


  ### PMA only keep completed and partially completed results
  if (unique(df$nid) %in% c(493322,493511,493363))  df <- df[extravar_cont_1 == 1 | extravar_cont_1 == 5]

  ### PMA combine variables that are split for women with and without children
  if (grepl("jhsph_performance_for_action_pma", tolower(unique(df$survey_name)))) {
    if ("extravar_cont_3" %in% names(df)) df[is.na(desire_gate), desire_gate := extravar_cont_3]
    if ("extravar_cont_4" %in% names(df)) df[is.na(desire_unit), desire_unit := extravar_cont_4]
  }

  ## PMA reason_no_contra extraction changed to show all reasons. Labels no longer being used--update string so it is not captured in "need_nomens" part of indicator calculation.
  if("reason_no_contra" %in% names(df)) df[,reason_no_contra:=gsub("not_menstruated", "no menses since last birth",reason_no_contra)]

  ### PMA get timing for intent -- no specific timing
  if (grepl("jhsph_performance_for_action_pma", tolower(unique(df$survey_name)))) {
    if ("intent_use" %in% names(df)) {
      df[, intent_timing := intent_use]
      df[!is.na(intent_gate) | !is.na(intent_gate_preg), intent_use := 0][intent_gate==1 | intent_gate_preg ==1, intent_use := 1] ## use gateway variables for ever use contraception
    }
  }

}

if ("first_cohabit_year" %in% names(df)) df[,first_cohabit_year := as.numeric(first_cohabit_year)]
if ("last_birth_date" %in% names(df)) df[,last_birth_date := as.numeric(last_birth_date)]
if ("recent_cohabit_start_date" %in% names(df)) df[,recent_cohabit_start_date := as.numeric(recent_cohabit_start_date)]
if ("interview_date" %in% names(df)) df[,interview_date := as.numeric(interview_date)]

## Convert intent use to binary if being extracted incorrectly
if ("intent_use" %in% names(df)) {
  ## intent use being captured as character, not binary
  if (is.character(df$intent_use) & "0" %ni% unique(df$intent_use)) {
    df[, intent_use := as.numeric(as.factor(intent_use))]
    df <- df %>% mutate(intent_use=recode(intent_use, `1`=0,`2`=1, .default = NaN))
  }
  df[, intent_use := as.numeric(intent_use)]
}

## DHS3 and a few DHS4 SURVEYS
## lactational amenorrhea was not asked about as a contraceptive method, but had a separate question asking if women are
## currently relying on breastfeeding to not get pregnant; using year cutoff to remove untrustworthy/bogus responses
if (unique(grepl("dhs3", tolower(unique(df$file_path)))) | unique(df$nid) %in% c(19670,19656,20947)) {
  if ("extravar_cont_1" %in% names(df)) df[extravar_cont_1 == 1 & current_contra == "not using" & months_since_last_birth <= 12, current_contra := "lactational amenorrhea method"]
}

## BFA MICS 2006
if (unique(df$nid) == 1927) {
  # capturing additional desire gateway
  df[is.na(desire_gate), desire_gate := extravar_cont_1]
  df[extravar_cont_1 == 3, desire_children_infertile := 1]
}


## KEN DHS 2003
if (unique(df$nid) %in% c(20145)) {
  df[is.na(survey_series), survey_series := "MACRO_DHS"]
}

##  NGA HH_SCHOOL_AND_HEALTH_FACILITY_SURVEYS 2007
if (unique(df$nid) %in% c(50426)) {
  df[int_year != 2007, int_year := 2007] ##Interview yer listed as 1900 and 1901 for 7 observations. Results show it should be in 2007
  df[is.na(survey_series), survey_series := "COUNTRY_SPECIFIC"]
}

##  NGA NARHS 2007
if (unique(df$nid) %in% c(325046)) df[is.na(survey_series), survey_series := "COUNTRY_SPECIFIC"]

## KEN AIDS Indicator survey has gateway before marital status question and also didn't
## count infertility from sterilization as contraceptive use in some cases
if (unique(df$nid) == 133304) {
  df[is.na(curr_cohabit), former_cohabit := 0]
  df[is.na(curr_cohabit), curr_cohabit := 0]
  df[no_menses == 1, current_contra := "female sterilization"]
  df[no_menses == 1, current_use := 1]
  df[, no_menses := NULL]
}

## NGA General Household Survey 2010-2011 and NGA General Household Survey 2006-2007
## ever pregnant used as a gateway for currently pregnant, can assume all NA's are not pregnant
if (unique(df$nid) %in% c(151802,24890)) df[is.na(pregnant), pregnant := 0]
## additionally the NGA GHS 2006-2007 Survey has an issue where every woman has a method coded despite the survey flow
## instructing them to skip method if they say no to the previous question of current using family planning
## only keep method response if current_use == 1
if (unique(df$nid == 24890)) df[current_use != 1, current_contra := '']
if (unique(df$nid == 24915)) df[is.na(survey_series), survey_series := "COUNTRY_SPECIFIC"]

## NGA NAIIS
if (unique(df$nid == 399235)) df[is.na(survey_series), survey_series := "COUNTRY_SPECIFIC"]
if (unique(df$nid == 399235)) df[is.na(pregnant), pregnant := 0] ## women who report no pregnancies not asked about being currently pregnant

## KEN URBAN 2010 Survey
if (unique(df$nid) == 165740) df[extravar_cont_1 == 2, current_use := 0]

## NGA LIVING STANDARDS SURVEY 2008-10 coded contraceptives using strings of numbers
if (unique(df$nid) == 151719) {
  df[current_contra == "..",  current_contra := "not using"]
  df[current_contra == "01",  current_contra := "pill"]
  df[current_contra == "02",  current_contra := "condom"]
  df[current_contra == "03",  current_contra := "injection"]
  df[current_contra == "04",  current_contra := "iud"]
  df[current_contra == "05",  current_contra := "female sterilization"]
  df[current_contra == "06",  current_contra := "male sterilization"]
  df[current_contra == "07",  current_contra := "douche"]
  df[current_contra == "08",  current_contra := "implant"]
  df[current_contra == "09",  current_contra := "foam"]
  df[current_contra == "10",  current_contra := "diaphragm"]
  df[current_contra == "16",  current_contra := "other"]

  df[is.na(survey_series), survey_series := "COUNTRY_SPECIFIC"]
}

## NGA National Nutrition Health Survey NNHS 2014
if (unique(df$nid)==153674) df[int_year==2012, int_year := 2014]

## Special Case: fix iso3 code
if (unique(df$nid)==155335)  df[, ihme_loc_id := "KEN_44797"]
if (unique(df$nid)==7401)  df[, ihme_loc_id := "KEN_44795"]

## MICS 2008 KEN-- desire only asked to women using contraception. Crosswalking, so set to NULL
if (unique(df$nid)==7401)  df[, desire_soon := NULL][, desire_timing := NULL][, desire_unit := NULL][, desire_gate := NULL]

## MICS additional information captured to recreate indicators -- format dates
if (grepl("UNICEF_MICS", unique(df$survey_series))) {
    if ("recent_cohabit_start_date" %in% names(df)) df[, recent_cohabit_start_date := as.numeric(recent_cohabit_start_date)]
    if ("last_birth_date" %in% names(df)) df[, last_birth_date := as.numeric(last_birth_date)]
    if ("interview_date" %in% names(df)) df[, interview_date := as.numeric(interview_date)]
}

## GENERAL FIXES --------------------------------------------------------------------------

# catch instances in which interview_year was reported as YYYY and last_birth_year as YY, and vice versa
if (all(c("interview_year", "last_birth_year") %in% names(df))) {
  df[nDigits(interview_year) == 4 & nDigits(last_birth_year) == 2, last_birth_year := round_any(interview_year, 100, floor) + last_birth_year]
  df[nDigits(interview_year) == 2 & nDigits(last_birth_year) == 4, interview_year := round_any(last_birth_year, 100, floor) + interview_year]
}

# catch instances in which interview_year was reported as YYYY and first_cohabit_year as YY, and vice versa
if (all(c("interview_year", "first_cohabit_year") %in% names(df))) {
  df[nDigits(interview_year) == 4 & nDigits(first_cohabit_year) == 2, first_cohabit_year := round_any(interview_year, 100, floor) + first_cohabit_year]
  df[nDigits(interview_year) == 2 & nDigits(first_cohabit_year) == 4, interview_year := round_any(first_cohabit_year, 100, floor) + interview_year]
}

# catch instances in which marital status was a gateway before currently pregnant
# does not matter what the women not asked get assigned to since they will get dropped
# from the contraception estimates, but is important for marital status calculations
if (all(c("curr_cohabit","former_cohabit","pregnant") %in% names(df))) {
  if ("contra_evermar_only" %in% names(df)) df[is.na(pregnant) & curr_cohabit==0 & former_cohabit==0, pregnant:=0]
  if ("contra_currmar_only" %in% names(df)) df[is.na(pregnant) & curr_cohabit==0, pregnant:=0]
}

# catch instances in which abstinence (not periodic abstinence) was counted as a contraceptive method
if ("current_contra" %in% names(df)) {
  if ("abstinence" %in% tolower(unique(df$current_contra))) {
    if ("current_use" %in% names(df)) df[tolower(current_contra)=="abstinence", current_use:=0]
    df[tolower(current_contra)=="abstinence", current_contra:='not using - abstinence']
  }
  if ("fp method: abstinence" %in% tolower(unique(df$current_contra))) {
    if ("current_use" %in% names(df)) df[tolower(current_contra)=="fp method: abstinence", current_use:=0]
    df[tolower(current_contra)=="fp method: abstinence", current_contra:='not using - abstinence']
  }
  if ("abstention" %in% tolower(unique(df$current_contra))) {
    if ("current_use" %in% names(df)) df[tolower(current_contra)=="abstention", current_use:=0]
    df[tolower(current_contra)=="abstention", current_contra:='not using - abstinence']
  }
}

# current use sometimes created (but all NA) in this process, remove these cases
if ("current_use" %in% names(df)){
  if (length(unique(df$current_use)) == 1){
    if (is.na(unique(df$current_use))) df[,current_use := NULL]
  }
}

# curr_cohabit sometimes created (but all NA) in this process, remove these cases
if ("curr_cohabit" %in% names(df)){
  if (length(unique(df$curr_cohabit)) == 1){
    if (is.na(unique(df$curr_cohabit))) df[,curr_cohabit := NULL]
  }
}

# Remove all men and those women that are outside of ages 15-49
df <- df[sex_id == 2 & age_year >= 15 & age_year < 50]

