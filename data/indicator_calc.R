####################################################################################################
## Topic-specific code for contraception ubcov extraction
##          Main goals are to:
##          1. Fix survey-specific issues to make all surveys comparable
##          2. Determine which women were asked about contraceptive usage, and how many of them are using a modern method
##          3. Determine which women who are not using a method fit our definition of having a need for contraception
##          4. Classify women as having a met demand for contraception (or an unmet demand)
##          5. Allow re-extractions of surveys under certain counterfactual scenarios (ex. if missing all info on fecundity)
####################################################################################################

# for reference: %ni% is opposite of %in%

# by default, study-level covariates are false unless otherwise specified
if ("cv_subgeo" %ni% names(df)) df[,cv_subgeo := 0]

## helper functions 
# return unique characters in a string
uniqchars <- function(x) unique(strsplit(x, "")[[1]])

# convert CMC date to year and month 
get_cmc_year <- function(x) 1900 + as.integer((as.integer(x) - 1) / 12) 
get_cmc_month <- function(x) as.integer(x) - 12 * (get_cmc_year(x) - 1900)
  
####################################################################################################
###### CALCULATE SEXUAL ACTIVITY AND TIMING OF DESIRE FOR CHILDREN #################################
####################################################################################################

## For some surveys, time since last ___ was coded with two variables, a number and its units (ex. days, weeks, months), so have
## to calculate who falls into which category manually

###### LAST SEXUAL ACTIVITY #######################################################
if (all(c("last_sex","last_sex_unit") %in% names(df))) {

  ## Global Fund survey and KEN URBAN survey
  df[grepl("gf_household_survey|urban_reproductive_health_initiative|cpv_rhs_1998",tolower(file_path)) & ((last_sex >= 100 & last_sex <= 131) | (last_sex >= 200 & last_sex <= 204) | last_sex == 300), sex_in_last_month := 1]
  
  ## SPECIAL DHS SURVEYS, PMA2020, PMA, MICS, KEN AIDS 2007, EDSA BOL 2016
  df[(nid %in% c(133219,323944) | grepl("uzb_sp_dhs4|gha_sp_dhs4|gha_sp_dhs7|mwi_kap|tza_kap|sen_dhs4_1999|uga_in_dhs3|nic_dhs_endesa_2011|tls_dhs_2003|_pma_|_pma2020|_mics",tolower(file_path))) &
       ((last_sex_unit == 1 & last_sex <= 30) | (last_sex_unit == 2 & last_sex <= 4) | (last_sex_unit == 3 & last_sex < 1)), sex_in_last_month := 1]
  
  ### Special situation, MICS 2006 and 2007 surveys only asked sexual activity to women 15-24 (NIDS 1927 and 9516)
  if(df$nid == 1927 || df$nid == 9516) df[, sex_in_last_month := NULL]
}

  ### Special situation for NGA HH_SCHOOL_AND_HEALTH_FACILITY_SURVEYS: last sexual activity for days, weeks, and months are in different variables ###
  if(df$nid == 50393 || df$nid == 50426 || df$nid == 50441){
    df$temp_weeks <- df$last_sex ##number of weeks since last sexual activity
    df$temp_months <- df$last_sex_unit ##number of months since last sexual activity
    df[is.na(last_sex), last_sex := temp_months][is.na(last_sex), last_sex := sex_in_last_month] ##fill in last_sex variable (currently only weeks) with days and months
    df[!is.na(sex_in_last_month), last_sex_unit := 1][!is.na(temp_weeks), last_sex_unit := 2][!is.na(temp_months), last_sex_unit := 3] ##fill in unit information
    
    df[is.na(sex_in_last_month) & !is.na(temp_weeks) & temp_weeks <=4 & temp_weeks >=1, sex_in_last_month := 1][is.na(sex_in_last_month) & !is.na(temp_weeks) & temp_weeks >4, sex_in_last_month := 0] ##add in week information for T/F
    df[is.na(sex_in_last_month) & !is.na(temp_months) & temp_months <=1, sex_in_last_month := 1][is.na(sex_in_last_month) & !is.na(temp_months) & temp_months >1, sex_in_last_month := 0] ##add in month information for T/F
    
    df$temp_weeks <- NULL
    df$temp_months <- NULL
    }

##### TIMING FOR DESIRE OF FUTURE CHILDREN #########################################
## Women who do not want any more children or are unsure OR want children in 2+ years or are undecided on timing have need for contraception and should be marked as 1 for desire_later

# identify women who do not want any more children or are unsure 
if ("desire_gate" %in% names(df)) {
  
  # catch desire_gate for pregnant women when asked separately (necessary for counterfactual re-extractions)
  if ("desire_gate_preg" %in% names(df)) df[is.na(desire_gate), desire_gate := desire_gate_preg]
  
  
  # create desire_limiting variable to only capture desire responses for future children without consideration of timing 
  df[, desire_limiting := as.numeric(NA)]
  
  # non-standardized surveys
  df[nid %in% c(26919) & desire_gate == 0, desire_limiting := 1]
  
  df[nid %in% c(27301,43381,43294) & desire_gate == 1, desire_limiting := 1]
  
  df[nid %in% c(20467,23219,23183) & desire_gate %in% c(2,3), desire_limiting := 1]
  
  df[nid %in% c(323944,43016,24159,125230,165390,462712) & desire_gate %in% c(2,4), desire_limiting := 1]
  
  df[nid %in% c(13218,11160,115896,11240,11300,115895,11239,11214,11271,11299) & desire_gate %in% c(2,7), desire_limiting := 1]
  
  df[nid %in% c(27638,27563,4779,133304,133219,165740,10001,20596,104316,20351,90705,20998.8750) & desire_gate %in% c(2,8), desire_limiting := 1]
  
  df[nid %in% c(27511,27551,10364,10326,27615,27621,27630,5025,24006,90707,43189,30261,23982) & desire_gate %in% c(2,9), desire_limiting := 1]
  
  df[nid %in% c(141948) & desire_gate %in% c(2,98), desire_limiting := 1]
  
  df[nid %in% c(326837) & desire_gate %in% c(2,3,4), desire_limiting := 1]
  
  df[nid %in% c(18815,20888,23258) & desire_gate %in% c(2,4,5), desire_limiting := 1]
  
  df[nid %in% c(50393,50426,50441) & desire_gate %in% c(2,5,6), desire_limiting := 1]
  
  df[nid %in% c(153674) & desire_gate %in% c(2,4,5,88), desire_limiting := 1]
  
  df[nid %in% c(27486,27494,95336,27321) & desire_gate %in% c(2,4,8), desire_limiting := 1]
  
  df[nid %in% c(126952,22125) & desire_gate %in% c(3,5), desire_limiting := 1]
  
  df[nid %in% c(404407,240604,14486,10370,27525,9270) & desire_gate %in% c(3,9), desire_limiting := 1]
  
  df[nid %in% c(27215) & desire_gate %in% c(3,5,6), desire_limiting := 1]
  
  df[nid %in% c(43126,42958,200685,281744,403068) & desire_gate %in% c(5,9), desire_limiting := 1]
  
  # survey series
  if (all(is.na(df$desire_limiting))) {
    
    # World Fertility Surveys and DHS 
    df[grepl('_wfs|_dhs',tolower(file_path)) & desire_gate %in% c(2,3), desire_limiting := 1]
    
    # PAPCHILD 
    df[grepl('_papchild',tolower(file_path)) & desire_gate %in% c(2,4), desire_limiting := 1]
    
    # MICS
    df[grepl("_mics|_papfam",tolower(file_path)) & desire_gate %in% c(2,8), desire_limiting := 1]
    
    # PMA 2020
    df[grepl("_pma2020_|_pma_",tolower(file_path)) & desire_gate %in% c(2,-88), desire_limiting := 1]
  }
  
  # update desire_later
  df[desire_limiting == 1, desire_later := 1]
}

# identify women who do want more children, but want to wait 2+ years 
if (all(c("desire_timing","desire_unit") %in% names(df)) ) {
  
  # create desire_spacing variable
  df[, desire_spacing := as.numeric(NA)]
  
  # KEN AIDS Indicator survey 2012
  df[nid == 133304 & (desire_unit == 88 | (desire_unit == 1 & desire_timing >= 24) | (desire_unit == 2 & desire_timing >= 2)), desire_spacing := 1]
  
  # KEN AIDS Indicator survey 2007
  df[nid == 133219 & (desire_timing %in% c(95,98) | (desire_unit == 1 & desire_timing >= 24) | (desire_unit == 2 & desire_timing >= 2)), desire_spacing := 1]
  
  # KEN URBAN
  df[grepl("urban_reproductive_health_initiative",tolower(file_path)) & (desire_timing >= 202 & desire_timing != 993), desire_spacing := 1]
  
  # NGA HOUSEHOLD SCHOOL AND HEALTH FACILITY SURVEY 
  df[nid %in% c(50393,50426,50441) & (desire_timing == 95 | desire_timing == 98 | desire_unit >= 2), desire_spacing := 1]
  
  # survey series
  if (all(is.na(df$desire_spacing))) {
    
    # ALL PMA2020 SURVEYS
    df[grepl("_pma2020_|_pma_",tolower(file_path)) & (desire_unit == 5 | desire_unit == -88 | (desire_unit == 1 & desire_timing >= 24) | (desire_unit == 2 & desire_timing >= 2)), desire_spacing := 1]
    
    # ALL MICS SURVEYS (THAT HAVE DESIRE OF FUTURE CHILDREN INFORMATION)
    df[grepl("_mics",tolower(file_path)) & desire_timing >= 99, desire_timing := NA]
    df[grepl("_mics",tolower(file_path)) & (desire_timing >= 95 | (desire_unit == 1 & desire_timing >= 24) | (desire_unit == 2 & desire_timing >= 2)), desire_spacing := 1]
  }
  
  df[desire_spacing == 1, desire_later := 1]
}

## if there is desire information, set NAs to 0
if ("desire_later" %in% names(df)){
  if ("desire_soon" %in% names(df)) df[is.na(desire_later) & !is.na(desire_soon), desire_later := 0]
  if ("desire_gate" %in% names(df)) df[is.na(desire_later) & !is.na(desire_gate), desire_later := 0]
  if ("desire_unit" %in% names(df)) df[is.na(desire_later) & !is.na(desire_unit), desire_later := 0]
  if ("desire_timing" %in% names(df)) df[is.na(desire_later) & !is.na(desire_timing), desire_later := 0]
} else if ("desire_soon" %in% names(df)){
  if ("desire_gate" %in% names(df)) df[is.na(desire_soon) & !is.na(desire_gate), desire_soon := 0]
  if ("desire_unit" %in% names(df)) df[is.na(desire_soon) & !is.na(desire_unit), desire_soon := 0]
  if ("desire_timing" %in% names(df)) df[is.na(desire_soon) & !is.na(desire_timing), desire_soon := 0]
}

####################################################################################################
###### LAST MENSTRUATION (CONTINUOUS IN MONTHS) ####################################################
####################################################################################################

## generate continuous estimate (in months) of time since last menstruation
## set months_since_last_menses to 999 for "before last birth" responses to identify amenorrheic women 

if (all(c("last_menses","last_menses_unit") %in% names(df))) {
  
  df[nid == 240604 & last_menses_unit == 1, months_since_last_menses := last_menses/30]
  df[nid == 240604 & last_menses_unit == 2, months_since_last_menses := last_menses/4.3]
  df[nid == 240604 & last_menses_unit == 3, months_since_last_menses := last_menses]
  df[nid == 240604 & last_menses_unit == 4, months_since_last_menses := last_menses*12]
  
  ## ALL PMA2020 and PMA SURVEYS
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 1, months_since_last_menses := last_menses/30]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 1 & last_menses > 99, months_since_last_menses := 1/30]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 2, months_since_last_menses := last_menses/4.3]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 2 & last_menses > 99, months_since_last_menses := 1/4.3]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 3, months_since_last_menses := last_menses]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 3 & last_menses > 99, months_since_last_menses := 1]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 4, months_since_last_menses := last_menses*12]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 4 & last_menses > 99, months_since_last_menses := 1*12]
  df[grepl("_pma2020_|_pma_",tolower(file_path)) & last_menses_unit == 6, months_since_last_menses := 999]
  
  ## MICS SURVEYS that have info on last menstruation
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 1, months_since_last_menses := last_menses/30]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 1 & last_menses == 99, months_since_last_menses := 1/30]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 2, months_since_last_menses := last_menses/4.3]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 2 & last_menses == 99, months_since_last_menses := 1/4.3]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 3, months_since_last_menses := last_menses]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 3 & last_menses == 99, months_since_last_menses := 1]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 4, months_since_last_menses := last_menses*12]
  df[grepl("_mics",tolower(file_path)) & last_menses_unit == 4 & last_menses == 99, months_since_last_menses := 1*12]
  df[grepl("_mics[1-5]",tolower(file_path)) & last_menses_unit == 9 & last_menses == 95, months_since_last_menses := 999]
  df[grepl("_mics6",tolower(file_path)) & last_menses_unit == 9 & last_menses == 94, months_since_last_menses := 999]


} else if ("last_menses" %in% names(df)) {
  
  ## amenorrheic women
  df[nid %in% c(27590) & last_menses == 46, months_since_last_menses := 999]
  df[nid %in% c(27615,14486,27572) & last_menses == 96, months_since_last_menses := 999]
  df[nid %in% c(14243,7161) & last_menses == 97, months_since_last_menses := 999]
  df[nid %in% c(27599,27606) & last_menses == 444, months_since_last_menses := 999]
  df[nid %in% c(27630,9270) & last_menses == 555, months_since_last_menses := 999]
  df[(nid %in% c(10326,10364,10370,27525) | grepl("district_level_household_survey",tolower(file_path))) & last_menses == 994,months_since_last_menses := 999]
  df[(nid %in% c(4779, 165290,27215) | grepl("dhs_prog|macro_dhs|urban_reproductive_health_initiative|per_endes|cpv_rhs",tolower(file_path))) & last_menses == 995,months_since_last_menses := 999]
  
}

####################################################################################################
##### TIMING OF LAST BIRTH #########################################################################
####################################################################################################

## calculate time since most recent birth (done very differently depending on info available in each survey)
if (all(c("last_birth_year","last_birth_month","interview_year","interview_month") %in% names(df))) {
  ## MICS & PMA: determine months since most recent birth 
  ## (PMA requires some variables updates in ubcov_survey_fixes prior to this step)
  df[last_birth_month > 12,last_birth_month := 6]
  df[interview_month > 12,interview_month := 6]
  df[last_birth_year > 9990, last_birth_year := NA]
  df[interview_year > 9990, interview_year := NA]
  df[ grepl("_pma2020_",tolower(df[1,file_path])) & year_start<2019 & last_birth_year==2020, last_birth_year := NA]
  df[ grepl("_pma2020_",tolower(df[1,file_path])) & year_start<2019 & interview_year==2020, interview_year := NA]
  df[ grepl("_pma_",tolower(df[1,file_path])) & year_start>=2019 & last_birth_year==2030, last_birth_year := NA]
  df[ grepl("_pma_",tolower(df[1,file_path])) & year_start>=2019 & interview_year==2030, interview_year := NA]
  
  df[last_birth_month > interview_month, months_since_last_birth := ((interview_year-last_birth_year-1)*12) + (interview_month + 12 - last_birth_month)]
  df[interview_month >= last_birth_month, months_since_last_birth := ((interview_year-last_birth_year)*12) + (interview_month - last_birth_month)]
}

## determine who is postpartum amenorrheic (regardless of how long they have been amenorrheic)
if (all(c("months_since_last_menses","months_since_last_birth") %in% names(df))) df[pregnant == 0 & months_since_last_menses >= months_since_last_birth,ppa := 1]
if ("reason_no_contra" %in% names(df)) df[pregnant == 0 & grepl("amenorr|no menses since last birth|Post partum",reason_no_contra),ppa := 1]

## correct women who identified as ppa but are actually not
if (all(c("ppa","months_since_last_menses","months_since_last_birth") %in% names(df))) df[ppa == 1 & months_since_last_menses <= months_since_last_birth, ppa := 0]


## Women without their period for 6 or more months are considered infecund as long as they are not postpartum amenorrheic
## If they are postpartum amenorrheic, must have been 5 years or more since last child was born to be considered infecund
## When missing_fecund or no_ppa, not possible to calculate this
  if (all(c("ppa","months_since_last_menses") %in% names(df))) {
    df[is.na(ppa) & months_since_last_menses >= 6, no_menses := 1]
    if ("months_since_last_birth" %in% names(df)) df[ppa == 1 & months_since_last_birth >= 60, no_menses := 1]
  } else if ("months_since_last_menses" %in% names(df)) {
    df[months_since_last_menses >= 6, no_menses := 1]
  } else if ("ppa" %in% names(df)){
    df[ppa == 1, no_menses := 1]
  }

####################################################################################################
############ CONTRACEPTIVE USAGE ###################################################################
####################################################################################################
  
## clarify missingness that arises when question about former cohabitation is separate and asked to a subset of those who said "no" to
## current cohabitation
if (all(c("curr_cohabit","former_cohabit") %in% names(df))) {
  df[is.na(former_cohabit) & !is.na(curr_cohabit), former_cohabit := 0]
  df[is.na(curr_cohabit) & !is.na(former_cohabit), curr_cohabit := 0]
  df[curr_cohabit == 1,former_cohabit := 0]
}

## Most surveys use the answer to whether or not a woman is pregnant as a gateway for asking about contraceptive usage. The codebook is set up such that
## any answer that passes through the gateway is coded as 0 (since it is always "no" or "not sure"), and those that specifically say they are pregnant are 1.
## Therefore, any missingness in pregnant variable correspond to individuals who were not asked about contraception, and should be dropped
## However, some surveys only bothered to fill out the variable when pregnant == 1, leaving everything else NA. In these scenarios, must assume that everyone with NA was not pregnant
if (nrow(df[pregnant == 0]) == 0) df[is.na(pregnant), pregnant := 0]

df[pregnant == 1, any_contra := 0]
if ("current_use" %ni% names(df)) df[is.na(pregnant) & current_contra == "", any_contra := -1] ## exclude these cases from calculation

## COUNTERFACTUAL set up: Set contra usage for non-cohabitating women
if(counterfac_cohab_contra==1) df[curr_cohabit==0, current_use := 0][curr_cohabit==0, never_used_contra := NA][curr_cohabit==0, current_contra := ""]

## create string patterns found in current_contra that correspond to non-use of contraception, and also string patterns for
## answers that don't actually tell us if or what contraceptive is being used
nonuse <- paste(c("none",
                  "not using",
                  "no method",
                  "not current user",
                  "not currently using",
                  "no contraceptive",
                  "no usa",
                  "no esta usa",
                  "no estan usa",
                  "nonuser",
                  "non-user",
                  "not expose",
                  "non expose",
                  "n expose",
                  "not user",
                  "no current use",
                  "nutilise"),
                collapse = "|")

unknown <- paste(c("refused",
                   "^dna",
                   "don't know",
                   "dont know",
                   "not stated",
                   "not eligible",
                   "no respon",
                   "non repons",
                   "-99",
                   "-88",
                   "^na$"), 
                 collapse = "|")

## When current_use variable is missing, must assume everyone not pregnant was asked about contraception
if ("current_use" %ni% names(df)) {
  
  ## unless specifically marked as missing/unknown, assume women were asked and should therefore be in denominator
  df[!is.na(current_contra) & !grepl(nonuse,tolower(current_contra)) & !grepl(unknown,tolower(current_contra)) & is.na(any_contra),any_contra := 1]
  df[!grepl(unknown,tolower(current_contra)) & is.na(any_contra), any_contra := 0]
  df[grepl(unknown,tolower(current_contra)) & is.na(any_contra), any_contra := -1]
  
  
} else {
  ## Survey does have a gateway variable corresponding to current use
  
  ## some surveys still leave current usage as missing if women already answered that she had never used a method
  if ("never_used_contra" %in% names(df)) df[never_used_contra == 1, current_use := 0]
  
  ## Some surveys do not ask about current usage if the woman was sterilized, even if it was for family planning purposes,
  ## so current_contra may say "sterilized" when current_use = 0.
  ## This fixes that issue by saying any actual methods provided in current_contra mean that they are currently using
  df[current_contra != "" & !grepl(nonuse,tolower(current_contra)) & !grepl(unknown,tolower(current_contra)), current_use := 1] 
  df[current_contra != "" & grepl(nonuse,tolower(current_contra)), current_use := 0]
  
  ## any women who gave an answer regarding current usage should be in the denominator
  df[current_use ==1 & !grepl(nonuse,tolower(current_contra)) & !grepl(unknown,tolower(current_contra)) & is.na(any_contra),any_contra := 1]
  df[!is.na(current_use) & is.na(any_contra),any_contra := 0]
  
}

## Mark modern contraceptive usage as 1 if current method contains strings pertaining to modern contraceptives
## Strings should cover all modern answers in any of the survey languages, and should
## not be ambiguous enough to apply to any methods that are not modern https://medlineplus.gov/languages/birthcontrol.html 
methods <- paste(c("modern", #anything specified as modern (usually "other modern")
                   "pill", "pilul", "pildora", "pastilla", "orales", #the pill
                   "condom", "condon", "preservati", "camasinha", "codom", #condoms
                   "diaphragm", "diafragma", "diaphram", "cervical cap", "cones", "fem sci", "sci fem", "scien fem", "other female", "oth fsci", #diaphragms
                   "tablet", "foam", "jelly", "jalea", "mousse", "espuma", "creme", "cream", "gelee", "spermicid", "eponge", "esponja", "esonja", "sponge", "vaginale", "comprimidos vaginais", "vaginal method", #spermicides and sponges
                   "insert", "mplant", "nplant", "rplant", "transplant", #implants
                   "injec", "inyec", "sayana", "piqure", #injections
                   "iud", "diu", "spiral", "sterilet", "coil", "iucd", "i.u.d.", "loop", "ota ring", "copper t", "copper 7", #iuds
                   "sterilization", "sterilisation", "sterilized", "ester", "esterizacao", "vasectom", "ligation", "ligadura", "ligature", 'ester\\.', "est\\.", "m ster", "male ster", "male-ster", "other meths inc ster", "operacion fem", "operacion masc", #male or female sterilization
                   "patch", "parche anticonceptivo","parche",  #contraceptive patch
                   "contraceptive ring","anillo", #contraceptive ring
                   "emergency", "morning-after"), #emergency contraceptives
                 collapse = "|")
df[grepl(methods,tolower(current_contra)) & is.na(any_contra),any_contra := 1]

## if contraception information missing for non-cohabiting women, assume no contraception use
if(unique(df$contra_currmar_only)== 1 | counterfac_cohab_contra==1) df[is.na(any_contra) & (curr_cohabit!=1 | is.na(curr_cohabit)), any_contra := 0]

df[any_contra == -1, any_contra := NA]


### Modern contraceptive usage
df[!is.na(any_contra), mod_contra := 0]
df[grepl(methods,tolower(current_contra)),mod_contra := 1]

### If modern method not specified, it is traditional
df[!is.na(any_contra), trad_contra := 0]
df[any_contra==1 & mod_contra == 0, trad_contra := 1]

####################################################################################################
##### NEED FOR CONTRACEPTION #######################################################################
####################################################################################################

## Women who have have a need for contraceptives are women who:
## 1. have had sex in the last 30 days or are married/in union
## 2. said that they do not want a child in the next 2 years
## 3. have not hit menopause, had a hysterectomy, or are otherwise known to be
##    infecund (including having never menstruated or not having menstruated
##    in at least 6 months if not postpartum amenorrheic, or having been postpartum
##    amenorrheic for 5 or more years)
## 4. have not been continuously married/living with a man for 5 years without
##    having a child despite answering that they have never used any method of
##    contraception.
## 5. For pregnant women and women who are postpartum amenorrheic from a birth in
##    the last 2 years, need is determined separately based on whether they wanted
##    to space or limit their current/most recent pregnancy

df[, need_sex := as.numeric(NA)][, need_cohabit := as.numeric(NA)] ##calculate first condition
df[, need_no_desire := as.numeric(NA)] ##calculate second condition
df[, need_infertile := as.numeric(NA)][, need_nomens := as.numeric(NA)][, need_nochild := as.numeric(NA)] ## calculate  third or fourth condition
df[, need_preg := as.numeric(NA)][, need_ppa := as.numeric(NA)] ##calculate fifth condition

## regardless of answers to any other questions, if a woman is currently using any contraceptive method then
## she is considered to have a need for contraception
df[any_contra == 1, need_contra := 1]


##### PREGNANT AND POST-PARTUM AMENORRHEIC WOMEN ###################################################

## Pregnant and postpartum amenorrheic women from a birth in the last 2 years can still contribute
## to unmet demand for contraception if they indicate that they wanted to space or limit their
## current/most recent pregnancy

# ## pregnant women are assumed to not need contraception as they are not at risk for pregnancy
df[pregnant == 1 & is.na(need_contra), need_contra := 0]
  
## Counterfactual set up: edit pregnancy data to match dataset to that being recreated
  if (counterfac_no_preg == 1) df[, preg_not_wanted:= NULL] ## skip section if survey had no information regarding pregnant women
  if (counterfac_cohab_preg==1) df[!is.na(pregnant) & curr_cohabit==0, pregnant:= NA][!is.na(preg_not_wanted) & curr_cohabit==0, preg_not_wanted:= NA]
  
## Pregnant women who had a need
  if (all(c("pregnant","preg_not_wanted") %in% names(df))){
    df[pregnant == 1 & preg_not_wanted == 1 & need_contra == 0,need_contra := 1]
    df[is.na(need_preg), need_preg := 0][pregnant == 1 & preg_not_wanted == 1, need_preg := 1] # & !is.na(preg_not_wanted)
  }

## Counterfactual set up: remove PPA data to match dataset to that being recreated
  if (counterfac_no_ppa == 1) df[, ppa:= NULL] ## skip section if survey had no information regarding postpartum amenorrheic women
  if (counterfac_cohab_ppa==1) df[!is.na(ppa) & curr_cohabit==0, ppa:= NA]
  
## Determine which postpartum amenorrheic women gave birth within the last 2 years
  if (all(c("ppa","months_since_last_birth") %in% names(df))) df[ppa == 1 & months_since_last_birth <= 24,ppa24 := 1]
  if (all(c("ppa","birth_in_last_two_years") %in% names(df))) df[ppa == 1 & birth_in_last_two_years == 1,ppa24 := 1]
  
## postpartum amenorrheic are asssumed to not be using/need contraception as their periods have not returned 
  if ("ppa24" %in% names(df)) {
    df[ppa24 == 1 & is.na(mod_contra),mod_contra := 0]
    df[ppa24 == 1 & is.na(trad_contra),trad_contra := 0]
    df[ppa24 == 1 & is.na(any_contra),any_contra := 0]
    df[ppa24 == 1 & is.na(need_contra), need_contra := 0]
  }
  
## Designate which postpartum amenorrheic women had a need, and adjust the all-woman contraception denominator to match (in case
## postpartum women were not asked about contraception, similar to pregnant women)
  if (all(c("ppa24","ppa_not_wanted") %in% names(df))) {
    df[ppa24 == 1 & ppa_not_wanted == 1 & need_contra == 0,need_contra := 1]
    df[is.na(need_ppa), need_ppa := 0][ppa24 == 1 & ppa_not_wanted == 1,need_ppa := 1]
  }

##### INFECUNDITY ###################################################

## Now we exclude women from having a need for contraception if they have expressed that
## they are infertile, or can be assumed to be infertile based on their recent menstruation or lack of children
## after 5 years of marriage with no contraceptive usage

## COUNTERFACTUAL set up: 
if (counterfac_missing_nomens == 1) df[, no_menses:= NULL][,reason_no_contra:=NA]
if (counterfac_missing_infertile == 1) df[, desire_children_infertile:= NULL][,reason_no_contra:=NA]

## Indicated lack of menstruation (in any way)
if ("no_menses" %in% names(df)) {
  df[no_menses == 1 & is.na(need_contra), need_contra := 0]
  df[is.na(need_nomens), need_nomens := 0][no_menses == 1, need_nomens := 1]
}

## Indicated inability to have a child
if ("desire_children_infertile" %in% names(df)){
 df[desire_children_infertile == 1 & is.na(need_contra), need_contra := 0]
 df[is.na(need_infertile), need_infertile := 0][desire_children_infertile == 1, need_infertile := 1]
}

## Indicated lack of menstruation or inability to have a child as reason why she was not using a contraceptive method
  infecund <- paste(c("infecund",
                      "menopaus",
                      "hyst",
                      "histerect",
                      "never mens",
                      "menstrua",
                      "removal of uterus",
                      "cannot have children",
                      "cant become pregnant",
                      "no puede quedar emb",
                      "infertil",
                      "nao pode",
                      "impossible to have"),
                    collapse="|")
  infertile <- paste(c("infecund",
                       "hyst",
                       "histerect",
                       "removal of uterus",
                       "cannot have children",
                       "cant become pregnant",
                       "no puede quedar emb",
                       "infertil",
                       "nao pode",
                       "impossible to have"),
                     collapse="|")
  no_menstruation <- paste(c("never mens", 
                             "menopaus",
                             "menstrua"), 
                           collapse="|")
  
  if ("reason_no_contra" %in% names(df)) df[is.na(need_infertile) & !is.na(reason_no_contra), need_infertile := 0][grepl(infertile,tolower(reason_no_contra)), need_infertile := 1]
  if ("reason_no_contra" %in% names(df)) df[is.na(need_nomens) & !is.na(reason_no_contra), need_nomens := 0][grepl(no_menstruation,tolower(reason_no_contra)), need_nomens := 1]
  if ("reason_no_contra" %in% names(df)) df[grepl(infecund,tolower(reason_no_contra)) & is.na(need_contra), need_contra := 0]

if (counterfac_missing_nochild == 0) {  
  ## Exclude others from need based on assumed infertility after 5 years without a child. Relevant variables
  ## coded differently depending on the survey series)
  if (all(c("curr_cohabit","never_used_contra") %in% names(df))) {
    ## DHS surveys
    ## can only deduce criteria in DHS among women who are still in their first marriage/union
    if (all(c("in_first_cohabit","years_since_first_cohabit","months_since_last_birth") %in% names(df))) {
      df[ is.na(need_contra) & curr_cohabit == 1 & ##currently married/in union
           never_used_contra == 1 & ##never used contraception
           in_first_cohabit == 1 & ##still in first marriage/union
           !is.na(years_since_first_cohabit) & ##make sure years fo marriage is known
           years_since_first_cohabit > 4 & ##married for at least 5 years
           (months_since_last_birth >= 60 | ##5 years since last birth or never given birth
              is.na(months_since_last_birth)),
         need_contra := 0]
      
      df[is.na(need_nochild), need_nochild := 0]
      df[curr_cohabit == 1 & ##currently married/in union
           never_used_contra == 1 & ##never used contraception
           in_first_cohabit == 1 & ##still in first marriage/union
           !is.na(years_since_first_cohabit) & ##make sure years fo marriage is known
           years_since_first_cohabit > 4 & ##married for at least 5 years
           (months_since_last_birth >= 60 | ##5 years since last birth or never given birth
              is.na(months_since_last_birth)),
         need_nochild := 1]
    }
    
    ## MICS surveys and some special DHS
    ## similar to normal DHS, but uses different variables
    if (all(c("in_first_cohabit","first_cohabit_year","last_birth_year","interview_year") %in% names(df))) {
      df[is.na(need_contra) & curr_cohabit == 1 & ##currently married/in union
           never_used_contra == 1 & ##never used contraception
           in_first_cohabit == 1 & ##still in first marriage/union
           interview_year < 9990 & ##make sure interview year is known
           interview_year - first_cohabit_year > 5 & ##been together for 5 years
           (interview_year - last_birth_year > 5 | ##5 years since last birth or never given birth
              is.na(last_birth_year)),
         need_contra := 0]
      
      df[is.na(need_nochild), need_nochild := 0]
      df[curr_cohabit == 1 & ##currently married/in union
           never_used_contra == 1 & ##never used contraception
           in_first_cohabit == 1 & ##still in first marriage/union
           interview_year < 9990 & ##make sure interview year is known
           interview_year - first_cohabit_year > 5 & ##been together for 5 years
           (interview_year - last_birth_year > 5 | ##5 years since last birth or never given birth
              is.na(last_birth_year)),
         need_nochild := 1]
    }
    
    ## PMA 2020 surveys and a few other random surveys
    ## actually asks about most recent marriage/union, so can deduce for all women
    if (all(c("recent_cohabit_start_date","interview_date","last_birth_date","interview_year") %in% names(df))) {
      if (class(df$recent_cohabit_start_date) == "character" & "year_interview" %in% names(df)) {
        ##parse dates for the years
        df[recent_cohabit_start_date == ".",recent_cohabit_start_date := NA]
        df[,year_married := str_sub(recent_cohabit_start_date,-4) %>% as.numeric]
        df[is.na(need_contra) & curr_cohabit == 1 & ##currently married/in union
             never_used_contra == 1 & ##never used contraception
             !is.na(year_interview) & ##make sure interview year is known
             year_interview - year_married > 5 & ##been together for 5 years
             (year_interview - year_birth > 5 | ##5 years since last birth or never given birth
                is.na(year_birth)),
           need_contra := 0]
        
        df[is.na(need_nochild), need_nochild := 0]
        df[curr_cohabit == 1 & ##currently married/in union
             never_used_contra == 1 & ##never used contraception
             !is.na(year_interview) & ##make sure interview year is known
             year_interview - year_married > 5 & ##been together for 5 years
             (year_interview - year_birth > 5 | ##5 years since last birth or never given birth
                is.na(year_birth)),
           need_nochild := 1]
      } else { ##assume CMC format
        df[is.na(need_contra) & curr_cohabit == 1 & ##currently married/in union
             never_used_contra == 1 & ##never used contraception
             !is.na(interview_date) & ##make sure interview year is known
             interview_date - recent_cohabit_start_date > 60 & ##been together for 5 years
             (interview_date - last_birth_date > 60 | ##5 years since last birth or never given birth
                is.na(last_birth_date)),
           need_contra := 0]
        
        df[is.na(need_nochild), need_nochild := 0]
        df[curr_cohabit == 1 & ##currently married/in union
             never_used_contra == 1 & ##never used contraception
             !is.na(interview_date) & ##make sure interview year is known
             interview_date - recent_cohabit_start_date > 60 & ##been together for 5 years
             (interview_date - last_birth_date > 60 | ##5 years since last birth or never given birth
                is.na(last_birth_date)),
           need_nochild := 1]
      }
    }
  }
}  


##combine infecund components into one variable
for (i in c("infertile", "nomens", "nochild")) {
  x <- paste0("counterfac_missing_", i)
  y <- paste0("need_", i)
  if (get(x) == 0) { ## Check counterfac status
    df[get(y) == 1 & (pregnant == 1), eval(y) := 0] ## pregnant women are not infecund, correct if needed
    if ("ppa24" %in% names(df)) df[get(y) == 1 & (ppa24 ==1), eval(y) := 0] ## ppa women are not infecund, correct if needed
  }
}

df[ !(is.na(need_infertile) & is.na(need_nomens) & is.na(need_nochild)), need_infecund := 0]
df[need_infertile == 1 | need_nomens == 1 | need_nochild == 1, need_infecund := 1]


##### DESIRE AND SEXUALLY ACTIVE / LIVING WITH PARTNER ###################################################

## The only women who are eligible for having a need for contraception are those that are married/in-union or
## have been sexually active in the last month, and have expressed that they do not want children in the next 2 years
## For some surveys, the period for when they wanted children differed by a small amount, but in others women were only asked
## about the desire for a child right now (the desire_soon variable. Is crosswalked later). In others, no question about
## desire for children is asked at all, and all married/in-union/sexually active women are assumed to have a need (also crosswalked later)

{ 
## Set up for counterfactual extractions -- edit gold standard datasets to match what is seen in non-gold
  if (counterfac_missing_desire_timing == 1) df[is.na(desire_limiting), desire_later := NA]
  if (counterfac_cohab_desire_timing == 1) df[is.na(desire_limiting) & curr_cohabit==0, desire_later := NA]
  if (counterfac_missing_desire_later == 1) df[, desire_later := NULL]
  if (counterfac_cohab_desire_later == 1) df[curr_cohabit==0, desire_later := NA]
  if (counterfac_missing_desire == 1)  df[, desire_later := NULL][, desire_soon := NULL]
  if (counterfac_cohab_desire == 1) df[curr_cohabit==0, desire_later := NA][curr_cohabit==0, desire_soon := NA]
  if (counterfac_missing_sex==1) df[, sex_in_last_month := NULL]
  if (counterfac_cohab_sex==1) df[curr_cohabit==0, sex_in_last_month := NA]
}

  if ("desire_later" %in% names(df)) {
    ## Survey has information on women's desire for a child in the next 2 years
    df[curr_cohabit == 1 & desire_later == 1 & is.na(need_contra), need_contra := 1]
    if ("sex_in_last_month" %in% names(df)) df[sex_in_last_month == 1 & desire_later == 1 & is.na(need_contra), need_contra := 1]
    
    if ("desire_later" %in% names(df)) df[is.na(need_no_desire), need_no_desire := 0][desire_later == 1, need_no_desire := 1]
    if ("sex_in_last_month" %in% names(df)) df[is.na(need_sex), need_sex := 0][sex_in_last_month == 1, need_sex := 1]
    if ("curr_cohabit" %in% names(df)) df[is.na(need_cohabit), need_cohabit := 0][curr_cohabit == 1, need_cohabit := 1]
    
  } else if ("desire_soon" %in% names(df)) {
    ## Survey only has info regarding desire for a child right now. Assume that anyone who is married/sexually active
    ## and did not answer that they want a child right now has a need
    df[curr_cohabit == 1 & (is.na(desire_soon) | desire_soon == 0) & is.na(need_contra), need_contra := 1]
    if ("sex_in_last_month" %in% names(df)) df[sex_in_last_month == 1 & (is.na(desire_soon) | desire_soon == 0) & is.na(need_contra), need_contra := 1]
    
    if ("desire_soon" %in% names(df)) df[is.na(need_no_desire), need_no_desire := 0][(is.na(desire_soon) | desire_soon == 0), need_no_desire := 1]
    if ("sex_in_last_month" %in% names(df)) df[is.na(need_sex), need_sex := 0][sex_in_last_month == 1, need_sex := 1]
    if ("curr_cohabit" %in% names(df)) df[is.na(need_cohabit), need_cohabit := 0][curr_cohabit == 1, need_cohabit := 1]
    
  } else {
    ## No info regarding desire for children, assume that everyone who's married/sexually active has a need
    if ("curr_cohabit" %in% names(df)) df[curr_cohabit == 1 & is.na(need_contra),need_contra := 1]
    if ("sex_in_last_month" %in% names(df)) df[sex_in_last_month == 1 & is.na(need_contra), need_contra := 1]
    ## A Cuba survey has neither sexual activity nor marital status, and we thus assume that everyone has a need
    if (all(c("curr_cohabit","sex_in_last_month") %ni% names(df))) df[is.na(need_contra),need_contra := 1]
    
    if ("sex_in_last_month" %in% names(df)) df[is.na(need_sex), need_sex := 0][sex_in_last_month == 1, need_sex := 1]
    if ("curr_cohabit" %in% names(df)) df[is.na(need_cohabit), need_cohabit := 0][curr_cohabit == 1, need_cohabit := 1]
  }

## Assume not in need if information not available
df[is.na(need_contra), need_contra := 0]


## Restrict need_contra to those observations where we know women were actually asked about contraception,
## making it consistent with the denominator of modern contraceptive usage (since otherwise it's impossible
## to know whether that need was met or not)
df[is.na(mod_contra), need_contra := NA]

####################################################################################################
##### MARITAL STATUS ADJUSTMENTS ###################################################################
####################################################################################################

# fill in implied 0s for unmarried/partnered women on surveys that restrict to married/partnered women
# use the 'curr_cohabit' variable, which is a combination of the questions on cohabitation and marital
# status, depending on what is available in a given survey.
df[contra_currmar_only == 1, list(mean(is.na(any_contra)), .N), keyby = curr_cohabit]
df[contra_currmar_only == 1 & curr_cohabit == 0,
     c("trad_contra", "mod_contra", "any_contra") := list(0, 0, 0)]

df[currmar_only == 1, list(mean(is.na(need_contra)), .N), keyby = curr_cohabit]
df[currmar_only == 1 & curr_cohabit == 0 & is.na(need_contra), need_contra := 0]


####################################################################################################
##### INTENTION TO USE CONTRACEPTION IN THE FUTURE #################################################
####################################################################################################

df[, intent_to_use := as.numeric(NA)] 

if ("intent_use" %in% names(df)){
  
  ## counterfactual -- extract gold standard for intent within 12 months
  if (counterfac_intent_12m==1) {
    ## DHS 1999 and PMA surveys restrict to only accept 12months
    if (df$nid %in% c(19076,20132)) df[intent_gate==2 | intent_gate==3, intent_use := 0] ## remove 12+ months and timing unsure from accepted options
    
    if (grepl("jhsph_performance_for_action_pma", tolower(unique(df$file_path)))) {
      df[, intent_use:= 0][is.na(intent_gate) & is.na(intent_gate_preg), intent_use := NA]
      df[intent_gate==1 & intent_unit==1 & intent_timing <=12, intent_use:= 1] ## within 12 months
      df[intent_gate==1 & intent_unit==2 & intent_timing <=1, intent_use:= 1] ## within 1 year
      df[intent_gate==1 & intent_unit==3, intent_use:= 1] ## soon
      df[intent_gate_preg==1 & intent_unit==4, intent_use:= 1] ## after pregnancy
    }
  }
  
  df[, intent_to_use := 0] 
  df[intent_use == 1, intent_to_use := 1]
  df[is.na(intent_use), intent_to_use := -1]  ## Track cases without a response for intent, but question was asked in survey
  
  ## Women not in need and currently using should be 0
  df[any_contra == 1, intent_to_use := 0] 
  df[need_contra == 0, intent_to_use := 0] 
  
  ## which surveys should be used for gold standard
  if ("intent_timing" %in% names(df)) df[!is.na(intent_timing), intent_use_gold := 1] ##PMA surveys with timing for months/years in future
  if ("intent_gate" %in% names(df)) df[intent_gate==1 & survey_name=="DHS", intent_use_gold := 1] ## DHS surveys with "1" response (use within 12 months)
  
}

df[need_contra==1 & any_contra==0, intent_in_need := intent_to_use]
