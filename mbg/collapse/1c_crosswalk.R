### Crosswalking: Estimate unbiased values from systematically biased results
## Model inputs: covariates = countries in the dataset
##               study_id = NID + admin_1_id


## Setup -------------------------------------------------------------------------------------------
print('Crosswalking data...')

library(crosswalk002)
library(ggpubr)
library(qpdf)
library(pdftools)


# counterfactuals
counterfactuals <- as.data.table(read.csv("<<<< FILEPATH REDACTED >>>>/counterfacs_to_run.csv"))
counterfactuals <- subset(counterfactuals, !is.na(N))
counterfactuals <- subset(counterfactuals, !((none == "none" & cohabit=="nochild") | (none == "none" & is.na(cohabit)))) ## remove counterfactuals that would not capture anything
counterfactuals[, name:= ifelse(!is.na(counterfactuals$cohabit), paste(counterfactuals$none, counterfactuals$cohabit, sep="_"), as.character(counterfactuals$none))]
counterfactuals[name == "none_contra", name:= "contra"]

# intent gold standard -- needed to subset for specific crosswalk rounds
gold_name <- list.files("<<<< FILEPATH REDACTED >>>>/0_gold_standard", pattern = ".csv", full.names=T)
gold_nid <- read.csv(gold_name) %>% subset(gold_intent==1)
gold_intent_nid <- unique(gold_nid$nid)

dstep <- "iterative"
gbd_round <- 7
years <- seq(1995, 2022)

id.vars <- c("nid","admin_1_id","country", "year","variable", "survey_name")

all_coeffs <- data.table()
small_se_nids <- list()

#### Set up functions
# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

## Create function to calculate offset as the 0.5(min value) and 0.5(1-max_value) for each indicator
get_offset <- function(data){
  offset <- data[value != 0 & value !=1, .(max = 1-(1-max(value))/2, min = min(value)/2), by="variable"]
  for(x in unique(data$variable)){
    data[variable== x & value ==0, value := offset[offset$variable==x,"min"]] ## replace 0 values with 0.5(min value)
    data[variable== x & value ==1, value := offset[offset$variable==x,"max"]] ## replace 1 values with 1-0.5(1-max_value)
  }
  return(data)
}

## PREP DATA ---------------------------------------------------------------

### data prep before this point
# create survey_id for easy tracking
all[,survey_id := paste(survey_name,country,year,nid)]

## Flag which counterfactual will be needed for each NID
nids_cf <- read.csv("<<<< FILEPATH REDACTED >>>>/counterfacs_by_nid.csv")
all <- merge(all, nids_cf, by="nid", all.x=T)
setnames(all,"comb","counterfactual")
all[(variable=="any_contra" | variable== "mod_contra") & !grepl("contra", counterfactual), counterfactual:= "none"]
all[is.na(counterfactual), counterfactual:= "none"]

# offset extreme values and fill in missing standard_error's
all <- get_offset(all)
all[, standard_error := sqrt(value*(1-value)/N)] ## calculate SE if missing

# transform val and standard_error into logit space for adjustment later on
all[, c("logit_val", "logit_se")] <- as.data.table(crosswalk002::delta_transform(mean = all$value,
                                                                                            sd = all$standard_error,
                                                                                            transformation = 'linear_to_logit'))

## Make dummy variables for each country, which allows model to run for each individually
for (c in unique(all$country)) all[, eval(c) := 0][country==c, eval(c) := 1]
all$row_id <- paste0("row", 1:nrow(all))

all$id2 <- as.integer(as.factor(all$nid)) ## Grouping for NID
all[, id := paste(nid, admin_1_id)]
all$id3 <- as.integer(as.factor(all$id)) ## Grouping for NID and Admin 1

track_id2 <- unique(all[,c("nid","id2")])
track_id3 <- unique(all[,c("id","id3")])


## crosswalked data -----------------------------------------------------------

all_comb <- data.table()
gold_comb <- data.table()
## loops through all counterfactuals
for (i in 1:nrow(counterfactuals)) {
xwalk_comb <- data.table()

topic <- counterfactuals$name[i]
input_dir <- file.path("<<<< FILEPATH REDACTED >>>>", topic)
## get most recent date for CF
processed_version <- most_recent_date(input_dir)
df2 <- as.data.table(readRDS(file.path(input_dir, paste0("collapsed_",topic, "_", processed_version, ".RDS"))))

# offset extreme values and fill in missing standard_error's
df2 <- get_offset(df2)
df2[, standard_error := sqrt(value*(1-value)/N)] ## calculate SE

# format counterfactual extractions
df2 <- df2[,c(id.vars,"value","standard_error"), with = F]
setnames(df2, c("value","standard_error"), c("prev_alt","prev_se_alt"))
df2[,dorm_alt := topic]
df2 <- subset(df2, !is.na(prev_se_alt))

  ##save dataset to working space by CF name
  assign(topic, df2)

# subset df to gold standard extractions which have corresponding counterfactual extractions
## subset all data to observations with CF extractions for the topic
input_dir <- file.path("<<<< FILEPATH REDACTED >>>>/0_gold_standard")
## get most recent date for CF
processed_version <- most_recent_date(input_dir)
gold <- as.data.table(readRDS(paste0(input_dir, "/collapsed_gold_standard_", processed_version, ".RDS")))

# offset extreme values and fill in missing standard_error's
gold <- get_offset(gold)
gold[, standard_error := sqrt(value*(1-value)/N)] ## calculate SE

# format gold standard extractions
gold <- gold[,c(id.vars,"value","standard_error"), with = F]
setnames(gold, c("value","standard_error"), c("prev_ref","prev_se_ref"))
gold[,dorm_ref := "gold_standard"]

# combine gold standard and counterfactual extractions. Match alternative and reference observations for these conditions.
df_matched <- merge(gold, df2, by = id.vars)

# prepare data for crosswalking -- logit required for CWData function to ensure that the crosswalk adjustment remains bounded correctly.
## Get differences between gold and CF in logit form
dat_diff <- as.data.frame(cbind(
  crosswalk002::delta_transform(
    mean = df_matched$prev_alt,
    sd = df_matched$prev_se_alt,
    transformation = "linear_to_logit" ),
  crosswalk002::delta_transform(
    mean = df_matched$prev_ref,
    sd = df_matched$prev_se_ref,
    transformation = "linear_to_logit")
))
names(dat_diff) <- c("mean_alt", "mean_se_alt", "mean_ref", "mean_se_ref")

# get table of matched reference and gold standard data pairs
## calculates means and SDs for differences between random variables, ensuring that the alternative defintion/method is in the numerator: log(alt/ref) = log(alt) - log(ref)
df_matched[, c("logit_diff", "logit_diff_se")] <- calculate_diff(
  df = dat_diff,
  alt_mean = "mean_alt", alt_sd = "mean_se_alt",
  ref_mean = "mean_ref", ref_sd = "mean_se_ref" )

df_matched <- df_matched[logit_diff != 0]

# remove any entries with abnormally small standard errors
bad_nids <- df_matched[prev_se_alt < .00001 | prev_se_ref < .00001, unique(nid)]
small_se_nids <- append(small_se_nids, bad_nids)
df_matched <- df_matched[prev_se_alt > .00001 & prev_se_ref > .00001]

## remove rows for any_contra and mod_contra if CF doesn't affect contraception
if(unique(!grepl("contra", df_matched$dorm_alt))) df_matched <- subset(df_matched, variable!= "any_contra" & variable!= "mod_contra")
if(unique(df_matched$dorm_alt=="contra")) df_matched <- subset(df_matched, variable!= "need_contra" & variable!= "intent_use")

## Loop  through all indicators
for (var in unique(df_matched$variable)) {
cat("Topic:",topic,"Variable:",var)

# subset to all rows for this variable
df_xwalk <- df_matched[variable == var]

## subset for intent crosswalk -- only gold standards with ability to calculate intent within 12 months should be used
if (var == "intent_use" & grepl("intent", topic)) df_xwalk <- subset(df_xwalk, nid %in% gold_intent_nid)

# dataset to that matches with what is being crosswalked
all_xwalk <- all[all$counterfactual == topic & all$variable == var]

if(nrow(df_xwalk)==0 | nrow(all_xwalk)==0) next ##if no rows in subset, skip this loop

  df_xwalk$country_id <- as.integer(as.factor(df_xwalk$country))
  df_xwalk[, id := paste(nid, admin_1_id)]

  ##add in id2 based on what matches with original dataset
  df_xwalk<- merge(df_xwalk, track_id2, by="nid", all.x=T) ## Group by NID
  df_xwalk<- merge(df_xwalk, track_id3, by="id", all.x=T) ## Group by NID and Admin 1

  covs_list <- list()
  covs_list2 <- list()
  if(length(unique(df_xwalk$country))>1) {
    for (c in unique(df_xwalk$country)){
      ## Make dummy variables for each country, which allows model to run for each individually.
      df_xwalk[, eval(c) := 0][country==c, eval(c) := 1]

      ## Determine which countries are in the dataset and can be run through the model
      covs_list <- append(covs_list, c)
      covs_list2 <- append(covs_list2, CovModel(eval(c)))
    }
  } else {
    ## if only 1 country is in the dataset, it should run on the intercept model, otherwise it breaks
    covs_list <- unique(df_xwalk$country)
    covs_list2 <- list(CovModel("intercept"))
  }


# format data for meta-regression; pass in data.frame and variable names
dat1 <- CWData(
  df = df_xwalk,
  obs = "logit_diff",       # matched differences in logit space
  obs_se = "logit_diff_se", # SE of matched differences in logit space
  alt_dorms = "dorm_alt",   # var for the alternative def/method
  ref_dorms = "dorm_ref",   # var for the reference def/method
  covs = covs_list,         # list of (potential) covariate columns #covs_list
  study_id = "id3"          # var for random intercepts; i.e. (1|study_id) ## Use NID as group to determine between-study heterogeneity
)

# create crosswalk object called fit1. Meta regression model, data needs to be formatted by CWData function.
fit1 <- CWModel(
  cwdata = dat1,               # result of CWData() function call
  obs_type = "diff_logit",     # must be "diff_logit" or "diff_log"
  cov_models = covs_list2,     # specify covariate details
  gold_dorm = "gold_standard"  # level of 'ref_dorms' that's the gold standard
)

## adjust without study_id
preds1 <- adjust_orig_vals(
  fit_object = fit1, # object returned by `CWModel()`
  df = all_xwalk,
  orig_dorms = "counterfactual",
  orig_vals_mean = "value",
  orig_vals_se = "standard_error",
  data_id = "row_id" # optional argument to add a user-defined ID to the predictions;
)

all_xwalk[,
          c("meanvar_adjusted", "sdvar_adjusted",
            "pred_logit", "pred_se_logit", "data_id")] <- preds1
all_xwalk[, gamma := fit1$gamma]
all_comb <- rbind(all_comb, all_xwalk, fill = T)


## check how xwalk affect cf gold standard
preds3 <- adjust_orig_vals(
  fit_object = fit1, # object returned by `CWModel()`
  df = df_xwalk,
  ref_dorms = "dorm_ref",
  orig_dorms = "dorm_alt",
  orig_vals_mean = "prev_alt",
  orig_vals_se = "prev_se_alt"
)

df_xwalk[,
          c("meanvar_adjusted", "sdvar_adjusted",
            "pred_logit", "pred_se_logit", "data_id")] <- preds3
df_xwalk[, gamma := fit1$gamma]
gold_comb <- rbind(gold_comb, df_xwalk, fill = T)

# pull coefficients and other variables from crosswalk object, save to all_coeffs
## For each country and CF, amount of bias compared to gold (in logit space)
df_result <- data.table(fit1$create_result_df())
df_result$variable <- var
df_result$country <- df_result$cov_names
if(length(unique(df_xwalk$country))==1) df_result[, country := unique(df_xwalk$country)] ## specify country if intercept model used

## keep gamma and random effect values
end_val <- ncol(df_result)-2
if(ncol(df_result) > 10) df_result[2:6,5:end_val] <- df_result[1,5:end_val]

df_result <- df_result[df_result$dorms == topic, ]
all_coeffs <- rbind(all_coeffs, df_result, fill = T)

}

}

all_comb[, xwalk := 1]

all_nocf <- rbind(all_comb, all, fill=T)
all_nocf <- all_nocf[order(xwalk),]
all <- unique(all_nocf, by=c( "nid","variable","country","survey_name","admin_1_id", "point" ,"shapefile" ,"location_code" ,"latitude","longitude", "year", "value", "N", "N_obs","sum_of_sample_weights"))

## 3. ADJUST IMPERFECT DATA -----------------------------------------------------------

# save original val and standard error values
all[,raw_val := value]
all[,raw_se := standard_error]

# adjust df according to which combination of missing need components applies
all[xwalk==1, value := meanvar_adjusted]
all[xwalk==1, standard_error := sdvar_adjusted]

# transform original mean and standard_error into logit space
all[, c("raw_val_logit", "raw_se_logit")] <- as.data.table(crosswalk002::delta_transform(mean = all$raw_val,
                                                                                         sd = all$raw_se,
                                                                                         transformation = 'linear_to_logit'))

all[, c("logit_val", "logit_se")] <- as.data.table(crosswalk002::delta_transform(mean = all$value,
                                                                                             sd = all$standard_error,
                                                                                             transformation = 'linear_to_logit'))

## Calculate new N effective for crosswalked data
all[, N_original := N]
all[, N := (value*(1-value))/(standard_error^2)]

## remove additional  variables
all <- all[,-c("survey_id", "row_id", "id2", "id3", "standard_error",  "logit_val", "logit_se" , "BFA","KEN","NGA",
               "meanvar_adjusted","sdvar_adjusted","pred_logit","pred_se_logit","data_id", "gamma", "raw_se","check_mean","check_se",
               "check_mean_linear","check_se_linear", "raw_val_logit","raw_se_logit","label")]
setnames(all, "raw_val", "value_original")
