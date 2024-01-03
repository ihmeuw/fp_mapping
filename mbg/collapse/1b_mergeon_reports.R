############################################################################################################
## Purpose: Compile collapsed contraception microdata with report exractions. Exclude sources determined to be outliers. 
###########################################################################################################


## PREP REPORT DATA ---------------------------------------------------------------------
print('Prepping report data...')

library("openxlsx")

## read in newer extraction_sheet and format
reports <- data.table(read.xlsx("<<<< FILEPATH REDACTED >>>>/report_extractions.xlsx", sheet = "Sheet2"))[-1]

### Subset to only regional breakdowns
### Location_code 182 == Nigeria national code
reports <- subset(reports, location_code != 182 & !is.na(location_code))

## Format sheet
string_cols <- c("notes","survey_name","shapefile", "file_path","country","aggregated_methods","category_mismatch","intent_aggregated_methods")
numeric_cols <- names(reports)[!names(reports) %in% string_cols]
percentage_cols <- numeric_cols[grepl("contra|unmet_need|met_demand|total_demand|prop_|intent_use",numeric_cols)]
reports[, c(numeric_cols) := lapply(.SD,as.numeric),.SDcols = numeric_cols]
reports[, c(percentage_cols) := lapply(.SD,function(x) {x/100}),.SDcols = percentage_cols]
reports[,year := floor((year_start + year_end)/2)]
setnames(reports,"outlier","is_outlier")

## fill in missingness in flagging variables
reports[is.na(main_analysis),main_analysis := 0]
reports[is.na(verified),verified := 0]
reports[is.na(multi_method),multi_method := 0]
reports[is.na(is_outlier),is_outlier := 0]
reports[is.na(demand_row_only),demand_row_only := 0]
reports[is.na(marital_row_only),marital_row_only := 0]
reports[is.na(currmar_only),currmar_only := 0]
reports[is.na(evermar_only),evermar_only := 0]
reports[is.na(formermar_only),formermar_only := 0]
reports[is.na(unmar_only),unmar_only := 0]

reports[, flag := as.character(NA)]
reports[currmar_only==1, flag := "contra"]
  
## sum up mod_contra, trad_contra, and any_contra assuming no miscategorization of "other" categories
methods <- names(reports)[grepl("contra_",names(reports))]
trad_methods <- methods[grepl("lactat|withdraw|rhythm|calendar|trad",methods)]
mod_methods <- methods[!methods %in% trad_methods]

## if mod_contra or trad_contra is missing and the specific methods underlying them are not also missing (so method-specific	
## data was actually collected by the survey), sum up the specific methods	
reports[is.na(mod_contra) & multi_method == 0 & rowSums(!is.na(reports[,mod_methods,with=F])) != 0,mod_contra := rowSums(.SD,na.rm = T),.SDcols = mod_methods]
reports[is.na(trad_contra) & multi_method == 0 & rowSums(!is.na(reports[,trad_methods,with=F])) != 0,trad_contra := rowSums(.SD,na.rm = T),.SDcols = trad_methods]
reports[is.na(any_contra) & multi_method == 0,any_contra := mod_contra + trad_contra]

## check that individual methods do not sum to more than most (or more than 100%) of women in any_contra
reports[(mod_contra >= .95 | any_contra >= .95) & multi_method == 0, c('location_code', 'year_start', 'any_contra')]
reports[(mod_contra + trad_contra != any_contra ) & multi_method == 0, c('location_code', 'year_start', 'mod_contra', 'trad_contra', 'any_contra')]

## Any contra configuration
contra_data <- subset(reports,!is.na(sample_size) | !is.na(mod_contra))
contra_data <- contra_data[, c("nid", "country", "survey_name", "shapefile", "location_code", "year", "any_contra", "sample_size", "flag")]
setnames(contra_data, c("sample_size", "any_contra"), c("N_obs", "value"))
contra_data$variable <-  ifelse(type == "classic", "indic_contra", "any_contra")
contra_data$point <- 0
contra_data <- subset(contra_data, !is.na(N_obs)) 

## Mod contra configuration
mod_data <- subset(reports,!is.na(sample_size) | !is.na(mod_contra))
mod_data <- mod_data[, c("nid", "country", "survey_name", "shapefile", "location_code", "year", "any_contra", "mod_contra", "sample_size", "flag")]
mod_data$variable <-  ifelse(type == "classic", "group_e", "mod_contra")
mod_data$point <- 0
mod_data$sample_size_mod <- mod_data$any_contra * mod_data$sample_size ### mod_contra denominator is only those using contraception
mod_data$mod_contra <- mod_data$mod_contra / mod_data$any_contra ### mod_contra denominator is only those using contraception
mod_data <- mod_data[, -c("any_contra", "sample_size")]
setnames(mod_data, c("sample_size_mod", "mod_contra"), c("N_obs", "value"))
mod_data <- subset(mod_data, !is.na(N_obs))

if(type=="normal"){
  
  ## Need contra configuration
  need_data <- subset(reports,!is.na(demand_sample_size))
  need_data <- need_data[, c("nid", "country", "survey_name", "shapefile", "location_code", "year", "total_demand_all", "any_contra",  "demand_sample_size", "flag")]
  need_data$demand_sample_size <- (1-need_data$any_contra) * need_data$demand_sample_size ### need_contra denominator is only those NOT using contraception
  need_data$total_demand_all <- need_data$total_demand_all / (1-need_data$any_contra) ### need_contra denominator is only those NOT using contraception
  need_data <- need_data[, -c("any_contra")]
  setnames(need_data, c("demand_sample_size", "total_demand_all"), c("N_obs", "value"))
  need_data$variable <- "need_contra"
  need_data$point <- 0
  
  ## Intent use configuration 
  intent_data <- subset(reports,!is.na(intent_sample_size))
  intent_data <- intent_data[, c("nid", "country", "survey_name", "shapefile", "location_code", "year", "intent_use", "total_demand_all", "any_contra", "intent_sample_size", "flag")]
  intent_data$intent_sample_size <- (1-intent_data$any_contra) * intent_data$total_demand_all * intent_data$intent_sample_size ### intent_contra denominator is only those NOT using contraception and NOT in need
  intent_data$intent_use <- (intent_data$intent_use / (1-intent_data$any_contra)) / (need_data$total_demand_all / (1-need_data$any_contra)) ### intent_contra denominator is only those NOT using contraception and NOT in need
  intent_data <- intent_data[, -c("any_contra", "total_demand_all")]
  setnames(intent_data, c("intent_sample_size", "intent_use"), c("N_obs", "value"))
  intent_data$variable <-  "intent_use"
  intent_data$point <- 0
}

## Combine report data with microdata
all <- rbind(all, contra_data, mod_data,  fill = T)
if(type=="normal") all <- rbind(all, need_data, intent_data, fill = T)
all[is.na(N), N := N_obs]
all[is.na(sum_of_sample_weights), sum_of_sample_weights := N_obs]
  
### EXCLUDE REPORTS THAT ARE OUTLIERS
print('Excluding outliers...')
  
outliers <- data.table(read.xlsx("<<<< FILEPATH REDACTED >>>>/outliers_GBD.xlsx"))

outliers$list <- apply(outliers[,7:13], 1, function(x) colnames(outliers[,7:13])[which(x == 1)])
outliers <- subset(outliers, is_outlier == 1)

### Reorganize into long format table to merge onto shapefile. 
outliersnew <- data.frame(variable = unlist(outliers$list), 
                     nid = rep(outliers$nid, lengths(outliers$list)), 
                     outlier = rep(outliers$is_outlier, lengths(outliers$list)))

if(type=="classic"){
  outliersnew <- as.data.table(outliersnew)
  outliersnew[variable=="any_contra", variable := "indic_contra"]
  outliersnew[variable=="mod_contra", variable := "group_e"]
  outliersnew[variable=="need_contra", variable := "indic_demand"]
  outliersnew[variable=="unmet_need", variable := "indic_unmet"]
  outliersnew[variable=="intent_use", variable := "indic_intent"]
  
  outliers_classic <- subset(outliersnew, variable=="group_e")
  outliers_classic[variable=="group_e", variable := "group_d"]
  
  outliersnew <- rbind(outliersnew, outliers_classic, fill=T)
  outliersnew <- as.data.frame(outliersnew)
}

all <- merge(all, outliersnew, all.x=T, by=c("nid", "variable"))
all <- subset(all, is.na(outlier))
all <- all [, -c("outlier")]


print('Data preparation complete...')
