##################################################
### Using current extractions, determine which counterfactual extractions will be performed
##################################################

## SET-UP --------------------------------------------------------------------
# settings
pacman::p_load(data.table,magrittr,ggplot2,haven,stringr,parallel, dplyr, weights, openxlsx,purrr)

# create function for the opposite of %in%
'%ni%' <- Negate('%in%')

# save location
out <- "<<<< FILEPATH REDACTED >>>>"

## Read data ----------------------------------------------------------------------

### Geomatched and processed winnower files 
df_all[, country_series := paste(country, survey_name, sep="_")]
df_all[, country_series := gsub("JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020", "PMA2020", country_series)]
df_all[, country_series := gsub("JHSPH_PERFORMANCE_FOR_ACTION_PMA", "PMA", country_series)]
df_all[, country_series := gsub("NGA_NGA", "NGA", country_series)][, country_series := gsub("KEN_KEN", "KEN", country_series)]
df_all[, country_series := gsub("_SURVEY$", "", country_series)][, country_series := gsub("_SURVEYS$", "", country_series)]

names(df_all)[grepl("need", names(df_all))]

df_all[intent_12m_only==1,intent_12m_only := NA][intent_12m_only==0,intent_12m_only := 1]

## Save need components by cohabit staus of each NID -----------------------------------------------------
dt1 <- unique(df_all[, c("nid", "country", "year_start", "survey_name")])

vars <- c("any_contra", "need_sex", "need_no_desire", "need_infertile", "need_nomens", "need_nochild", "need_preg", "need_ppa", "intent_12m_only")
for(var in vars){
  check <- df_all[, .N, by=c("nid", "country", "year_start", "curr_cohabit", var)]
  dt2 <- as.data.table(reshape(check, idvar = c("nid", "country", "year_start", var), timevar = "curr_cohabit", direction = "wide"))
  dt2[!is.na(N.0) & !is.na(N.1) & !is.na(get(var)), resp:= "all"][is.na(N.0) & !is.na(N.1) & !is.na(get(var)), resp:= "cohabit"][is.na(N.0) & is.na(N.1), resp:= "none"]
  
  if(length(unique(dt2$nid))<nrow(dt2)){
    dt2 <- dt2[order(dt2$nid, dt2$resp, na.last=F), ]
    dt2 <- dt2[!duplicated(dt2$nid, fromLast = TRUE), ]
  }
  
  name <- gsub(".*_", "", var)
  name <- gsub(".*_", "", name)
  name <- ifelse(name=="only", "intent", name)
  
  assign(name, unique(dt2[, c("nid", "resp")]))
  setnames(get(name), "resp", name)
  
}

dt1 <-Reduce(function(...) merge(...,by=c("nid"), all=TRUE), list(dt1, contra,sex,desire,infertile,nomens,nochild,preg,ppa,intent))
dt1[is.na(dt1)] <- "none"

## Counterfactual extractions -----------------------------------------------------

### list all possible combinations of missing variables; will do a separate counterfactual extraction for each
### Read in prepared document with descriptions of combinations
counterfacs <- names(dt1)[c(5:13)]
counterfactuals <- data.table(counterfactuals=c(counterfacs, lapply(seq_along(counterfacs)[-1L], function(y) combn(counterfacs, y, paste0, collapse = "_")), recursive = TRUE) %>% keep(~ str_count(.x, 'desire') <= 1)
)
counterfactuals$description <-as.character(NA)
describe <- data.table(cf=names(dt1)[c(5:13)], desc=c("Contraception information not captured.",
                                                     "Time since last sexual activity not captured.",
                                                     "No questions related to desire for a child now/in the future.",
                                                     "Lacks questions to determine infertility.",
                                                     "Lacks questions to determine lact of menstruation due to infecundity.",
                                                     "Lacks questions to determine ability to have a child during cohabitation and sexual activity.",
                                                     "Cannot identify pregnant women, or questions about wantedness of current birth were not asked to these women.",
                                                     "Cannot identify PPA women, or questions about wantedness of last birth were not asked to these women.",
                                                     "Intent only captured for 12 month time period."))
for(i in counterfacs){
  desc <- describe[cf==i,desc]
  counterfactuals[grep(i, counterfactuals),description:=ifelse(is.na(description),desc,paste(description, desc))]
}

## add in report-only surveys
reports <- data.table(read.xlsx("<<<< FILEPATH REDACTED >>>>/report_extractions.xlsx", sheet = "Sheet2"))[-1]
reports <- subset(reports, location_code != 182)
reports <- unique(reports[,c("nid", "country", "year_start", "survey_name","currmar_only")])
reports[, names(dt1)[-c(1:4)] := "all"] ## add in CF component columns, expecting no conditions
reports[currmar_only==1, names(dt1)[-c(1:4)] := "cohabit"] ## add in cohabit conditions as needed
reports[,currmar_only:= NULL]

dt1 <- rbind(dt1, reports)

dt1 <- as.data.table(dt1)
dt1$none <- as.character(NA)
dt1$cohabit <- as.character(NA)
for(n in names(dt1)[c(5:13)]){
  dt1[get(n)=="none", none:=ifelse(is.na(none),n,paste(none,n,sep="_"))]
  dt1[get(n)=="cohabit", cohabit:=ifelse(is.na(cohabit),n,paste(cohabit,n,sep="_"))]
}

dt1[is.na(none),none:= "none"]


## For crosswalking, which NIDS need which crosswalking
nids_cfs <- dt1[, c("nid","none","cohabit")]
nids_cfs[none=="none", comb := "none"]
nids_cfs[none!="none", comb := as.character(ifelse(!is.na(cohabit), paste(none, cohabit, sep="_"), none))]
nids_cfs[none=="none" & cohabit=="contra_sex_desire_infertile_nomens_nochild_preg_ppa_intent", comb := "contra"]
nids_cfs <- nids_cfs[, -c("none","cohabit")]
write.csv(nids_cfs, paste0("<<<< FILEPATH REDACTED >>>>","counterfacs_by_nid.csv"), row.names = FALSE)  

## For CF extractions, which are needed
df_cfs <- dt1[, .N, by=c("none", "cohabit")]
df_cfs <- merge(df_cfs, as.data.table(counterfactuals), by.x="none", by.y="counterfactuals", all=T)
df_cfs[cohabit=="contra_sex_desire_infertile_nomens_nochild_preg_ppa_intent", cohabit := "contra"]
setorderv(df_cfs, "N", order = -1, na.last=T)
write.csv(df_cfs, paste0("<<<< FILEPATH REDACTED >>>>","counterfacs_to_run.csv"), row.names = FALSE)  
