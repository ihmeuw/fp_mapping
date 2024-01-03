###################################
# POST EXTRACTION GEOGRAPHY MATCHING
###################################

## Setup----------------------------------------------------------------------
rm(list=ls())

topic <- "<<<< TEAM NAME REDACTED >>>>"
folder_in <- "<<<< FILEPATH REDACTED >>>>"
folder_out <- "<<<< FILEPATH REDACTED >>>>"

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(haven, stringr, plyr, data.table, magrittr, parallel, doParallel, rgdal)

options(warn=-1)
module_date <- format(Sys.Date(), "%Y_%m_%d")

read_add_name_col <- function(file){
  #FOR GEOGRAPHY CODEBOOKS. READS THEM IN AND ADDS A COLUMN WITH THEIR CORRESPONDING SURVEY_SERIES
  rn <- gsub(".csv", "", file, ignore.case=T)
  spl <- strsplit(rn, "/") %>% unlist()
  svy <- spl[length(spl)]
  df <- fread(file, integer64="character", data.table=T)
  df[, survey_series := svy]
  df <- lapply(df, as.character)
  return(df)
}

## Read data and bind together----------------------------------------------------------------------
extractions <- list.files(folder_in, full.names=T, pattern="*.dta", recursive = T)

#bind all extracted surveys together
topics <- rbindlist(lapply(extractions, read_dta), fill = T)

## get extraction date from save date of file
details <- file.info(list.files(folder_in, full.names=T, pattern="*.dta", recursive = T))
details <- details[with(details, order(as.POSIXct(mtime))), ]
details$file_name <- gsub(".*/", "", rownames(details))
details$nid <- sub("\\D*_(\\d+).*", "\\1", details$file_name)
details$nid <- gsub(".*PMA2020", "", details$nid)

details$nid <- as.numeric(details$nid)
details$mtime <- gsub(" .*", "", details$mtime)
details <- details[, c("nid", "mtime")]
colnames(details) <- c("nid", "file_date")

topics <- merge(topics, details, by="nid")

#This NID needs pweight filled in -- use HH weight if that is provided
topics[nid == 24890 & is.na(pweight), pweight := hhweight]
topics[nid == 24915 & is.na(pweight), pweight := hhweight]
topics[nid == 26642 & is.na(pweight), pweight := hhweight]

##weighting variable separated for this ID, but all responses are weight==1.
topics[nid == 50441 & is.na(pweight), pweight := 1]

## DHS pweights need to be divided by 1 million to be accurate (per DHS documentation)
topics[survey_name == "DHS",  pweight := pweight/1000000]

# # Read in and prepare geocodebooks----------------------------------------------------------------------
geo_k <- fread("<<<< FILEPATH REDACTED >>>>/geocodebook.csv")

geo_k$iso3 <- str_trim(geo_k$iso3)
geo_k$iso3_hold <- geo_k$iso3
geo_k$geospatial_id <- gsub("^0", "", geo_k$geospatial_id)
geo_k$geospatial_id <- gsub("\\.00$", "", geo_k$geospatial_id)

source("<<<< FILEPATH REDACTED >>>>/geocodebook_snap.r")
geo_k <- snap_codebook(geo_k) ## snap data points < 15 km from country boundary to within it
geo_k$iso3 <- geo_k$iso3_hold

# # Merge extracted dataset and geography dataset together
all <- merge(geo_k, topics, by.x=c("nid", "iso3", "geospatial_id"),
             by.y=c("nid", "ihme_loc_id", "geospatial_id"), all.x=F, all.y=T)

## Check how much has been geomatched for each ID
missing_review <- all[,length(location_code), by = c("point", "nid")]

missing_geography <- subset(all, is.na(point))
missing_geography$shapefile <- NA
all <- subset(all, !(is.na(point)))
all <- rbind(all, missing_geography, fill=T)
rm(missing_shapefile)

all <- as.data.table(all)
all <- all[, -c("latitude","longitude")]
setnames(all, c("iso3", "lat", "long"), c("country", "latitude", "longitude"))
all$ihme_loc_id <- all$country
all$country <- substr(all$country, 1, 3)
all$nid  <- as.numeric(all$nid)

## Get admin_1_id when it is missing from data
source("<<<< FILEPATH REDACTED >>>>/get_location_metadata.R")
{
  r <- get_location_metadata(location_set_id=125, release_id=13)
  r <- subset(r, grepl("BFA|NGA|KEN", ihme_loc_id))
  r <- r[, c("parent_id", "location_id","location_ascii_name", "ihme_loc_id", "location_type")]
  r$cid <- r$ihme_loc_id
  r$country <- gsub("_.*","", r$ihme_loc_id)
  setnames(r,"location_ascii_name","admin_1")
  r[, admin_1:= tolower(admin_1)]
  r[, admin_1:= gsub(" local government area", "", admin_1)]
  r$ihme_loc_id <- NULL
  
  get_adm1 <- subset(all, is.na(admin_1_id) | admin_1_id=="")
  get_adm1 <- get_adm1[, c("nid","country","ihme_loc_id","admin_1","admin_1_id", "admin_2", "admin_2_id","geospatial_id","location_code","location_name")]
  get_adm1[, admin_1 := str_trim(admin_1)]
  get_adm1[grepl("^[0-9]",admin_1), admin_1 := gsub('.*\\.', '', admin_1)]
  get_adm1[is.na(get_adm1$admin_1) | get_adm1$admin_1=="", admin_1 := location_name]
  get_adm1[nid==399235, admin_1 := location_name]
  
  get_adm1[, admin_1:= tolower(admin_1)]
  get_adm1[, admin_1 := gsub('_', '-', admin_1)]
  get_adm1[, admin_1 := str_trim(admin_1)]
  get_adm1[admin_1=="ajeromi/ ifelodun", admin_1 := "ajeromi/ifelodun"]
  get_adm1[admin_1=="eti osa", admin_1 := "eti-osa"]
  get_adm1[admin_1=="fct, abuja", admin_1 := "fct (abuja)"]
  get_adm1[admin_1=="federal capital territory", admin_1 := "fct (abuja)"]
  
  ##convert BFA english to french
  get_adm1[country=="BFA", admin_1 := gsub("center","centre",admin_1)]
  get_adm1[country=="BFA", admin_1 := gsub("east","est",admin_1)]
  get_adm1[country=="BFA", admin_1 := gsub("west","ouest",admin_1)]
  get_adm1[country=="BFA", admin_1 := gsub("north","nord",admin_1)]
  get_adm1[country=="BFA", admin_1 := gsub("south","sud",admin_1)]
  get_adm1[country=="BFA", admin_1 := gsub(" ","-",admin_1)]
  get_adm1[grepl("haut",admin_1) & country=="BFA", admin_1 := "haut-bassins"]
  get_adm1[grepl("mouhoun",admin_1) & country=="BFA", admin_1 := "boucle du mouhoun"]
  get_adm1[admin_1=="cascade" & country=="BFA", admin_1 := "cascades"]
  get_adm1[admin_1=="central/sud" & country=="BFA", admin_1 := "centre/sud"]
  
  ##specific adjustment
  get_adm1[nid==470503 & ihme_loc_id=="NGA_25342", admin_1 := "kano"]
  get_adm1[nid==470503 & ihme_loc_id=="NGA_25337", admin_1 := "lagos"]
  get_adm1[nid==470507 & geospatial_id==	854191026, admin_1 := "haut-bassins"]
  get_adm1[nid==470507 & geospatial_id==	854141002, admin_1 := "centre-est"]
  get_adm1[nid==347047 & geospatial_id==4990 & is.na(admin_1) , admin_1 := "west pokot"]
  get_adm1[nid==347047 & geospatial_id==4674 & is.na(admin_1) , admin_1 := "kilifi"]
  
  
  adm3_26642 <- c("bobo-dioulasso", "bouss??", "diapaga", "diebougou", "djibo", "kongoussi", "leo", "manga", "nouna", "ouagadougou", "ouahigouya", "ouargaye", "sindou")
  adm1_26642 <- c("haut-bassins", "plateau-central", "est", "sud-ouest", "sahel", "centre-nord", "centre-ouest", "centre-sud", "boucle du mouhoun", "centre", "nord", "centre-est", "cascades")
  for(i in 1:length(adm3_26642)){
    get_adm1[admin_1==adm3_26642[i] & nid==26642, admin_1 := adm1_26642[i]]
  }
  
  get_adm1 <- unique(get_adm1[,c("nid","country", "geospatial_id","ihme_loc_id","admin_1")])
  get_adm1 <- merge(get_adm1, r, by=c("country", "admin_1"), all=T)
  get_adm1 <- get_adm1[!is.na(get_adm1$nid),c("nid", "geospatial_id", "admin_1", "cid", "parent_id")]
  setnames(get_adm1,"cid","admin_1_id")
  
  ##50441, replace admin 2 with admin 1 
  get_adm1[nid==50441 & parent_id != 214, admin_1_id := paste0("NGA_", parent_id) ]
  
  ##19076 admin_1 is a older version doesn't match with current boundaries. Make own IDs for these that are not valid, but will separate from other locations
  for(i in 1:length(unique(get_adm1$admin_1[get_adm1$nid==19076]))){
    name <- unique(get_adm1$admin_1[get_adm1$nid==19076])[i]
    get_adm1[admin_1==name & nid==19076, admin_1_id := paste0("BFA_19076_",i)]
  }
  
  ##399235 only keep admin 1 levels
  get_adm1 <- subset(get_adm1, nid!= 399235 | (nid==399235 & parent_id==214))
  
  all <- merge(all, get_adm1, by=c("nid", "geospatial_id"), all.x=T)
  all[is.na(admin_1_id.x) | admin_1_id.x=="", admin_1_id.x := admin_1_id.y]
  all <- all[, -c("admin_1.y", "admin_1_id.y")]
  setnames(all,c("admin_1.x", "admin_1_id.x"),c("admin_1", "admin_1_id"))
  
  ##157057, replace admin 2 with admin 1 
  all[nid==157057, admin_1 := admin_2][nid==157057, admin_1_id := admin_2_id]
  
  ##274708, replace admin 2 with admin 1 
  replace <- c("NGA_25873","NGA_76489","NGA_76490","NGA_76491","NGA_76494")
  for(i in replace){
    parent <- paste0("NGA_", r$parent_id[r$cid==i])
    all[nid==274708 & admin_1_id==i, admin_1_id := parent]
  }
  
  ##399235 get admin 1 names
  all[nid==399235, admin_1 := location_name]
}  

## Save RDS file of matched extractions----------------------------------------------------------------------
saveRDS(all, file=paste0(folder_out, "/", topic, "_", module_date, ".RDS"))
