
############################
# Taxon matching BIEN WCSP #
############################
  library(rgdal)
  library(tidyverse)
  library(BIEN)
  
  # Get data
test_download <- BIEN:::.BIEN_sql(query = "SELECT scrubbed_family,scrubbed_species_binomial,
  	                                       scrubbed_taxon_name_no_author,
  	                                       scrubbed_author,taxonobservation_id,latitude,longitude 
	                                         FROM view_full_occurrence_individual LIMIT 1000000;")



  ## BIEN
  # Switch to chose data volume, based on directory (server or local)
  if(getwd()=="/data_vol/melanie"){
    file_in    <- file("/BIEN_6col_download/all_bien_occurrences_6cols_rm_na.csv","r")
    #dat <- readLines(file_in)
    dat <- read.csv(file="database/all_bien_occurrences_6cols_rm_na.csv")
  }else{
    #file_in    <- file("database/all_bien_occurrences_6cols_rm_na.csv","r")
    chunk_size <- 10000 # choose the best size for you
    #dat <- readLines(file_in, n=chunk_size)
    #dat <- fread("database/all_bien_occurrences_6cols_rm_na.csv", nrow=0)
    dat <- read.csv(file="database/all_bien_occurrences_6cols_rm_na.csv",nrows=chunk_size)
  }

bien <- readRDS("all_bien_occurrences_6cols.rds")

# dat <- strsplit(dat, split="\\|")
# dat <- unlist(dat)
# dat <- matrix(dat, ncol=6, byrow=TRUE)
# dat <- as.data.frame(dat)
# names(dat) <- c("species","scrubbed_taxon_name_no_author","scrubbed_author","taxonobservation_id", "lat", "lng")
# dat <- dat[-grep("[a-z]", dat$lat),]
# dat$lat <- as.numeric(as.character(dat$lat))
# dat$lng <- as.numeric(as.character(dat$lng))

# clean data
dat <- dat[!abs(dat$lat)>90,] # remove species with impossible coordinates
dat <- dat[!abs(dat$lng)>180,] # remove species with impossible coordinates
bien <- dat
rm(dat)



## WCSP
#wc_all <- read_delim("database/checklist_names.txt", delim="|", ) # 26 columns
# does not read the whole file.... lots of lines are missing. why?
# Workaround! use readLines, but insert a whitespace at the end of each line before splitting into seperate strings,
# otherwise you end up with unequal number of columns in cases where the last entry is empty

# wc_all <- readLines("database/checklist_names.txt") # 1 356 708 lines
# wc1 <- wc_all[1:678354]
# wc2 <- wc_all[678355:length(wc_all)]
# rm(wc_all)
# 
# wc1 <- paste0(wc1, " ")
# wc1 <- strsplit(wc1, "|", fixed=TRUE)
# wc1 <- unlist(wc1)
# wc1 <- matrix(wc1, ncol=26, byrow=TRUE)
# # save names
# nomen <- wc1[1,]
# nomen <- str_trim(nomen)
# 
# wc1 <- as.data.frame(wc1)
# 
# wc2 <- paste0(wc2, " ")
# wc2 <- strsplit(wc2, "|", fixed=TRUE)
# wc2 <- unlist(wc2)
# wc2 <- matrix(wc2, ncol=26, byrow=TRUE)
# wc2 <- as.data.frame(wc2)
# 
# wc_all <- rbind(wc1, wc2)
# names(wc_all) <- nomen
# wc_all <- wc_all[-1,]
# wc_all$basionym_plant_name_id <- str_trim(wc_all$basionym_plant_name_id)
# 
# saveRDS(wc_all, file="wcp_dec_19.rds")

wc_all <- readRDS("database/wcp_dec_19.rds")
# 
# # remove genera
# table(wc_all$taxon_rank)
# test <- wc_all[-wc_all$taxon_rank=="Genus",]




# Tax compare
## step 1: accepted name matching
table(as.character(wc_all$accepted_plant_name_id)==as.character(wc_all$plant_name_id))


length(unique(wc_all$taxon_name[wc_all$taxon_status=="Accepted"]))

# get accepted name: get taxon name from accepted IDs attached to the df
# wc_all$accepted_name <- merge(wc_all, wc_all[,c("accepted_plant_name_id", "taxon_name")],
#                               by.x="plant_name_id", by.y="accepted_plant_name_id", all.x=TRUE)
test <- merge(wc_all, wc_all[,c("accepted_plant_name_id", "taxon_name")],
                              by.x="plant_name_id", by.y="accepted_plant_name_id", all.x=TRUE)
## does not work right now....




table(as.character(wc_all$primary_author) == as.character(wc_all$taxon_authors))
  
bien$match <- NA
bien$match[which(bien$scrubbed_species_binomial %in% wc_all$accepted_name)] <- "level1_match"

## step 2: synonym matching
wc_all$binomial <- paste(wc_all$genus, wc_all$species)
#bien$match[which(bien$species[is.na(bien$match)] %in% wc_all$binomial)] <- "level2_match"
bien$match[intersect(which(is.na(bien$match)), which(bien$species %in% wc_all$binomial))] <- "level2_match"
# table(bien$match, useNA="ifany")

### get those with unique matching in WCSP only
bien_sub <- bien[bien$match=="level2_match",]
bien_sub$accepted_wcsp <- NA
level2_species <- unique(bien_sub$species)
j <- 0
for(i in level2_species){
  temp <- wc_all[wc_all$binomial==i,]
  
  # test accepted names length in wcsp
  if(length(unique(temp$accepted_name))==1){
    bien_sub$accepted_wcsp[bien_sub$species==i] <- unique(temp$accepted_name)
  }else{
    
    #bien_sub$accepted_wcsp[bien_sub$species==i] <- "level3_match" # kind of unneccessary to treat them separately, throw them back in the yet unmatched pool ?
  }
  j <- j+1
  if(!j%%10)cat(j,"\r")
}

### merge them back into the complete list
bien <- merge(bien, bien_sub[,c("taxonobservation_id", "accepted_wcsp")], by="taxonobservation_id", all.x=TRUE)
rm(bien_sub)


## step 3: test for all remaining using the infraspecific information
### GET MORE BIEN INFO: Additional BIEN download data
# not all NA match species are in the additional download. this is just for multimatch species, not for species that show no match at all. do i need this as well?

# read in additional infraspecific info for species that correspond to binomial multimatching species
# files <- dir()
# files <- files[grepl("[0-9].rds", files)]
# df <- files %>%
#   map_dfr(readRDS)


# check species names: get missing taxon IDs
missing1 <- unique(bien$species[which(is.na(bien$match))]) # NAs
missing2 <- bien$species[which(bien$match=="level2_match" & is.na(bien$accepted_wcsp))]

missing_bi <- c(as.character(missing1), as.character(missing2))

# add more BIEN tax info
BIEN_occurrence_species_mod <- function (species, only.new.world = FALSE, ...) 
{
  .is_log(only.new.world)
  .is_char(species)
  newworld_ <- .newworld_check(only.new.world)
  query <- paste("SELECT scrubbed_species_binomial,scrubbed_taxon_name_no_author,scrubbed_author,taxonobservation_id,latitude, longitude",
                 "\n                 FROM view_full_occurrence_individual \n                 WHERE scrubbed_species_binomial in (", 
                 paste(shQuote(species, type = "sh"), collapse = ", "), 
                 ")",  
                 "\n                 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) \n                 ORDER BY scrubbed_species_binomial ;")
  return(.BIEN_sql(query, ...))
}
assignInNamespace('BIEN_occurrence_species',BIEN_occurrence_species_mod,ns='BIEN')
environment(BIEN_occurrence_species_mod)<-asNamespace('BIEN') 

missing_bien <- BIEN_occurrence_species_mod(missing_bi, only.new.world = FALSE)

bien <- merge(bien, missing_bien[,c("taxonobservation_id", "scrubbed_taxon_name_no_author", "scrubbed_author")], all.x=TRUE)


bien_sub <- bien[is.na(bien$accepted_wcsp) & bien$match=="level2_match",]

#

## create wcsp taxon name
# scrubbed_taxon_name_no_author matches WCSP taxon name (create new variable for that from Genus, infra rank + epiphet, species)
wc_all$taxon_name_no_author <- paste(wc_all$genus, wc_all$species, wc_all$infraspecific_rank, wc_all$infraspecific_epithet)
wc_all$taxon_name_no_author <- str_trim(wc_all$taxon_name_no_author) # strip spaces from strings, super important

multimatch_species <- unique(bien_sub$scrubbed_taxon_name_no_author[which(bien_sub$scrubbed_taxon_name_no_author %in% wc_all$taxon_name_no_author)])
j <- 0
for(i in multimatch_species){
  temp <- wc_all[wc_all$taxon_name_no_author==i,]
  
  # test accepted names length in wcsp
  if(length(unique(temp$accepted_name))==1){
    bien_sub$accepted_wcsp[bien_sub$species==i] <- unique(temp$accepted_name)
  }
  j <- j+1
  if(!j%%10)cat(j,"\r")
}

### merge them back into the complete list
bien <- merge(bien, bien_sub[,c("taxonobservation_id", "accepted_wcsp")], by="taxonobservation_id", all.x=TRUE)
rm(bien_sub)


## CHECK  
table(is.na(bien$accepted_wcsp.x) == is.na(bien$accepted_wcsp.y))
# Check taxonomy for those still missing: is it all mosses and lichen? Then we are good




