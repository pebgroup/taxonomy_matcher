# BIEN presence maps using WCSP regions
# Produces species richness counts using BIEN occurrences in the WCSP regions. 
# Produces a list with all BIEN species names per WCSP region.

library(rgdal)
library(tidyverse)

# Switch to chose data volume, based on directory (server or local)
if(getwd()=="/data_vol/melanie"){
  file_in    <- file("../emil/all_bien_occurrences_4column.rm.na.csv","r")
  dat <- readLines(file_in)
}else{
  file_in    <- file("all_bien_occurrences_4column.rm.na.csv","r")
  chunk_size <- 100000 # choose the best size for you
  dat <- readLines(file_in, n=chunk_size)
}

dat <- strsplit(dat, ",")
dat <- unlist(dat)
dat <- matrix(dat, ncol=4, byrow=TRUE)
dat <- as.data.frame(dat)
names(dat) <- c("taxonobservation_id", "species", "lat", "lng")
dat <- dat[-grep("[a-z]", dat$lat),]
dat$lat <- as.numeric(as.character(dat$lat))
dat$lng <- as.numeric(as.character(dat$lng))


# clean coordinates
dat <- dat[!abs(dat$lat)>90,] # remove species with impossible coordinates
dat <- dat[!abs(dat$lng)>180,] # remove species with impossible coordinates


# read in additional infraspecific info for species that correspond to binomial multimatching species
files <- dir()
files <- files[grepl("[0-9].rds", files)]
df <- files %>%
  map_dfr(readRDS)

which(dat$taxonobservation_id %in% df$taxonobservation_id)
dat <- merge(dat, df[,c("taxonobservation_id", "scrubbed_taxon_name_no_author", "scrubbed_author")], all.x=TRUE)

# read WCSP shapefile
shp <- readOGR("shapefile/level3.shp")

# loop over regions instead of species (--> less iterations)
coord <- dat[,c("lat", "lng")]
coordinates(coord) <- ~  lng + lat  # convert dataframe to spatial points object
proj4string(coord) <- proj4string(shp) # match projection attributes of both objects


# BIEN species counts & list per WCSP region #####################
# record all occurring species and calculate the intersection percentage with the WCSP species in that region

head(df)

# add synonyms part


ptm <- proc.time() # Start the clock

regions <- shp$LEVEL_3_CO
res <- data.frame(region=rep(NA,length(regions)), sr=rep(0, length(regions)))
spec.list <- list()
for(i in 1:length(regions)){
  temp <- shp[i,]
  # get species that occur in this region
  region_occurrences <-  over(temp, geometry(coord), returnList = TRUE)
  region_species <- dat$species[as.numeric(unlist(region_occurrences))]
  # accumulate species occurrences to presence data
  res[i, 1] <- as.character(temp$LEVEL_3_CO)
  res[i, 2] <- length(unique(region_species))
  # write species names into the list for each region
  #names(spec.list[i]) <- as.character(temp$LEVEL_3_CO)
  spec.list[[i]] <- unique(region_species)
  if(!i%%10)cat(i,"\r")
}
names(spec.list) <- shp$LEVEL_3_CO
print(proc.time() - ptm)


if(getwd()=="/data_vol/melanie"){
  save(res, spec.list, file="BIEN_in_WCSP_regions.RData")
}else{
  save(res, spec.list, file="BIEN_in_WCSP_regions_local_subset.RData")
}


# add notification if run is finished



################### END SERVER OUTSOURCING #############################



    




#### TRASH & OLD STUFF ##############
# ptm <- proc.time() # Start the clock
# for(i in 1:length(regions)){
#   temp <- shp[i,]
#   # get species that occur in this region
#   region_occurrences <-  over(temp, geometry(coord), returnList = TRUE)
#   region_species <- dat$species[as.numeric(unlist(region_occurrences))]
#   # accumulate species occurrences to presence data
#   res[i, 1] <- as.character(temp$LEVEL_3_CO)
#   res[i, 2] <- length(unique(region_species))
#   if(!i%%10)cat(i,"\r")
# }
# print(proc.time() - ptm)
# print(res)
# # 1 mio occurrences in 117 seconds 117*157/60/60 --> 5 hours
# 
# write.csv(res, file="sr_bien_wcsp_regions.csv")






# intersection of species names to reduce data
## get WCSP species names
# wc <- read.csv("../WCSP_analysis/database/published_names_19_10_2018.csv", sep="|", stringsAsFactors = FALSE)
# wc2 <- read.csv("../WCSP_analysis/database/unpublished_names_19_10_2018.csv", sep="|", stringsAsFactors = FALSE)
# wc_all <- rbind(wc, wc2)
# rm(wc, wc2)
# wc.df <- wc_all[,c("accepted_name", "checklist_id")]
# 
# #accepted name & checklist_id
# saveRDS(wc.df, file="species_names_WCSP.rds")
# rm()
# 
# wc.df <- readRDS("species_names_WCSP.rds")
# species.wc <- unique(wc.df$accepted_name)
# # species names intersection
# table(dat$species %in% species.wc)
# dat <- dat[dat$species %in% species.wc, ] # remove the species not included in WCSP