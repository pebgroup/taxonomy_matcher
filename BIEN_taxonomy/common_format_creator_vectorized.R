## Script to build a common format to feed into the taxonomy matcher. 
# Processed databases are BIEN and GBIF. 
# Each database has it`s own format, requiring partly individual treatment

## Computational requirements:
# BIEN data requires >8 GB RAM, runs on a 128GB machine. Always test with local subset (chose in GET DATA section)

## Notes on BIEN
# BIEN download data requires following columns: # scrubbed_taxon_name_no_author, scrubbed_family, scrubbed_author.
# We SQL-queried the BIEN database via BIEN:::.BIEN_sql()

## Notes on GBIF
# This script starts with the downloaded list obtained via rgbif::name_usage(), so it assumes you have your required GBIF IDs.
# It was written and tested for taxonomic information for tip labels of the Smith&Brown 2018 seed plant phylogeny. 


library(data.table)


#### CHOSE DATABASE ##############################################################################
# options are BIEN and GBIF

# GBIF requires download via name_usage() function from gbif package to get all relevant columns
# script checks if required columns are present

db <- "GBIF"


#### GET DATA ####################################################################################

if(db=="BIEN"){
  
  #  Switch to chose data volume according to whether working on server locally
  if(getwd()=="/data_vol/melanie/BIEN_download"){
    bien <- fread(file="all_bien_occurrences_7cols_rm_na.csv")
  }else{
    chunk_size <- 1000000
    bien <- fread(file="../data/all_bien_occurrences_7cols_rm_na.csv", nrows=chunk_size)
  }
}

if(db=="GBIF"){
  
  gbif <- readRDS("../data/gbif_all.rds")
}




#### BIEN PROCESSING #############################################################################
if(db=="BIEN"){

# data check
required <- c("scrubbed_taxon_name_no_author", "scrubbed_family", "scrubbed_author")
if(all(required %in% names(bien))){
  print("All required taxon info present in download")
}else{stop("Required information is missing! Check your download")}


# clean data
bien <- bien[!abs(bien$latitude)>90,] # remove species with impossible coordinates
bien <- bien[!abs(bien$longitude)>180,] # remove species with impossible coordinates


# create unique taxon ID that contains all possible information:
bien$taxon_author_ID <- paste(bien$scrubbed_taxon_name_no_author, bien$scrubbed_author)
bien_sub <- bien[!duplicated(bien[,"taxon_author_ID"]),]

# set up common format
split_length <- unlist(lapply(strsplit(as.character(bien_sub$scrubbed_taxon_name_no_author), split = " "), length))
bien_input <- data.frame(scrubbed_taxon_name_no_author=bien_sub$scrubbed_taxon_name_no_author,
                         family = bien_sub$scrubbed_family,
                         author=bien_sub$scrubbed_author, 
                         split_length=split_length,
                         genus_hybrid = rep(NA, nrow(bien_sub)),
                         species_hybrid = rep(NA, nrow(bien_sub)),
                         genus = rep(NA, nrow(bien_sub)),
                         species = rep(NA, nrow(bien_sub)),
                         taxon_rank = rep(NA, nrow(bien_sub)),
                         infra_name = rep(NA, nrow(bien_sub)),
                         comment = rep(NA, nrow(bien_sub)),
                         usable = rep(NA, nrow(bien_sub)),
                         taxon_author_ID = bien_sub$taxon_author_ID
)

# order the dataframe by split length
bien_input <- bien_input[order(bien_input$split_length),]
bien_input$id <- c(1:nrow(bien_input))

#id_name_connect <- bien_input[,c(bien_input$taxon_author_ID, bien_input$id)]
#saveRDS()

split_list <- strsplit(as.character(bien_input$scrubbed_taxon_name_no_author), split = " ")
names(split_list) <- bien_input$id

# Vectorize + loop mix

## split length == 1
ind <- which(bien_input$split_length==1)
bien_input$genus[ind] <- as.character(bien_input$scrubbed_taxon_name_no_author[ind])
bien_input$taxon_rank[ind] <- "genus"

## split length == 2
ind <- which(bien_input$split_length==2)
bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
bien_input$species[ind] <- sapply(split_list[ind], "[[", 2)
bien_input$taxon_rank[ind] <- "species"

## split length == 3
ind <- which(bien_input$split_length==3)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][1]=="x"){
    bien_input$genus_hybrid[ind[i]] <- "x"
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][2]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][3]
    bien_input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][2]=="x"){ # does not occur but you never know
    if(grepl("[A-Z]",split_list[[ind[i]]][3])){
      bien_input$genus_hybrid[ind[i]] <- "x"
      bien_input$usable[ind[i]] <- "no"
    }else{
      bien_input$species_hybrid[ind[i]] <- "x"
      bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
      bien_input$species[ind[i]] <- split_list[[ind[i]]][[3]]
      bien_input$taxon_rank[ind[i]] <- "species"
    }
  }
  if(split_list[[ind[i]]][1]!="x" & split_list[[ind[i]]][2]!="x" & !grepl("[A-Z]",split_list[[ind[i]]][3])){ 
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    bien_input$taxon_rank[ind[i]] <- NA
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[3]]
    }
  if(split_list[[ind[i]]][2]!="x" & grepl("[A-Z]",split_list[[ind[i]]][3])){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$taxon_rank[ind[i]] <- "genus"
  }
}

## split length == 4
ind <- which(bien_input$split_length==4)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][[1]]=="x" & split_list[[ind[i]]][[3]]=="x"){
    bien_input$genus_hybrid[ind[i]] <- "x"
    bien_input$species_hybrid[ind[i]] <- "x"
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[2]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[4]]
    bien_input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]=="x"){
    bien_input$species_hybrid[ind[i]] <- "x"
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
#    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
    bien_input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]!="x"){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][[3]]
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
    if(split_list[[ind[i]]][[3]]==""){bien_input$taxon_rank[ind[i]] <- NA}
  }
  if(!i%%100)cat(i,"\r")
}
  
## split length == 5
ind <- which(bien_input$split_length==5)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][2]=="x"){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][1]
    bien_input$species_hybrid[ind[i]] <- split_list[[ind[i]]][2]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][3]
    bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][4]
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][5]
  }
  if(split_list[[ind[i]]][4]=="x"){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][1]
    bien_input$species_hybrid[ind[i]] <- split_list[[ind[i]]][4]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][2]
    bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][3]
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][5]
  }
  if(split_list[[ind[i]]][3]=="x"){  # this is aiming at Dieffenbachia nitidipetiolada x d. oerstedii
    bien_input$usable[ind[i]] <- "no"
  }
}

## split length == 6 
ind <- which(bien_input$split_length==6)
# bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
# bien_input$species_hybrid[ind] <- sapply(split_list[ind], "[[", 2)
# bien_input$species[ind] <- sapply(split_list[ind], "[[", 3)
# bien_input$taxon_rank[ind] <- sapply(split_list[ind], "[[", 4)
# bien_input$infra_name[ind] <- sapply(split_list[ind], "[[", 5)
bien_input$usable[ind] <- "no"

  bien_input$usable[bien_input$split_length %in% c(7,8)] <- "no"
  bien_input <- bien_input[-which(bien_input$usable=="no"),]
}

saveRDS(bien_input, "bien_input_vectorized4.rds")



#### GBIF PROCESSING #############################################################################
if(db=="GBIF"){
  
# data check and preparation
required <- c("key", "scientificName", "authorship",
              "taxonomicStatus", "rank", "family", "genus",
              "species")
if(all(required %in% names(gbif[[1]]))){
  print("All required taxon info present in download")
}else{stop("Required information is missing! Check your download")}  


# transform list to dataframe
## removing lines that do not include all required columns
func1 <- function(x){all(required %in% names(x))}
todrop <- which(!sapply(gbif, func1))
if(length(todrop>0)){gbif <- gbif[-todrop]} # avoid empty todrop effect

gbif.df <- data.frame(taxonID = sapply(gbif, "[[", "key"), 
                      scientificName= sapply(gbif, "[[", "scientificName"),
                      authorship = sapply(gbif, "[[", "authorship"),
                      taxonomicStatus = sapply(gbif, "[[", "taxonomicStatus"),
                      rank = sapply(gbif, "[[", "rank"),
                      family = sapply(gbif, "[[", "family"),
                      genus = sapply(gbif, "[[", "genus"),
                      species = sapply(gbif, "[[", "species"))
#rm(gbif)

# remove authors from scientific name
library(stringr)
gbif.df$scientificName <- as.character(gbif.df$scientificName)
gbif.df$authorship <- as.character(gbif.df$authorship)
# store the original
gbif.df$scientificName_org <- gbif.df$scientificName

# remove special characters
special_characters <- list('Š'='S', 'Ş'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'Č'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y', 'Ř'='R', 'İ'='I', 'Ż'='Z', 'Ř'='R')

gbif.df$scientificName <- chartr(paste(names(special_characters), collapse=''),
                                  paste(special_characters, collapse=''),
                                  gbif.df$scientificName)

# GBIF does not hold Genus hybrids, so removing everything after the second capital should work. 
# matching author patterns excludes in some cases start of genus name. this works, it`s just a bit slow (4mins)

for(i in 1:nrow(gbif.df)){
  str <- as.character(gbif.df[i,"scientificName"])
  tmp_str <- unlist(strsplit(str, ""))
  ind <- grep("[A-Z]", tmp_str)[2]
  if(is.na(ind)){next}
  gbif.df[i,"scientificName"] <- paste0(c(tmp_str[1:(ind-1)]), collapse="")
  if(!i%%1000)cat(i,"\r")
}

# remove last brackets
gbif.df[grepl("\\(", gbif.df$scientificName),"scientificName"] <- gsub("\\(", "", gbif.df[grepl("\\(", gbif.df$scientificName),"scientificName"])

# remove hort. ex part
gbif.df[grepl("hort. ex", gbif.df$scientificName),"scientificName"] <- gsub("hort. ex", "", gbif.df[grepl("hort. ex", gbif.df$scientificName),"scientificName"])
gbif.df[grepl("hort.", gbif.df$scientificName),"scientificName"] <- gsub("hort.", "", gbif.df[grepl("hort.", gbif.df$scientificName),"scientificName"])

# remove space at end
gbif.df$scientificName <- str_trim(gbif.df$scientificName)

# speciaal for the dutch and belgium authors ^^ 
#speciaal <- c(" du", " de", " van", " la", " ex", " 't", " d'")
gbif.df$scientificName <- gsub(" du$| de$| van$| la$| ex$| 't$| d'$| den$| v\\.$| f\\.$| t'$| van den$| van der$| von$| f\\. ex$| van de.*$", "", gbif.df$scientificName)
gbif.df$scientificName <- str_trim(gbif.df$scientificName)
gbif.df$scientificName <- gsub(" du$| de$| van$| la$| ex$| 't$| d'$| den$| v\\.$| f\\.$| t'$| van den$| van der$| von$| f\\. ex$| van de.*$", "", gbif.df$scientificName)
# this is not a typo, need to run this twice because remove one pattern sometimes only reveals the next

# remove some really special cases in GBIF
gbif.df$scientificName <- gsub(" s$| t \\&|,.*| borb$| aggr$", "", gbif.df$scientificName)

# remove space at end
gbif.df$scientificName <- str_trim(gbif.df$scientificName)
gbif.df$split <- unlist(lapply(strsplit(gbif.df$scientificName, split = " "), length))


# set up common format ###################################################################
split_length <- unlist(lapply(strsplit(gbif.df$scientificName, split = " "), length))
input <- data.frame(taxonID=gbif.df$taxonID,
                    taxon_name = gbif.df$scientificName,
                    family = gbif.df$family,
                    author= gbif.df$authorship, 
                    split_length=split_length,
                    genus_hybrid = NA,
                    species_hybrid = NA,
                    species = sub(".* ", "", gbif.df$species),
                    genus = gbif.df$genus,
                    taxon_rank = tolower(gbif.df$rank),
                    infra_name = NA,
                    comment = NA,
                    usable = NA)


## Cleaning
# remove sp. species
if(any(grepl("sp\\.", as.character(input$taxon_name)))){
  input <- input[!grepl("sp\\.", as.character(input$taxon_name)),]
  table(grepl("sp\\.", as.character(input$taxon_name)))}

# # remove aggregates
# if(any(grepl("aggr$", as.character(input$taxon_name)))){
#   input <- input[!grepl("aggr$", as.character(input$taxon_name)),]
#   table(grepl("aggr$", as.character(input$taxon_name)))}



# order the dataframe by split length
input <- input[order(input$split_length),]
input$id <- c(1:nrow(input))

# create split list
split_list <- strsplit(as.character(input$taxon_name), split = " ")
names(split_list) <- input$id








# Vectorize + loop mix ###############################################################

## split length == 1
ind <- which(input$split_length==1)
input$usable[input$split_length==1] <- "no"

## split length == 2
ind <- which(input$split_length==2)

# # get authors
# input$author[ind] <- gsub("^[A-Z][a-z| ]+", "", input[ind, "taxon_name"])

# # get the rest
# input$genus[ind] <- sapply(split_list[ind], "[[", 1)
# input$species[ind] <- sapply(split_list[ind], "[[", 2)
# input$taxon_rank[ind] <- "species"

ind.hyb <- which(grepl("^×", input$taxon_name))
#ind.hyb <- which(grepl("^×", input$genus))
#input$genus[ind.hyb] <- gsub("^×", "", sapply(split_list[ind.hyb], "[[", 1))
input$genus_hybrid[ind.hyb] <- "x"
input$usable[ind.hyb] <- "no"


## split length == 3
ind <- which(input$split_length==3)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][2]=="sp."){
    #    input$genus[ind[i]] <- split_list[[ind[i]]][1]
    #    input$species[ind[i]] <- split_list[[ind[i]]][2]
    #    input$taxon_rank[ind[i]] <- "species"
    input$usable[ind[i]] <- "no"
    
  }
  if(split_list[[ind[i]]][2]=="x"){ # does not occur but you never know
    if(grepl("[A-Z]",split_list[[ind[i]]][3])){
      input$genus_hybrid[ind[i]] <- "x"
      input$usable[ind[i]] <- "no"
    }else{
      input$species_hybrid[ind[i]] <- "x"
      #      input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
      #      input$species[ind[i]] <- split_list[[ind[i]]][[3]]
      #      input$taxon_rank[ind[i]] <- "species"
    }
  }
  if(split_list[[ind[i]]][1]!="x" & split_list[[ind[i]]][2]!="x" & !grepl("[A-Z]",split_list[[ind[i]]][3])){ 
    #    input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    #    input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    #    input$taxon_rank[ind[i]] <- NA
    input$infra_name[ind[i]] <- split_list[[ind[i]]][[3]]
  }
  if(split_list[[ind[i]]][2]!="x" & grepl("[a-z]",split_list[[ind[i]]][3])){
    #    input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    #    input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    #    input$taxon_rank[ind[i]] <- "species"
    #input$usable[ind[i]] <- "no"
  }
  if(grepl("sp\\.", split_list[[ind[i]]][[2]])){
    input$usable[ind[i]] <- "no"
  }
}

## split length == 4
ind <- which(input$split_length==4)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][[1]]=="x" & split_list[[ind[i]]][[3]]=="x"){
    input$genus_hybrid[ind[i]] <- "x"
    input$species_hybrid[ind[i]] <- "x"
    input$genus[ind[i]] <- split_list[[ind[i]]][[2]]
    input$species[ind[i]] <- split_list[[ind[i]]][[4]]
    input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]=="x"){
    input$species_hybrid[ind[i]] <- "x"
    input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    #    input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
    input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]!="x"){
    input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    #input$taxon_rank[ind[i]] <- split_list[[ind[i]]][[3]]
    input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
    if(split_list[[ind[i]]][[3]]==""){input$taxon_rank[ind[i]] <- NA}
  }
}

# remove all unusable taxa
input <- input[-which(input$usable=="no"),]



## STATS INCLUDED / NOT INCLUDED
table(gbif.df$taxonID %in% input$taxonID)/nrow(gbif.df)


saveRDS(input, "../data/input_tip_labels_new_sript.rds")
  
}



#### END ######

