
############################
# Taxon matching BIEN WCSP #
############################
  library(rgdal)
  library(tidyverse)
  library(BIEN)
  
# Get data

# ## BIEN
# #  Switch to chose data volume, based on directory (server or local)
#   if(getwd()=="/data_vol/melanie/BIEN_download"){
#     bien <- read.csv(file="all_bien_occurrences_7cols_rm_na.csv")
#   }else{
#     chunk_size <- 1000000 # choose the best size for you
#     bien <- read.csv(file="database/all_bien_occurrences_7cols_rm_na.csv",nrows=chunk_size)
#   }
# 
# #bien <- readRDS("all_bien_occurrences_6cols.rds") # takes 9.95 GB of memory: object.size(bien)
# 
# # clean data
# bien <- bien[!abs(bien$latitude)>90,] # remove species with impossible coordinates
# bien <- bien[!abs(bien$longitude)>180,] # remove species with impossible coordinates
# 

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

#head(wc_all)
#table(wc_all$taxon_rank, useNA="ifany")
#View(wc_all[wc_all$infraspecific_rank=="nothosubsp.",])
#View(unique(wc_all[,c("infraspecific_rank", "taxon_rank")]))


####################################################################
## extract tax info happens in common_format_creator_vectorized.R ##
####################################################################


bien_input <- readRDS("database/bien_input_vectorized2.rds")

## Cleanup
# remove usable == no
bien_input <- bien_input[-which(bien_input$usable=="no"),]


# RENAMING BIEN
table(bien_input$taxon_rank)

# straighten formats
bien_input$taxon_rank[which(bien_input$taxon_rank=="[unranked]")] <- "unranked"
bien_input$taxon_rank <- gsub(".", "", bien_input$taxon_rank, fixed=TRUE)

# rename
bien_input$taxon_rank <- gsub("cv", "convar", bien_input$taxon_rank, fixed=TRUE)
bien_input$taxon_rank <- gsub("fo", "f", bien_input$taxon_rank, fixed=TRUE)
bien_input$taxon_rank <- gsub("ssp", "subsp", bien_input$taxon_rank, fixed=TRUE)
bien_input$taxon_rank <- gsub("unranked", "undefined", bien_input$taxon_rank, fixed=TRUE)
bien_input$taxon_rank[which(is.na(bien_input$taxon_rank))] <- "undefined"


# RENAMING WCSP
View(unique(wc_all[,c("taxon_rank", "infraspecific_rank")]))
wc_all$tax_comb <- wc_all$infraspecific_rank
wc_all$tax_comb <- gsub(".", "", wc_all$tax_comb, fixed=TRUE)
wc_all$tax_comb <- gsub(",", "", wc_all$tax_comb, fixed=TRUE)

empty_ranks <- which(wc_all$tax_comb=="")
wc_all$tax_comb[empty_ranks] <- as.character(wc_all$taxon_rank[empty_ranks])
wc_all$tax_comb <- gsub("Species", "species", wc_all$tax_comb)
#wc_all$tax_comb[which(wc_all$tax_comb=="")] <- NA

sort(table(wc_all$tax_comb), useNA="ifany")
sort(table(bien_input$taxon_rank), useNA="ifany")

cbind(unique(bien_input$taxon_rank), unique(bien_input$taxon_rank) %in% wc_all$tax_comb)




## Stuff that needs to be there
# no genus matches:
View(bien_input[is.na(bien_input$genus),])
# no species matches:
View(bien_input[is.na(bien_input$species),])
# no family matches:
View(bien_input[is.na(bien_input$family),])
# no authors
View(bien_input[is.na(bien_input$author),])
table(bien_input$split_length[is.na(bien_input$author)])
table(bien_input$taxon_rank[is.na(bien_input$author)], useNA="ifany")

library(VIM)
aggr_plot <- aggr(bien_input[,c("family", "author", "genus", "species", "taxon_rank")],
                  col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(bien_input[,c("family", "author", "genus", "species", "taxon_rank")]),
                  cex.axis=.7, cex.numbers=0.7, gap=3, digits=4,
                  ylab=c("Histogram of missing data","Pattern"))




# MATCH #
wc_all_sub <- wc_all[,c("tax_comb", "family", "genus", "genus_hybrid", "species",
                        "species_hybrid", "infraspecies", "taxon_authors", "accepted_plant_name_id")]
names(wc_all_sub) <- c("taxon_rank", "family", "genus", "genus_hybrid", "species", 
                        "species_hybrid", "infra_name", "author", "accepted_plant_name_id")
wc_all_sub[wc_all_sub==""]<-NA
wc_all_sub <- unique(wc_all_sub)

str(wc_all_sub)
str(bien_input)

wc_all_sub[,1:ncol(wc_all_sub)] <- lapply(wc_all_sub[,1:ncol(wc_all_sub)], as.character)
bien_input[,1:2] <- lapply(bien_input[,1:2], as.character)

# strict match 

res <- merge(bien_input, wc_all_sub, all.x=TRUE,
             by=c("taxon_rank", "family", "genus", "genus_hybrid",
                  "species", "species_hybrid", "infra_name", "author"))
res$match_type <- NA
res <- unique(res)
res$match_type[which(!is.na(res$accepted_plant_name_id))] <- "strict match"
table(res$match_type, useNA = "ifany") / nrow(res)

multimatch_strict_id <- res$id[which(duplicated(res$id))]
View(res[res$id %in% multimatch_strict_id,])
table(res[res$id %in% multimatch_strict_id,"family"])

## remove the NAs in matched
res <- res[-which(res$id %in% multimatch_strict_id & is.na(res$match_type)),]
sort(table(res[res$id %in% multimatch_strict_id,"family"]))



# strict match without author name

res2 <- merge(bien_input, wc_all_sub, all.x=TRUE, 
             by=c("taxon_rank", "family", "genus", "genus_hybrid",
                  "species", "species_hybrid", "infra_name"))
res2 <- unique(res2)
res2$match_type <- NA
res2$match_type[which(!is.na(res2$accepted_plant_name_id))] <- "no_author_match"
table(res2$match_type, useNA = "ifany") / nrow(res2)
multimatch_strict_id_no_author <- res$id[which(duplicated(res$id))]
## remove the NAs in matched
res2 <- res2[-which(res2$id %in% multimatch_strict_id_no_author & is.na(res2$match_type)),]


## merging different match types into one DF

res3 <- merge(res[,c("id", "accepted_plant_name_id", "match_type")], 
              res2[,c("id", "accepted_plant_name_id", "match_type")],
              all.x = TRUE, by="id")
res3$accepted_plant_name_id.x[which(is.na(res3$accepted_plant_name_id.x))] <- res3$accepted_plant_name_id.y[which(is.na(res3$accepted_plant_name_id.x))]
res3$match_type.x[which(is.na(res3$match_type.x))] <- res3$match_type.y[which(is.na(res3$match_type.x))]

res3 <- res3[,c("id", "accepted_plant_name_id.x", "match_type.x")]
names(res3) <- c("id", "accepted_id", "match_type")
res3 <- unique(res3)

View(res3)
table(is.na(res3$accepted_id))/nrow(res3)
which(duplicated(res3$id))


# strict match without taxon rank and author
## uncludes undefinded taxon rank
temp <- res3[is.na(res3$accepted_id),]

res4 <- merge(bien_input[bien_input$id %in% temp$id,], wc_all_sub, all.x=TRUE, 
              by=c("family", "genus", "genus_hybrid",
                   "species", "species_hybrid", "infra_name"))
res4 <- unique(res4[,c("id", "accepted_plant_name_id")])
res4$match_type <- NA
res4$match_type[which(!is.na(res4$accepted_plant_name_id))] <- "no_author_no_tax_rank"
table(is.na(res4$accepted_plant_name_id))

names(res4) <- c("id", "accepted_id", "match_type")
res4 <- unique(res4)

fin <- rbind(res3[!is.na(res3$accepted_id),], res4)
table(fin$match_type, useNA = "ifany") / nrow(fin)


# What are the 17% that are hard to match? Moss and stuff?
bien_input_fin <- merge(bien_input, fin, all.x=TRUE, by="id")

par(mar = c(7, 4, 2, 1))
barplot(tail(sort(table(bien_input_fin$family[is.na(bien_input_fin$match_type)])), 30),
        las=2, ylab="Taxa", cex.names = 0.8)
dev.off()



# TRASH
# # Tax compare
# ## step 1: accepted name matching
# table(as.character(wc_all$accepted_plant_name_id)==as.character(wc_all$plant_name_id))
# 
# 
# length(unique(wc_all$taxon_name[wc_all$taxon_status=="Accepted"]))
# 
# # get accepted name: get taxon name from accepted IDs attached to the df
# # wc_all$accepted_name <- merge(wc_all, wc_all[,c("accepted_plant_name_id", "taxon_name")],
# #                               by.x="plant_name_id", by.y="accepted_plant_name_id", all.x=TRUE)
# test <- merge(wc_all, wc_all[,c("accepted_plant_name_id", "taxon_name")],
#               by.x="plant_name_id", by.y="accepted_plant_name_id", all.x=TRUE)
# ## does not work right now....
# 
# 
# 
# 
# table(as.character(wc_all$primary_author) == as.character(wc_all$taxon_authors))
# 
# bien$match <- NA
# bien$match[which(bien$scrubbed_species_binomial %in% wc_all$accepted_name)] <- "level1_match"
# 
# ## step 2: synonym matching
# wc_all$binomial <- paste(wc_all$genus, wc_all$species)
# #bien$match[which(bien$species[is.na(bien$match)] %in% wc_all$binomial)] <- "level2_match"
# bien$match[intersect(which(is.na(bien$match)), which(bien$species %in% wc_all$binomial))] <- "level2_match"
# # table(bien$match, useNA="ifany")
# 
# ### get those with unique matching in WCSP only
# bien_sub <- bien[bien$match=="level2_match",]
# bien_sub$accepted_wcsp <- NA
# level2_species <- unique(bien_sub$species)
# j <- 0
# for(i in level2_species){
#   temp <- wc_all[wc_all$binomial==i,]
#   
#   # test accepted names length in wcsp
#   if(length(unique(temp$accepted_name))==1){
#     bien_sub$accepted_wcsp[bien_sub$species==i] <- unique(temp$accepted_name)
#   }else{
#     
#     #bien_sub$accepted_wcsp[bien_sub$species==i] <- "level3_match" # kind of unneccessary to treat them separately, throw them back in the yet unmatched pool ?
#   }
#   j <- j+1
#   if(!j%%10)cat(j,"\r")
# }
# 
# ### merge them back into the complete list
# bien <- merge(bien, bien_sub[,c("taxonobservation_id", "accepted_wcsp")], by="taxonobservation_id", all.x=TRUE)
# rm(bien_sub)
# 
# 
# ## step 3: test for all remaining using the infraspecific information
# ### GET MORE BIEN INFO: Additional BIEN download data
# # not all NA match species are in the additional download. this is just for multimatch species, not for species that show no match at all. do i need this as well?
# 
# # read in additional infraspecific info for species that correspond to binomial multimatching species
# # files <- dir()
# # files <- files[grepl("[0-9].rds", files)]
# # df <- files %>%
# #   map_dfr(readRDS)
# 
# 
# # check species names: get missing taxon IDs
# missing1 <- unique(bien$species[which(is.na(bien$match))]) # NAs
# missing2 <- bien$species[which(bien$match=="level2_match" & is.na(bien$accepted_wcsp))]
# 
# missing_bi <- c(as.character(missing1), as.character(missing2))
# 
# # add more BIEN tax info
# BIEN_occurrence_species_mod <- function (species, only.new.world = FALSE, ...) 
# {
#   .is_log(only.new.world)
#   .is_char(species)
#   newworld_ <- .newworld_check(only.new.world)
#   query <- paste("SELECT scrubbed_species_binomial,scrubbed_taxon_name_no_author,scrubbed_author,taxonobservation_id,latitude, longitude",
#                  "\n                 FROM view_full_occurrence_individual \n                 WHERE scrubbed_species_binomial in (", 
#                  paste(shQuote(species, type = "sh"), collapse = ", "), 
#                  ")",  
#                  "\n                 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) \n                 ORDER BY scrubbed_species_binomial ;")
#   return(.BIEN_sql(query, ...))
# }
# assignInNamespace('BIEN_occurrence_species',BIEN_occurrence_species_mod,ns='BIEN')
# environment(BIEN_occurrence_species_mod)<-asNamespace('BIEN') 
# 
# missing_bien <- BIEN_occurrence_species_mod(missing_bi, only.new.world = FALSE)
# 
# bien <- merge(bien, missing_bien[,c("taxonobservation_id", "scrubbed_taxon_name_no_author", "scrubbed_author")], all.x=TRUE)
# 
# 
# bien_sub <- bien[is.na(bien$accepted_wcsp) & bien$match=="level2_match",]
# 
# #
# 
# ## create wcsp taxon name
# # scrubbed_taxon_name_no_author matches WCSP taxon name (create new variable for that from Genus, infra rank + epiphet, species)
# wc_all$taxon_name_no_author <- paste(wc_all$genus, wc_all$species, wc_all$infraspecific_rank, wc_all$infraspecific_epithet)
# wc_all$taxon_name_no_author <- str_trim(wc_all$taxon_name_no_author) # strip spaces from strings, super important
# 
# multimatch_species <- unique(bien_sub$scrubbed_taxon_name_no_author[which(bien_sub$scrubbed_taxon_name_no_author %in% wc_all$taxon_name_no_author)])
# j <- 0
# for(i in multimatch_species){
#   temp <- wc_all[wc_all$taxon_name_no_author==i,]
#   
#   # test accepted names length in wcsp
#   if(length(unique(temp$accepted_name))==1){
#     bien_sub$accepted_wcsp[bien_sub$species==i] <- unique(temp$accepted_name)
#   }
#   j <- j+1
#   if(!j%%10)cat(j,"\r")
# }
# 
# ### merge them back into the complete list
# bien <- merge(bien, bien_sub[,c("taxonobservation_id", "accepted_wcsp")], by="taxonobservation_id", all.x=TRUE)
# rm(bien_sub)
# 
# 
# ## CHECK  
# table(is.na(bien$accepted_wcsp.x) == is.na(bien$accepted_wcsp.y))
# # Check taxonomy for those still missing: is it all mosses and lichen? Then we are good
# 
# 
# 
# 
# 



# 
# # unique the species - time to subset
# ## create a unique taxon ID that contains all possible information:
# bien$taxon_author_ID <- paste(bien$scrubbed_taxon_name_no_author, bien$scrubbed_author)
# bien_sub <- bien[!duplicated(bien[,"taxon_author_ID"]),]
# 
# # set up common format
# split_length <- unlist(lapply(strsplit(as.character(bien_sub$scrubbed_taxon_name_no_author), split = " "), length))
# bien_input <- data.frame(scrubbed_taxon_name_no_author=bien_sub$scrubbed_taxon_name_no_author,
#                          family = bien_sub$scrubbed_family,
#                          author=bien_sub$scrubbed_author, 
#                          split_length=split_length,
#                          genus_hybrid = rep(NA, nrow(bien_sub)),
#                          species_hybrid = rep(NA, nrow(bien_sub)),
#                          genus = rep(NA, nrow(bien_sub)),
#                          species = rep(NA, nrow(bien_sub)),
#                          taxon_rank = rep(NA, nrow(bien_sub)),
#                          infra_name = rep(NA, nrow(bien_sub)),
#                          comment = rep(NA, nrow(bien_sub))
# )
# 
# # order the dataframe by split length
# bien_input <- bien_input[order(bien_input$split_length),]
# bien_input$id <- c(1:nrow(bien_input))
# 
# split_list <- strsplit(as.character(bien_input$scrubbed_taxon_name_no_author), split = " ")
# names(split_list) <- bien_input$id
# 
# 
# # Vectorize + loop mix
# 
# ## split length == 1
# ind <- which(bien_input$split_length==1)
# bien_input$genus[ind] <- as.character(bien_input$scrubbed_taxon_name_no_author[ind])
# 
# ## split length == 2
# ind <- which(bien_input$split_length==2)
# bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
# bien_input$species[ind] <- sapply(split_list[ind], "[[", 2)
# 
# ## split length == 3
# ind <- which(bien_input$split_length==3)
# for(i in 1:length(ind)){
#   if(split_list[[ind[i]]][1]=="x"){
#     bien_input$genus_hybrid[ind[i]] <- "x"   # ???
#     bien_input$genus[ind[i]] <- split_list[[ind[i]]][2]
#     bien_input$species[ind[i]] <- split_list[[ind[i]]][3]
#   }
#   if(split_list[[ind[i]]][2]=="x"){
#     if(split_list[[ind[i]]][2]=="x" & grepl("[A-Z]",split_list[[ind[i]]][3])){
#       bien_input$genus_hybrid[i] <- "x"
#       # WHICH GENUS TO RECORD??
#     }else{
#       bien_input$species_hybrid[ind[i]] <- "x"
#       bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
#       bien_input$species[ind[i]] <- split_list[[ind[i]]][[3]]
#     }
#   }
#   if(!any(grepl("x", split_list[[ind[i]]]))){bien_input$comment[ind[i]] <- "no hybrid"}
# }
# 
# ## split length == 4
# ind <- which(bien_input$split_length==4)
# for(i in 1:length(ind)){
#   if(split_list[[ind[i]]][[1]]=="x" & split_list[[ind[i]]][[3]]=="x"){
#     bien_input$genus_hybrid[ind[i]] <- "x"
#     bien_input$species_hybrid[ind[i]] <- "x"
#     bien_input$genus[ind[i]] <- split_list[[ind[i]]][[2]]
#     bien_input$species[ind[i]] <- split_list[[ind[i]]][[4]]
#   }
#   if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]=="x"){
#     ## WHAT TO RECORD HERE??? ###
#     bien_input$species_hybrid[ind[i]] <- "x"
#     bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
#     bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
#     bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
#   }
#   if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]!="x"){
#     bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
#     bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
#     bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][[3]]
#     bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
#   }
#   if(!i%%100)cat(i,"\r")
# }
# 
# ## split length == 5
# ind <- which(bien_input$split_length==5)
# for(i in 1:length(ind)){
#   if(split_list[[ind[i]]][2]=="x"){
#     bien_input$genus[ind[i]] <- split_list[[ind[i]]][1]
#     bien_input$species_hybrid[ind[i]] <- split_list[[ind[i]]][2]
#     bien_input$species[ind[i]] <- split_list[[ind[i]]][3]
#     bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][4]
#     bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][5]
#   }
#   if(split_list[[ind[i]]][4]=="x"){
#     bien_input$genus[ind[i]] <- split_list[[ind[i]]][1]
#     bien_input$species_hybrid[ind[i]] <- split_list[[ind[i]]][4]
#     bien_input$species[ind[i]] <- split_list[[ind[i]]][2]
#     bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][3]
#     bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][5]
#   }
# }
# 
# ## split length == 6 
# ind <- which(bien_input$split_length==6)
# bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
# bien_input$species_hybrid[ind] <- sapply(split_list[ind], "[[", 2)
# bien_input$species[ind] <- sapply(split_list[ind], "[[", 3)
# bien_input$taxon_rank[ind] <- sapply(split_list[ind], "[[", 4)
# bien_input$infra_name[ind] <- sapply(split_list[ind], "[[", 5)
# 

# for(i in 1:nrow(bien_input)){
#   
#   # split length == 2
#   if(bien_input$split_length[i]==2){
#     # if(split_list[[i]][1]=="x"){
#     #   bien_input$genus_hybrid[i] <- "x"
#     #   bien_input$genus[i] <- split_list[[i]][[2]]
#     # }else{
#       bien_input$genus[i] <- split_list[[i]][[1]]
#       bien_input$species[i] <- split_list[[i]][[2]]
#     #}
#     
#   }
#   
#   # split length == 3
#   if(bien_input$split_length[i]==3){
#     if(split_list[[i]][1]=="x"){
#       bien_input$genus_hybrid[i] <- "x"   # ???
#       bien_input$genus[i] <- split_list[[i]][[2]]
#       bien_input$species[i] <- split_list[[i]][[3]]
#     }
#     if(split_list[[i]][2]=="x"){
#       if(split_list[[i]][2]=="x" & grepl("[A-Z]",split_list[[i]][3])){
#         bien_input$genus_hybrid[i] <- "x"
#         # WHICH GENUS TO RECORD??
#       }else{
#       bien_input$species_hybrid[i] <- "x"
#       bien_input$genus[i] <- split_list[[i]][[1]]
#       bien_input$species[i] <- split_list[[i]][[3]]
#       }
#     }
#     if(!any(grepl("x", split_list[[i]]))){bien_input$comment[i] <- "no hybrid"}
#   }
#   
#   #split length == 4
#   if(bien_input$split_length[i]==4){
#     if(split_list[[i]][[1]]=="x" & split_list[[i]][[3]]=="x"){
#       bien_input$genus_hybrid[i] <- "x"
#       bien_input$species_hybrid[i] <- "x"
#       bien_input$genus[i] <- split_list[[i]][[2]]
#       bien_input$species[i] <- split_list[[i]][[4]]
#     }else{
#       bien_input$genus[i] <- split_list[[i]][[1]]
#       bien_input$species[i] <- split_list[[i]][[2]]
#       bien_input$taxon_rank[i] <- split_list[[i]][[3]]
#       bien_input$infra_name[i] <- split_list[[i]][[4]]
#     }
#   }
#   
#   #split length == 6
#   if(bien_input$split_length[i]==6){bien_input$comment[i] <- "weirdo"}
#   
#   if(!i%%100)cat(i,"\r")    
#   
# }
