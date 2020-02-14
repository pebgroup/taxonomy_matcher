
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


####################################################################
## extract tax info happens in common_format_creator_vectorized.R ##
####################################################################


bien_input <- readRDS("database/bien_input_vectorized2.rds")
    
## Cleanup
# remove usable == no
bien_input <- bien_input[-which(bien_input$usable=="no"),]


# RENAMING BIEN

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
wc_all$tax_comb[which(wc_all$tax_comb=="")] <- NA

# fix hybrid notation
wc_all$genus_hybrid <- as.character(wc_all$genus_hybrid)
empty_ranks <- which(wc_all$genus_hybrid=="")
wc_all$genus_hybrid[empty_ranks] <- NA

wc_all$species_hybrid <- as.character(wc_all$species_hybrid)
empty_ranks <- which(wc_all$species_hybrid=="")
wc_all$species_hybrid[empty_ranks] <- NA

wc_all$genus_hybrid[which(!is.na(wc_all$genus_hybrid))] <- "x"
wc_all$species_hybrid[which(!is.na(wc_all$species_hybrid))] <- "x"




## Some checks on completeness
# # no genus matches:
# View(bien_input[is.na(bien_input$genus),])
# # no species matches:
# View(bien_input[is.na(bien_input$species),])
# # no family matches:
# View(bien_input[is.na(bien_input$family),])
# # no authors
# View(bien_input[is.na(bien_input$author),])
# table(bien_input$split_length[is.na(bien_input$author)])
# table(bien_input$taxon_rank[is.na(bien_input$author)], useNA="ifany")

library(VIM)
aggr_plot <- aggr(bien_input[,c("family", "author", "genus", "species", "taxon_rank")],
                  col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(bien_input[,c("family", "author", "genus", "species", "taxon_rank")]),
                  cex.axis=.7, cex.numbers=0.7, gap=3, digits=4,
                  ylab=c("Histogram of missing data","Pattern"))









# MATCHING ##########################

## preparation
wc_all_sub <- wc_all[,c("tax_comb", "family", "genus", "genus_hybrid", "species",
                        "species_hybrid", "infraspecies", "taxon_authors", "accepted_plant_name_id")]
names(wc_all_sub) <- c("taxon_rank", "family", "genus", "genus_hybrid", "species", 
                        "species_hybrid", "infra_name", "author", "accepted_plant_name_id")
wc_all_sub[wc_all_sub==""]<-NA
wc_all_sub <- unique(wc_all_sub)

str(wc_all_sub)
str(bien_input)
wc_all_sub[,1:ncol(wc_all_sub)] <- lapply(wc_all_sub[,1:ncol(wc_all_sub)], as.character)
bien_input[,1:3] <- lapply(bien_input[,1:3], as.character)


# # exclude families from BIEN not represented in WCSP?
# fam_in_wcp <- unique(bien_input$family[bien_input$family %in% wc_all$family])
# bien_input <- bien_input[bien_input$family %in% fam_in_wcp,]

# exclude Bryophyta
bry <- read.csv("database/bryophyta.csv")
bryos <- as.character(bry[,1])
bryos <- gsub(" ", "", bryos)
bien_input <- bien_input[!bien_input$family %in% bryos,]



##### STRICT MATCHING, no author #####

res <- merge(bien_input, wc_all_sub, all.x=TRUE,
             by=c("taxon_rank", "family", "genus", "genus_hybrid",
                  "species", "species_hybrid", "infra_name"))

res$match_type <- NA
res <- unique(res)
res$match_type[which(!is.na(res$accepted_plant_name_id))] <- "strict match"
table(res$match_type, useNA = "ifany") / nrow(res)

## get species ID with multiple matches
multimatch_strict_id <- unique(res$id[which(duplicated(res$id))])
### remove those with NAs
res <- res[-which(res$id %in% multimatch_strict_id & is.na(res$match_type)),]
multimatch_strict_id <- unique(res$id[which(duplicated(res$id))])

done <- res[!res$id %in% multimatch_strict_id & !is.na(res$accepted_plant_name_id), c("id", "accepted_plant_name_id", "match_type")]

# setup for next level matching
no_match <- res[is.na(res$accepted_plant_name_id),]
mm <- res[res$id %in% multimatch_strict_id,]

# check if there are no IDs intersecting between dataframes
any(no_match$id %in% done$id)
any(mm$id %in% done$id)





###### MULTIMATCHES #####

# make author names easier:
## some authors names differ by spaces only: remove all spaces from author names
mm$author.x <- gsub(" ", "", mm$author.x)
mm$author.y <- gsub(" ", "", mm$author.y)

## assign matching authors status
mm$match_type[which(mm$author.x == mm$author.y)] <- "matching authors"
mm$match_type[which(mm$author.x != mm$author.y)] <- "no matching authors"
### those with one author missing, therefore no comparisson possible, remain "strict matches". however all of them are here because of duplicated, they should be irrelevant

# now loop through taxon IDs and check (ca 30 seconds)
IDs <- unique(mm$id)
one_match <- c()
more_match <- c()
no_match <- c()
for(i in 1:length(IDs)){
  temp <- mm[mm$id==IDs[i],]
  if(length(which(temp$match_type=="matching authors"))==1){
    one_match <- c(one_match, IDs[i])
  }
  if(length(which(temp$match_type=="matching authors"))>1){
    more_match <- c(more_match, IDs[i])
  }
  if(length(which(temp$match_type=="matching authors"))==0){
    no_match <- c(no_match, IDs[i])
  }
  if(!i%%100)cat(i,"\r")
}
# move one match ones to done df and remove from multimatches
temp <- mm[mm$id %in% one_match & mm$match_type=="matching authors",]
done <- rbind(done, temp[, c("id", "accepted_plant_name_id", "match_type")])

unresolved_mm <- mm[!mm$id %in% temp$id,]

length(unique(unresolved_mm$id))
par(mar = c(7, 4, 2, 1))
barplot(tail(sort(table(unresolved_mm$family)), 30),
        las=2, ylab="Taxa", cex.names = 0.8)
dev.off()




###### NO STRICT MATCHES #####

## no family, but authors
no_match <- res[is.na(res$accepted_plant_name_id),]
no_match$author <- no_match$author.x
no_match <- no_match[,!names(no_match) %in% c("author.x", "author.y", "accepted_plant_name_id")]


res2 <- merge(no_match, wc_all_sub, all.x=TRUE, 
              by=c("taxon_rank", "genus", "genus_hybrid",
                   "species", "species_hybrid", "infra_name", "author"))
res2$match_type <- NA
res2$match_type[which(!is.na(res2$accepted_plant_name_id))] <- "strict match no family"

## get species ID with multiple matches
multimatch_strict_id <- unique(res2$id[which(duplicated(res2$id))])
res2_sub <- res2[!res2$id %in% multimatch_strict_id,]
res2_sub <- res2_sub[!is.na(res2_sub$match_type),]

done <- rbind(done, res2_sub[, c("id", "accepted_plant_name_id", "match_type")])

# put multimatches in unresolved multimatches
res2_mm <- res2[res2$id %in% multimatch_strict_id,]
res2_mm <- res2_mm[!is.na(res2_mm$accepted_plant_name_id),]

# keep them in separate dataframes for now to save the contradicting family information




## infraspecific does not match, if infra == species name → match on species level
nms <- no_match[which(no_match$infra_name == no_match$species),]
## avoid doube assignment, exclude all taxa that have been matched via no family match
nms <- nms[!nms$id %in% done$id,]

res3 <- merge(nms, wc_all_sub, all.x=TRUE, 
              by=c("family", "genus", "genus_hybrid",
                   "species", "species_hybrid", "author"))
res3$match_type[which(!is.na(res3$accepted_plant_name_id))] <- "no infra (same same)"

## get species ID with multiple matches
multimatch_strict_id <- unique(res3$id[which(duplicated(res3$id))])
res3_sub <- res3[!res3$id %in% multimatch_strict_id,]
res3_sub <- res3_sub[!is.na(res3_sub$accepted_plant_name_id),]

done <- rbind(done, res3_sub[, c("id", "accepted_plant_name_id", "match_type")])

# put multimatches in unresolved multimatches
res3_mm <- res3[res3$id %in% multimatch_strict_id,]
res3_mm <- res3_mm[!is.na(res3_mm$accepted_plant_name_id),]

# keep them in separate dataframes for now to save the contradicting family information
## unresolved matches are following dataframes: unresolved_mm, res2_mm, res3_mm


any(duplicated(done$id))
# NA lines keep being introduced
done <- done[!is.na(done$match_type),]


######## SUMMARY ########
bien_output <- merge(bien_input, done, all.x=TRUE)

unsolved <- rbind(unresolved_mm[,c("id", "accepted_plant_name_id", "match_type")], res2_mm[,c("id", "accepted_plant_name_id", "match_type")])
unsolved <- rbind(unsolved, res3_mm[,c("id", "accepted_plant_name_id", "match_type")])
unsolved$match_type <- "multimatches"

any(unsolved$id %in% done$id)

bien_output$match_type[which(bien_output$id %in% unsolved$id)] <- "multimatch"

table(bien_output$match_type, useNA = "ifany")/nrow(bien_output)



#which species are unmatched?
table(bien_output$family[bien_output$match_type %in% c("multimatch")], useNA = "ifany")















     
     
# TRASH

# 
# m_authors <- which(mm$author.x == mm$author.y)
# mm$match_type[m_authors] <- "matching authors"
# matching_authors <- mm[mm$match_type=="matching authors",]
# 
# any(duplicated(matching_authors$id)) # careful, more multmatches ahead...
# multi_IDs <- matching_authors$id[which(duplicated(matching_authors$id))]
# 
# ## store single matching author and remove them from mm to the done df
# done <- rbind(done, matching_authors[!matching_authors$id %in% multi_IDs, 
#                                      c("id", "accepted_plant_name_id", "match_type")])
# 
# ok we want to keep multi_IDs and no matches
# matching_authors[matching_authors$id %in% multi_IDs]
# # the rest
# 
# mm <- mm[-which(!mm$id %in% matching_authors[!matching_authors$id %in% multi_IDs),]
#                 
#                 
                
                # multimatches take 2: modify author names
                ## some authors names differ by spaces only: remove all spaces from author names
                # mm$author_simp <- gsub(" ", "", mm$author.x)
                # mm$author_simp.y <- gsub(" ", "", mm$author.y)
                # 
                # m_authors <- which(mm$author_simp == mm$author_simp.y)
                # mm$match_type[m_authors] <- "matching authors_simp"
                # m_author_id <- mm$id[m_authors]
                # 
                # ## take those who have a matching author and remove them from mm to the done df
                # done <- rbind(done, mm[m_authors, c("id", "accepted_plant_name_id", "match_type")])
                # mm <- mm[-which(mm$id %in% m_author_id),]





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
