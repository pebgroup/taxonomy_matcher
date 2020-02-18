
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
unique(wc_all[,c("taxon_rank", "infraspecific_rank")])
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
aggr_plot <- aggr(bien_input[,c("family", "author", "genus", "species", 
                                "taxon_rank", "infra_name", "genus_hybrid", "species_hybrid")],
                  col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  cex.axis=.7, cex.numbers=0.7, gap=3, digits=4,
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot <- aggr(bien_input[,c("family", "author", "genus", "species")],
                  col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE,
                  cex.axis=.7, cex.numbers=0.7, gap=3, digits=4,
                  ylab=c("Histogram of missing data","Pattern"))








# MATCHING ##########################

## data preparation
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


# exclude Bryophyta
bry <- read.csv("database/bryophyta.csv")
bryos <- as.character(bry[,1])
bryos <- gsub(" ", "", bryos)
bien_input <- bien_input[!bien_input$family %in% bryos,]

# exclude taxa without extracted genus
bien_input <- bien_input[-which(is.na(bien_input$genus)),]





##### STRICT MATCHING, no author #####
  
res <- merge(bien_input, wc_all_sub, all.x=TRUE,
             by=c("taxon_rank", "family", "genus", "genus_hybrid",
                  "species", "species_hybrid", "infra_name"))
nrow(res)-nrow(bien_input)

# store initial matching for later
temp <- res
# remove author names for easier handling
res <- res[ , -which(names(res) %in% c("author.x", "author.y"))]

res  <- unique(res)
res$match_type <- NA
res$match_type[which(!is.na(res$accepted_plant_name_id))] <- "strict match"
table(res$match_type, useNA = "ifany") / nrow(res)

## get species ID with multiple matches
multimatch_strict_id <- unique(res$id[which(duplicated(res$id))])

### remove those with NAs
res <- res[-which(res$id %in% multimatch_strict_id & is.na(res$match_type)),]
multimatch_strict_id <- unique(res$id[which(duplicated(res$id))])

done <- res[!res$id %in% multimatch_strict_id & !is.na(res$accepted_plant_name_id), c("id", "accepted_plant_name_id", "match_type")]

# setup for next level matching
mm <- temp[temp$id %in% multimatch_strict_id,]
mm <- mm[-which(is.na(mm$accepted_plant_name_id)),]

no_match <- temp[-which(temp$id %in% mm$id),]
no_match <- no_match[-which(no_match$id %in% done$id),]

# check if there are no IDs intersecting between dataframes
any(no_match$id %in% done$id)
any(mm$id %in% done$id)
any(mm$id %in% no_match$id)

# all IDs still represented?
all(unique(c(mm$id, no_match$id, done$id)) %in% bien_input$id)



###### MULTIMATCHES #####

# simplify author names: remove all spaces from author names
mm$author.x <- gsub(" ", "", mm$author.x)
mm$author.y <- gsub(" ", "", mm$author.y)

## assign matching authors status
mm$match_type[which(mm$author.x == mm$author.y)] <- "matching authors"
mm$match_type[which(mm$author.x != mm$author.y)] <- "no matching authors"
# cases for which one author missing, therefore no comparisson possible, are NAs

# now loop through taxon IDs and check (ca 30 seconds)
IDs <- unique(mm$id)
one_match <- c()
more_match <- c()
zero_match <- c()
for(i in 1:length(IDs)){
  temp <- mm[mm$id==IDs[i],]
  if(length(which(temp$match_type=="matching authors"))==1){
    one_match <- c(one_match, IDs[i])
  }
  if(length(which(temp$match_type=="matching authors"))>1){
    more_match <- c(more_match, IDs[i])
  }
  if(length(which(temp$match_type=="matching authors"))==0){
    zero_match <- c(zero_match, IDs[i])
  }
  if(!i%%100)cat(i,"\r")
}
# move one match ones to done df and remove from multimatches
temp <- mm[mm$id %in% one_match & mm$match_type=="matching authors",]
done <- rbind(done, temp[, c("id", "accepted_plant_name_id", "match_type")])

unresolved_mm <- mm[!mm$id %in% temp$id,]
wcp_conflicts <- unresolved_mm[unresolved_mm$id %in% more_match,]
length(more_match)

length(unique(unresolved_mm$id))
par(mar = c(7, 4, 2, 1))
barplot(tail(sort(table(unresolved_mm$family)), 30),
        las=2, ylab="Taxa", cex.names = 0.8)
barplot(tail(sort(table(wcp_conflicts$family)), 40),
        las=2, ylab="Taxa", cex.names = 0.8)
dev.off()




###### NO STRICT MATCHES #####

## no family, but authors
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
res3$match_type <- NA
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

table(bien_output$taxon_rank[is.na(bien_output$match_type)])















