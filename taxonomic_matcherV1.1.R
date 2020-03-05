############################
# Taxon matching taxonomy to WCSP #
############################
rm(list=ls())

library(tidyverse)


# read in world checklist data  

wc_all <- readRDS("./data/wcp_dec_19.rds")

wc_all <- data.frame(wc_all, stringsAsFactors=FALSE)
#grub and clean the some columns in the world checklist data

# RENAMING WCSP
# unique(wc_all[,c("taxon_rank", "infraspecific_rank")])
#add in new column as "tax_comb" to unify all the format of the infraspecific ranks
wc_all$tax_comb <- wc_all$infraspecific_rank
wc_all$tax_comb <- gsub("\\.|,", "", wc_all$tax_comb)
# wc_all$tax_comb <- gsub(",", "", wc_all$tax_comb, fixed=TRUE)

# empty_ranks <- which(wc_all$tax_comb=="")
# wc_all$tax_comb[empty_ranks] <- as.character(wc_all$taxon_rank[empty_ranks])
wc_all$tax_comb <- gsub("Species", "species", wc_all$tax_comb)
wc_all$tax_comb <- gsub("Genus", "genus", wc_all$tax_comb)
# wc_all$tax_comb[which(wc_all$tax_comb=="")] <- NA

wc_all[wc_all==""]<-NA

# fix hybrid notation
wc_all$genus_hybrid <- as.character(wc_all$genus_hybrid)
# empty_ranks <- which(wc_all$genus_hybrid=="")
# wc_all$genus_hybrid[empty_ranks] <- NA
# 
wc_all$species_hybrid <- as.character(wc_all$species_hybrid)
# empty_ranks <- which(wc_all$species_hybrid=="")
# wc_all$species_hybrid[empty_ranks] <- NA
wc_all$genus_hybrid[!is.na(wc_all$genus_hybrid)] <- "x"
wc_all$species_hybrid[!is.na(wc_all$species_hybrid)] <- "x"


#subset the dataset with key columns
wc_all_sub <- wc_all %>% select(tax_comb, taxon_status, family, genus, genus_hybrid, species, 
                                species_hybrid, infraspecies, taxon_authors, accepted_plant_name_id) %>% unique()

# wc_all_sub <- unique(wc_all_sub)
names(wc_all_sub)[names(wc_all_sub) %in% c("tax_comb", "infraspecies", "taxon_authors")] <- c("taxon_rank", "infra_name", "author")

# str(wc_all_sub)
# str(bien_input)
wc_all_sub[,1:ncol(wc_all_sub)] <- lapply(wc_all_sub[,1:ncol(wc_all_sub)], as.character)
# saveRDS(wc_all_sub, file="./results/wc_all_sub.rds")
wc_all_sub <- readRDS("./results/wc_all_sub.rds")

#### define  the dataset want to reconcile with WCSP ####

#### NCBI ####

DB.name <- "NCBI"

if(DB.name=="NCBI"){
  
  dataset <- read.csv(paste0("./data/",DB.name, ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
  
  #cleaning
  dataset$infraspecific_rank <- gsub("\\.|,", "", dataset$infraspecific_rank)
  dataset[dataset==""]<-NA
  dataset$genus_hybrid[!is.na(dataset$genus_hybrid)] <- "x"
  dataset$species_hybrid[!is.na(dataset$species_hybrid)] <- "x"
  
  #reorder, maybe not neccesary
  dataset <- dataset %>% select(infraspecific_rank, order, family, genus, genus_hybrid, species, species_hybrid, infraspecies, taxon_authority, taxon_rank, ncbi_id) %>% unique()
  names(dataset)[names(dataset) %in% c("infraspecific_rank", "infraspecies", "taxon_authority", "taxon_rank", "ncbi_id")] <- c("taxon_rank", "infra_name", "author", "Trank.ncbi.full", "id")
  dataset$author <- gsub("_", " ", dataset$author)
  
  dataset[,1:ncol(dataset)] <- lapply(dataset[,1:ncol(dataset)], as.character)
}

#### BIEN ####
#reformat for BIEN dataset

if(dataset="BIEN"){
  
  library(rgdal)
  library(BIEN)
  library(VIM)
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
  
  # bien_input[,1:3] <- lapply(bien_input[,1:3], as.character)
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
  # simplify author names: remove all dots from author names
  
  
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
  saveRDS(wcp_conflicts, file="wcp_conflicts.rds")
  length(more_match)
  
  length(unique(unresolved_mm$id))
  par(mar = c(7, 4, 2, 1))
  barplot(tail(sort(table(unresolved_mm$family)), 30),
          las=2, ylab="Taxa", cex.names = 0.8)
  barplot(tail(sort(table(wcp_conflicts$family)), 40),
          las=2, ylab="Taxa", cex.names = 0.8)
  dev.off()
  
  View(wc_all[wc_all$plant_name_id %in% wcp_conflicts$accepted_plant_name_id,])
  
  
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
  fin <- merge(bien_input, done, all.x=TRUE)
  
  unsolved <- rbind(unresolved_mm[,c("id", "accepted_plant_name_id", "match_type")], res2_mm[,c("id", "accepted_plant_name_id", "match_type")])
  unsolved <- rbind(unsolved, res3_mm[,c("id", "accepted_plant_name_id", "match_type")])
  unsolved$match_type <- "multimatches"
  
  any(unsolved$id %in% done$id)
  
  fin$match_type[which(fin$id %in% unsolved$id)] <- "multimatch"
  
  table(fin$match_type, useNA = "ifany")/nrow(fin)
  
  saveRDS(fin, file="fin.rds")
  
  #which species are unmatched?
  table(fin$family[fin$match_type %in% c("multimatch")], useNA = "ifany")
  
  table(fin$taxon_rank[is.na(fin$match_type)])
  
  # SPECIES LEVEL MATCHING
  ## subset wcsp to species occurring in BIEN 
  ids <- unique(fin$accepted_plant_name_id)
  wc_sub <- wc_all[wc_all$plant_name_id %in% ids,]
  
  ## subset to those with taxon rank < species
  table(wc_sub$tax_comb, useNA="ifany")
  spec_match <- wc_sub[!wc_sub$tax_comb %in% c("species", "Genus"),]
  
  wc_species_level <- wc_all[wc_all$tax_comb %in% c("species", "Genus") & wc_all$taxon_status=="Accepted",]
  
  wc_match <- merge(spec_match[,c("plant_name_id", "species", "genus")],
                    wc_species_level[,c("accepted_plant_name_id", "species", "genus")],
                    by=c("species", "genus"),
                    all.x=TRUE)
  
  nrow(wc_match)-nrow(spec_match)
  
  wc_match$elevated_to_species_id <- wc_match$accepted_plant_name_id
  
  ## get species ID with multiple matches
  multimatch_id <- unique(wc_match$plant_name_id[which(duplicated(wc_match$plant_name_id))])
  wc_mms <- wc_match[wc_match$plant_name_id %in% multimatch_id,]
  
  wc_match_sub <- wc_match[!wc_match$plant_name_id %in% multimatch_id,]
  wc_match_sub <- wc_match_sub[!is.na(wc_match_sub$accepted_plant_name_id),]
  
  #add to fin
  fin <- merge(fin, wc_match_sub[,c("plant_name_id", "elevated_to_species_id")],
               by.x="accepted_plant_name_id",
               by.y="plant_name_id", all.x=TRUE)
  
  table(is.na(fin$elevated_to_species_id), useNA="ifany")
}

#### Shared Functionality #############

##### STRICT MATCHING, no author #####
res <- left_join(dataset, wc_all_sub, all.x=TRUE,
              by=c("taxon_rank", "family", "genus", "genus_hybrid",
                   "species", "species_hybrid", "infra_name"))
# res.tmp <- inner_join(dataset, wc_all_sub, all.x=TRUE,
#                   by=c("taxon_rank", "family", "genus", "genus_hybrid",
#                        "species", "species_hybrid", "infra_name"))
res.tmp <- res %>% select(-author.x, -author.y) %>% unique() %>% mutate(match_type=ifelse(is.na(accepted_plant_name_id), NA, "strict match"))

#duplicates after removing author.x and author.y
# dupl.no.author <- res %>% select(-author.x, -author.y) %>% duplicated()
# res[dupl.no.author,]

no_match <- res.tmp %>% filter(is.na(accepted_plant_name_id))

match <- res.tmp %>% filter(!is.na(accepted_plant_name_id) & !is.na(match_type))

multimatch_strict_id <- unique(match$id[duplicated(match$id)])
done <- match %>% filter(!(id %in% multimatch_strict_id)) %>% select(id, accepted_plant_name_id, match_type)
multimatch <- res %>% filter(id %in% multimatch_strict_id) %>% arrange(id)

## check if there are no IDs intersecting between dataframes
if(any(no_match$id %in% done$id)){
  #nomatch, no accepted_plant_name_id, id also in done
  no_match_done <- no_match %>% filter(no_match$id %in% done$id)
  #clean no_match after remove no_match_done
  no_match <- no_match %>% filter(!(no_match$id %in% done$id))
}
if(any(multimatch$id %in% no_match$id)){
  
  #nomatch, no accepted_plant_name_id, id also in multimatch
  no_match_multimatch <- no_match %>% filter(no_match$id %in% multimatch$id)
  
  #clean no_match after remove no_match_done
  no_match <- no_match %>% filter(!(no_match$id %in% multimatch$id))
}

# last overall check
try(if(all(unique(c(multimatch$id, no_match$id, done$id)) %in% dataset$id)){
  print("all IDs still represented!")
}else{
  stop("NOT all IDs still represented!")
})


###### MULTIMATCHES #####
mm <- multimatch
# simplify author names: remove all spaces from author names
mm$author.x <- gsub(" |\\.|[0-9]*|,", "", mm$author.x)
mm$author.y <- gsub(" |\\.|[0-9]*|,", "", mm$author.y)

## assign matching authors status 

mm$match_type <- ifelse(mm$author.x == mm$author.y, "matching authors", "no matching authors")

### I realized ""matching authors"" as not as efficent to resue all the names, unless comnined with "taxon_status=="Accepted"
### since you can tell the name is corretly matched, even though mm$author.x != mm$author.y (e.g., id==1099790, 1288263)
### or run code below

# mm %>% filter(taxon_status=="Accepted" & mm$match_type!="matching authors")

# mm %>% filter(taxon_status!="Accepted" & mm$match_type=="matching authors")

#### multi_match_checker ####
#### define a function to check multiple match####
multi_match_checker <- function(mm_ids, subdata){
  one_match <- NULL
  more_match <- NULL
  zero_match <- NULL
  resolved_mm <- NULL
  un_resolved_mm <- NULL
  
  for (id in unique(mm_ids)){
    temp <- subdata[subdata$id==id,]
    if(sum(!is.na(temp$match_type)) ==1){
      one_match <- c(one_match, id)
      resolved_mm <- rbind(resolved_mm, temp[!is.na(temp$match_type),])
    }
    
    if(sum(!is.na(temp$match_type)) > 1){
      more_match <- c(more_match, id)
      if("Accepted"  %in% unique(temp$taxon_status)){
        resolved_mm <- rbind(resolved_mm, temp[!is.na(temp$match_type) & temp$taxon_status=="Accepted",])
      }
      else if ("Synonym" %in% unique(temp$taxon_status) & "matching authors" %in% unique(temp$match_type)){
        resolved_mm <- rbind(resolved_mm, temp[!is.na(temp$match_type) & temp$taxon_status=="Synonym",])
      }else{
        un_resolved_mm <- rbind(un_resolved_mm,temp)
      }
      
    }
    
    # making a ssuamption that multiple match, authors not match, but the taxon_status=="Accepted" should be the "good" one
    if(sum(!is.na(temp$match_type))==0){
      zero_match <- c(zero_match, id)
      resolved_mm <- rbind(resolved_mm, temp[temp$taxon_status=="Accepted",])
    }
    
  }
  
  results <- list(
    "one_match" = one_match,
    "more_match" = more_match,
    "zero_match" = zero_match,
    "resolved_mm" = resolved_mm,
    "un_resolved_mm"  =un_resolved_mm
  )
  return(results)
}

mm_results <- multi_match_checker(multimatch_strict_id, mm)
# unresolved_mm <- mm_results[[5]]
# unresolved_mm <- mm[!(mm$id %in% resolved_mm$id),]

wcp_conflicts1 <- mm_results$un_resolved_mm %>% filter(id %in% mm_results$more_match)

#output some summary reports

if(!dir.exists("results")){
  dir.creat("results")
  write.csv(wcp_conflicts1, "./results/wcp_conflicts1.csv", row.names = FALSE, quote=FALSE)
  # saveRDS(wcp_conflicts, file=paste0("./results/wcp_conflicts_", dataset, ".rds", sep=""))
  
  pdf(paste0("./results/wcp_unresolved_conflicts_", dataset, ".pdf", sep=""))
  par(mfrow=c(2,1), mar = c(7, 4, 2, 1))
  barplot(rev(tail(sort(table(unresolved_mm$family)), 30)),
          las=2, ylab="Taxa", cex.axis=0.65, cex.names = 0.8)
  title(main = "Famies with the most unresolved names")
  
  barplot(rev(tail(sort(table(wcp_conflicts$family)), 40)),
          las=2, ylab="Taxa", cex.names = 0.8)
  title(main = "Families with the most conflict names")
  
  dev.off()
  
}

# View(wc_all[wc_all$plant_name_id %in% wcp_conflicts$accepted_plant_name_id,])

#update done data set with resoved entries
done2 <- rbind(done, mm_results$resolved_mm %>% select(id, accepted_plant_name_id, match_type))

# done2 <- unique(done2)

###### NO STRICT MATCHES #####

## no family, but authors
no_match <- res %>% filter(res$id %in% no_match$id) %>% select(-author.y, -taxon_status, -accepted_plant_name_id) %>% unique()
names(no_match)[names(no_match) %in% "author.x"] <- "author"

# no_match.dupl <- no_match$id[res %>% filter(res$id %in% no_match$id) %>%select(id) %>% duplicated()]


res2 <- left_join(no_match, wc_all_sub, all.x=TRUE, 
              by=c("taxon_rank", "genus", "genus_hybrid",
                   "species", "species_hybrid", "infra_name", "author")) %>% mutate(match_type=ifelse(is.na(accepted_plant_name_id), NA, "strict match no family"))

## get species ID with multiple matches
multimatch_strict_id <- unique(res2$id[duplicated(res2$id)])

res2_sub <- res2 %>% filter(!(res2$id %in% multimatch_strict_id) & !is.na(match_type) & !is.na(match_type))

done3 <- rbind(done2, res2_sub %>% select(id, accepted_plant_name_id, match_type))

# generate multimatches dataset, altough differtent families
res2_mm <- res2 %>% filter(id %in% multimatch_strict_id & !is.na(accepted_plant_name_id))

# resulted by "multi_match_checker" function

mm_results2 <- multi_match_checker(multimatch_strict_id, res2_mm)

done4 <- rbind(done3, mm_results2$resolved_mm %>% select(id, accepted_plant_name_id, match_type))





####work on no_match ####

## infraspecific does not match, if infra == species name → match on species level
## avoid doube assignment, exclude all taxa that have been matched via no family match
nms <- no_match %>% filter(infra_name == species & !(id %in% done3$id))

res3 <- left_join(nms, wc_all_sub, all.x=TRUE, 
              by=c("family", "genus", "genus_hybrid",
                   "species", "species_hybrid", "author"))%>% mutate(match_type=ifelse(is.na(accepted_plant_name_id), NA, "no infra (same same)"))

## get species ID with multiple matches
multimatch_strict_id <- unique(res3$id[duplicated(res3$id)])
res3_sub <- res3 %>% filter(!(id %in% multimatch_strict_id) & !is.na(accepted_plant_name_id) & !is.na(match_type))

done5 <- rbind(done4, res3_sub %>% select(id, accepted_plant_name_id, match_type))

# generate multimatches dataset, altough differtent families
res3_mm <- res3 %>% filter(id %in% multimatch_strict_id & !is.na(accepted_plant_name_id))
# resulted by "multi_match_checker" function

mm_results3 <- multi_match_checker(multimatch_strict_id, res3_mm)

if(any(duplicated(mm_results3[[4]]$id))){
  dupl.id <- mm_results3[[4]]$id[duplicated(mm_results3[[4]]$id)]
  mm_results3[[4]] <- mm_results3[[4]] %>% filter(!(id %in% dupl.id))
  wcp_conflicts2 <- mm_results3[[4]] %>% filter(id %in% dupl.id)
  write.csv(wcp_conflicts2, "./results/wcp_conflicts2.csv", row.names = FALSE, quote=FALSE)
}

# update the done list with the largest appendix #
done6 <- rbind(done5, mm_results3[[4]] %>% select(id, accepted_plant_name_id, match_type))

#final check
done6 <- done6 %>% filter(!is.na(match_type) | !is.na(accepted_plant_name_id) | is.na(id)) %>% unique()
if(any(duplicated(done6$id))){
  dupl.id <- done6$id[duplicated(done6$id)]
  
  done6 <- done6 %>% filter(!(id %in% dupl.id))
}

write.csv(done6, paste0("./results/wcsp_",DB.name," _matching_list_done6.csv", sep=""), row.names = FALSE, quote=FALSE)

######## SUMMARY ########
fin <- left_join(dataset, done6, by="id", all.x=TRUE)

unsolved <- bind_rows(mm_results$un_resolved_mm, mm_results2$un_resolved_mm, mm_results3$un_resolved_mm)
saveRDS(unsolved, file="./results/unsolved.rds")

if(any(fin$id %in% unsolved$id)){
  fin$match_type[fin$id %in% unsolved$id] <- "multimatch"
}

table(fin$match_type, useNA = "ifany")/nrow(fin)

saveRDS(fin, file="./results/fin.rds")
  
#which species are unmatched?
table(fin$family[fin$match_type %in% c("multimatch")], useNA = "ifany")

table(fin$taxon_rank[is.na(fin$match_type)])


# 
# #### SPECIES LEVEL MATCHING ####
# ## subset wcsp to species occurring in database 
# ids <- fin %>% filter(!is.na(accepted_plant_name_id)) %>% select(accepted_plant_name_id) %>% unique()
# wc_sub_fin <- wc_all %>% filter(plant_name_id %in% ids$accepted_plant_name_id)
# 
# ## subset to those with taxon rank < species
# table(wc_sub_fin$tax_comb, useNA="ifany")
# spec_match <- wc_sub_fin %>% filter(taxon_rank=="Species" & is.na(tax_comb) & taxon_status=="Accepted")
# 
# 
# wc_species_level <- wc_all[wc_all$tax_comb %in% c("species", "Genus") & wc_all$taxon_status=="Accepted",]
# 
# wc_match <- merge(spec_match[,c("plant_name_id", "species", "genus")],
#                   wc_species_level[,c("accepted_plant_name_id", "species", "genus")],
#                   by=c("species", "genus"),
#                   all.x=TRUE)
# 
# nrow(wc_match)-nrow(spec_match)
# 
# wc_match$elevated_to_species_id <- wc_match$accepted_plant_name_id
# 
# ## get species ID with multiple matches
# multimatch_id <- unique(wc_match$plant_name_id[which(duplicated(wc_match$plant_name_id))])
# wc_mms <- wc_match[wc_match$plant_name_id %in% multimatch_id,]
# 
# wc_match_sub <- wc_match[!wc_match$plant_name_id %in% multimatch_id,]
# wc_match_sub <- wc_match_sub[!is.na(wc_match_sub$accepted_plant_name_id),]
# 
# #add to fin
# fin <- merge(fin, wc_match_sub[,c("plant_name_id", "elevated_to_species_id")],
#               by.x="accepted_plant_name_id",
#               by.y="plant_name_id", all.x=TRUE)
# 
# table(is.na(fin$elevated_to_species_id), useNA="ifany")
# 

#no match means
# > zero_match[1:3]
# [1] "1000417" "1000420" "1001170"
# > temp <- mm[mm$id=="1001170",]
# > temp
# taxon_rank        order       family     genus genus_hybrid    species species_hybrid infra_name author.x
# 11       <NA> Malpighiales Hypericaceae Hypericum         <NA> silenoides           <NA>       <NA>     <NA>
#   12       <NA> Malpighiales Hypericaceae Hypericum         <NA> silenoides           <NA>       <NA>     <NA>
#   Trank.ncbi.full      id taxon_status author.y accepted_plant_name_id match_type
# 11         species 1001170     Accepted     Juss             516649-wcs       <NA>
#   12         species 1001170   Misapplied    Kunth             516782-wcs       <NA>
#   > grep("516782-wcs", res$accepted_plant_name_id)
# [1] 94843 94948
# > res[c("94843", "94948"),]
# taxon_rank        order       family     genus genus_hybrid      species species_hybrid infra_name author.x
# 94843       <NA> Malpighiales Hypericaceae Hypericum         <NA> thesiifolium           <NA>       <NA>     <NA>
#   94948       <NA> Malpighiales Hypericaceae Hypericum         <NA>   silenoides           <NA>       <NA>     <NA>
#   Trank.ncbi.full      id taxon_status author.y accepted_plant_name_id
# 94843         species 1137036     Accepted    Kunth             516782-wcs
# 94948         species 1001170   Misapplied    Kunth             516782-wcs
# > 
