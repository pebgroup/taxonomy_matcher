rm(list=ls())
library("tidyverse")
library("stringr")


#### SETUP ################################################################

# Choose data source ------------------------------------------------------
# possible values: WCP, NCBI, BIEN, GBIF, more than 1 is possible (e.g.
# c("BIEN", "NCBI", "GBIF"))

db <- "BIEN"

data_folder_path <- "../data/" 

## specify input file names:
ncbi_input_filename <- "NCBI_old.csv"
wcp_input_filename <- "wcp_jul_21.rds"
bien_input_filename <- "bien_common_format_Sep21_download_Jan2020.rds"
gbif_input_filename <- "input_tip_labels_new_sript.rds"

bryophyta <- "bryophyta.csv"





# APG edits ################################################################
f.apg <- read.csv(paste0(data_folder_path, "apgweb_parsed.csv"), stringsAsFactors = F)

# remove clades with invalid or fossil names ("near_XXX", "fossil")
f.apg <- f.apg %>% 
  filter(!(str_detect(Clade, "near_") | str_detect(Clade, "fossil"))) %>% 
  select(Syn_Fam, Acc_Fam, Clade)

# "Adoxaceae" is treated as "syn" name of "Viburnaceae" in APWeb, which is
# conflict with APG IV; need to swap:
f.apg[grep("Adoxaceae", f.apg$Syn_Fam),]$Acc_Fam <- "Adoxaceae"
f.apg[grep("Viburnaceae", f.apg$Syn_Fam),]$Acc_Fam <- "Adoxaceae"

# add family Rhipogonaceae, missing in APWeb download (is online by now)
# add family Osmundaceae,  as its accepted according to
# http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126775-1

f.apg <- rbind.data.frame(f.apg, c("Rhipogonaceae", "Ripogonaceae", "Liliales"),
                          c("Ripogonaceae", "Ripogonaceae", "Liliales"),
                          c("Osmundaceae", "Osmundaceae", "")) %>%  
  arrange(Syn_Fam) %>% 
  unique()

# remove special character from Isoetaceae 
f.apg$Syn_Fam <- gsub("Isoëtaceae", "Isoetaceae", f.apg$Syn_Fam)
f.apg$Acc_Fam <- gsub("Isoëtaceae", "Isoetaceae", f.apg$Acc_Fam)

# caution serval invalid strings in fern clade names (check before do)
names(f.apg) <- c("family", "family.apg", "order")


# NCBI #####################################################################
if("NCBI" %in% db){
  NCBI <- read.csv(paste0(data_folder_path, ncbi_input_filename), stringsAsFactors = F)
  

  #Ripogonaceae
  f.ncbi <- NCBI %>% select(family) %>% unique()
  fam.apg <-f.apg%>% select(family) %>% unique()
  
  setdiff(f.ncbi$family, fam.apg$family) # should be empty

  NCBI.tmp <- NCBI %>% 
    select(-order) %>% 
    arrange(family) %>% 
    unique()
  
  NCBI.apg <- left_join(NCBI.tmp, f.apg, by="family")
  
  #saveRDS(NCBI.apg, paste0(data_folder_path, ncbi_output_filename))
  write.csv(NCBI.apg, "./results/Spermatophyta_NCBI_APG_checked.csv", row.names = F, quote = F)
#   checklist <- NULL
#   for (i in NCBI.apg$ncbi_id[duplicated(NCBI.apg$ncbi_id)]){
#     dd <- NCBI.apg[NCBI.apg$ncbi_id==i,] %>% select(family, family.apg, order)
#     checklist <- rbind.data.frame(checklist, dd)
#   }
# 
#   checklist <- checklist %>% arrange() %>% unique()
# }

# WCP ####################################################################
if("WCP" %in% db){
  WCP <- readRDS(paste0(data_folder_path, wcp_input_filename))
  
  # check if all families are represented and add if necessary manually
  setdiff(unique(WCP$family), unique(f.apg$family))
  # [1] "Incertae_sedis"  "Pseudotubulare"  "Gigaspermaceae" "Tiganophytaceae"
  
  # Pseudotubulare = not a family name, Synonym entry
  # Gigaspermaceae = moss family
  # Tiganophytaceae = rosid
  # Incertae_sedis:
  unique(WCP[grep("Incertae_sedis", WCP$family),]$genus)
  # [1] "Cipum"       "Anonymos"    "Theodoricea" "Thuraria"    "Urceola"     "Pouslowia"   "Euphrona"    "Ivonia"
  
  # remove mosses & incertae_sedis. uncomment this line below if want to keep ferns
  ## Gigaspermatacea: 2 cases, both synonyms (415628-az, 411899-az)
  ## Invertae_sedis: 20 taxa, all synonyms or unplaced. Could be matched as synonym when ignoring the family (step3), so keep them
  moss.inc <- c("Gigaspermaceae")
  WCP1 <- WCP %>% 
    filter(!(family %in% moss.inc)) 
  #    filter(accepted_plant_name_id!="1142939-az") #what`s wrong with this taxon? its an accepted Schizaeaceae fern species`
  
  #comment line below if want to keep ferns and uncomment the lines above
  #ferns.moss.inc <- c("Aspleniaceae", "Osmundaceae", "Polypodiaceae", "Isoetaceae", "Ophioglossaceae", "Schizaeaceae", "Gigaspermaceae", "Incertae_sedis")
  #WCP1 <- WCP %>% filter(!(family %in% ferns.moss.inc)) %>% filter(accepted_plant_name_id!="1142939-az")
  
  setdiff(unique(WCP1$family), unique(f.apg$family))
  
  # manual fix
  WCP1 <- WCP1[!WCP1$family=="Pseudotubulare",]
  WCP.apg <- left_join(WCP1, f.apg, by="family")
  
  saveRDS(WCP.apg, paste0(data_folder_path, "apg_", WCP_input_filename))
}

# BIEN ######################################################################
if("BIEN" %in% db){
  bien <- readRDS(paste0(data_folder_path, bien_input_filename))
  
  # remove Bryophyta
  bry <- read.csv(paste0(data_folder_path, bryophyta))
  bryos <- as.character(bry[,1])
  bryos <- gsub(" ", "", bryos)
  bien <- bien[!bien$family %in% bryos,]
  
  missing <- setdiff(unique(bien$family), unique(f.apg$family))

  library(rgbif)
  
  alg.list <- list()
  for(i in 1:length(missing)){
    alg.list[[i]] <- rgbif::name_backbone(missing[i])  
    print(i)
  }

  phylum <- sapply(alg.list, "[[", "phylum")
  # fill in missing
  l <- which(unlist(lapply(phylum, is.null)))
  phylum[l] <- NA
  phylum <- unlist(phylum)
  unique(phylum)
  # "Marchantiophyta" liverworts
  # "Bryophyta" hornworts
  # "Tracheophyta" vascular plant

  phylum <- data.frame(missing, phylum)
  phylum$phylum <- as.character(phylum$phylum)
  phylum$phylum[is.na(phylum$phylum)] <- "not defined"
  
  bien <- left_join(bien, phylum, by=c("family"="missing"))
  bien$phylum <- as.character(bien$phylum)
  table(bien$phylum[!is.na(bien$phylum)])
  
  # remove all non tracheophyta entries
  bien <- bien[-which(!is.na(bien$phylum) & bien$phylum!="Tracheophyta"),]
  
  bien_sub <- bien[which(bien$phylum=="Tracheophyta"),]
  nrow(bien_sub)
  unique(bien_sub$family)
  # [1] "Plagiogyriaceae"   "Cluisaceae"  "Arthropteridaceae" "Xanthoceraceae"
  # Plagiogyriaceae is an accepted fern family
  # Cluisaceae has one genus named Guttiferae which is not on IPNI: remove
  # Arthropteridaceae = fern, IPNI, GBIF says synonym of Tectariaceae, keep it like this for now
  # Xanthoceraceae = typo of Xanthoceratceae (missing t) which is in f.apg
  rem <- c("Cluisaceae")
  if(any(bien$family %in% rem)){
    bien <- bien[-which(bien$family %in% rem),]    
  }
  bien$family <- as.character(bien$family)
  bien$family[which(bien$family=="Xanthoceraceae")] <- "Xanthoceratceae"

  # add Plagiogyriaceae and Arthropteridaceae to the f.apg file 
  f.apg <- rbind.data.frame(f.apg, c("Plagiogyriaceae", "Plagiogyriaceae", ""),
                            c("Arthropteridaceae", "Arthropteridaceae", ""))
  
  setdiff(unique(bien$family), unique(f.apg$family)) # should be null now
  
  bien.apg <- left_join(bien, f.apg, by="family")
  
  saveRDS(bien.apg, paste0(data_folder_path, "apg_", bien_input_filename))
}  

# GBIF #######################################################################################
if("GBIF" %in% db){
  gbif <- readRDS(paste0(data_folder_path, gbif_input_filename)) 
  
  (sort(fams <- setdiff(unique(gbif$family), unique(f.apg$family))))
  # manual research:
  # Cyclostigmataceae: source = PBDB, probably doubtful: remove
  # Hookeriaceae = moss: Elharveya visicularioides
  # Hymenochaetaceae = fungus, remove
  # Sphaerocystidaceae = green algea, should not be in the tree: Planochloris pyrenoidifera
  # Stixaceae = typo of Stixidaceae (included in APG) --> change family name manually here 
  # Ulotrichaceae = green algae Hormidium ... 181911

  
  gbif$family <- as.character(gbif$family)
  gbif$family[which(gbif$family=="Stixaceae")] <- "Stixidaceae"
  todrop <-  c("Sphaerocystidaceae",
               "Hookeriaceae",
               "Cyclostigmataceae", 
               "Ulotrichaceae",
               "Hymenochaetaceae")
  if(length(which(gbif$family %in% todrop))>0){
    gbif <- gbif[-which(gbif$family %in% todrop),]
  }
  
  setdiff(unique(gbif$family), unique(f.apg$family))
  # should just be NAs for missing family information
  
  gbif.apg <- left_join(gbif, f.apg, by="family")
  
  saveRDS(gbif.apg, paste0(data_folder_path, "apg_", gbif_input_filename))
}
  
  