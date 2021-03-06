rm(list=ls())
library("tidyverse")
#library("taxonlookup")
library("stringr")
# devtools::install_github("bmewing/mgsub")
## current WCSP setup keeps everything in the WCSP except for Gigaspermaceae.
## current BIEN setup cleans out 26450 taxa (6.6%), including the following: 
# "Rhodophyta" Red algae
# "Chlorophyta" Green algae
# "Marchantiophyta" liverworts
# "Bryophyta" hornworts
# "Ascomycota" fungus
# "Cyanobacteria" ...
# "Basidiomycota" fungus
# "Ochrophyta"  yellow-green algae
# "Anthocerotophyta" hornworts
# "Euglenozoa"  thats not even a plant
# and 2 entries which are most likely errors: Cluisaceae + Hemionitidaceae, see code comments for details


#### SETUP ###################################################################################

# Choose database, possible values: WCSP, NCBI, BIEN, GBIF ###################################
# You can chose more than 1 (e.g. c("BIEN", "NCBI", "GBIF")) 
db <- "GBIF"

data_folder_path <- "./data/" # depends on where your working directory is set

## specify input file names:
ncbi_input_filename <- "NCBI_old.csv"
wcsp_input_filename <- "wcp_jun_20.rds"
bien_input_filename <- "bien_input_vectorized3.rds"
gbif_input_filename <- "input_tip_labels_new_sript.rds" # # input_tip_labels_new_sript.rds

bryophyta <- "bryophyta.csv"





# APG edits ###################################################################################
f.apg <- read.csv(paste0(data_folder_path, "apgweb_parsed.csv"), stringsAsFactors = F)
#grep("near_|fossil_", f.apg$Clade)

# remove some clade names don't have a valida name ("near_XXX") or a fossil name ("fossil")

f.apg <- f.apg %>% 
  filter(!(str_detect(Clade, "near_") | str_detect(Clade, "fossil"))) %>% 
  select(Syn_Fam, Acc_Fam, Clade)

# Intresting thing: "Adoxaceae" is treated as "syn" name of "Viburnaceae" in APWeb, which is conflict with APG IV; need to swap

# > f.apg[grep("Adoxaceae", f.apg$Syn_Fam),]
# Syn_Fam     Acc_Fam      Clade
# 25 Adoxaceae Viburnaceae Dipsacales
f.apg[grep("Adoxaceae", f.apg$Syn_Fam),]$Acc_Fam <- "Adoxaceae"
f.apg[grep("Viburnaceae", f.apg$Syn_Fam),]$Acc_Fam <- "Adoxaceae"

# also one more family names "Rhipogonaceae" was not recorded in APWeb
# so Manual added in "Rhipogonaceae" == "Ripogonaceae"  
# manually adding Osmundaceae as its accepted according to http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77126775-1

f.apg <- rbind.data.frame(f.apg, c("Rhipogonaceae", "Ripogonaceae", "Liliales"),
                          c("Ripogonaceae", "Ripogonaceae", "Liliales"),
                          c("Osmundaceae", "Osmundaceae", "")) %>%  
  arrange(Syn_Fam) %>% 
  unique()

# remove special character from Isoetaceae 
f.apg$Syn_Fam <- gsub("Isoëtaceae", "Isoetaceae", f.apg$Syn_Fam)
f.apg$Acc_Fam <- gsub("Isoëtaceae", "Isoetaceae", f.apg$Acc_Fam)

# caution serval invalid strings in fern clade names (check before do)
# grep("_", f.apg$Clade)
#f.apg <- f.apg %>% filter(!(str_detect(Clade, "_"))) # uncomment this line if want to remove ferns
names(f.apg) <- c("family", "family.apg", "order")
#write.csv(f.apg, "./results/apgweb_Spermatophyta_only_checked.csv", row.names = F, quote = F)


# NCBI ###################################################################################
if("NCBI" %in% db){
  NCBI <- read.csv(paste0(data_folder_path, ncbi_input_filename), header=T, stringsAsFactors = F)
  

  #Ripogonaceae
  f.ncbi <- NCBI %>% select(family) %>% unique()
  fam.apg <-f.apg%>% select(family) %>% unique()
  
  setdiff(f.ncbi$family, fam.apg$family)
  # should be empty.   # [1] "Ripogonaceae" has been acounted for in line 33

  NCBI.tmp <- NCBI %>% 
    select(-order) %>% 
    arrange(family) %>% 
    unique()
  NCBI.apg <- left_join(NCBI.tmp, f.apg, by="family")
  saveRDS(NCBI.apg, paste0(data_folder_path, ncbi_output_filename))
  write.csv(NCBI.apg, "./results/Spermatophyta_NCBI_APG_checked.csv", row.names = F, quote = F)
  # checklist <- NULL
  # for (i in NCBI.apg$ncbi_id[duplicated(NCBI.apg$ncbi_id)]){
  #   dd <- NCBI.apg[NCBI.apg$ncbi_id==i,] %>% select(family, family.apg, order)
  #   checklist <- rbind.data.frame(checklist, dd)
  # }
  # 
  # checklist <- checklist %>% arrange() %>% unique()  
}

# WCSP ###################################################################################
if("WCSP" %in% db){
  wcsp <- readRDS(paste0(data_folder_path, wcsp_input_filename))
  
  #check for no match and fix
  
  setdiff(unique(wcsp$family), unique(f.apg$family))
  # Should be:  [1] "Byxaceae"       "Gigaspermaceae" "Incertae_sedis"    "Oligomeris"     "Schoberia"      "v" 

  ########################################################################
  # caution: several invalid strings in fern clade names (check before do)#
  ########################################################################
  # #ferns families
  # "Aspleniaceae"
  # "Osmundaceae" 
  # "Polypodiaceae"
  # "Isoetaceae"
  # "v"=="Ophioglossum"="1142939-az"
  # "Ophioglossaceae" 
  # "Schizaeaceae"
  # #mosse remove
  # "Gigaspermaceae"
  #Incertae_sedis
  # > unique(wcsp[grep("Incertae_sedis", wcsp$family),]$genus)
  # [1] "Angeja"      "Anonymos"    "Cipum"       "Euphrona"    "Ivonia"      "Pouslowia"  
  # [7] "Theodoricea" "Thuraria"    "Urceola"
  
  # remove mosses & incertae_sedis uncomment this line below if want to keep ferns
  ## Gigaspermatacea: 2 cases, both synonyms (415628-az, 411899-az)
  ## Invertae_sedis: 20 taxa, all synonyms or unplaced. could be matched as synonym when ignoring the family (step3), so keep them
  moss.inc <- c("Gigaspermaceae")
  wcsp1 <- wcsp %>% 
    filter(!(family %in% moss.inc)) 
#    filter(accepted_plant_name_id!="1142939-az") #what`s wrong with this taxon? its an accepted Schizaeaceae fern species`
  
  #comment line below if want to keep ferns and uncomment the lines above
  #ferns.moss.inc <- c("Aspleniaceae", "Osmundaceae", "Polypodiaceae", "Isoetaceae", "Ophioglossaceae", "Schizaeaceae", "Gigaspermaceae", "Incertae_sedis")
  #wcsp1 <- wcsp %>% filter(!(family %in% ferns.moss.inc)) %>% filter(accepted_plant_name_id!="1142939-az")
  
  #rename
  # "Oligomeris" "Resedaceae", "1012613-az"
  # 
  # "Schoberia" "Amaranthaceae"
  
  # > unique(wcsp[grep("869344-az", wcsp$accepted_plant_name_id),]$family)
  # [1] "Buxaceae" "Byxaceae"
  
  # wcsp1$family <- mgsub::mgsub(wcsp1$family, c("Byxaceae", "Oligomeris", "Schoberia"), c("Buxaceae", "Resedaceae", "Amaranthaceae"))
  # correct famlies names
  wcsp1$family <- gsub("Byxaceae", "Buxaceae", wcsp1$family)
  wcsp1$family <- gsub("Oligomeris", "Resedaceae", wcsp1$family)
  wcsp1$family <- gsub("Schoberia", "Amaranthaceae", wcsp1$family)
  
  # wcsp2 <- wcsp1 %>% filter(accepted_plant_name_id!="1142939-az")
  setdiff(unique(wcsp1$family), unique(f.apg$family))
  
  # manual fix
  wcsp1$family[wcsp1$family=="v"] <- "Ophioglossaceae"
  
  wcsp.apg <- left_join(wcsp1, f.apg, by="family")
  
  saveRDS(wcsp.apg, paste0(data_folder_path, "apg_", wcsp_input_filename))
  #write.csv(wcsp.apg, "./results/Spermatophyta_WCSP_APG_checked.csv", row.names = F, quote = F)
  
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

  #View(bien[bien$family %in% missing,])
  # 957 taxa, plenty of green algae
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
  # "Rhodophyta" Red algae
  # "Chlorophyta" Green algae
  # "Marchantiophyta" liverworts
  # "Bryophyta" hornworts
  # "Ascomycota" fungus
  # "Tracheophyta" vascular plant
  # "Cyanobacteria" ...
  # "Basidiomycota" fungus
  # "Ochrophyta"  yellow-green algae
  # "Anthocerotophyta" hornworts
  # "Euglenozoa"  thats not even a plant
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
  # [1] "Plagiogyriaceae"   "Cluisaceae"        "Hemionitidaceae"   "Arthropteridaceae" "Xanthoceraceae"
  # Plagiogyriaceae is an accepted fern family
  # Cluisaceae has one genus named Guttiferae which is not on IPNI: remove
  # Hemionitidaceae, same names as the genus it belongs to, wrong entry: remove
  # Arthropteridaceae = fern, IPNI, GBIF says synonym of Tectariaceae, keep it like this for now
  # Xanthoceraceae = typo of Xanthoceratceae (missing t) which is in f.apg
  bien <- bien[-which(bien$family %in% c("Cluisaceae", "Hemionitidaceae")),]
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
  gbif <- readRDS(paste0(data_folder_path, gbif_input_filename)) #_new_sript.rds is the one built using GBIF names only
  
  (sort(fams <- setdiff(unique(gbif$family), unique(f.apg$family))))
  # manual research
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
  
  