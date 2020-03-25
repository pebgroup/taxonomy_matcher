rm(list=ls())
library("tidyverse")
library("taxonlookup")
library("stringr")
# devtools::install_github("bmewing/mgsub")
NCBI <- read.csv("./data/NCBI_old.csv", header=T, stringsAsFactors = F)

f.apg <- read.csv("./data/apgweb_parsed.csv", stringsAsFactors = F)
#grep("near_|fossil_", f.apg$Clade)
f.apg <- f.apg %>% filter(!(str_detect(Clade, "near_") | str_detect(Clade, "fossil"))) %>% select(Syn_Fam, Acc_Fam, Clade)

# > f.apg[grep("Adoxaceae", f.apg$Syn_Fam),]
# Syn_Fam     Acc_Fam      Clade
# 25 Adoxaceae Viburnaceae Dipsacales
f.apg[grep("Adoxaceae", f.apg$Syn_Fam),]$Acc_Fam <- "Adoxaceae"
f.apg[grep("Viburnaceae", f.apg$Syn_Fam),]$Acc_Fam <- "Adoxaceae"
#Ripogonaceae
# f.ncbi <- NCBI %>% select(family) %>% unique()
# fam.apg <-f.apg%>% select(Syn_Fam) %>% unique()

# > setdiff(f.ncbi$family, fam.apg$Syn_Fam)
# [1] "Ripogonaceae"

f.apg <- rbind.data.frame(f.apg, c("Rhipogonaceae", "Ripogonaceae", "Liliales"),c("Ripogonaceae", "Ripogonaceae", "Liliales")) %>% arrange(Syn_Fam) %>% unique()

#remove ferns
# grep("_", f.apg$Clade)
f.apg <- f.apg %>% filter(!(str_detect(Clade, "_")))
names(f.apg) <- c("family", "family.apg", "order")
write.csv(f.apg, "./results/apgweb_Spermatophyta_only_checked.csv", row.names = F, quote = F)

NCBI.tmp <- NCBI %>% select(-order) %>% arrange(family) %>% unique()
NCBI.apg <- left_join(NCBI.tmp, f.apg, by="family")
saveRDS(NCBI.apg, "./data/NCBI.apg.rds")
write.csv(NCBI.apg, "./results/Spermatophyta_NCBI_APG_checked.csv", row.names = F, quote = F)
# checklist <- NULL
# for (i in NCBI.apg$ncbi_id[duplicated(NCBI.apg$ncbi_id)]){
#   dd <- NCBI.apg[NCBI.apg$ncbi_id==i,] %>% select(family, family.apg, order)
#   checklist <- rbind.data.frame(checklist, dd)
# }
# 
# checklist <- checklist %>% arrange() %>% unique()

#### WCSP ####
WCSP <- readRDS("./data/wcp_dec_19.rds")

#check for no match and fix

# setdiff(unique(WCSP$family), unique(f.apg$family))
# [1] "Aspleniaceae"   "Byxaceae"       "Gigaspermaceae" "Incertae_sedis" "Isoetaceae"    
# [6] "Oligomeris"     "Osmundaceae"    "Polypodiaceae"  "Schoberia"      "v" 

# #ferns remove
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
# > unique(WCSP[grep("Incertae_sedis", WCSP$family),]$genus)
# [1] "Angeja"      "Anonymos"    "Cipum"       "Euphrona"    "Ivonia"      "Pouslowia"  
# [7] "Theodoricea" "Thuraria"    "Urceola"

ferns.moss.inc <- c("Aspleniaceae", "Osmundaceae", "Polypodiaceae", "Isoetaceae", "Ophioglossaceae", "Schizaeaceae", "Gigaspermaceae", "Incertae_sedis")
WCSP1 <- WCSP %>% filter(!(family %in% ferns.moss.inc)) %>% filter(accepted_plant_name_id!="1142939-az")

#rename
# "Oligomeris" "Resedaceae", "1012613-az"
# 
# "Schoberia" "Amaranthaceae"

# > unique(WCSP[grep("869344-az", WCSP$accepted_plant_name_id),]$family)
# [1] "Buxaceae" "Byxaceae"

# WCSP1$family <- mgsub::mgsub(WCSP1$family, c("Byxaceae", "Oligomeris", "Schoberia"), c("Buxaceae", "Resedaceae", "Amaranthaceae"))
 WCSP1$family <- gsub("Byxaceae", "Buxaceae", WCSP1$family)
 WCSP1$family <- gsub("Oligomeris", "Resedaceae", WCSP1$family)
 WCSP1$family <- gsub("Schoberia", "Amaranthaceae", WCSP1$family)
# WCSP2 <- WCSP1 %>% filter(accepted_plant_name_id!="1142939-az")


WCSP.apg <- left_join(WCSP1, f.apg, by="family")

saveRDS(WCSP.apg, "./data/WCSP.apg.rds")
write.csv(WCSP.apg, "./results/Spermatophyta_WCSP_APG_checked.csv", row.names = F, quote = F)
