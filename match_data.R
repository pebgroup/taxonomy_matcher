# setwd("~/Documents/WOLF/PROJECTS/58 World Checklist paper/analyses 2019")

library(ape)
library(phytools)
library(parallel)

#load("match_data.RData")
source("plant_sr/functions.R")

# path to tree
tree_path <- "v0.1"

# read trees (from where actually? - what the abbreviations
read.tree(paste(tree_path, "/ALLMB.tre", sep="")) -> phylo_a
read.tree(paste(tree_path, "/ALLOTB.tre", sep="")) -> phylo_b

# exclude rogue Asteraceae
phylo_a <- drop.tip(phylo_a, c("Schmidtia_capensis", "Hypochaeris_arachnoides"))
phylo_b <- drop.tip(phylo_b, c("Schmidtia_capensis", "Hypochaeris_arachnoides"))

# path to WCSP download
wcsp_path = "database"

# read WCSP
read.csv(paste(wcsp_path, "/published_names_19_10_2018.csv", sep=""), header=TRUE, sep="|") -> published
read.csv(paste(wcsp_path, "/unpublished_names_19_10_2018.csv", sep=""), header=TRUE, sep="|") -> unpublished
published$published <- "published"
unpublished$published <- "unpublished"
wcsp <- rbind(published,unpublished)
rm(published, unpublished)

rownames(wcsp) <- wcsp$checklist_id

#read in latest download
#ld <- readLines(paste0(wcsp_path, "/powoNames/taxon.txt"))

# remove ferns from WCSP
fernfam = as.vector(read.csv("fernfam_plantlist.csv")$x)
#ferns <-  wcsp[wcsp$accepted_family %in% fernfam, ]
wcsp = wcsp[!wcsp$accepted_family %in% fernfam, ]

# splitting wcsp into species and infraspecific taxa to save computation time
wcsp_species <- wcsp[wcsp$infraspecific_rank=="",]
# exclude genera
wcsp_species <- wcsp_species[wcsp_species$species!="",] 
wcsp_infra   <- wcsp[wcsp$infraspecific_rank!="",]
# get rid of punctuation in ranks (because it is inconsistent)
wcsp_infra$infraspecific_rank <- as.vector(gsub("\\.","",wcsp_infra$infraspecific_rank)) 

# Magallon
MATCHES_a_a <- mclapply(1:length(phylo_a$tip.label), get_matches, phylo=phylo_a, wcsp_species = wcsp_species, wcsp_infra = wcsp_infra, mc.cores=30)
# Open Tree
MATCHES_b_a <- mclapply(1:length(phylo_b$tip.label), get_matches, phylo=phylo_b, wcsp_species = wcsp_species, wcsp_infra = wcsp_infra, mc.cores=30)

# collapse lists and remove multiples
MATCHES_a_a <- remove_multiples(MATCHES_a_a)
MATCHES_b_a <- remove_multiples(MATCHES_b_a)

# load genbank only tree: Magallon
read.tree(paste(tree_path, "/GBMB.tre", sep="")) -> phylogb_a
# ... Opentree
read.tree(paste(tree_path, "/GBOTB.tre", sep="")) -> phylogb_b

MATCHES_a_a <- resolve_multiple(MATCHES_a_a, wcsp, phylo_a, phylogb_a) # completed without warnings 
MATCHES_b_a <- resolve_multiple(MATCHES_b_a, wcsp, phylo_b, phylogb_b) # completed without warnings

# get match stats 1: how many tip labels in the tree have a match in WCSP?
sum(!is.na(MATCHES_a_a))/length(MATCHES_a_a) # 0.753631
sum(!is.na(MATCHES_b_a))/length(MATCHES_b_a) # 0.753896

# get match stats 2: how many "good species" in WCSP have a mtach in the tree?
length(unique(MATCHES_a_a[!is.na(MATCHES_a_a)]))/nrow(wcsp[wcsp$species!="" & wcsp$infraspecific_rank == "" & wcsp$species_hybrid_marker == "" & wcsp$taxon_status_description == "Accepted",]) # 0.8328144
length(unique(MATCHES_b_a[!is.na(MATCHES_b_a)]))/nrow(wcsp[wcsp$species!="" & wcsp$infraspecific_rank == "" & wcsp$species_hybrid_marker == "" & wcsp$taxon_status_description == "Accepted",]) # 0.8258143

# reformat matches for export
MATCHES_a_a <- data.frame(tip = phylo_a$tip.label, conservative = MATCHES_a_a)
for(i in 1:ncol(MATCHES_a_a)) MATCHES_a_a[,i] <- as.vector(MATCHES_a_a[,i])
MATCHES_b_a <- data.frame(tip = phylo_b$tip.label, conservative = MATCHES_b_a)
for(i in 1:ncol(MATCHES_b_a)) MATCHES_b_a[,i] <- as.vector(MATCHES_b_a[,i])

save(MATCHES_a_a, MATCHES_b_a, phylo_a, phylo_b, wcsp, file = "MATCHES.RData")

save.image("match_data.RData")


  