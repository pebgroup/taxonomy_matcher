# Analyse BIEN presence in WCSP regions data



library(rgdal)
library(tidyverse)
library(plyr)
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())

# read community matrix for WCSP (from process_geography.R)
# read results lists from the BIEN occurrences (server run)
load("comm.RData") # object = comm
load("BIEN_in_WCSP_regions.RData") # objects = spec.list, res

# basic check if list and SR counts line up
test <- lapply(spec.list, length)
test <- unlist(test)
test <- as.data.frame(test)
test$test == res$sr # check
  
bien.list <- spec.list
rm(spec.list)

# The fastest way go get a species richness area map with BIEN occurrences in  is to create a community matrix from BIEN species list like comm and compare this, or create a list like spec.list from comm. Comparing the lists: 369 iterations, accessing the list objects. Comparing the matrices: 369 iterations, accessing all possible species columns.
# ANSWER
# fastest way should be comparing the lists. To create a list from the community matrix does not take very long (<1min)

# ?Get a community matrix that codes the regions x species, 0=occurrs in none, 1=occurrs in WCSP, 2=occurrs in BIEN, 3=occurrs in both


# read WCSP regions shapefile
shp <- readOGR("shapefile/level3.shp")

#  Prepare WCSP data. Get list with accepted names from comm. Loop through rows (=regions)
# get WCSP species names
wc <- read.csv("database/published_names_19_10_2018.csv", sep="|", stringsAsFactors = FALSE)
wc2 <- read.csv("database/unpublished_names_19_10_2018.csv", sep="|", stringsAsFactors = FALSE)
wc_all <- rbind(wc, wc2)
rm(wc, wc2)
wc.df <- wc_all[,c("accepted_name", "accepted_name_id")]

# get list with species names for each region
wcsp.list <- list()
for(i in 1:nrow(comm)){
  temp <- comm[i,]
  comm.name <- names(which(temp!=0))
  comm.name.bin <- wc.df$accepted_name[wc.df$accepted_name_id %in% comm.name]
  wcsp.list[[i]] <- as.factor(comm.name.bin)
  if(!i%%10)cat(i,"\r")
}
names(wcsp.list) <- shp$LEVEL_3_CO

# calculate intersection etc: a %in% b /
## bien.list = BIEN species presence per WCSP region
## wcsp.list = WCSP species presence per W  CSP region

# regions complete?
all(names(bien.list) == names(wcsp.list))

# total comparison across region borders
bien.species <- unique(as.character(unlist(bien.list)))
wcsp.species <- unique(as.character(unlist(wcsp.list))) 




#### Synonyms ####
# 1) how many BIEN names are not in the WCSP?
# 2) check if these species are synonyms of species
# 3) get a final count of species not listed in BIEN (and vice versa) and check some manually

# Can we get more taxon name info from BIEN than just binomial name?
library(BIEN)
#bien.test <- BIEN_occurrence_species(bien.species[1:10], only.new.world = F)
bien.test <- BIEN:::.BIEN_sql(query = "SELECT * FROM view_full_occurrence_individual LIMIT 100;")
View(bien.test[,c(83:ncol(bien.test))])
names(bien.test[,c(83:ncol(bien.test))])


# 1) 
round(table(wc_all$taxon_status_description)/nrow(wc_all),2)
wc_all$spec <- paste(wc_all$genus, wc_all$species, sep=" ")
wcsp.all.species <- unique(wc_all$spec)
length(unique(wcsp.all.species))

bs_no_wcp <- bien.species[which(!bien.species %in% wcsp.all.species)]
length(bs_no_wcp)
length(bs_no_wcp)/length(bien.species)
bs_in_wcp <- bien.species[which(bien.species %in% wcsp.all.species)]


## Which species are the ones not occurring in WCSP?

genera_no_wcp <- unique(sapply((strsplit(bs_no_wcp, " ")), "[[", 1))
# Tortella fragilis: moss
# Mnium : moss
# Letharia vulpina: lichen
# Dicranum fuscescens: moss
# Fabiana: vascular plant!
bs_no_wcp[which(grepl("Fabiana", bs_no_wcp))]
View(wc_all[which(wc_all$genus=="Fabiana"),])
# Fabiana petunoides"  "Fabiana weberbaueri"
# Seems to be mostly non-vascular plants (non-tracheaphyta)
library(VennDiagram)
venn.diagram(list(BIEN=bien.species, WCSP=wcsp.all.species),
                          "bien_wcsp_venn.tiff",
             height = 2000, width = 2000)


## Those BIEN species occurring; how many are accepted names?
acc <- unique(wc_all$accepted_name)
#wc_sub <- unique(wc_all[,c("taxon_status_description", "spec")])
table(bs_in_wcp %in% acc) / length(bs_in_wcp)




# Adjust BIEN names ###########################################

 ## WCSP has issues when matching binomial names. There is cases where the accepted name depends on the variety, subsespecies etc -  but we don`t have this information from BIEN (yet?)
bien.wrongs <- bs_in_wcp[which(!bs_in_wcp %in% acc)]
rep_list <- wc_all[wc_all$spec %in% bien.wrongs, c("accepted_name", "spec", "taxon_name", "genus", "species")]
rep_list <- unique(rep_list)

rep_list_sub <- rep_list[rep_list$spec %in% bien.wrongs, c("accepted_name", "spec", "genus", "species")]
rep_list_sub <- unique(rep_list_sub)
temp <- tapply(rep_list_sub$accepted_name, rep_list_sub$spec, length)
hist(as.numeric(temp))
range(as.numeric(temp))

sort(temp, decreasing = TRUE)[1:50]
# How many species that are classified as synonym & occur in BIEN are affected?
table(temp>1)
table(temp>1)/length(temp)


# The Bad Ones ##################################################################
#View(wc_all[wc_all$spec %in% names(temp[temp>1]),])
prob.species <- names(temp[temp>1])

# modify BIEN function
BIEN_occurrence_species_mod <- function (species, only.new.world = FALSE, ...) 
{
    .is_log(only.new.world)
    .is_char(species)
    newworld_ <- .newworld_check(only.new.world)
     query <- paste("SELECT scrubbed_species_binomial,scrubbed_taxon_name_no_author,scrubbed_author,taxonobservation_id,latitude, longitude",
                   "\n                 FROM view_full_occurrence_individual \n                 WHERE scrubbed_species_binomial in (", 
                   paste(shQuote(species, type = "sh"), collapse = ", "), 
                   ")",  
                   "\n                 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) \n                 ORDER BY scrubbed_species_binomial ;")
    return(.BIEN_sql(query, ...))
  }
  
assignInNamespace('BIEN_occurrence_species',BIEN_occurrence_species_mod,ns='BIEN')
environment(BIEN_occurrence_species_mod)<-asNamespace('BIEN') 
  
# # Get more info for multimatch species

# chunks <- c(seq(1,11000,1000), length(prob.species))
# for(i in 9:(length(chunks)-1)){
#   bien_mm <- BIEN_occurrence_species_mod(prob.species[chunks[i]:chunks[i+1]], only.new.world = FALSE)
#   saveRDS(bien_mm, file=paste0("bien_mm_", i, ".rds"))
#   rm(bien_mm)
# }
# bien_mm_2 <- BIEN_occurrence_species_mod(prob.species[1001:2000], only.new.world = FALSE)
# saveRDS(bien_mm_2, file="bien_mm_2.rds")
files <- dir()
files <- files[grepl("[0-9].rds", files)]
df <- files %>%
  map_dfr(readRDS)

# check how many species double match here
df.u <- unique(df[,c("scrubbed_species_binomial", "scrubbed_taxon_name_no_author", "scrubbed_author")])
View(df.u)
  
# run for i=1
# bien.list_update <- bien.list
# multi.matches <- list()
# for(i in 1:length(bien.list_update)){ 
#   temp <- bien.list_update[[i]]
#   w <- temp[which(temp %in% bien.wrongs)]
#   for(j in 1:length(w)){
#     right.name <- rep_list$accepted_name[rep_list$spec==temp[j]]
#     if(length(right.name>1)){
#       multi.matches[j] <- right.name
#     }
#     #bien.list_update[[i]][j] <-
#   }
#   if(!i%%10)cat(i,"\r")
# }

#################################################################
  

# Check for WCP species not in BIEN
wcp_no_bs <- wcsp.species[which(!wcsp.species %in% bien.species)]
length(wcp_no_bs)
length(wcp_no_bs)/length(wcsp.species)
## something is odd here.... 




# TOTAL NUMBERS FOR EACH REGION ###########
# species bien, species wcsp, intersection

length(which(bien.list[[1]] %in% wcsp.list[[1]]))/length(bien.list[[1]]) #  % of bien are in wcsp
table(wcsp.list[[1]] %in% bien.list[[1]])/length(wcsp.list[[1]]) #  % of wcsp are in bien (this could be complete)
table(wcsp.list[[1]] %in% bien.list[[1]])/length(wcsp.list[[1]]) #  % of wcsp are in bien (this should be complete)
# wcsp_in_bien <- length(which(wcsp.list[[1]] %in% bien.list[[1]]))/length(wcsp.list[[1]])s  % of wcsp are in bien

my.percent <- function(bien,wcsp){
  bien_total <- length(bien)
  bien_in_wcsp_p <- length(which(bien %in% wcsp))/length(bien)
  wcsp_total <- length(wcsp)
  wcsp_in_bien_p <- length(which(wcsp %in% bien))/length(wcsp)
  inter <- length(intersect(bien, wcsp))
  print(as.table(c(bien_total, bien_in_wcsp_p, wcsp_total, wcsp_in_bien_p, inter)))
  }
comp <- mapply(my.percent, bien.list, wcsp.list, SIMPLIFY = TRUE)
comp <- as_tibble(comp)

comp <- as_tibble(cbind(region = names(comp), t(comp)))
# clean
names(comp) <- c("region", "bien_total", "bien_in_wcsp", "wcsp_total", "wcsp_in_bien", "intersection")
apply(comp, 2, class)
comp <- comp %>% 
  mutate_at(vars(bien_total, bien_in_wcsp, wcsp_total, wcsp_in_bien, intersection), as.numeric)

# first analysis
cor.test(comp$bien_total, comp$wcsp_total, method="s")

ggplot(comp, aes(bien_total, wcsp_total, label=region))+
  geom_point(alpha=0.5)+
  geom_text(aes(label=region),hjust=0, vjust=0, alpha=0.5)


# attach the number to the shapefile to map them
shp@data <- merge(shp@data, comp, by.x="LEVEL_3_CO", by.y="region", all.x=TRUE)

shp@data$id = rownames(shp@data) # explicitly identify attribute rows
shp.points = fortify(shp, region="id") # melt polygons into points, each point tagged with the id
shp.df = join(shp.points, shp@data, by="id") # join points with attributes

# plot maps
wcsp_tot_plot <- ggplot(shp.df, aes(x=long, y=lat, group=group, fill=wcsp_total)) +
  geom_polygon()+
  geom_path(color="black", size=0.1)+
  scale_fill_gradient("Species richness WCSP", guide="colorbar", low="#feff80", high="#6b0000",
                      limits=c(min(shp$wcsp_total),max(shp$wcsp_total)))+
  guides(fill = guide_colourbar(barwidth = 30, direction="horizontal"))+ # stretch that colorbar
  coord_equal()+ # otherwise maps looks distorted
  theme_void()+
  theme(legend.position = "bottom")

bien_tot_plot <- ggplot(shp.df, aes(x=long, y=lat, group=group, fill=bien_total)) +
  geom_polygon()+
  geom_path(color="black", size=0.1)+
  scale_fill_gradient("Species richness BIEN", guide="colorbar", low="#feff80", high="#6b0000",
                      limits=c(min(shp$bien_total),max(shp$bien_total)))+
  guides(fill = guide_colourbar(barwidth = 30, direction="horizontal"))+ # stretch that colorbar
  coord_equal()+ # otherwise maps looks distorted
  theme_void()+
  theme(legend.position = "bottom")

grid.arrange(wcsp_tot_plot, bien_tot_plot)

# difference plot
ggplot(shp.df, aes(x=long, y=lat, group=group, fill=wcsp_total-bien_total)) +
  geom_polygon()+
  geom_path(color="black", size=0.1)+
  scale_fill_gradient("Diff WCSP-BIEN", guide="colorbar", low="#feff80", high="#6b0000")+
  guides(fill = guide_colourbar(barwidth = 30, direction="horizontal"))+ # stretch that colorbar
  coord_equal()+ # otherwise maps looks distorted
  theme_void()+
  theme(legend.position = "bottom")
hist(comp$wcsp_total-comp$bien_total, breaks=80)

# regions with less WCSP species than BIEN species in it:
comp$region[which(comp$wcsp_total-comp$bien_total < 0)]


# TOTAL NUMBERS FOR EACH REGION ###########
# create comparison matrix
# coding: 0=occurs in none, 1=occurs in WCSP, 2=occurs in BIEN, 3=occurs in both

# clear workspace
rm(bien_tot_plot, comm, shp.df, test, wc_all, wc.df, wcsp_tot_plot, temp)

species <- unique(c(wcsp.species, bien.species))
regions <- shp$LEVEL_3_CO
cc <- matrix(nrow=length(shp$LEVEL_3_CO), ncol=length(species), 0)
colnames(cc) <- species
row.names(cc) <- shp$LEVEL_3_CO

rm(shp)

# populate the matrix (13min)
ptm <- proc.time() # Start the clock
for(i in 1:length(regions)){
  b <- bien.list[[regions[i]]]
  w <- wcsp.list[[regions[i]]]
  int <- intersect(b,w)
  only_b <- setdiff(b, w)
  only_w <- setdiff(w, b)
  cc[i, which(colnames(cc) %in% int)] <- 3
  cc[i, which(colnames(cc) %in% only_b)] <- 2
  cc[i, which(colnames(cc) %in% only_w)] <- 1
  if(!i%%10)cat(i,"\r")
}
print(proc.time() - ptm)

# function that calculated the percentage of 1,2,3,0 in each column
my.table <- function(x){
  none <- length(which(x==0))
  b <- length(which(x==2))
  w <- length(which(x==1))
  both <- length(which(x==3))
  b_only_p <- length(which(x==2))/(length(x)-length(which(x==0)))
  w_only_p <- length(which(x==1))/(length(x)-length(which(x==0)))
  both_p <- length(which(x==3))/(length(x)-length(which(x==0)))
  print(as.table(c(none, b, b_only_p, w, w_only_p, both, both_p)))
}
cc2 <- apply(cc, 1, my.table)
cc2 <- as_tibble(cc2)
cc2 <- as_tibble(cbind(region = names(cc2), t(cc2)))

# clean
names(cc2) <- c("region", "none", "bien", "bien_perc", "wcsp", "wcsp_perc", "both", "both_p")

shp <- readOGR("shapefile/level3.shp")

shp@data <- merge(shp@data, cc2, by.x="LEVEL_3_CO", by.y="region", all.x=TRUE)
shp@data$id = rownames(shp@data) # explicitly identify attribute rows
shp.points = fortify(shp, region="id") # melt polygons into points, each point tagged with the id
shp.df = join(shp.points, shp@data, by="id") # join points with attributes

plot(shp@data$bien_perc, shp@data$wcsp_perc, col=as.numeric(shp@data$both_p))

hist(as.numeric(shp@data$bien_perc))
hist(as.numeric(shp@data$wcsp_perc))
hist(as.numeric(shp@data$both_p))


# plot maps
ggplot(shp.df, aes(x=long, y=lat, group=group, fill=as.numeric(both_p))) +
  geom_polygon()+
  geom_path(color="black", size=0.1)+
  scale_fill_gradient("% species in both databases", guide="colorbar", low="#feff80", high="#6b0000")+
  guides(fill = guide_colourbar(barwidth = 30, direction="horizontal"))+ # stretch that colorbar
  coord_equal()+ # otherwise maps looks distorted
  theme_void()+
  theme(legend.position = "bottom")
ggsave(filename="database_presences.png", dpi=600, width=10, height=7)

ggplot(shp.df, aes(x=long, y=lat, group=group, fill=as.numeric(bien_perc))) +
  geom_polygon()+
  geom_path(color="black", size=0.1)+
  scale_fill_gradient("% species in BIEN only", guide="colorbar", low="#feff80", high="#6b0000")+
  guides(fill = guide_colourbar(barwidth = 30, direction="horizontal"))+ # stretch that colorbar
  coord_equal()+ # otherwise maps looks distorted
  theme_void()+
  theme(legend.position = "bottom")
ggsave(filename="bien_only_presences.png", dpi=600, width=10, height=7)


ggplot(shp.df, aes(x=long, y=lat, group=group, fill=as.numeric(wcsp_perc))) +
  geom_polygon()+
  geom_path(color="black", size=0.1)+
  scale_fill_gradient("% species in WCSP only", guide="colorbar", low="#feff80", high="#6b0000")+
  guides(fill = guide_colourbar(barwidth = 30, direction="horizontal"))+ # stretch that colorbar
  coord_equal()+ # otherwise maps looks distorted
  theme_void()+
  theme(legend.position = "bottom")
ggsave(filename="wcsp_only_presences.png", dpi=600, width=10, height=7)

save.image(file="analyse.RData")


# per species:

