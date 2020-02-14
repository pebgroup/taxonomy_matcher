# WCSP mete script EMil
## how often are species assigned to differing accepted plant name IDs?


wc_all <- readRDS("wcp_dec_19.rds")
all_data_clean <- wc_all[!is.na(wc_all$species) & !is.na(wc_all$accepted_plant_name_id),]
relevant_data <- data.frame(paste(all_data_clean$genus, all_data_clean$species), all_data_clean$accepted_plant_name_id)
colnames(relevant_data)=c("species", "id")

lu.fun <- function(x){length(unique(x))}

#rv_sub <- relevant_data[1:10000,]
#rv_sub <- droplevels(rv_sub)
store <- table(tapply(relevant_data$id, relevant_data$species, lu.fun))
saveRDS(store, "store.rds")

## This does not consider infraspecific information!
