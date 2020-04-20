library(BIEN)
tryCatch({
  	all_bien_occurrences <- BIEN:::.BIEN_sql(query = "SELECT scrubbed_family,scrubbed_species_binomial,
  	                                       scrubbed_taxon_name_no_author,
  	                                       scrubbed_author,taxonobservation_id,latitude,longitude,is_geovalid,is_geovalid_issue 
	                                         FROM view_full_occurrence_individual;")
	print(dim(all_bien_occurrences)[1])
	saveRDS(all_bien_occurrences, "full_bien_occurrences_8cols.rds")
})


# Remove NAs
all_bien_occurrences <- as.data.frame(all_bien_occurrences)
cc <- which(complete.cases(all_bien_occurrences[,c(5:7)]))
all_bien_occurrences <- all_bien_occurrences[cc,]

write.csv(all_bien_occurrences, "full_bien_occurrences_8cols_rm_na.csv", row.names=FALSE)



# ##### TESTRUN #####
# test <- BIEN:::.BIEN_sql(query = "SELECT scrubbed_family,scrubbed_species_binomial,
#   	                                       scrubbed_taxon_name_no_author,
#   	                                       scrubbed_author,taxonobservation_id,latitude,longitude,
#   	                                       is_geovalid,is_geovalid_issue,observation_type
# 	                                         FROM view_full_occurrence_individual LIMIT 1000000;")
# 
# 
# data <- as.data.frame(test)
# cc <- which(complete.cases(data[,c(5:7)]))
# data2 <- data[cc,]
# 
# write.csv(data2, "test_bien_download.csv", row.names=FALSE)
# write.table(data2, "test_bien_download_sep.csv", row.names=FALSE, sep = "|")
# 
# dat <- read.csv("test_bien_download.csv")
# head(dat)
# 
# # tests
# geo_invalids <- data2[which(data2$is_geovalid==0),]
# View(geo_invalids)
# 
# library(maps)
# map(database = "world", interior = FALSE, mar = c(0,0,0,0))
# points(geo_invalids$longitude, geo_invalids$latitude, pch=3, col="red")
# 
# table(geo_invalids$observation_type)
#   
# # how does BIEN work...?
# library(BIEN)
# aa <- BIEN_occurrence_species("Abarema brachystachya", only.new.world = FALSE, cultivated = TRUE, natives.only = FALSE, is_geovalid)
# View(aa)
# # 286 rows
# # 287 geovalid entries when downloading from webpage, 871 total entries
# # BIEN::BIEN_occurrence_species gets only geovalidated occurrences
