library(BIEN)
tryCatch({
  	all_bien_occurrences <- BIEN:::.BIEN_sql(query = "SELECT scrubbed_family,scrubbed_species_binomial,
  	                                       scrubbed_taxon_name_no_author,
  	                                       scrubbed_author,taxonobservation_id,latitude,longitude 
	                                         FROM view_full_occurrence_individual;")
	print(dim(all_bien_occurrences)[1])
	saveRDS(all_bien_occurrences, "all_bien_occurrences_7cols.rds")
})

##### TESTRUN #####
# test_download <- BIEN:::.BIEN_sql(query = "SELECT scrubbed_family,scrubbed_species_binomial,
#   	                                       scrubbed_taxon_name_no_author,
#   	                                       scrubbed_author,taxonobservation_id,latitude,longitude 
# 	                                         FROM view_full_occurrence_individual LIMIT 1000000;")
# 
# 
# test_split <- strsplit(test_download$scrubbed_taxon_name_no_author, " ")
# table(unlist(lapply(test_split, length)))
# 
# test_split[which(unlist(lapply(test_split, length))==3)]
