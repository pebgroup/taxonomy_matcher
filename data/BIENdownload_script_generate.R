tryCatch({
	for (i in 3:10){
		#query nth 20 million = OFFSET (n-1)*20000000
		NN <- 20000000*(i-1)
		print(NN)
		cat("library(BIEN)", 
		"tryCatch({", 
		paste0('all_bien_occurrences <- BIEN:::.BIEN_sql(query = \"SELECT * FROM view_full_occurrence_individual ORDER BY taxonobservation_id LIMIT 20000000 OFFSET ', NN, ';\")', sep=""), 
		"print(dim(all_bien_occurrences)[1)]", 
		paste0('saveRDS(all_bien_occurrences,\"all_bien_occurrences_', i, '.rds\")', sep=""),
		"})",
		file=paste0("BIENdownload_", i, ".R", sep=""), 
		sep="\n")
	}
})
