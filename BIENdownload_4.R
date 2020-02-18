library(BIEN)
tryCatch({
all_bien_occurrences <- BIEN:::.BIEN_sql(query = "SELECT * FROM view_full_occurrence_individual ORDER BY taxonobservation_id LIMIT 20000000 OFFSET 60000000;")
print(dim(all_bien_occurrences)[1])
saveRDS(all_bien_occurrences,"all_bien_occurrences_4.rds")
})
