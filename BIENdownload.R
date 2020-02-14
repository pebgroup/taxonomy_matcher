library(BIEN)
all_bien_occurrences <- BIEN:::.BIEN_sql(query = "SELECT * FROM view_full_occurrence_individual;")
saveRDS(all_bien_occurrences, "all_bien_occurrences.rds")
