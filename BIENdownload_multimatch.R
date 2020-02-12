# old version:
# library(BIEN)
# tryCatch({
# 	all_bien_occurrences <- BIEN:::.BIEN_sql(query = "SELECT taxonobservation_id, scrubbed_species_binomial, latitude, longitude FROM view_full_occurrence_individual;")
# 	print(dim(all_bien_occurrences)[1])
# 	saveRDS(all_bien_occurrences, "all_bien_occurrences.rds")
# })


# new version
## only  selected species 
library(BIEN)
BIEN_occurrence_species_mod <- function (species, only.new.world = FALSE, ...) 
{
  .is_log(only.new.world)
  .is_char(species)
  newworld_ <- .newworld_check(only.new.world)
  query <- paste("SELECT scrubbed_species_binomial,scrubbed_taxon_name_no_author,scrubbed_author,taxonobservation_id,latitude, longitude, taxon_rank, scrubbed_family",
                 "\n                 FROM view_full_occurrence_individual \n                 WHERE scrubbed_species_binomial in (", 
                 paste(shQuote(species, type = "sh"), collapse = ", "), 
                 ")",  
                 "\n                 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) \n                 ORDER BY scrubbed_species_binomial ;")
  return(.BIEN_sql(query, ...))
}

assignInNamespace('BIEN_taxonomy_species',BIEN_occurrence_species_mod,ns='BIEN')
environment(BIEN_occurrence_species_mod)<-asNamespace('BIEN') 

test <- BIEN_occurrence_species_mod("Nepeta x faasenii", only.new.world = FALSE)

prob.species <- readRDS("multimatch_species_BIEN.rds")
bien_multimatch_species <- BIEN_occurrence_species_mod(prob.species[1:2000], only.new.world = FALSE)
saveRDS(bien_multimatch_species, "bien_multimatches.rds")


chunks <- c(seq(1,11000,1000), length(prob.species))
for(i in 9:(length(chunks)-1)){
  bien_mm <- BIEN_occurrence_species_mod(prob.species[chunks[i]:chunks[i+1]], only.new.world = FALSE)
  saveRDS(bien_mm, file=paste0("bien_mm_", i, ".rds"))
  rm(bien_mm)
}
bien_mm_2 <- BIEN_occurrence_species_mod(prob.species[1001:2000], only.new.world = FALSE)
saveRDS(bien_mm_2, file="bien_mm_2.rds")

## all species
# library(BIEN)
# tryCatch({
# 	all_bien_occurrences <- BIEN:::.BIEN_sql(query = "SELECT taxonobservation_id, scrubbed_species_binomial, latitude, longitude FROM view_full_occurrence_individual;")
# 	print(dim(all_bien_occurrences)[1])
# 	saveRDS(all_bien_occurrences, "all_bien_occurrences.rds")
# })