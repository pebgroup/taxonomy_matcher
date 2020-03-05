# Script information 
*common_format_creator_vectorized.R* breaks down taxon name strings from BIEN creating a matrix thats compatible with WCSP data. Requires the 7 column BIEN download (*all_bien_occurrences_7cols_rm_na.csv*, located on the linux server), needs to be run on the server (ca 45 minutes).

# Workflow

## Preparation
1. Read BIEN data. Required columns: 
   + scrubbed_taxon_name_no_author
   + scrubbed_family
   + scrubbed_species_binomial
   + scrubbed_author
2. Note: our download also includes additional columns with occurrence coordinates, therefore we are cleaning the data and exclude all occurrences with latitude values larger than 90 and longitudes absolute values larger than 180.
3. create unique taxon identifier by combining *scrubbed_taxon_name_no_author* with *scrubbed_author*
4. subset the data. We only need each unique taxon name once.
5. Split *taxon_name_no_author* by space. Calculate length of the resulting elements for each species and store as *split_length*
6. setup data frame to fill in results: *bien_input*. Directly enter family, author, split length, and taxon_name_no_author
7. Assign a unique ID to each taxon name
8. Split *taxon_name_no_author* by space and store results in list *split_list*. Each list entry contains all character strings extracted from taxon names.

## Filling in the data frame
The table in the beginning of each subsection shows the possible taxon format at this split length.

#### Split length == 1
Split length | Possible input | Rank assigned
-------------|----------------|---------------
1	     | Genus	      | genus


1. get rows from *bien_input* with split length 1. 
For these cases:

* assign "Genus" to *bien_input$taxon_rank*
* assign *scrubbed_taxon_name_no_author* to *bien_input$genus*

#### Split length == 2
Split length | Possible input | Rank assigned
-------------|----------------|---------------
2	     | Genus_species  | species


1. get rows from *bien_input* with split length 2.
For these cases:

* assign the first split item to *bien_input$genus*
* assign the second split item to *bien_input$species*
* assign "species" to *bien_input$taxon_rank*


#### Split length == 3
Split length | Possible input | Rank assigned
-------------|----------------|---------------
3	     | Genus\_x_species  | species
3	     | x\_Genus_species  | species
3	     | Genus\_x_Genus  | Genus
3	     | Genus\_xxx_Genus  | Genus
3	     | Genus\_species_species  | undefined

1. get rows from *bien_input* with split length 2.










