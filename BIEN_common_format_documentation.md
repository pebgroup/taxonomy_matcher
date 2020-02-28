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
1	           | Name	          | genus


For these cases:

* assign "Genus" to *bien_input$taxon_rank*
* assign *scrubbed_taxon_name_no_author* to *bien_input$genus*

#### Split length == 2
Split length | Possible input | Rank assigned
-------------|----------------|---------------
2     	     | Name_name      | species


For these cases:

* assign the first position to *bien_input$genus*
* assign the second position to *bien_input$species*
* assign "species" to *bien_input$taxon_rank*


#### Split length == 3
Split length | Possible input | Rank assigned
-------------|----------------|---------------
3	          | Name\_x_name    | species
3	          | x\_Name_name    | species
3	          | Name\_x_Name    | genus
3	          | Name\_xxx_Name  | genus
3	          | Name\_name_name | NA


Get rows for split length 3 and loop through them asking following conditions:

1. position 1 is "x"

* assign "x" to *bien_input$genus_hybrid*
* assign second position to *bien_input$genus*
* assign third position to *bien_input$species*
* assign "species" to *bien_input$taxon_rank*

2. position 2 is "x"
 
 * if position 3 starts with a capital letter:
    + assign "x" to *bien_input$species_hybrid*
    + mark taxon as "unusable"
 * else:
    + assign "x" to *bien_input$species_hybrid*
    + assign first position to *bien_input$genus*
    + assign third position to *bien_input$species*
    + assign "species" to *bien_input$taxon_rank*

3. no "x" in position 1 or 2, and no capital in position 3 
 
* assign first position to *bien_input$genus*
* assign second position to *bien_input$species*
* assign third position to *bien_input$infra_name*
* assign NA to *bien_input$taxon_rank*

4. no "x" in position 2 and capital in position 3

* assign first position to *bien_input$genus*
* assign "genus" to *bien_input$taxon_rank*


#### Split length == 4
Split length | Possible input          | Rank assigned
-------------|-------------------------|---------------
4	           | x_Name\_x_name          | species
4	           | Name\_name_blank\_name  | species
4	           | Name\_name_name\_name   | species
4	           | Name\_name_x\_name      | species

Get rows for split length 4 and loop through them asking following conditions:

1. position 1 and 3 are "x"

* assign "x" to *bien_input$genus_hybrid*
* assign "x" to *bien_input$species_hybrid*
* assign second position to *bien_input$genus*
* assign fourth position to *bien_input$species*
* assign "species" to *bien_input$taxon_rank*

2. position 3 is "x", position 1 is not "x"

* assign "x" to *bien_input$species_hybrid*
* assign first position to *bien_input$genus*
* assign second position to *bien_input$species*
* assign "species" to *bien_input$taxon_rank*

3. position 1 and 3 are not "x"

* assign first position to *bien_input$genus*
* assign second position to *bien_input$species*
* assign third position to *bien_input$taxon_rank*
* assign forth position to *bien_input$infra_name*
 + if the third position is blank:  assign NA to *bien_input$taxon_rank*


#### Split length == 5
Split length | Possible input          | Rank assigned
-------------|-------------------------|---------------
5	           | Name\_x_name_name\_name | infra
5	           | Name\_name_name\_x_name | infra

Get rows for split length 5 and loop through them asking following conditions:

1. position 2 is "x"

* assign first position to *bien_input$genus*
* assign second position to *bien_input$species_hybrid*
* assign third position to *bien_input$species*
* assign fourth position to *bien_input$taxon_rank*
* assign fifth position to *bien_input$infra_name*

2. position 4 is "x"

* assign first position to *bien_input$genus*
* assign fourth position to *bien_input$species_hybrid*
* assign second position to *bien_input$species*
* assign third position to *bien_input$taxon_rank*
* assign fifth position to *bien_input$infra_name*

3. position 3 is "x"

* assign "no" to *bien_input$usable*


#### Split length == 6, 7 or 8

* assign "no" to *bien_input$usable*

## Cleaning up and saving output

Remove all lines marked as unsuable. 

The final data frame is saved as RDS object *"bien_input_vectorized3.rds"*



