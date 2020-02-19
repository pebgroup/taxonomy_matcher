# TO DO
 * adjust merge script to keep NCBI IDs
 * generalize names
 * create 2 markdown files: seperate documentations for common format creator BIEN and NCBI  
 
 
# Script information 
*common_format_creator_vectorized.R* breaks down taxon name strings from BIEN creating a matrix thats compatible with WCSP data. Requires the 7 column BIEN download (*all_bien_occurrences_7cols_rm_na.csv*, located on the linux server), needs to be run on the server (ca 45 minutes).

For documentation on the scripts creating the common format, please refer to the corresponding markdown file: 
* *BIEN_common_format_documentation.md*
* *NCBI_common_format_documentation.md*

*taxon_matching_new.R* needs both *wcp_dec_19.rds* (located on the linux server) and the resulting rds file from *common_format_creator_vectorized.R*.

# Workflow for taxon matching
![workflow for matching](workflow_matching.png)