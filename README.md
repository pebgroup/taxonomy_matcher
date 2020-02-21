# TO DO
 * adjust merge script to keep NCBI IDs
 * generalize names
 * create 2 markdown files: seperate documentations for common format creator BIEN and NCBI  
 
 
# Script information  

**BIEN dataset reformat scripts (_R_)**  

*common_format_creator_vectorized.R* breaks down taxon name strings from BIEN creating a matrix thats compatible with WCSP data. Requires the 7 column BIEN download (*all_bien_occurrences_7cols_rm_na.csv*, located on the linux server), needs to be run on the server (ca 45 minutes).  


**NCBI dataset reformat scripts (_Python_)**  

    ncbi_name_extract_V3.py  
    Spermatophyta_plnDB_cleanerV1.1.sh  
    remove_duplicate.py  
    Spermatophyta_sp_authority_format_V5.py
    Spermatophyta_clean3.14snakeV3.py  
    
  _Note: see *[NCBI_common_format_documentation](/NCBI_taxonomy/README.md)* below for explaination of how to run these scripts._  
  
  
  **Documentation**

For documentation on the scripts creating the common format, please refer to the corresponding markdown file:  

* *BIEN_common_format_documentation.md*

* *[NCBI_common_format_documentation](/NCBI_taxonomy/README.md)*


**Taxomimic matching**  

_Run scipt below and apply the taxonomy database from [World Checklist of Selected Plant Families (WCSP)](https://wcsp.science.kew.org/home.do)_  

*taxon_matching_new.R* needs both *wcp_dec_19.rds* (located on the linux server) and the resulting rds file from *common_format_creator_vectorized.R*.

# Workflow for taxon matching
![workflow for matching](workflow_matching.png)