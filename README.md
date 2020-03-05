# TO DO

 * create documentation markdown for *taxonomic_matcherV1.2_MT.R*.
 
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

* *[BIEN_common_format_documentation](BIEN_common_format_documentation.md)*

* *[NCBI_common_format_documentation](/NCBI_taxonomy/README.md)*


**Taxonomic matching**  

*taxonomic_matcherV1.2_MT.R* performs the matching of the selected common format resulting from BIEN or NCBI data with the [World Checklist of Selected Plant Families (WCSP)](https://wcsp.science.kew.org/home.do).


##   Workflow for taxon matching logic
![workflow for matching](workflow_matching.png)