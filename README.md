# BIEN

Code for working with BIEN and WCSP data.
Common format creater script to get consistent taxonomy notation and a taxon merger script

*common_format_creator_vectorized.R* breaks down taxon name strings from BIEN creating a matrix thats compatible with WCSP data. Requires the 7 column BIEN download (*all_bien_occurrences_7cols_rm_na.csv*, located on the linux server), needs to be run on the server (ca 45 minutes).

*taxon_matching_new.R* needs both *wcp_dec_19.rds* (located on the linux server) and the resulting rds file from *common_format_creator_vectorized.R*.


![workflow for matching](workflow_matching.png)