#!/bin/bash

t_file=$1
output_file=$2

#this bash script will clean some "ugly" entries, and
#unclassified_
#_incertae_sedis
#_clade
#_superclade
#_Group
#_group
#_complex
#_(type_*)
#_lineages
#C3_
#C4_
#_sensu_lato
#_samples
#_alliance
#_division
#_hybrid_
#_cultivar
#_subgroup
#_form
#_mixed_ (example: Amaranthus_cruentus/Amaranthus_hypocondriacus_mixed_library,species)
#ungrouped_
#unpublished_
#no rank #keep
#_cybrid (e.g, 1520274,Nicotiana_tabacum/Hyoscyamus_niger_cybrid)
#class
#forma #keep
#section
#series
#species group
#subclass
#subfamily
#subgenus
#suborder
#subsection
#subtribe
#tribe
#cf.
#aff.
#environmental
#from
#sample
#uncultured_
#fossil_
#undetermined_
#_addition_line
#also remove extra "_" and replace"," as "_"
#_var._floribunda_var._floribunda_

#reformat is as "NCBI_id,species,rank"

sed 's/ /_/g;s/,/_/g;s/_\+/_/g;' $t_file >${output_file}.tmp
sed -i '/fossil_/d;/environmental_/d;/_sample/d;/fossil_/d;/_alien_/d;/undetermined_/d;/_addition/d;/_line/d;/uncultured_/d;/_from_/d;/unclassified_/d;/_incertae_sedis/d;/core_/d;/New_/d;/_clade/d;/_superclade/d;/_Group/d;/_group/d;/_host/d;/_mixed_/d;/_complex/d;/_\?cybrid_\?/d;/_(\?type_.*)\?/d;/_lineages/d;/C3_/d;/C4_/d;/_sensu_lato/d;/_samples/d;/_alliance/d;/_division/d;/_hybrid_/d;/_cultivar/d;/_subgroup/d;/_form/d;/_haplotype.*\?_/d;/_cf._/d;/_aff._/d;/ungrouped_/d;/unpublished_/d' ${output_file}.tmp


# sed -i '/no_rank/d' Spermatophyta58024_plnDB02032020_nodupl_bashcleaned.csv
sed '/	class	/d;/	section	/d;/	series	/d;/	species group	/d;/	subclass	/d;/	subfamily	/d;/	subgenus	/d;/	suborder	/d;/	subsection	/d;/	subtribe	/d;/	tribe	/d' ${output_file}.tmp|cut -f1,5,7 --output-delimiter "," >${output_file}.csv

#rescue one record with lowercase in genus name
sed -i 's/2497466,sidium_rotundidiscum_/2497466,Sidium_rotundidiscum_/g' ${output_file}.csv
rm ${output_file}.tmp


