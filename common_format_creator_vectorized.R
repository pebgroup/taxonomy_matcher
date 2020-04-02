# builds a common format based on the BIEN data to feed into the taxonomy matcher

# Get data

## BIEN
#  Switch to chose data volume, based on directory (server or local)
if(getwd()=="/data_vol/melanie/BIEN_download"){
  bien <- read.csv(file="all_bien_occurrences_7cols_rm_na.csv")
}else{
  chunk_size <- 2000000 # choose the best size for you
  bien <- read.csv(file="database/all_bien_occurrences_7cols_rm_na.csv",nrows=chunk_size)
}

# clean data
bien <- bien[!abs(bien$latitude)>90,] # remove species with impossible coordinates
bien <- bien[!abs(bien$longitude)>180,] # remove species with impossible coordinates


# create unique taxon ID that contains all possible information:
bien$taxon_author_ID <- paste(bien$scrubbed_taxon_name_no_author, bien$scrubbed_author)
bien_sub <- bien[!duplicated(bien[,"taxon_author_ID"]),]

# set up common format
split_length <- unlist(lapply(strsplit(as.character(bien_sub$scrubbed_taxon_name_no_author), split = " "), length))
bien_input <- data.frame(scrubbed_taxon_name_no_author=bien_sub$scrubbed_taxon_name_no_author,
                         family = bien_sub$scrubbed_family,
                         author=bien_sub$scrubbed_author, 
                         split_length=split_length,
                         genus_hybrid = rep(NA, nrow(bien_sub)),
                         species_hybrid = rep(NA, nrow(bien_sub)),
                         genus = rep(NA, nrow(bien_sub)),
                         species = rep(NA, nrow(bien_sub)),
                         taxon_rank = rep(NA, nrow(bien_sub)),
                         infra_name = rep(NA, nrow(bien_sub)),
                         comment = rep(NA, nrow(bien_sub)),
                         usable = rep(NA, nrow(bien_sub)),
                         taxon_author_ID = bien_sub$taxon_author_ID
)

# order the dataframe by split length
bien_input <- bien_input[order(bien_input$split_length),]
bien_input$id <- c(1:nrow(bien_input))

#id_name_connect <- bien_input[,c(bien_input$taxon_author_ID, bien_input$id)]
#saveRDS()

split_list <- strsplit(as.character(bien_input$scrubbed_taxon_name_no_author), split = " ")
names(split_list) <- bien_input$id

# Vectorize + loop mix

## split length == 1
ind <- which(bien_input$split_length==1)
bien_input$genus[ind] <- as.character(bien_input$scrubbed_taxon_name_no_author[ind])
bien_input$taxon_rank[ind] <- "genus"

## split length == 2
ind <- which(bien_input$split_length==2)
bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
bien_input$species[ind] <- sapply(split_list[ind], "[[", 2)
bien_input$taxon_rank[ind] <- "species"

## split length == 3
ind <- which(bien_input$split_length==3)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][1]=="x"){
    bien_input$genus_hybrid[ind[i]] <- "x"
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][2]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][3]
    bien_input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][2]=="x"){ # does not occur but you never know
    if(grepl("[A-Z]",split_list[[ind[i]]][3])){
      bien_input$genus_hybrid[ind[i]] <- "x"
      bien_input$usable[ind[i]] <- "no"
    }else{
      bien_input$species_hybrid[ind[i]] <- "x"
      bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
      bien_input$species[ind[i]] <- split_list[[ind[i]]][[3]]
      bien_input$taxon_rank[ind[i]] <- "species"
    }
  }
  if(split_list[[ind[i]]][1]!="x" & split_list[[ind[i]]][2]!="x" & !grepl("[A-Z]",split_list[[ind[i]]][3])){ 
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    bien_input$taxon_rank[ind[i]] <- NA
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[3]]
    }
  if(split_list[[ind[i]]][2]!="x" & grepl("[A-Z]",split_list[[ind[i]]][3])){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$taxon_rank[ind[i]] <- "genus"
  }
}

## split length == 4
ind <- which(bien_input$split_length==4)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][[1]]=="x" & split_list[[ind[i]]][[3]]=="x"){
    bien_input$genus_hybrid[ind[i]] <- "x"
    bien_input$species_hybrid[ind[i]] <- "x"
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[2]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[4]]
    bien_input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]=="x"){
    bien_input$species_hybrid[ind[i]] <- "x"
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
#    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
    bien_input$taxon_rank[ind[i]] <- "species"
  }
  if(split_list[[ind[i]]][[1]]!="x" & split_list[[ind[i]]][[3]]!="x"){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][[1]]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][[2]]
    bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][[3]]
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][[4]]
    if(split_list[[ind[i]]][[3]]==""){bien_input$taxon_rank[ind[i]] <- NA}
  }
  if(!i%%100)cat(i,"\r")
}
  
## split length == 5
ind <- which(bien_input$split_length==5)
for(i in 1:length(ind)){
  if(split_list[[ind[i]]][2]=="x"){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][1]
    bien_input$species_hybrid[ind[i]] <- split_list[[ind[i]]][2]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][3]
    bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][4]
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][5]
  }
  if(split_list[[ind[i]]][4]=="x"){
    bien_input$genus[ind[i]] <- split_list[[ind[i]]][1]
    bien_input$species_hybrid[ind[i]] <- split_list[[ind[i]]][4]
    bien_input$species[ind[i]] <- split_list[[ind[i]]][2]
    bien_input$taxon_rank[ind[i]] <- split_list[[ind[i]]][3]
    bien_input$infra_name[ind[i]] <- split_list[[ind[i]]][5]
  }
  if(split_list[[ind[i]]][3]=="x"){  # this is aiming at Dieffenbachia nitidipetiolada x d. oerstedii
    bien_input$usable[ind[i]] <- "no"
  }
}

## split length == 6 
ind <- which(bien_input$split_length==6)
# bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
# bien_input$species_hybrid[ind] <- sapply(split_list[ind], "[[", 2)
# bien_input$species[ind] <- sapply(split_list[ind], "[[", 3)
# bien_input$taxon_rank[ind] <- sapply(split_list[ind], "[[", 4)
# bien_input$infra_name[ind] <- sapply(split_list[ind], "[[", 5)
bien_input$usable[ind] <- "no"

  bien_input$usable[bien_input$split_length %in% c(7,8)] <- "no"
  bien_input <- bien_input[-which(bien_input$usable=="no"),]
  
  saveRDS(bien_input, "bien_input_vectorized4.rds")

