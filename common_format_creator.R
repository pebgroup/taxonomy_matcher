# builds a common format to feed into the  taxonomy matcher

# Get data

## BIEN
#  Switch to chose data volume, based on directory (server or local)
if(getwd()=="/data_vol/melanie/BIEN_download"){
  bien <- read.csv(file="all_bien_occurrences_7cols_rm_na.csv")
}else{
  chunk_size <- 1000000 # choose the best size for you
  bien <- read.csv(file="database/all_bien_occurrences_7cols_rm_na.csv",nrows=chunk_size)
}

# clean data
bien <- bien[!abs(bien$latitude)>90,] # remove species with impossible coordinates
bien <- bien[!abs(bien$longitude)>180,] # remove species with impossible coordinates


# unique the species - time to subset
## create a unique taxon ID that contains all possible information:
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
                         comment = rep(NA, nrow(bien_sub))
)

# order the dataframe by split length
bien_input <- bien_input[order(bien_input$split_length),]
bien_input$id <- c(1:nrow(bien_input))

split_list <- strsplit(as.character(bien_input$scrubbed_taxon_name_no_author), split = " ")
names(split_list) <- bien_input$id


# Cases with just one possible option

## split length == 1
ind <- which(bien_input$split_length==1)
bien_input$genus[ind] <- as.character(bien_input$scrubbed_taxon_name_no_author[ind])

## split length == 5
ind <- which(bien_input$split_length==5)
bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
bien_input$species_hybrid[ind] <- sapply(split_list[ind], "[[", 2)
bien_input$species[ind] <- sapply(split_list[ind], "[[", 3)
bien_input$taxon_rank[ind] <- sapply(split_list[ind], "[[", 4)
bien_input$infra_name[ind] <- sapply(split_list[ind], "[[", 5)

## split length == 6 
ind <- which(bien_input$split_length==6)
bien_input$genus[ind] <- sapply(split_list[ind], "[[", 1)
bien_input$species_hybrid[ind] <- sapply(split_list[ind], "[[", 2)
bien_input$species[ind] <- sapply(split_list[ind], "[[", 3)
bien_input$taxon_rank[ind] <- sapply(split_list[ind], "[[", 4)
bien_input$infra_name[ind] <- sapply(split_list[ind], "[[", 5)

# Cases with > 1 possible option

for(i in 1:nrow(bien_input)){
  
  # split length == 2
  if(bien_input$split_length[i]==2){
    if(split_list[[i]][1]=="x"){
      bien_input$genus_hybrid[i] <- "x"
      bien_input$genus[i] <- split_list[[i]][[2]]
    }else{
      bien_input$genus[i] <- split_list[[i]][[1]]
      bien_input$species[i] <- split_list[[i]][[2]]
    }
    
  }
  
  # split length == 3
  if(bien_input$split_length[i]==3){
    if(split_list[[i]][1]=="x"){
      bien_input$genus_hybrid[i] <- "x"   # ???
      bien_input$genus[i] <- split_list[[i]][[2]]
      bien_input$species[i] <- split_list[[i]][[3]]
    }
    if(split_list[[i]][2]=="x"){
      if(split_list[[i]][2]=="x" & grepl("[A-Z]",split_list[[i]][3])){
        bien_input$genus_hybrid[i] <- "x"
        # WHICH GENUS TO RECORD??
      }else{
        bien_input$species_hybrid[i] <- "x"
        bien_input$genus[i] <- split_list[[i]][[1]]
        bien_input$species[i] <- split_list[[i]][[3]]
      }
    }
    if(!any(grepl("x", split_list[[i]]))){bien_input$comment[i] <- "no hybrid"}
  }
  
  #split length == 4
  if(bien_input$split_length[i]==4){
    if(split_list[[i]][[1]]=="x" & split_list[[i]][[3]]=="x"){
      bien_input$genus_hybrid[i] <- "x"
      bien_input$species_hybrid[i] <- "x"
      bien_input$genus[i] <- split_list[[i]][[2]]
      bien_input$species[i] <- split_list[[i]][[4]]
    }else{
      bien_input$genus[i] <- split_list[[i]][[1]]
      bien_input$species[i] <- split_list[[i]][[2]]
      bien_input$taxon_rank[i] <- split_list[[i]][[3]]
      bien_input$infra_name[i] <- split_list[[i]][[4]]
    }
  }
  
  #split length == 6
  if(bien_input$split_length[i]==6){bien_input$comment[i] <- "weirdo"}
  
  if(!i%%100)cat(i,"\r")    
  
}

saveRDS(bien_input, "bien_input.rds")