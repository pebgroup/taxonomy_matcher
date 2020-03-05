rm(list=ls())

data <- readRDS("all_bien_occurrences_6cols.rds")

data <- as.data.frame(data)
cc <- which(complete.cases(data[,c(3:6)]))

data2 <- data[cc,]
  
write.csv(data2, "all_bien_occurrences_6cols_rm_na.csv", row.names=FALSE, sep = "|")
