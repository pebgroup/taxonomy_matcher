rm(list=ls())

data <- readRDS("all_bien_occurrences_7cols.rds")

data <- as.data.frame(data)
cc <- which(complete.cases(data[,c(5:7)]))

data2 <- data[cc,]
  
write.csv(data2, "all_bien_occurrences_7cols_rm_na.csv", row.names=FALSE, sep = "|")
