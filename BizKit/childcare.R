library(readr)

child.care <- read_csv("~/Downloads/childcare.csv")
child.care <- child.care[,c(2,4,11)]

child.care <- cbind(child.care, sapply (strsplit(child.care$`Physical Address`,","),`[`, 2))
names(child.care) <- c("name","address", "type","suburb")
