library(doBy)
#Verify member file
members <- read.fwf(file.choose(), c(6,3,2,1,1), stringsAsFactors = FALSE)
names(members) <- c("id", "x", "age", "gender", "plan")
summaryBy(age ~ age, data = members, FUN = length)

#Verify claims file
claims <- read.fwf(file.choose(), c(6,5), stringsAsFactors = FALSE)
claims$V3 <- sub(" ", "", claims$V2)
names(claims) <- c("id", "dx_factor", "icd9")
summaryBy(id ~ icd9, data = claims, FUN = length)

#Verify mapping file
map <- read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE)
names(map) <- c("icd9", "hcc")

#Verify score_map file
scoreMap <- read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE)
names(scoreMap) <- c("cc", "score")