#Load datasets
setwd("~/Code/data_mining/MPL")

spambaseDataset <- read.csv("spambase", header = FALSE)
winequalityDataset <- read.table("winequality-red", header = TRUE)

#checar se ha missing na ou nan
for (i in 1:ncol(spambaseDataset)) {
  print(table(is.na(spambaseDataset[,i])))
  print(table(is.nan(spambaseDataset[,i])))
} #nao ha

#checar se ha missig values no wine
for (i in 1:ncol(winequalityDataset)) {
  print(table(is.na(winequalityDataset[,i])))
  print(table(is.nan(winequalityDataset[,i])))
} #nao ha
