#package link: https://cran.r-project.org/web/packages/arules/arules.pdf
#tutorial: https://en.wikibooks.org/w/index.php?title=Data_Mining_Algorithms_In_R/Frequent_Pattern_Mining/The_Apriori_Algorithm
#https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/

library(arules)
library(arulesViz)

setwd("~/Code/data_mining/associacao")

#preciso pesquisar como carregar os dados para o algoritmo
contraceptivoDataset <- read.csv("contraceptivo", header = FALSE, stringsAsFactors = TRUE)
t25Dataset <- loadT25Dataset()


#Load Dataset
#This one is just a mock dataset

rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)


loadT25Dataset <- function() {
  textFile <- scan(file = "T25I10D10", what = "character", multi.line = TRUE, sep = '\n', skip = 1)
  transactionLabels <- rep(0, 1000)
  for (i in 1:1000) {
    aux <- textFile[i]
    aux <- strsplit(aux, " ", fixed = TRUE)
    transactionLabels[as.numeric(aux[[1]][1])] <- aux[[1]][2]
  }
  
  transactions <- sapply(1002:(length(textFile)-1), FUN = function(i) {textFile[i]})
  return(list(labels = transactionLabels, data = transactions))
}

loadCongressDataset <- function() {
  dataset <- read.csv("congress", header = FALSE)
  dataset$V2 <- sapply(dataset$V2, function (item) { paste("handicapped-infants-", item) })
  dataset$V3 <- sapply(dataset$V3, function (item) { paste("handicapped-infants-", item) })
  for (column in names(dataset)) {
    dataset[,column] <- replace(as.character(dataset[,column]), dataset[,column] == "?", "absent")
  }
  return(dataset)
}

concatStrings <- function(x2, x1) {
  paste(x1, x2)
}