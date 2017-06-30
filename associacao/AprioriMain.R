#package link: https://cran.r-project.org/web/packages/arules/arules.pdf
#tutorial: https://en.wikibooks.org/w/index.php?title=Data_Mining_Algorithms_In_R/Frequent_Pattern_Mining/The_Apriori_Algorithm
#https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/

library(arules)
library(arulesViz)

setwd("~/Code/data_mining/associacao")

#preciso pesquisar como carregar os dados para o algoritmo
contraceptivoDataset <- read.csv("contraceptivo", header = FALSE)
t25Dataset <- read.table("T25I10D10", header = FALSE, skip = 1002)

#checar se ha missing na ou nan
for (i in 1:ncol(espiralDataset)) {
  print(table(is.na(espiralDataset[,i])))
  print(table(is.nan(espiralDataset[,i])))
} #nao ha

#Load Dataset
#This one is just a mock dataset
data("Adult")
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)

main()