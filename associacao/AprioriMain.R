#package link: https://cran.r-project.org/web/packages/arules/arules.pdf
#tutorial: https://en.wikibooks.org/w/index.php?title=Data_Mining_Algorithms_In_R/Frequent_Pattern_Mining/The_Apriori_Algorithm
#https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/

library(arules)
library(arulesViz)

setwd("~/Code/data_mining/associacao")

#preciso pesquisar como carregar os dados para o algoritmo
contraceptivoDataset <- as.matrix(read.csv("contraceptivoDataset", header = FALSE))
t25Dataset <- read.table("T25I10D10", header = FALSE, skip = 1002)

#checar se ha missing na ou nan
for (i in 1:ncol(contraceptivoDataset)) {
    print(table(is.na(contraceptivoDataset[, i])))
    print(table(is.nan(contraceptivoDataset[, i])))
} #nao ha

#Load Dataset
#This one is just a mock dataset
data("Adult")
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)


write.table(contraceptivoDataset, file = "contraceptivo.csv", sep = ",", quote = TRUE, row.names = FALSE)
# I intend to create a csv file, so I use 'sep=","' to separate the entries by a comma, 'quote=TRUE' to quote all the entries, and 'row.names=F to prevent the creation of an extra column containing the row names (which is the default behavior of write.table() )

# Now place the dataset into a "data" directory (either via R or via the operating system, doesn't make any difference):
 dir.create("data")
# create the directory
file.rename(from = "contraceptivo.csv", to = "data/contraceptivo.csv")
# move the file

# Now we can finally load the dataset:
data("contraceptivo")
# data(mydataset) works as well, but quoted is preferable - less risk of conflic
Contraceptivos = as(contraceptivo, "transactions")
rules = apriori(Contraceptivos, parameter = list(support = 0.001, confidence = 0.001))
rules
inspect(head(sort(rules, by = "lift"), 5))
plot(rules)
head(quality(rules))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading="order",control=list(main = "Two-key plot"))
main()