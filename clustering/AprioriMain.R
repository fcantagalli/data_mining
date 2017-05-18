main <- function() {
  #package link: https://cran.r-project.org/web/packages/arules/arules.pdf
  #tutorial: https://en.wikibooks.org/w/index.php?title=Data_Mining_Algorithms_In_R/Frequent_Pattern_Mining/The_Apriori_Algorithm
  #https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/
  
  library(arules)
  library(arulesViz)
  
  #Load Dataset
  #This one is just a mock dataset
  data("Adult")
  rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
  summary(rules)
  
}

main()