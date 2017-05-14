main <- function() {
  
  source('algorithm/kmeans.R')
  data("iris") #load iris dataset
  dataset <- iris
  dataset$Species <- NULL #drop class column
  k <- 3
  ourKMeans(dataset, k)
  
  #graph
  library(ggplot2)
  ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point(color = partitionMatrix) + 
    geom_point(data=kmeansPrototypes, aes(Petal.Length, Petal.Width), color = "green", shape = 17, size = 2)
  
}

main()