main <- function() {
  
  source('algorithm/kmeans.R')
  data("iris") #load iris dataset
  dataset <- iris
  dataset$Species <- NULL #drop class column
  initializePrototypes(dataset, 3)
  print(partitionMatrix)
}

main()
