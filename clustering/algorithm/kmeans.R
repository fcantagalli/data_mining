partitionMatrix <- NULL
KMEANS_ERROR <- 0.1
KMEANS_MAX_NUM_ITERATIONS <- 200;
kmeansPrototypes <- NULL

#initialize prototype arrays with random elements from the dataset
initializePrototypes <- function(dataset, k) {
  indexes <- sample(0:nrow(dataset), k)
  kmeansPrototypes <<- dataset[indexes,]
}

#initialize partion matrix with 0
initializePartitionMatrix <- function(k, n) {
  partitionMatrix <<- matrix(0, k, n)
}

# just a clean up method if necessary
clearKMeans <- function () {
  rm(partitionMatrix)
  rm(kmeansPrototypes)
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#Determ the stop condition
shouldStop <- function(oldCentroid, newCentroid, iterations) {
  #check if the difference between the prototypes are not smalled than our error
  isPrototypeStable <- all(abs(oldCentroid - newCentroid) < KMEANS_ERROR)
  return(KMEANS_MAX_NUM_ITERATIONS >= iterations || isPrototypeStable)
}

#update partition matrix
updatePartitionMatrix <- function(dataset, prototypes) {
  numPrototypes <- nrow(prototypes)
  for (i in 1:nrow(dataset)) {
    closest <- 999 #stupid default number
    smallerDistance <- .Machine$double.xmax#stupid default number
    for(j in 1:numPrototypes) {
      if (euc.dist(dataset[i,], prototypes[j,]) < smallerDistance) closest <- j
    }
    partitionMatrix[,i] <- 0 #reset to 0
    partitionMatrix[closest,i] <- 1 #set the proper group
  }
  return(partitionMatrix)
}



#run K-means
ourKMeans(dataset, k) {
  numElements <- nrow(dataset)
  initializePartitionMatrix(k, numElements)
  initializePrototypes(dataset, k)
  
  iterations <- 0
  oldPrototypes <- NULL
  
  while(!shouldStop(oldPrototypes, kmeansPrototypes, iterations)) {
    oldPrototypes <- kmeansPrototypes
    iterations <- iterations + 1
    
    partitionMatrix <<- updatePartitionMatrix(dataset, kmeansPrototypes)
    update
  }
  
}
