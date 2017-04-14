partitionMatrix <- NULL
KMEANS_ERROR <- 0.001
KMEANS_MAX_NUM_ITERATIONS <- 200;
kmeansPrototypes <- NULL

#initialize prototype arrays with random elements from the dataset
initializePrototypes <- function(dataset, k) {
  indexes <- sample(0:nrow(dataset), k)
  kmeansPrototypes <<- dataset[indexes,]
}

#initialize partion matrix with 0
initializePartitionMatrix <- function(n) {
  partitionMatrix <<- rep(0, n)
}

# just a clean up method if necessary
clearKMeans <- function () {
  rm(partitionMatrix)
  rm(kmeansPrototypes)
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#Determ the stop condition
shouldStop <- function(oldCentroid, newCentroid, iterations) {
  if (is.null(oldCentroid)) return(FALSE)
  
  #check if the difference between the prototypes are not smalled than our error
  isPrototypeStable <- all(abs(oldCentroid - newCentroid) < KMEANS_ERROR)
  if (KMEANS_MAX_NUM_ITERATIONS <= iterations || isPrototypeStable) {
    printf("parou: iteracoes: %s ficou estavel: %s", iterations, isPrototypeStable)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#update partition matrix
updatePartitionMatrix <- function(dataset) {
  numPrototypes <- nrow(kmeansPrototypes)
  for (i in 1:nrow(dataset)) {
    closest <- 999 #stupid default number
    smallerDistance <- .Machine$double.xmax#stupid default number
    for(j in 1:numPrototypes) {
      distance = euc.dist(dataset[i,], kmeansPrototypes[j,])
      if (distance < smallerDistance) {
        closest <- j
        smallerDistance <- distance
      }
    }
    partitionMatrix[i] <- closest #set the proper group
  }
  return(partitionMatrix)
}

updatePrototypes <- function(dataset, k) {
  # for each k
  for (cluster in 1:k) {
    indexes <- which(partitionMatrix == cluster) #all elements that belongs to that class
    kmeansPrototypes[cluster,] <- colMeans(dataset[indexes,]) #generate a mean for each property
  }
  return(kmeansPrototypes)
}

#run K-means
ourKMeans <- function(dataset, k) {
  numElements <- nrow(dataset)
  initializePartitionMatrix(numElements)
  initializePrototypes(dataset, k)
  
  iterations <- 0
  oldPrototypes <- NULL
  while(!shouldStop(oldPrototypes, kmeansPrototypes, iterations)) {
    oldPrototypes <- kmeansPrototypes
    #print(oldPrototypes)
    iterations <- iterations + 1
    
    partitionMatrix <<- updatePartitionMatrix(dataset)
    kmeansPrototypes <<- updatePrototypes(dataset, k)
    #print(kmeansPrototypes)
  }
}

printf <- function(...) invisible(print(sprintf(...)))
