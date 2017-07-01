#Load dataset
setwd("~/Code/data_mining/MPL")
source("MLP.R")
require(MLmetrics)

#dataset <- read.table("seed", header = FALSE)

#simpleNetwork <- createNetwork(2,1,2)

#network$hiddenLayer[[1]]$weights <- c(0.13436424411240122, 0.8474337369372327)
#network$hiddenLayer[[1]]$bias <- 0.763774618976614
#network$hiddenLayer[[2]]$weights <- c(0.2550690257394217, 0.49543508709194095)
#network$hiddenLayer[[2]]$bias <- 0.4494910647887381

#network$outputLayer[[1]]$weights <- c(0.651592972722763, 0.7887233511355132)
#network$outputLayer[[1]]$bias <- 0.0938595867742349
#network$outputLayer[[2]]$weights <- c(0.02834747652200631, 0.8357651039198697)
#network$outputLayer[[2]]$bias <- 0.43276706790505337


#---------------------------------------------------------------
set.seed(1)
#dataset <- read.table("seed", header = FALSE)
dataset <-read.table("winequality-red", header = TRUE)
minmax <- datasetMinMax(dataset)
dataset <- normalizeDataset(dataset, minmax)

l_rate <- 0.4
n_epoch <- 10
n_hidden <- 5
n_outputs <- 10

repeat {
  if (n_epoch == 500) {
    break;
  }
  scores <- evaluateAlgorithm(dataset, trainTestMPL, l_rate, n_epoch, n_hidden, n_outputs)
  n_epoch <- n_epoch + 10
}

#print('Mean Accuracy: %.3f%', (sum(scores)/float(len(scores))))

evaluateAlgorithm <- function (dataset, algorithm, ...) {
  # 75% treinamento 25% teste
  sample_size <- floor(0.75 * nrow(dataset))
  train_set_indexes <- sample(seq_len(nrow(dataset)), size = sample_size)
  train_set <- dataset[train_set_indexes,]
  test_set <- dataset[-train_set_indexes,]
  
  predicted <- algorithm(train_set, test_set, ...)
  actual <- test_set[,ncol(test_set)]
  accuracy <- Accuracy(y_pred = predicted, y_true = actual)
  printf("Accuracy: %f", accuracy)
  return(list(predicted = predicted, actual = actual))
}

trainTestMPL <- function(train, test, l_rate, n_epoch, n_hidden, n_outputs = NA) {
  n_inputs <- ncol(train) - 1
  if (is.na(n_outputs)) 
    n_outputs <- length(unique(train[,ncol(train)]))
  network <- createNetwork(n_inputs, n_hidden, n_outputs)
  network <- trainNetwork(network, train, l_rate, n_epoch, n_outputs)
  predictions <- sapply(1:nrow(test), function (rowIndex) {
    rowTest <- test[rowIndex, -ncol(test)]
    predict(network, rowTest)
  })
  #print(predictions)
  return(predictions)
}
