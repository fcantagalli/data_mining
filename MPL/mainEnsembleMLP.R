#Load datasets
setwd("~/Code/data_mining/MPL")

require(MLmetrics)
source("MLP.R")

set.seed(1)
dataset <- read.table("seed", header = FALSE)
#spambaseDataset <- read.csv("spambase", header = FALSE)
#winequalityDataset <- read.table("winequality-red", header = TRUE)

#EN.checkForMissingValues(dataset)

#process dataset to normalize it
minmax <- datasetMinMax(dataset)
dataset <- normalizeDataset(dataset, minmax)

n_classifiers <- 5
l_rate <- 0.3
n_epoch <- 400
list_of_n_hidden <- c(5, 5, 5, 5, 5)

scores <- EN.evaluateAlgorithm(dataset, EN.trainTestEnsembleMPL, n_classifiers, l_rate, n_epoch, list_of_n_hidden)

EN.evaluateAlgorithm <- function (dataset, algorithm, ...) {
  # 75% treinamento 25% teste
  sample_size <- floor(0.75 * nrow(dataset))
  train_set_indexes <- sample(seq_len(nrow(dataset)), size = sample_size)
  train_set <- dataset[train_set_indexes,]
  test_set <- dataset[-train_set_indexes,]
  
  predicted <- algorithm(train_set, test_set, ...)
  actual <- test_set[,ncol(test_set)]
  accuracy <- Accuracy(y_pred = predicted, y_true = actual)
  print(accuracy)
  return(list(actual = actual,predicted = predicted))
}

EN.trainTestEnsembleMPL <- function(train, test, n_classifiers, l_rate, n_epoch, list_of_n_hidden) {
  n_inputs <- ncol(train) - 1
  n_outputs <- length(unique(train[,ncol(train)]))
  network <- EN.trainMLPEnsemble(train, n_classifiers, l_rate, n_epoch, list_of_n_hidden)
  predictions <- sapply(1:nrow(test), function (rowIndex) {
    rowTest <- test[rowIndex, -ncol(test)]
    EN.predict(network, rowTest)
  })
  print(predictions)
  return(predictions)
}

EN.predict <- function(trainedEnsemble, row) {
  predictions <- sapply(1:length(trainedEnsemble), function (i) {
    return(predict(trainedEnsemble[[i]], row))
  })
  print(predictions)
  return(EN.majorityVote(predictions))
}

EN.majorityVote <- function(votes) {
  votesTable <- table(votes)
  return(as.numeric(names(which.max(votesTable))))
}

EN.trainMLPEnsemble <- function(train, n_classifiers, l_rate, n_epoch, list_of_n_hidden) {
  n_inputs <- ncol(train) - 1
  n_outputs <- length(unique(train[,ncol(train)]))
  ensemble <- lapply(X = 1:n_classifiers, function (i) {
    network <- createNetwork(n_inputs, list_of_n_hidden[i], n_outputs)
    bootstrapedSample <- EN.bootstrapSample(train)
    network <- trainNetwork(network, bootstrapedSample, l_rate, n_epoch, n_outputs)
  })
  return(ensemble)
}

EN.bootstrapSample <- function (dataset)  {
  sampledData <- sample(x = seq_len(nrow(dataset)), size = nrow(dataset), replace = TRUE)
  return(dataset[sampledData,])
}

EN.checkForMissingValues <- function(dataset) {
  #checar se ha missing na ou nan
  for (i in 1:ncol(dataset)) {
    print(table(is.na(dataset[,i])))
    print(table(is.nan(dataset[,i])))
  } #nao ha
}
