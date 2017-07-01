#Based on http://machinelearningmastery.com/implement-backpropagation-algorithm-scratch-python/

require(MLmetrics)

#Load dataset
setwd("~/Code/data_mining/MPL")
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
#dataset <- read.table("testdata", header= TRUE)
dataset <- read.table("seed", header = FALSE)
minmax <- datasetMinMax(dataset)
dataset <- normalizeDataset(dataset, minmax)

l_rate <- 0.4
n_epoch <- 500
n_hidden <- 5
#trainedNetwork <- trainNetwork(network, dataset, 0.5, 20, n_outputs)
scores <- evaluateAlgorithm(dataset, trainTestMPL, l_rate, n_epoch, n_hidden)
#print('Scores: %s' % scores)
#print('Mean Accuracy: %.3f%%' % (sum(scores)/float(len(scores))))

evaluateAlgorithm <- function (dataset, algorithm, ...) {
  # 75% treinamento 25% teste
  sample_size <- floor(0.75 * nrow(dataset))
  train_set_indexes <- sample(seq_len(nrow(dataset)), size = sample_size)
  train_set <- dataset[train_set_indexes,]
  test_set <- dataset[-train_set_indexes,]
  
  predicted <- algorithm(train_set, test_set, ...)
  actual <- test_set[,ncol(test_set)]
  accuracy <- Accuracy(y_pred = predicted, y_true = actual)
  print(accuracy)
  return(accuracy)
}

trainTestMPL <- function(train, test, l_rate, n_epoch, n_hidden) {
  n_inputs <- ncol(train) - 1
  n_outputs <- length(unique(train[,ncol(train)]))
  network <- createNetwork(n_inputs, n_hidden, n_outputs)
  network <- trainNetwork(network, train, l_rate, n_epoch, n_outputs)
  predictions <- sapply(1:nrow(test), function (rowIndex) {
    predict(network, test[rowIndex,])
  })
  print(predictions)
  return(predictions)
}

#We will organize layers as arrays of dictionaries and treat the whole network as an array of layers.

datasetMinMax <- function(dataset) {
  minmax <- apply(X = dataset, MARGIN = 2, FUN = function(column) {
    return( list(min = min(column), max = max(column)) )
  })
  return(minmax)
}

normalizeDataset <- function(dataset, minmax) {
  for (i in 1:nrow(dataset)) {
    for(j in 1:(ncol(dataset)-1)) {
      dataset[i,j] <- (dataset[i,j] - minmax[[j]]$min) / (minmax[[j]]$max - minmax[[j]]$min)
    }
  }
  return(dataset)
}

#create a single neuron 
createNeuron <- function(n_inputs) {
  neuron <- list(weights = runif(n_inputs, 0.0, 1.0), bias = runif(1, 0.0, 1.0), output = 0.0, delta = 0.0)
  return(neuron)
}

#Create the neural network
createNetwork <- function(n_inputs, n_hidden, n_outputs) {
  hiddenLayer <- lapply(rep(n_inputs, n_hidden), FUN = createNeuron)
  outputLayer <- lapply(rep(n_hidden, n_outputs), FUN = createNeuron)
  network <- list(hiddenLayer = hiddenLayer, outputLayer = outputLayer)
  return(network)
}

#activate a neuron based on a input
activateNeuron <- function (neuron, input) {
  #activation: soma ponderada do input com os pesos da rede mais o bias
  activationResult <- neuron$bias + sum(neuron$weights * input)
  return(activationResult)
}

#sigmoid function 1 / 1 + e^(-a)
#this is the same as transfer function
activationFunction <- function(weighted_input_sum) {
  afResult <- 1.0 / (1.0 + exp(-weighted_input_sum))
  return(afResult)
}

#derivada da funcao sigmoid
#This is the same as transfer_derivative function
activationFunctionDerivate <- function(output) {
  return (output * (1.0 - output))
}

#Forward propagate input to a network output
forwardPropagate <- function(network, row) {
  inputs <- row
  network <- lapply(network, function(layer) {
    new_inputs <- rep(0, length(layer))
    for (i in 1:length(layer)) {
      activation <- activateNeuron(layer[[i]], inputs)
      layer[[i]]$output <- activationFunction(activation)
      new_inputs[i] <- layer[[i]]$output
    }
    inputs <<- new_inputs
    return(layer)
  })
  return(network)
}

#backpropagation error and store in neuron
backwardPropagateError <- function(network, expected) {
  networkLength <- length(network)
  for (i in networkLength:1) {
    layer <- network[[i]]
    errors <- c()
    if (i != networkLength) { #hidden layer
      #for (neuron in layer) {
      for (j in 1:length(layer)) {
        #calculate the error for the hidden layer
        error <- 0.0
        for (upperNeuron in network[[i +1]]) {
        #for (upperNeuronIndex in 1:length(network[[i + 1]])) {
          error <- error + (upperNeuron$weights[j] * upperNeuron$delta)
        }
        #append error to the output
        errors <- c(errors, error)
      }
    } else { #output layer
      errors <- sapply(1:length(layer), function(index) {
        neuron <- layer[[index]]
        return(expected[index] - neuron$output)
      })
    }
    for (index in 1:length(layer)) {
      neuron <- layer[[index]]
      neuron$delta = errors[index] * activationFunctionDerivate(neuron$output)
      layer[[index]] <- neuron
    }
    network[[i]] <- layer
  }
  return(network)
}

#return a new wieght taking into account the error, input and learning rate
updatedWeight <- function(weight, learning_rate, error, input = 1.0) {
  return(weight + (learning_rate * error * input))
}

#update network weights
updateNetworkWeights <- function(network, row, l_rate) {
  for (i in 1:length(network)) {
    inputs <- head(row, -1)
    if (i != 1) { #adapt this part for multi hidden layers
      inputs <- sapply(network[[i-1]],FUN = function(neuron) {return(neuron$output)})
    }
    network[[i]] <- lapply(network[[i]], function(neuron) {
      neuron$weights <- sapply(1:length(inputs), function (j) {
        return(updatedWeight(neuron$weights[j], l_rate, neuron$delta, inputs[j]))
      })
      neuron$bias <- updatedWeight(neuron$bias, l_rate, neuron$delta)
      return (neuron)
    })
  }
  return(network)
}


#train network
#online learning - error is calculated for each training pattern
trainNetwork <- function(network, train_dataset, l_rate, n_epoch, n_outputs) {
  for (epoch in 1:n_epoch) {
    sum_error <- 0
    for (rowIndex in 1:nrow(train_dataset)) {
      row <- as.numeric(train_dataset[rowIndex,])
      network <- forwardPropagate(network, head(row, -1)) #checked
      outputs <- getNetworkOutputs(network)
      expected <- rep(0, n_outputs)
      expected[row[length(row)]] <- 1
      sum_error <- sum_error + sum((expected - outputs)^2)
      network <- backwardPropagateError(network, expected)
      network <- updateNetworkWeights(network, row, l_rate)
    }
    printf('>epoch=%d, lrate=%.3f, error=%.3f',epoch, l_rate, sum_error)
  }
  return(network)
}

predict <- function (network, row) {
  network <- forwardPropagate(network, row)
  outputs <- getNetworkOutputs(network)
  print(outputs)
  return (which.max(outputs))
}

getNetworkOutputs <- function (network) {
  return(sapply(network$outputLayer, function(neuron) {neuron$output}))
}

printf <- function(...) invisible(print(sprintf(...)))
