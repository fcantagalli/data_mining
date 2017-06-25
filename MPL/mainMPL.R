#Based on http://machinelearningmastery.com/implement-backpropagation-algorithm-scratch-python/

#Load dataset
dataset <- read.table("seed", header = FALSE)

simpleNetwork <- createNetwork(2,1,2)
#We will organize layers as arrays of dictionaries and treat the whole network as an array of layers.

#create a single neuron 
createNeuron <- function(n_inputs) {
  neuron <- list(weights = runif(n_inputs, 0.0, 1.0), bias = 0.0)
  return(neuron)
}

#Create the neural network
createNetwork <- function(n_inputs, n_hidden, n_outputs) {
  hiddenLayer <- lapply(rep(n_inputs, n_inputs), FUN = createNeuron)
  outputLayer <- lapply(rep(n_hidden, n_hidden), FUN = createNeuron)
  network <- list(hiddenLayer = hiddenLayer, outputLayer = outputLayer)
  return(network)
}

#activate a neuron based on a input
activateNeuron <- function (neuron, input) {
  #activation: soma ponderada do input com os pesos da rede mais o bias
  activationResult <- sum(neuron$weights * input) + neuron$bias
  return(activationResult)
}

#sigmoid function 1 / 1 + e^(-a)
#this is the same as transfer function
activationFunction <- function(weighted_input_sum) {
  afResult <- 1.0 / (1.0 + exp(-weighted_input_sum))
  return(afResult)
}