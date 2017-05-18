main <- function() {
  # For SOM we are using the package kohonen
  # https://cran.r-project.org/web/packages/kohonen/kohonen.pdf
  # tutorial link: https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/
  
  #Laod package
  library(kohonen)
  require(kohonen)
  #Load dataset and prepare it
  data(wines)
  set.seed(7)
  
  # Center and scale all variables to give them equal importance during
  # the SOM training process. 
  data_train_matrix <- as.matrix(scale(wines))
  
  # Create the SOM Grid - you generally have to specify the size of the 
  # training grid prior to training the SOM. Hexagonal and Circular 
  # topologies are possible
  # essa eh a defini????o da camada topologica
  som_grid <- somgrid(xdim = 6, ydim=6, topo="hexagonal")
  
  # Finally, train the SOM, options for the number of iterations,
  # the learning rates, and the neighbourhood are available
  som_model <- som(data_train_matrix, 
                   grid=som_grid, 
                   rlen=100, 
                   alpha=c(0.1,0.05), 
                   keep.data = TRUE,
                   n.hood="circular" )
  
  #As the SOM training iterations progress, the distance from each node???s 
  #weights to the samples represented by that node is reduced.
  plot(som_model, type="changes")
  
  #visualise the count of how many samples are mapped to each node on the map.
  #Large values in some map areas suggests that a larger map would be benificial. 
  #Empty nodes indicate that your map size is too big for the number of samples.
  plot(som_model, type="count")
  
  #visualisation is of the distance between each node and its neighbours.
  plot(som_model, type="dist.neighbours")
  
  #Heatmaps
  #The default Kohonen heatmap is created by using the type ???heatmap???, 
  #and then providing one of the variables from the set of node weights.
  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
  plot(som_model, type = "property", property = som_model$codes[,1],
       main=names(som_model$data)[1], palette.name=coolBlueHotRed)
}

main()