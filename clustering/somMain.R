
# For SOM we are using the package kohonen
# https://cran.r-project.org/web/packages/kohonen/kohonen.pdf
# tutorial link: https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/

#Laod package
library(kohonen)
require(kohonen)
#Load dataset and prepare it

setwd("~/Code/data_mining/clustering")
espiralDataset <- read.table(file = "espiral", header = FALSE)
#espiralDataset <-as.matrix( read.csv(file="C:/Users/li_fb/AppData/Local/Temp/espiral.txt.utf8", header=FALSE, row.names=NULL, encoding="UTF-8", sep="", dec=".", quote="\"", comment.char=""))
t48Dataset <- read.table(file = "t48", header = FALSE)

#checar se ha missing na ou nan
for (i in 1:ncol(espiralDataset)) {
  print(table(is.na(espiralDataset[,i])))
  print(table(is.nan(espiralDataset[,i])))
} #nao ha

#checar se ha missing na ou nan
for (i in 1:ncol(t48Dataset)) {
  print(table(is.na(t48Dataset[,i])))
  print(table(is.nan(t48Dataset[,i])))
} #nao ha

# Center and scale all variables to give them equal importance during
# the SOM training process. 
data_train_matrix <- as.matrix(scale(t48Dataset))
data_train_matrix2 <- as.matrix(scale(espiralDataset))


# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
# essa eh a defini????o da camada topologica
som_grid <- somgrid(xdim = 34, ydim=34, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=300,
                 alpha=c(0.2,0.1), 
                 keep.data = TRUE)

som_model2 <- som(data_train_matrix2,
                 grid = som_grid,
                 rlen = 100,
                 alpha = c(0.1, 0.05),
                 keep.data = TRUE,
                 n.hood = "circular")
#As the SOM training iterations progress, the distance from each node???s 
#weights to the samples represented by that node is reduced.
plot(som_model, type="changes")
plot(som_model2, type = "changes")

#visualise the count of how many samples are mapped to each node on the map.
#Large values in some map areas suggests that a larger map would be benificial. 
#Empty nodes indicate that your map size is too big for the number of samples.
plot(som_model, type="count", palette.name)

#visualisation is of the distance between each node and its neighbours.
plot(som_model, type="dist.neighbours", palette.name = grey.colors)

plot(som_model, type="codes")

#Heatmaps
#The default Kohonen heatmap is created by using the type ???heatmap???, 
#and then providing one of the variables from the set of node weights.
#coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
#lot(som_model, type = "property", property = som_model$codes[,1],
#     main=names(som_model$data)[1], palette.name=coolBlueHotRed)

plot(som_model, type = "property", property = som_model$codes[[1]][,2], palette.name=coolBlueHotRed)

var <- 2 #define the variable to plot
var_unscaled <- aggregate(as.numeric(t48Dataset[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(t48Dataset)[var], palette.name=coolBlueHotRed)



################-----------------------------


plot(t48Dataset[,1], t48Dataset[,2], main="t48 Dataset",
     xlab="V1", ylab="V2", pch=19)

library(hexbin)
bin<-hexbin(t48Dataset[,1], t48Dataset[,2], xbins=35)
plot(bin, main="Hexagonal Binning t48 Dataset") 

library(scatterplot3d)
scatterplot3d(espiralDataset[,1],espiralDataset[,2],espiralDataset[,3], xlab="V1",  ylab = "V2", zlab = "V3", main="3D Scatterplot")
