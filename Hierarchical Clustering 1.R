rm(list=ls())

library(datasets)
head(iris)

set.seed(20)

clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)
rect.hclust(clusters, h = 3)

# We can see from the figure that the best choices for total number of clusters are either 3 or 4:

# To do this, we can cut off the tree at the desired number of clusters using cutree.

clusterCut <- cutree(clusters, 3)

table(clusterCut, iris$Species)

# It looks like the algorithm successfully classified all the 
# flowers of species setosa into cluster 1, and virginica into 
# cluster 2, but had trouble with versicolor. If you look at the 
# original plot showing the different species, you can understand why:

library(ggplot2)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# Let us see if we can better by using a different linkage method. This time, we will use the mean linkage method:
  
clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)
rect.hclust(clusters, h = 3)

# We can see that the two best choices for number of clusters are either 3 or 5.
# Let us use cutree to bring it down to 3 clusters.

clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

# We can see that this time, the algorithm did a much better job of 
# clustering the data, only going wrong with 6 of the data points.

# We can plot it as follows to compare it with the original data:
  
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))

# All the points where the inner color doesn't match the outer color are the ones which were clustered incorrectly.

####################################################

## Heirarchical clustering
## For heirarchical clustering , we are going to use the mtcars dataset

# Look at the column names
names(mtcars)
colnames(mtcars)
# Find distances 
cars.dist <- dist(mtcars)

# # create distance matrix
# cars.dist <- as.matrix(cars.dist)

# use hclust function
clusters <- hclust(cars.dist)

# create dendogram
plot(clusters)

# apply rectangles at specified height
rect.hclust(clusters, h = 200)

# install "cluster" package to use agnes function
# install.packages("cluster")

library(cluster)

# cluster.agnes <- agnes(mtcars)

cluster.agnes <- agnes(mtcars, method = 'complete')

plot(cluster.agnes)
