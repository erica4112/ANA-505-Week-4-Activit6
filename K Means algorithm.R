#K-Means Clustering

require("datasets")
data("iris") # load Iris Dataset
str(iris) #view structure of dataset

summary(iris) #view statistical summary of dataset

head(iris, 3) #view top  rows of dataset

#Preprocess the dataset
#Since clustering is a type of Unsupervised Learning, 
#we would not require Class Label(output) during execution of our algorithm. 
#We will, therefore, remove Class Attribute “Species” and store it in another variable. 
#We would then normalize the attributes between 0 and 1 using our own function.

iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new, 3)

head(iris.class, 3)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

#Apply the K-means clustering algorithm
result<- kmeans(iris.new,3) #aplly k-means algorithm with no. of centroids(k)=3
result$size # gives no. of records in each cluster

result$centers # gives value of cluster center datapoint value(3 centers for k=3)

result$cluster #gives cluster vector showing the custer where each record falls

#Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)

table(result$cluster,iris.class)

#Results of the table show that Cluster 1 corresponds to Virginica, 
data(iris)
Virginica <- iris[, c("Petal.Length", "Petal.Width")]
num_clusters <- 1
kmeans_result <- kmeans(Virginica, centers = num_clusters)

print(kmeans_result$centers)

print(kmeans_result$cluster)

hierarchical_result <- hclust(dist(Virginica), method = "ward.D2")

plot(hierarchical_result)


#Cluster 2 corresponds to Versicolor 
data(iris)
Versicolor <- iris[, c("Petal.Length", "Petal.Width")]
num_clusters <- 2
kmeans_result <- kmeans(Versicolor, centers = num_clusters)

print(kmeans_result$centers)

print(kmeans_result$cluster)

hierarchical_result <- hclust(dist(Versicolor), method = "ward.D2")

plot(hierarchical_result)

#and Cluster 3 to Setosa.
data(iris)
Setosa. <- iris[, c("Petal.Length", "Petal.Width")]
num_clusters <- 2
kmeans_result <- kmeans(Setosa., centers = num_clusters)

print(kmeans_result$centers)

print(kmeans_result$cluster)

hierarchical_result <- hclust(dist(Setosa.), method = "ward.D2")

plot(hierarchical_result)

#Total number of correctly classified instances are: 36 + 47 + 50= 133
#Total number of incorrectly classified instances are: 3 + 14= 17

#How did the model do?
#TASK: Accuracy = number of correctly classified/(total classified) = ?
#i.e our model has achieved ?% accuracy!

library(cluster)
data(iris)

k <- 3
kmeans_result <- kmeans(iris[, -5], centers = k)
cluster_assignments <- kmeans_result$cluster
species_labels <- iris$Species

contingency_table <- table(cluster_assignments, species_labels)
accuracy_rate <- sum(max(contingency_table)) / sum(contingency_table)
cat("Accuracy Rate:", accuracy_rate, "\n")




