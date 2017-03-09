#Importing the data
dataset = read.csv("Mall_Customers.csv")
X = dataset[,4:5]

#Find out the optimal number of clusters (elbow method)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(X,i)$withinss)
plot(1:10,wcss, type = "b", main = "Cluster of clients", xlab = "Number of clusters", ylab = "WCSS")

#Applying k-means to the dataset
set.seed(6)
kmeans = kmeans(X, 5, iter.max = 200, nstart = 10)

#Visualising the clusters
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster of clients",
         xlab = "Annual Income",
         ylab = "Spending Score")