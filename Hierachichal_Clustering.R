#Importing the data
dataset = read.csv("Mall_Customers.csv")
X = dataset[,4:5]

#Using dendograms to work out how many clusters should there be
dendogram = hclust(dist(X, method = "euclidean"),
                   method = "ward.D")
plot(dendogram,
     main = "Dendogram",
     xlab = "Customers",
     ylab = "Distance between clusters")

#Doing the hierachichal clustering on the dataset
hc = hclust(dist(X, method = "euclidean"),
                       method = "ward.D")

y_hc = cutree(hc,5)

#Visualising the clusters
library(cluster)
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster of clients",
         xlab = "Annual Income",
         ylab = "Spending Score")