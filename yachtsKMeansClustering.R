## set working directory
setwd("~/DBS/AdvancedDataAnalytics/Assignments/CA3-Yachts/Submission")

## check working directory
getwd()

## read in dataset
yachts <- read.csv("yachtData.csv", header = TRUE)

## view dataset
View(yachts)
colnames(yachts)
colnames(yachts) <- c("YachtBuilder", "LOA", "Beam", "Draft", "Displacement")

## check
View(yachts)

## check summary
summary(yachts)

install.packages("NbClust")
install.packages("flexclust")

## Unlike hierarchical clustering, K-means clustering requires that the number of clusters
## to extracted be specified in advance. Again, the NbClust package can be used as a guide.
## Additionally, a plot of the total within-groups sums of squares against the number of
## clusters in a K-means solution can be helpful.
## A bend in the graph can suggest the appropriate number of clusters.
## The graph can be produced by the following function:

wssplot <- function(data, nc=20, seed=1234){
        wss <- (nrow(data)-1)*sum(apply(data,2,var))
        for (i in 2:nc){
                set.seed(seed)
                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
        plot(1:nc, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares")}

## The data parameter is the numeric dataset to be analyzed,
## nc is the maximum number of clusters to consider, and seed is a random number seed.

##check
View(yachts)
head(yachts)

## standardist the yachts dataset
yachtsStandardised <- scale(yachts[-1])
yachtsStandardised


## determine number of clusters
wssplot(yachtsStandardised)

library(NbClust)
set.seed(1234)
nc <- NbClust(yachtsStandardised, min.nc = 2, max.nc = 10, method = "kmeans")
table(nc$Best.n[1, ])

barplot(table(nc$Best.n[1, ]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")

set.seed(1234)
## K-means cluster analysis
## put into 25 groups first, then keep running algortihm
## until get down to 4 clusters
kMeansCluster <- kmeans(yachtsStandardised, 4, nstart=25)
kMeansCluster$size

## print out centres of each one of the cluster points per variable
kMeansCluster$centers

aggregate(yachts[-1], by = list(cluster = kMeansCluster$cluster), mean)

## Since the variables vary in range, they are standardized prior to clustering (#1).
## Next, the number of clusters is determined using the wwsplot() and NbClust()functions (#2).
## Figure 1 indicates that there is a distinct drop in within groups sum of squares
## when moving from 1 to 3 clusters. After three clusters, this decrease tapers off,
## suggesting that a 3-cluster solution may be a good fit to the data.
## In figure 2, 14 of 24 criteria provided by the NbClust package suggest a 3-cluster solution.
## Note that not all 30 criteria can be calculated for every dataset.

## A final cluster solution is obtained with kmeans() function
## and the cluster centroids are printed (#3).
## Since the centroids provided by the function are based on standardized data,
## the aggregate() function is used along with the cluster memberships
## to determine variable means for each cluster in the original metric.

## So how well did the k-means clustering uncover the actual structure of the data
# contained in the YachtBuilder variable?
## A cross-tabulation of YachtBuilder and cluster membership is given by:
crossTabkMeans <- table(yachts$YachtBuilder, kMeansCluster$cluster)
crossTabkMeans
## 40 mis-classifed
## model correctly classified 240 of the 280 (i.e. circa 86%)
## Now, it is possible to quantify the agreement between YachtBuilder and cluster,
## using an adjusted Rank Index, as provided by the package "flexclust".

library(flexclust)
randIndex(crossTabkMeans)

## The Adjusted Rank Index (ARI) provides a measure of the agreement between 2 partitions,
## adjusted for chance.
## It ranges from -1 (no agreement) to 1 (perfect agreement).
## Agreement between the YachtBuilder and the cluster solution is 0.69.
## ... an OK result, but definitely not a brilliant result!
