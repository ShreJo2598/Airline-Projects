

##### Shreya Joshi #####

##### Airline Big Data Analytics and Clustering using R #####

######## Clustering Analysis #######
# Clear the workspace
rm(list=ls())
cat("\014")

######## Q1 ########  
# load in the data file into data.frame
df <- read.csv(file='AirlineLoyalty(1).csv', stringsAsFactors = FALSE)

row.names(df) <- df[,1]
# remove the ID column
df <- df[,-1]

###### normalize input variables ######
# scale the data
df.norm <- data.frame(sapply(df, scale))
# add row names: 
row.names(df.norm) <- row.names(df) 

##-------------------------------------------------------####

## Q2 ###
###### hierarchical clustering using all variables ######
# calculate the distance matrix
dist <- dist(df.norm, method = "euclidean")
hc2 <- hclust(dist, method = "ward.D")
plot(hc2, hang = -1, ann = FALSE)

###### cut the tree into k clusters ######
# specify # of clusters k
member1 <- cutree(hc2, k = 2)  # Cut the dendrogram such that k clusters are produced
# check the number of records in each cluster
table(member1)
member1
## Attaching class label to the original DF #
df$cluster_hc_ward <- cutree(hc2, k = 2) 


###---------------------------------------------------------####

### Q3 & Q4 ###


### Without PCA ##
# for replication
set.seed(123)
clus.out_no_pca <- kmeans(df.norm[, 1:11], centers = 2, nstart = 10)
clus.out_no_pca$size


## with PCA ##
######## PCA + Clustering Analysis ########  
pca.out = prcomp(df.norm[, 1:11]) # perform PCA on columns 1~11
# summary report
summary(pca.out)

# get factor loadings/weights
rot = as.data.frame(pca.out$rotation)  # rotation matrix: rows are variable, columns are components

# now let us look at the pc scores for the observations
scores = as.data.frame(pca.out$x)

# now we perform cluster analysis
# instead of using Xs in df.nona, we want to using these PCs in scores
# lets use the first 6 components and make 2 clusters
clus.out <- kmeans(scores[,1:6],  centers = 2, nstart = 10)
# ratio of between-cluster variation to within-cluster variation => higher than using all the variables in the raw data
clus.out$betweenss/clus.out$tot.withinss
clus.out$size

###### Choosing K Using "elbow plot" or "Silhouette" ###### 

#install.packages('cluster')  
library(cluster)   # for use of silhouette()

# (1) "elbow plot" 
# -> calculate the total within-cluster sum of square measures
# -> the location of elbow (i.e., bend) indicate the appropriate number of clusters.
# (2) "Silhouette"
# use silhouette() in the cluster package to compute the average silhouette score
# -> A high average silhouette indicates a good clustering. 
# -> The optimal number of clusters k is the one that maximizes the average silhouette.

choosek <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(choosek) <- c("numClusters", "totWithinSS", "avg_silhouette")
for (k in 1:10) {
  set.seed(123)
  tempkm <- kmeans(scores[,1:6],  centers = k, nstart = 10)
  
  # Note that silhouette statistics are only defined if 2 <= k <= n-1.
  if (k==1) {
    ss <- 0
  } else {
    ss <- silhouette(tempkm$cluster, dist(scores[,1:7]))[, 3]
  }
  
  # append statistics
  tempdf <- data.frame(numClusters = k, totWithinSS = tempkm$tot.withinss, avg_silhouette = mean(ss))
  choosek <- rbind(choosek, tempdf)
}

require(ggplot2)

# elbow plot
g <- ggplot(choosek, aes(numClusters, totWithinSS))
g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Total Within-Cluster Squared Distance") +geom_text(aes(label=round(totWithinSS, 2)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))

# Silhouette #### Q3
g <- ggplot(choosek, aes(numClusters, avg_silhouette))
g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Average Silhouette") +geom_text(aes(label=round(avg_silhouette, 3)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))


######--------------------------------------------------####

#### Q 4 B)Interpreting Results of Clustering ###

### Clustering using HCLUST ###
table(member1) ## Cluster 1: 1573 (39 %) Cluster 2 : 2426( 60% )
survey2 = cbind(df[,c(1,2,3,4,11)], cluster = member1)
aggdf <- aggregate(cbind(Balance,Qual_miles,cc1_miles,cc2_miles,Award.) ~ cluster, data=survey2, mean )
aggdf


### Clustering using PCA and Kmeans ##
clus.out ###Cluster 1: 2894 (72 % ) Cluster 2 ( 28 %) : 1105
clus.out$size
survey = cbind(df[,c(1,2,3,4,11)], cluster = clus.out$cluster)


aggdf_k_means <- aggregate(cbind(Balance,Qual_miles,cc1_miles,cc2_miles,Award.) ~ cluster, data=survey, mean )
aggdf_k_means



### If we compare results from HCLUST and KMEANS clustering we can notice that 
### HCLUST has given more balanced clustering results as compared to KMEANS #

## By focusing on clusters obtained by HCLUST then we have 2 clusters.

## Characteristics of Cluster 1 and 2 ----
## 1) Since Balance is only 39163 which is less compared to cluster 2's 95930, we can conclude that customer 1 is a 
## non frequent flier and customer 2 is a frequent flier.
## 2) Customer 1 has less credit card usage because his cc1 ans cc2 are less than customer 2 
## 3) Customer 1 has not earned and ward travel yet and customer 2 is very close to earning his award travel as he is at
## 0.6104699 points.

## Offers for custormers in Cluster 1 (Non Frequent flyers)------ ###
## --- ***** Offer 1 *****-----##
## In cluster 1 ; the mean Balance (Number of miles eligible for award travel) is 39163.85.
## This Cluster tells us that the customer is not a frequent flyer and we should aim to make this cutomer choose our airline everytime he flies.
## We can offer him redumption of his Balance towards his flight ticket where redeeming 10000 balance means a discount of 100 USD.

## --- ***** Offer 2 *****-----##
## In cluster 1 ; the mean Balance (Number of miles eligible for award travel) is 39163.85.
## This Cluster tells us that the customer is not a frequent flyer and we should aim to make this cutomer choose our airline everytime he flies.
## We can give this customer an offer that if he books a round trip with us for a travel of more than 10000 miles, he gets an upgrade of
## business class for 60% off.

##----- Offers for customers in CLuster 2 ( Frequent flyers) ------- ###
##----******* Offer 1 *******-----####
## In cluster 2 ; the mean Balance (Number of miles eligible for award travel) is 95930.33.
## It clearly states that cluster 2 customer is a person who is used to flying with us and is used to choosing our airline.
## We must use his liking towards our airline to attract more customers through him. So we offer him a Premium membership on completing 
## 100000 balance Which allows him to book an additional adult ticket at 50% off.

##----******* Offer 2 *******-----####
## In cluster 2 ; the mean Qual_miles (Number of miles counted as qualifying for top flight status) is 95930.33.
## It clearly states that cluster 2 customer is a person who is used to flying with us and is used to choosing our airline.
## We can offer this customer to upgrade his ticket to business class whenever he books for two adults by redeeming his balance points towards the additional upgrade cost only.

## ----***** Offer 3 *****----####
##In cluster 2 ; the mean Qual_miles (Number of miles counted as qualifying for top flight status) is 95930.33.
## It clearly states that cluster 2 customer is a person who is used to flying with us and is used to choosing our airline.
## A referral offer can be given to this customer where if he refers two friends and they fly with us, he recieves a 
## free returning ticket on his upcoming flight.