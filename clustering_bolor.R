# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
table(wine$Type)
# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
str(wine)

#change interger variables to numeric

wine$Magnesium<-as.numeric(wine$Magnesium)
wine$Proline<-as.numeric(wine$Proline)

# remove first column and scale the data

wine2 <- scale(wine[,-1])
summary(wine2)


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine2)

# Exercise 2:
#   * How many clusters does this method suggest?

# - The plot suggests that there should be 3 clusters because the sharp drop/bend is on the 3rd cluster.


#   * Why does this method work? What's the intuition behind it?

# - I think the intuition behind is there isn't much change within groups after 3 clusters. 
# - For example, there wouldn't be meaningful 4th cluster.


#   * Look at the code for wssplot() and figure out how it works

# - wssplot(wine)


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine2, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

#This method suggests also 3 is the best cluster. 

#***** Conclusion *****                            
  
#  * According to the majority rule, the best number of clusters is  3.


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km


fit.km <- kmeans(wine2, centers=3,  nstart=25)

# Now we want to evaluate how well this clustering does.

table(fit.km$cluster,wine$Type)

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

#   1  2  3
# 1  0  3 48
# 2  0 65  0
# 3 59  3  0

str(fit.km)
wineCluster=fit.km$cluster
table(wineCluster)

# It is hard to tell at this point.

ClusterGroups=split(wine, wineCluster)
str(ClusterGroups)
sort(colMeans(ClusterGroups[[1]]))
sort(colMeans(ClusterGroups[[2]]))
sort(colMeans(ClusterGroups[[3]]))
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(pam(wine2,3), shade=TRUE)

#More than 55% of the data is explained with these 2 components. And the clustering is 
#reasonable and ok for first-trial to see what's going on in the data. Not good for actual use.


