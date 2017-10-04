# Airline market segmentation
airlines <- read.csv("AirlinesCluster.csv")
# Looking at the summary of airlines, 
# which TWO variables have (on average) the smallest values?

names(sort(colMeans(airlines))[1:2])

# Which TWO variables have (on average) the largest values?
names(sort(colMeans(airlines), decreasing=TRUE)[1:2])

# Why is it important to normalize the data before clustering?
# If we don't normalize the data, the clustering will be dominated by 
# the variables that are on a larger scale.

library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
dists <- dist(airlinesNorm)

clust <- hclust(dists, method="ward.D")
plot(clust)

# Suppose that after looking at the dendrogram and discussing with the marketing 
# department, the airline decides to proceed with 5 clusters. 
# Divide the data points into 5 clusters by using the cutree function. 
# How many data points are in Cluster 1?

airlinesNorm$cluster <- cutree(clust, k=5)
airlines$cluster <- cutree(clust, k=5)


table(airlinesNorm$cluster)[[1]]
# 776

# Compared to the other clusters, Cluster 1 has the 
# largest average values in which variables (if any)?

library(dplyr)

means <- airlines %>% 
  group_by(cluster) %>%
  summarise_all(funs(mean))

# Compared to the other clusters, Cluster 1 has 
# the largest average values in which variables?
gather(means, var, val, -cluster) %>% 
  group_by(var) %>% 
  filter(val==max(val), cluster==1) %>% 
  select(var)
# DaysSinceEnroll
# How would you describe the customers in Cluster 1?
# Infrequent but loyal customers.

# Compared to the other clusters, Cluster 2 
# has the largest average values in which variables (if any)?
gather(means, var, val, -cluster) %>% 
  group_by(var) %>% 
  filter(val==max(val), cluster==2) %>% 
  select(var)

# How would you describe the customers in Cluster 2?
# Customers who have accumulated a large amount of miles, 
# and the ones with the largest number of flight transactions. 

# Compared to the other clusters, Cluster 3 
# has the largest average values in which variables (if any)?
gather(means, var, val, -cluster) %>% 
  group_by(var) %>% 
  filter(val==max(val), cluster==3) %>% 
  select(var)
# How would you describe the customers in Cluster 3?
# Customers who have accumulated a large amount of miles, 
# mostly through non-flight transactions. 

# Compared to the other clusters, Cluster 4 
# has the largest average values in which variables (if any)? 
gather(means, var, val, -cluster) %>% 
  group_by(var) %>% 
  filter(val==max(val), cluster==4) %>% 
  select(var)
