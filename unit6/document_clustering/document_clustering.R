library(distances)

# Document Clustering
documents <- read.csv("dailykos.csv")

# Running the dist function will probably take you a while. Why?
dists <- dist(documents)
clust <- hclust(dists, method="ward.D")
plot(clust)

# Thinking about the application, it is probably better to show 
# the reader more categories than 2 or 3. 
# These categories would probably be too broad to be useful. 
# Seven or eight categories seems more reasonable.

documents$cluster <- cutree(clust, k=7)

documents_1 <- subset(documents, cluster==1)
documents_2 <- subset(documents, cluster==2)
documents_3 <- subset(documents, cluster==3)
documents_4 <- subset(documents, cluster==4)
documents_5 <- subset(documents, cluster==5)
documents_6 <- subset(documents, cluster==6)
documents_7 <- subset(documents, cluster==7)


hierarhical_cluster <- split(documents, documents$cluster)

# How many observations are in cluster 3?
nrow(documents_3)
# 374

which.max(table(documents$cluster))
# 1

which.min(table(documents$cluster))
# 4

names(documents_1)

tail(sort(colMeans(documents_1)), 6)
tail(sort(colMeans(documents_2)), 6)
tail(sort(colMeans(documents_3)), 6)
tail(sort(colMeans(documents_4)), 6)
tail(sort(colMeans(documents_5)), 6)
tail(sort(colMeans(documents_6)), 6)
tail(sort(colMeans(documents_7)), 6)

set.seed(1000)
kmeans_clusters <- kmeans(documents, 7)
kmeans_clusters_lst = split(documents, kmeans_clusters$cluster)

main_words <- function(df) { tail(sort(colMeans(df)), 6) }

lapply(hierarhical_cluster,  main_words)

# Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
main_words_cluster_2 <- lapply(kmeans_clusters_lst,  main_words)[[2]]

# Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
table(documents$cluster, kmeans_clusters$cluster)[2,]
# Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
table(documents$cluster, kmeans_clusters$cluster)[,3]

# Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
table(documents$cluster, kmeans_clusters$cluster)[,7]

# Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
table(documents$cluster, kmeans_clusters$cluster)[,6]
