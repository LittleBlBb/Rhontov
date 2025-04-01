
data <- read.table("C:/Users/Kurdicks/Downloads/Telegram Desktop/assess.dat", header = TRUE, stringsAsFactors=FALSE)
data_num <- data[, -1]


dist_matrix <- dist(data_num, method = "euclidean")


hc <- hclust(dist_matrix, method = "ward.D2")


plot(hc, labels = data$NAME, main = "Дендрограмма кандидатов", 
     xlab = "Кандидаты", ylab = "Расстояние")


clust3 <- cutree(hc, k = 3)

clust4 <- cutree(hc, k = 4)


data$Cluster3 <- clust3
data$Cluster4 <- clust4


cluster3_means <- aggregate(data[,2:11], by=list(data$Cluster3), mean)
cluster4_means <- aggregate(data[,2:11], by=list(data$Cluster4), mean)


print("Средние значения по 3 кластерам:")
print(cluster3_means)
print("Средние значения по 4 кластерам:")
print(cluster4_means)


data$Total <- rowSums(data[,2:11])


print(data)


data_num <- data[, -1]


dist_matrix <- dist(data_num, method = "euclidean")


hc <- hclust(dist_matrix, method = "ward.D2")


plot(hc, labels = data$NAME, main = "Дендрограмма кандидатов", 
     xlab = "Кандидаты", ylab = "Расстояние")


clust3 <- cutree(hc, k = 3)

clust4 <- cutree(hc, k = 4)


data$Cluster3 <- clust3
data$Cluster4 <- clust4


cluster3_means <- aggregate(data[,2:11], by=list(data$Cluster3), mean)
cluster4_means <- aggregate(data[,2:11], by=list(data$Cluster4), mean)


print("Средние значения по 3 кластерам:")
print(cluster3_means)
print("Средние значения по 4 кластерам:")
print(cluster4_means)


data$Total <- rowSums(data[,2:11])


print(data[, c("NAME", "Cluster3", "Cluster4", "Total")])