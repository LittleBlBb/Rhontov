setwd("C:/Users/Kurdicks/Desktop/r yazik/Lab5")

library(factoextra)
library(cluster)
library(parameters)
library(scatterplot3d)

# 1. Чтение и предварительная обработка
raw_data <- read.csv("List of largest Internet companies.csv", header = TRUE)

clean_num <- function(x) {
  as.numeric(gsub("[$,]", "", x))
}

raw_data$Revenue <- clean_num(raw_data$RevenueUSD.billions)
raw_data$Employees <- as.numeric(raw_data$Employees)
raw_data$MarketCap <- clean_num(raw_data$Market.cap.USD.billions)
raw_data$Founded <- as.numeric(raw_data$Founded)

clean_data <- na.omit(raw_data[, c("Company", "Revenue", "Employees", "MarketCap", "Founded")])
rownames(clean_data) <- clean_data$Company

# Логарифмирование (log(1 + x)) — особенно для масштабных переменных
log_data <- clean_data[, -1]
log_data$Founded <- log1p(log_data$Founded)
log_data$Revenue <- log1p(log_data$Revenue)
log_data$Employees <- log1p(log_data$Employees)
log_data$MarketCap <- log1p(log_data$MarketCap)

# 2. Дескриптивный анализ
cat("Статистика (логарифмированные данные):\n")
print(summary(log_data))
cat("\nСтандартное отклонение:\n")
print(sapply(log_data, sd))
cat("\nДисперсия:\n")
print(sapply(log_data, var))

for (col in colnames(log_data)) {
  hist(log_data[[col]], main = paste("Распределение (log):", col),
       xlab = col, col = "lightblue", border = "black")
}

boxplot(log_data, main = "Boxplot (log): Показатели интернет-компаний", col = rainbow(ncol(log_data)))

# 3. Нормализация и определение числа кластеров
maxs <- apply(log_data, 2, max)
mins <- apply(log_data, 2, min)
scaled_data <- scale(log_data, center = mins, scale = maxs - mins)
scaled_data <- as.data.frame(scaled_data)

fviz_nbclust(scaled_data, kmeans, method = "wss") + labs(title = "Метод локтя")
fviz_nbclust(scaled_data, kmeans, method = "silhouette") + labs(title = "Метод силуэта")

set.seed(123)
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 6, B = 50)
fviz_gap_stat(gap_stat) + labs(title = "Статистика разрыва")

set.seed(123)
n_clust <- n_clusters(scaled_data, package = c("easystats", "NbClust", "mclust"), standardize = FALSE)
plot(n_clust)

# 4. Иерархическая кластеризация
dist_matrix <- dist(scaled_data)
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, labels = rownames(clean_data), main = "Дендрограмма", cex = 0.7)
rect.hclust(hc, k = 3, border = "red")

# 5–6. K-means кластеризация
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

clustered_data <- log_data
clustered_data$cluster <- as.factor(kmeans_result$cluster)
write.csv(clustered_data, "clustered_data.csv", row.names = FALSE)
fviz_cluster(kmeans_result, data = scaled_data,
             geom = "point", ellipse.type = "norm",
             palette = "Set2", ggtheme = theme_minimal(),
             main = "Кластеры компаний (log)")

# Барплот
means <- aggregate(. ~ cluster, data = clustered_data, mean)
df_t <- t(means[,-1])
barplot(df_t, beside = TRUE, ylim = c(0, max(df_t)+1),
        col = c("red", "green", "blue"), main = "Средние значения по кластерам (log)")
legend("top", legend = rownames(df_t), fill = c("red", "green", "blue"), bty = "n")

# Boxplot
par(mfrow = c(2, 2))
for (col in colnames(log_data)) {
  boxplot(clustered_data[[col]] ~ clustered_data$cluster,
          main = paste("Boxplot по", col), col = c("red", "green", "blue"),
          xlab = "Кластер", ylab = col)
}
par(mfrow = c(1, 1))

# Scatterplot
selected_vars <- c("Revenue", "Employees", "MarketCap")
colors <- c("red", "green", "blue")[clustered_data$cluster]
pairs(log_data[, selected_vars], col = colors, pch = 19, main = "Scatterplot кластеров (log)")

# 3D Scatterplot
scatterplot3d(log_data$Revenue, log_data$Employees, log_data$MarketCap,
              color = colors, pch = 19, main = "3D scatterplot кластеров (log)",
              xlab = "Revenue", ylab = "Employees", zlab = "MarketCap")
