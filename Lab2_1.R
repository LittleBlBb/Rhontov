#install.packages("readxl")

library(readxl)

file_path <- "C:/Users/Kurdicks/Desktop/r debilniyi yazik/Excel/Beer.xlsx"

data <- read_excel(file_path, sheet = 1)

data_ratings = data[, c(-1,-ncol(data))]

min_values <- apply(data_ratings, 2, min)

max_values <- apply(data_ratings, 2, max)

mean_values <- round(apply(data_ratings, 2, mean), 2)

print("Max values:")
print(max_values)

print("Min values:")
print(min_values)

print("Mean values:")
print(mean_values)


count_high <- colSums(data_ratings > 7)

count_low <- colSums(data_ratings < 3)

print("Count of people with ratings > 7:")
print(count_high)

print("Count of people with ratings < 3:")
print(count_low)


sorted_ratings <- sort(mean_values, decreasing = TRUE)


print("Beer ratings in descending order:")
print(sorted_ratings)

overall_mean <- round(mean(unlist(data_ratings)), 2)
print(overall_mean)


barplot(sorted_ratings, main = "Average Beer Ratings", xlab = "Beers", ylab = "Average Rating", col = "blue")
abline(h = overall_mean, col = "red", lwd = 3)
