data <- read.csv("C:/Users/Kurdicks/Downloads/Пивчик - Ответы на форму (1).csv", header=TRUE, stringsAsFactors=FALSE)

data_numeric <- data[, -c(1,ncol(data))]

empty_rows <- which(data$"ФИО" %in% c("", " ", NA))
empty_rows2 <- which(data$"ФИО" == "" | data$"ФИО" == " ")

data_without_empty_FIO <- data[-empty_rows, ]

data_numeric[] <- lapply(data_numeric, as.numeric)

max_values <- apply(data_numeric, 2, max, na.rm=TRUE)
min_values <- apply(data_numeric, 2, min, na.rm=TRUE)
mean_values <- apply(data_numeric, 2, mean, na.rm=TRUE)

selected_beer <- "Corona.extra"
sub_set <- data_numeric[data_numeric[, selected_beer], ]
subdataset <- data_numeric[data_numeric[, selected_beer] > 7, ]
print("NewNewNewNew")
print(subdataset)
dim(subdataset)

hist(sub_set$"Corona.extra", main="Гистограмма оценок Corona", col="blue", breaks=5, ylab = "Частота", xlab = "Оценки")

boxplot(subdataset,
        main="Boxplot оценок",
        col="orange",
        las=2,
        xlab = "пиво",
        ylab = "оценка",
        cex.axis = 0.7)


mean_values_sub <- apply(subdataset, 2, mean, na.rm=TRUE)
median_values_sub <- apply(subdataset, 2, median, na.rm=TRUE)
sd_values_sub <- apply(subdataset, 2, sd, na.rm=TRUE)

result_sub <- data.frame(Среднее=mean_values_sub, Медиана=median_values_sub, Отклонение=sd_values_sub,Минимум=min_values,Максимум=max_values)
print(result_sub)

df1 <- data.frame(id = c(1,2,3,4), name = c("Andrey", "Vitaliy", "Artem", "Bogdan"))
df2 <- data.frame(id = c(1,2,4,5), mark = c(3,4,5,5))

merged_df = merge(df1, df2, by = "id")
merged_full_df = merge(df1, df2, by = "id", all = TRUE)
rbind(merged_full_df, c(6, "Lera", 5))
no_1 <- merged_full_df[, -1]
full_data <- rbind(c("Moscow", "Tokyo"), c("Minsk", "Berlin"))

