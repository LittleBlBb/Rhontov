setwd("C:/Users/Kurdicks/Desktop/r yazik/Excel")

library(readxl)

dataset <- read_excel("datasetmen.xls", sheet = "DataSet")
datasetwoman <- read_excel("datasetmen.xls", sheet = "WomanDS")
datasetman <- read_excel("datasetmen.xls", sheet = "ManDS")

years <- seq(from = 1984, to = 2024, by = 4)

menMedals <- rowSums(datasetman[, 2:4])
womenMedals <- rowSums(datasetwoman[, 2:4])

if (!is.numeric(years) || !is.numeric(menMedals) || !is.numeric(womenMedals)) {
  years <- as.numeric(years)
  menMedals <- as.numeric(menMedals)
  womenMedals <- as.numeric(womenMedals)
}

if (any(is.na(years)) || any(is.na(menMedals)) || any(is.na(womenMedals))) {
  warning("Данные содержат NA. Удаляем строки с NA.")
  valid <- !is.na(years) & !is.na(menMedals) & !is.na(womenMedals)
  years <- years[valid]
  menMedals <- menMedals[valid]
  womenMedals <- womenMedals[valid]
}

if (length(years) == 0 || length(menMedals) == 0 || length(womenMedals) == 0) {
  stop("Ошибка: данные пусты после очистки")
}

plot(years, menMedals, type = "n", main = "Тенденции изменения количества призовых мест\n(мужчины и женщины)",
     xlab = "Год", ylab = "Кол-во призовых мест", xaxt = "n", 
     ylim = c(0, max(c(menMedals, womenMedals), na.rm = TRUE)))

axis(1, at = years, labels = years, las = 2)

lines(years, menMedals, type = "b", col = "blue", pch = 16, lwd = 2)
lines(years, womenMedals, type = "b", col = "red", pch = 17, lwd = 2)

legend("topleft", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), 
       pch = c(16, 17), lwd = 2, bty = "n")

if (nrow(dataset) != length(years)) {
  warning(paste("Количество строк в dataset (", nrow(dataset), ") не соответствует количеству лет (", length(years), "). Обрезаем годы."))
  years <- years[1:nrow(dataset)] 
}

if (ncol(dataset) > 1) {
  category_data <- as.matrix(dataset[, -1])  
} else {
  category_data <- as.matrix(dataset)
}

if (ncol(category_data) > 8) {
  category_data <- category_data[, 1:8] 
} else if (ncol(category_data) < 1) {
  stop("Нет данных по категориям")
}

category_data <- t(category_data)

colors <- c("gold", "lightgrey", "orange", "grey80", "grey70", "grey60", "grey50", "grey40")[1:nrow(category_data)]

barplot(category_data, 
        beside = TRUE,           
        names.arg = years,        
        col = colors,             
        main = "Динамика достижений Китая\nна олимпийских играх по легкой атлетике по годам",
        xlab = "Год",
        ylab = "Места",
        las = 2,                 
        legend.text = paste(1:nrow(category_data), "-е место"), 
        args.legend = list(x = "topleft", bty = "n", cex = 0.8)) 
GOLD <- dataset[, 2]
years <- dataset[, 1]

positive_idx <- GOLD > 0
GOLD_positive <- GOLD[positive_idx]
years_positive <- years[positive_idx]

colors <- rainbow(length(GOLD_positive))
pie(GOLD_positive,  
    labels = GOLD_positive,
    main = "Распределение первых мест по годам", 
    col = colors)
legend("topright", 
       legend = paste(years_positive, "(", GOLD_positive, ")", sep = ""), 
       fill = colors)

last6 <- c(2004, 2008, 2012, 2016, 2020, 2024)

usa_gold <- c(36, 36, 46, 46, 39, 40)
chn_gold <- c(32, 51, 38, 26, 38, 40)
gbr_gold <- c(9, 19, 29, 27, 22, 14)
jpn_gold <- c(16, 9, 7, 12, 27, 20)
aus_gold <- c(17, 14, 8, 8, 17, 18)
fra_gold <- c(11, 7, 11, 10, 10, 16)
ger_gold <- c(13, 16, 11, 17, 10, 12)

usa_bronze <- c(26, 38, 30, 37, 33, 42)
chn_bronze <- c(15, 21, 21, 18, 18, 24)
gbr_bronze <- c(12, 15, 19, 23, 21, 29)
jpn_bronze <- c(12, 11, 17, 21, 14, 13)
aus_bronze <- c(16, 19, 19, 13, 22, 16)
fra_bronze <- c(13, 22, 15, 18, 13, 22)
ger_bronze <- c(20, 17, 19, 15, 16, 8)

plot(last6, usa_gold, 
     type = "n",
     ylim = c(0, max(usa_gold, chn_gold, gbr_gold, jpn_gold, aus_gold, fra_gold, ger_gold) + 5),
     main = "Тенденции золотых медалей\nТоп-7 стран",
     xlab = "Год",
     ylab = "Количество золотых медалей",
     xaxt = "n"
)

axis(1, at = last6, labels = last6, las = 2)

lines(last6, usa_gold, type = "b", col = "blue", pch = 16, lwd = 2)  # США
lines(last6, chn_gold, type = "b", col = "red", pch = 17, lwd = 2)   # Китай
lines(last6, gbr_gold, type = "b", col = "green", pch = 18, lwd = 2)  # Великобритания
lines(last6, jpn_gold, type = "b", col = "purple", pch = 19, lwd = 2) # Япония
lines(last6, aus_gold, type = "b", col = "gold", pch = 20, lwd = 2)   # Австралия
lines(last6, fra_gold, type = "b", col = "orange", pch = 21, lwd = 2) # Франция
lines(last6, ger_gold, type = "b", col = "black", pch = 22, lwd = 2)  # Германия

legend("topright", 
       legend = c("США", "Китай", "Великобритания", "Япония", "Австралия", "Франция", "Германия"), 
       col = c("blue", "red", "green", "purple", "gold", "orange", "black"), 
       pch = c(16, 17, 18, 19, 20, 21, 22), 
       lwd = 2, 
       bty = "n")

plot(last6, usa_bronze, 
     type = "n",
     ylim = c(0, max(usa_bronze, chn_bronze, gbr_bronze, jpn_bronze, aus_bronze, fra_bronze, ger_bronze) + 5),
     main = "Тенденции бронзовых медалей\nТоп-7 стран",
     xlab = "Год",
     ylab = "Количество бронзовых медалей",
     xaxt = "n"
)

axis(1, at = last6, labels = last6, las = 2)

lines(last6, usa_bronze, type = "b", col = "blue", pch = 16, lwd = 2)  # США
lines(last6, chn_bronze, type = "b", col = "red", pch = 17, lwd = 2)   # Китай
lines(last6, gbr_bronze, type = "b", col = "green", pch = 18, lwd = 2)  # Великобритания
lines(last6, jpn_bronze, type = "b", col = "purple", pch = 19, lwd = 2) # Япония
lines(last6, aus_bronze, type = "b", col = "gold", pch = 20, lwd = 2)   # Австралия
lines(last6, fra_bronze, type = "b", col = "orange", pch = 21, lwd = 2) # Франция
lines(last6, ger_bronze, type = "b", col = "black", pch = 22, lwd = 2)  # Германия

legend("topright", 
       legend = c("США", "Китай", "Великобритания", "Япония", "Австралия", "Франция", "Германия"), 
       col = c("blue", "red", "green", "purple", "gold", "orange", "black"), 
       pch = c(16, 17, 18, 19, 20, 21, 22), 
       lwd = 2, 
       bty = "n")
menMedals6 = menMedals[1:6]
womenMedals6 = womenMedals[1:6] 
menTotal = sum(menMedals6)
womenTotal = sum(womenMedals6)

par(mfrow = c(2,2), mar = c(4,4,2,2) + 0.1)
plot(last6, menMedals6,
     type="n", ylim = c(0, max(menMedals6, womenMedals6) + 1), 
     main = "Динамика призовых мест",
     xlab = "Год",
     ylab = "Кол-во призовых мест",
     xaxt = "n")


axis(1, at = last6, lables = last6, las = 2)

lines(last6, menMedals6, type = "b", col = "blue", pch = 16, lwd = 2)
lines(last6, womenMedals6, type = "b", col = "red", pch = 17, lwd = 2)

legend("topleft", 
       legend = c("Мужчины", "Женщины"), 
       col = c("blue", "red"), 
       pch = c(16, 17), 
       lwd = 2, 
       bty = "n")

bar_data <- matrix(c(menMedals6, womenMedals6), nrow = length(last6), byrow = FALSE)
write.csv(bar_data, file = "bar_data.csv", row.names = TRUE)

colnames(bar_data) <- c("Мужчины", "Женщины")
rownames(bar_data) <- last6

barplot(t(bar_data), 
        beside = TRUE,
        names.arg = last6,
        col = c("blue", "red"),
        main = "Призовые места по годам",
        xlab = "Год",
        ylab = "Количество призовых мест",
        las = 2,
        legend = colnames(bar_data),
        args.legend = list(x = "topleft", bty = "n")
)

pie_data <- c(menTotal, womenTotal)
pie_labels <- c(paste("Мужчины (", menTotal, ")", sep = ""),
                paste("Женщины (", womenTotal, ")", sep = ""))

pie(pie_data,
    labels = pie_labels,
    main = "Общее распределение\nпризовых мест",
    col = c("blue", "red"),
    clockwise = TRUE,
    init.angle = 90,
    radius = 1
)

par(mfrow = c(1, 1))