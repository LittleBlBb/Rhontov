setwd("C:/Users/Kurdicks/Desktop/r yazik/Excel")
########ЛАБА3#############

years <- seq(from = 1984, to = 2024, by = 4)
y1984 <- c(0,0,1, 1, 2, 0, 2, 1)
y1988 <- c(0,0,1, 0, 1, 0 ,1, 2)
y1992 <- c(1,1,2,1,4,1,0,2)
y1996 <- c(1,2,1,1,1,1,0,1)
y2000 <- c(1,0,0,0,0,0,1,1)
y2004 <- c(2,0,0,1,0,2,1,3)
y2008 <- c(0,0,2,4,1,0,2,1)
y2012 <- c(1,3,3,3,2,3,0,0)
y2016 <- c(2,2,2,3,4,1,2,1)
y2020 <- c(2,2,1,1,3,2,3,3)
y2024 <- c(1,1,2,0,1,0,3,1)

dataset <- matrix(rbind(y1984,y1988,y1992,y1996,y2000,y2004,y2008,y2012,y2016,y2020,y2024), nrow = length(years), ncol = 8)
write.csv(dataset, file = "dataset.csv", row.names=TRUE, col.names = TRUE)
rownames(dataset) <- years
colnames(dataset) <- seq(1, 8)

y1984w <- c(0,0,0,0,1,0,1,0)
y1988w <- c(0,0,1,0,1,0,1,2)
y1992w <- c(1,1,2,1,3,1,0,0)
y1996w <- c(1,2,1,1,1,1,0,1)
y2000w <- c(1,0,0,0,0,0,1,1)
y2004w <- c(1,0,0,0,0,1,1,3)
y2008w <- c(0,0,2,3,1,0,1,1)
y2012w <- c(0,2,2,2,2,2,0,0)
y2016w <- c(1,1,1,1,2,0,2,1)
y2020w <- c(2,1,1,0,3,1,1,1)
y2024w <- c(1,1,2,0,1,0,0,0)


datasetwomen <- matrix(rbind(y1984w,y1988w,y1992w,y1996w,y2000w,y2004w,y2008w,y2012w,y2016w,y2020w,y2024w), nrow = length(years), ncol = 8)
write.csv(datasetwomen, file="datasetwomen.csv", row.names = TRUE)

rownames(datasetwomen) <- years
colnames(datasetwomen) <- seq(1, 8)

y1984m <- y1984 - y1984w
y1988m <- y1988 - y1988w
y1992m <- y1992 - y1992w
y1996m <- y1996 - y1996w
y2000m <- y2000 - y2000w
y2004m <- y2004 - y2004w
y2008m <- y2008 - y2008w 
y2012m <- y2012 - y2012w 
y2016m <- y2016 - y2016w
y2020m <- y2020 - y2020w 
y2024m <- y2024 - y2024w 


datasetmen <- matrix(rbind(y1984m,y1988m,y1992m,y1996m,y2000m,y2004m,y2008m,y2012m,y2016m,y2020m,y2024m), nrow = length(years), ncol = 8)
write.csv(datasetmen, file = "datasetmen.csv", row.names = TRUE)
rownames(datasetmen) <- years
colnames(datasetmen) <- seq(1, 8)
menMedals = rowSums(datasetmen[, 1:3])

plot(years, menMedals, type="n", main="Тенденции изменения количества призовых мест \n по мужчинам",
     xlab = "Год", ylab = "Кол-во призовых мест", xaxt = "n")
axis(1, at = years, labels = years, las = 2)
lines(years, menMedals, type = "b", col = "blue", pch = 16, lwd = 2)

womenMedals = rowSums(datasetwomen[, 1:3])
plot(years, womenMedals, type="n", main="Тенденции изменения количества призовых мест \n по женщинам",
     xlab = "Год", ylab = "Кол-во призовых мест", xaxt = "n")
axis(1, at = years, labels = years, las = 2, )
lines(years, womenMedals, type = "b", col = "blue", pch = 16, lwd = 2)

barplot(t(dataset), 
        beside = TRUE,
        names.arg = years,
        col = c("gold", "lightgrey", "orange", "grey", "grey", "grey", "grey", "grey"),
        main = "Динамика достижений Китая \n на олимпийских играх по легкой \n атлетике по годам",  # Заголовок
        xlab = "Год",
        ylab = "Места",
        las = 2,
        legend = colnames(dataset),
        args.legend = list(x = "topleft", bty = "n")
)
GOLD <- dataset[, 1]
years_and_gold <- paste(years, " (", GOLD, ")", sep = "")
pie(GOLD, labels = years_and_gold, main = "Распределение первых мест по годам", width = 4, height = 5)


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