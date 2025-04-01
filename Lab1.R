#14
vec <- c(sample(-15:14, 20, replace = FALSE))
max = max(vec)
count = sum(abs(vec) > max)

print(count)

table <- table(vec)
nums <- c(3,5,7)

counts <- table[as.character(nums)]
print(counts)

print(table)


#18
n <- as.integer(readline(prompt = "Input number n: "))

vectors <- list()

for (i in 1:n){
  if (i %% 3 == 1)
  {
    vectors[[i]] <- c(1,2,3,4,5)
  }
  else if (i %% 3 == 2){
    vectors[[i]] <- c("a", "b", "c", "d", "e")
  }
  else{
    vectors[[i]] <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  }
}
for (i in 1:n){
  cat("Vector", i, "is instance of:", class(vectors[[i]]), "\n")
}

df <- as.data.frame(vectors)

colnames(df) <- paste("Column", 1:n)

num_rows <- nrow(df)
num_cols <- ncol(df)

cat("Count of rows in dataFrame:", num_rows, "\n")
cat("Count of columns in dataFrame:", num_cols, "\n")

print(df)



#1 global
p <- c(7, 6, 5, 4)
q <- c(0, 1, 2, 3)

sum_pq <- p + q
diff_pq <- p - q
prod_pq <- p * q
div_pq <- p / q
pow_pq <- p ^ q

print("Сложение:")
print(sum_pq)

print("Вычитание:")
print(diff_pq)

print("Умножение:")
print(prod_pq)

print("Деление:")
print(div_pq)

print("Возведение в степень:")
print(pow_pq)


#2 global
vec1 <- ifelse(1:20 %% 2 == 0, 1:20, 0)
print("Вектор 1:")
print(vec1)

vec2 <- 2^(1:20)
print("Вектор 2:")
print(vec2)

vec3 <- 10^(0:4)
print("Вектор 3:")
print(vec3)


#3 global
helpseq1 <- seq(1, 50)
helpseq2 <- seq(2, 51)
helpseq3 <- rep(1, 50)
ans1 = sum(helpseq3/(helpseq1 * helpseq2))
print(ans1)

help2seq1 <- 2^(0:20)
help2seq2 <- rep(1,21)
ans2 = sum(help2seq2/help2seq1)

help3seq1 <- 3^(0:9)
help3seq2 <- seq(1, 28, by = 3)
ans3 = sum(help3seq2/help3seq1)
ans32 = sum((help3seq2/help3seq1) > 0.5)



#4 global
vec3 <- seq(3, 27, by = 3)

values_2_5_7 <- vec3[c(2, 5, 7)]

pre_last <- vec3[length(vec3) - 1]

all_except_pre_last <- vec3[-length(vec3)]

all_except_6 <- vec3[-6]

value_100 <- vec3[100]

all_except_first_last <- vec3[-c(1, length(vec3))]

values_gt4_lt10 <- vec3[vec3 > 4 & vec3 < 10]

values_lt4_gt10 <- vec3[vec3 < 4 | vec3 > 10]

print("Вектор vec3:")
print(vec3)

print("2, 5 и 7 значения:")
print(values_2_5_7)

print("Предпоследнее значение:")
print(pre_last)

print("Все, кроме предпоследнего:")
print(all_except_pre_last)

print("Все, кроме шестого:")
print(all_except_6)

print("Сотое значение:")
print(value_100)

print("Все, кроме первого и последнего:")
print(all_except_first_last)

print("Значения больше 4, но меньше 10:")
print(values_gt4_lt10)

print("Значения меньше 4 или больше 10:")
print(values_lt4_gt10)