library(rvest)

html_list <- list()
nodes_list <- list()
df_list <- list()

for (year in 2021:2014) {
  url <- paste0('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=', year)
  html_page <- read_html(url)
  html_list[[as.character(year)]] <- html_page
  
  tables <- html_nodes(html_page, 'table')
  nodes_list[[as.character(year)]] <- tables
  
  df <- html_table(tables[[2]], fill = TRUE) %>% as.data.frame()
  rownames(df) <- df[, 2]
  df <- df[, 3:11]
  df_list[[as.character(year)]] <- df
}
country <- c("Turkey", "Spain", "Romania", "Slovenia", "Germany")
colors <- c('blue', 'green', 'red', 'purple', 'gold')

plot_index_over_years <- function(index_name, df_list, years, countries, colors, title, ylabel, legend_pos = 'topright') {
  index_data <- do.call(rbind, lapply(as.character(years), function(y) df_list[[y]][countries, index_name]))
  index_df <- as.data.frame(index_data, row.names = years)
  colnames(index_df) <- countries
  
  mn <- min(index_df, na.rm = TRUE)
  mx <- max(index_df, na.rm = TRUE)
  
  plot(years, index_df[[countries[1]]], type = 'b', col = colors[1], lwd = 2, pch = 1, lty = 1,
       ylim = c(mn - 13, mx + 13), xlab = 'Года', ylab = ylabel, main = title)
  
  for (i in 2:length(countries)) {
    lines(years, index_df[[countries[i]]], type = 'b', col = colors[i], lwd = 2, pch = 1, lty = 1)
  }
  
  legend(legend_pos, cex = 0.6, legend = countries, fill = colors)
  
  return(index_df)
}

years_full <- 2014:2021
years_climate <- 2016:2021  # для Climate Index, так как 2014 и 2015 отсутствуют

# Построения графиков по индексам
QLI <- plot_index_over_years('Quality of Life Index', df_list, years_full, country, colors,
                             'Оценка индекса качества жизни', 'Индекс качества жизни', 'bottomright')

PPI <- plot_index_over_years('Purchasing Power Index', df_list, years_full, country, colors,
                             'Оценка индекса покупательной способности', 'Индекс покупательной способности')

SI <- plot_index_over_years('Safety Index', df_list, years_full, country, colors,
                            'Оценка индекса безопасности', 'Индекс безопасности')

HCI <- plot_index_over_years('Health Care Index', df_list, years_full, country, colors,
                             'Оценка индекс медицинского обслуживания', 'Индекс медицинского обслуживания', 'bottomright')

CLI <- plot_index_over_years('Cost of Living Index', df_list, years_full, country, colors,
                             'Оценка индекса прожиточного минимума', 'Индекс прожиточного минимума')

PPIR <- plot_index_over_years('Property Price to Income Ratio', df_list, years_full, country, colors,
                              'Оценка отношения цены на жилье к доходу', 'Отношение цены на жилье к доходу')

TCTI <- plot_index_over_years('Traffic Commute Time Index', df_list, years_full, country, colors,
                              'Оценка индекса времени движения на дороге', 'Индекс времени движения на дороге')

PI <- plot_index_over_years('Pollution Index', df_list, years_full, country, colors,
                            'Оценка индекса загрязнения', 'Индекс загрязнения')

CI <- plot_index_over_years('Climate Index', df_list, years_climate, country, colors,
                            'Оценка климатического индекса', 'Климатический индекс', 'bottomright') 

url <- "https://ru.wikipedia.org/wiki/Список_музеев_Ростовской_области"
page <- read_html(url)

# Извлекаем таблицы с классом wikitable
tables <- html_nodes(page, "table.wikitable")

# Преобразуем таблицы в список data frame'ов
museum_list <- lapply(tables, html_table, fill = TRUE)
museum_data <- bind_rows(museum_list)

# Приводим названия столбцов к нижнему регистру и заменяем пробелы на подчеркивания
names(museum_data) <- tolower(names(museum_data))
names(museum_data) <- gsub("\\s+", "_", names(museum_data))

# Переименовываем столбцы для удобства
museum_data <- museum_data %>%
  rename(
    Name = starts_with("назв"),
    Location = matches("город|насел")
  )

# Извлекаем ссылки из второго столбца каждой таблицы
links <- lapply(tables, function(table) {
  # Извлекаем все строки таблицы
  rows <- html_nodes(table, "tr")
  # Для каждой строки извлекаем второй <td> и тег <a> внутри него
  sapply(rows[-1], function(row) { # Пропускаем первую строку (заголовок)
    second_td <- html_node(row, "td:nth-child(2)")
    if (!is.na(second_td)) {
      link <- html_node(second_td, "a")
      if (!is.na(link)) {
        html_attr(link, "href")
      } else {
        NA
      }
    } else {
      NA
    }
  })
})

# Объединяем ссылки из всех таблиц
all_links <- unlist(links)

# Преобразуем относительные ссылки в полные
full_links <- paste0("https://ru.wikipedia.org", all_links)

# Добавляем столбец Link и заполняем его ссылками
museum_data$Link <- NA
if (length(full_links) >= nrow(museum_data)) {
  museum_data$Link <- full_links[1:nrow(museum_data)]
} else {
  museum_data$Link[1:length(full_links)] <- full_links
}
# Просмотр результата
View(museum_data) 