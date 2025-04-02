library(readxl)
library(gridExtra)
library(ggplot2)

#1. Собираем данные из дата сета
df_woman <- read_excel("Olimp.xlsx", sheet = 1)
df_man <- read_excel("Olimp.xlsx", sheet = 2)

legend_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink")

#2. Столбчатые диаграммы
df_woman_count <- apply(df_woman[,-1], 2, sum)
df_man_count <- apply(df_man[,-1], 2, sum)

combined_counts <- rbind(df_woman_count, df_man_count)
rownames(combined_counts) <- c("Женщины", "Мужчины")

# Строим объединённый график
barplot(combined_counts, 
        beside = TRUE,
        names.arg = colnames(df_woman)[-1],
        col = c("pink", "blue"),
        main = "Рис.1 - Распределение мест 1-8 по лыжным гонкам в России",
        xlab = "Места", 
        ylab = "Количество",
        ylim = c(0, max(combined_counts) * 1.2))

legend("topright", 
       legend = c("Женщины", "Мужчины"), 
       fill = c("pink", "blue"), 
       bty = "n")

# Подсчитываем золотые медали по годам 
gold_woman_by_year <- data.frame(
  Year = df_woman$Год,
  Gold = df_woman[[2]]  # Второй столбец — золотые медали
)
gold_woman_by_year <- aggregate(Gold ~ Year, data = gold_woman_by_year, sum)

gold_man_by_year <- data.frame(
  Year = df_man$Год,
  Gold = df_man[[2]]  # Второй столбец — золотые медали
)
gold_man_by_year <- aggregate(Gold ~ Year, data = gold_man_by_year, sum)

#2. Создаём круговые диаграммы с подписями
p_woman <- ggplot(gold_woman_by_year, aes(x = "", y = Gold, fill = as.factor(Year))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Gold), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Рис. 2 - Доля золотых медалей по Олимпиадам (Женщины)", fill = "Год") +
  theme_void() +
  theme(legend.position = "right")

p_man <- ggplot(gold_man_by_year, aes(x = "", y = Gold, fill = as.factor(Year))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Gold), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Рис. 3 - Доля золотых медалей по Олимпиадам (Мужчины)", fill = "Год") +
  theme_void() +
  theme(legend.position = "right")

# Выводим две диаграммы рядом
grid.arrange(p_woman, p_man, ncol = 2)

#2. Функциональные графики
plot_graph <- function(df1, df2 = NULL, colors, title) {
  total_places1 <- rowSums(df1[, 2:4], na.rm = TRUE)
  if (!is.null(df2)) {
    total_places2 <- rowSums(df2[, 2:4], na.rm = TRUE)
    data_to_plot <- cbind(total_places1, total_places2)
  } else {
    data_to_plot <- total_places1
  }
  years <- df1$Год
  
  # Строим график
  matplot(years, data_to_plot, main = title, type = "b", pch = 19, lty = 1, col = colors[1:2],  # Используем только 2 цвета
          xlab = "Год", ylab = "Количество мест", xaxt = "n",     
          ylim = c(0, max(data_to_plot, na.rm = TRUE) + 0.5))
  axis(1, at = years, labels = years)
  if (!is.null(df2)) {
    legend("topright", 
           legend = c("Женщины", "Мужчины"), 
           col = colors[1:2], 
           lty = 1, 
           pch = 19, 
           bty = "n")
  }
}

#2. Графики изменения призовых мест
plot_graph(df_woman, df_man, legend_colors, "Тенденции изменения кол-ва призовых мест за последние 30 лет")

#3. Графики изменения золотых медалей (1 мест) по странам
df_gold <- read_excel("Olimp.xlsx", sheet = 3)
par(mar = c(5, 4, 4, 9)) 
matplot(df_gold$Год, 
        df_gold[, -1], 
        main = "Изменение кол-ва 1-х мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (золото)", 
        xaxt = "n", 
        ylim = c(0, max(df_gold[, -1] + 10)))
axis(1, at = df_gold$Год, labels = df_gold$Год)
legend("topright", legend = colnames(df_gold)[-1], xpd = TRUE, bty = "n", inset = -c(0.35, 0), fill = legend_colors)

#3. Графики изменения всех призовых мест по странам 
df_prize_places <- read_excel("Olimp.xlsx", sheet = 4)
par(mar = c(5, 4, 4, 9)) 
matplot(df_prize_places$Год, 
        df_prize_places[, -1], 
        main = "Изменение кол-ва призовых мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        ylim = c(0, max(df_prize_places[, -1] + 10)))
axis(1, at = df_prize_places$Год, labels = df_prize_places$Год)
legend("topright", legend = colnames(df_prize_places)[-1], xpd = TRUE, bty = "n", inset = -c(0.35, 0), fill = legend_colors)

#4. Динамика лыжных гонок за последние 6 Олимпиад
# Подсчёт призовых мест
total_places2 <- rowSums(df_woman[, 2:4], na.rm = TRUE)
total_places3 <- rowSums(df_man[, 2:4], na.rm = TRUE)
total_places_all <- cbind(df_woman[, 1], total_places2, total_places3)
colnames(total_places_all) <- c("Год", "Женщины", "Мужчины")
total_places_all <- total_places_all[order(total_places_all$Год), ]

n_rows <- nrow(total_places_all)
year <- total_places_all[(n_rows-5):n_rows, ]  # Берем последние 6 строк

par(oma = c(0, 0, 2, 0))
matplot(year$Год, 
        year[, -1],
        type = "b", 
        pch = 19, 
        lty = c(1, 3),
        col = c("pink", "blue"),
        xlab = "Год", 
        ylab = "Количество мест (призовые)", 
        xaxt = "n",
        lwd = 3,
        ylim = c(0, max(year[, -1]) + 0.5))

axis(1, at = year$Год, labels = year$Год, las = 1, cex.axis = 0.8)
legend('topleft', c('Женщины', 'Мужчины'), lty = c(1, 3), cex = 0.8, lwd = 4, col = c("pink", "blue"))

# 4. Столбчатая диаграмма
total_places_barplot <- total_places_last_6  # Используем данные только за последние 6 лет
colnames(total_places_barplot) <- c("Год", "Женщины", "Мужчины")

# Транспонируем и сортируем (хотя сортировка уже не нужна, так как данные отсортированы)
total_places_barplot_t <- t(total_places_barplot)
total_places_barplot_t_sorted <- total_places_barplot_t[, order(total_places_barplot_t[1, ])]

# Строим столбчатую диаграмму
barplot(as.matrix(total_places_barplot_t_sorted[-1, ]), 
        beside = TRUE,
        col = c("pink", "blue"),
        xlab = "Год", 
        names.arg = total_places_barplot_t_sorted[1, ],
        ylab = "Количество мест (призовые)", 
        ylim = c(0, max(total_places_barplot_t_sorted[-1, ], na.rm = TRUE) * 1.2),  # Динамический ylim
        cex.names = 0.8)
legend('topright', c('Женщины', 'Мужчины'), fill = c("pink", "blue"))

# 4. Круговая диаграмма
# Подсчитываем общее количество призовых мест за последние 6 Олимпиад
total_places_pie <- colSums(total_places_barplot[, -1], na.rm = TRUE)  # Суммируем по столбцам "Женщины" и "Мужчины"
names(total_places_pie) <- c("Женщины", "Мужчины")

# Строим круговую диаграмму
pie(total_places_pie, 
    labels = total_places_pie,  # Показываем значения на диаграмме
    col = c("pink", "blue"),
    sub = "Кол-во призовых мест за последние 6 Олимпиад\n (женщины, мужчины)")
legend('topright', c('Женщины', 'Мужчины'), fill = c("pink", "blue"))

# Общий заголовок
mtext("Общее количество призовых мест на 6 Олимпиадах по лыжному бегу", 
      outer = TRUE, cex = 1, font = 2)