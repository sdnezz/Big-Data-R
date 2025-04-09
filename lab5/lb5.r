# Подключаем необходимые библиотеки
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(cluster)
library(factoextra)
library(NbClust)
library(parameters)
library(scatterplot3d)

# 1) Импортируем датасет
dataset <- read.csv("StudentsPerformance.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, 
                    encoding = "UTF-8", 
                    check.names = FALSE)

# Преобразуем числовые столбцы в числовой формат
dataset$`math score` <- as.numeric(dataset$`math score`)
dataset$`reading score` <- as.numeric(dataset$`reading score`)
dataset$`writing score` <- as.numeric(dataset$`writing score`)

# Выбираем числовые столбцы для кластеризации
data_for_clustering <- dataset[, c("math score", "reading score", "writing score")]

# Удаляем строки с пропущенными значениями
data_for_clustering <- na.omit(data_for_clustering)

# Стандартизируем данные
data_scaled <- scale(data_for_clustering)

# Выполняем кластеризацию методом k-means с 3 кластерами (для последующих визуализаций)
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 10)

# Добавляем метки кластеров в исходный датасет
dataset$Cluster <- as.factor(km_res$cluster)

# 2) ======================= ДЕСКРИПТИВНЫЙ АНАЛИЗ =========================
# Частотное распределение для категориальных переменных
# 2.1. Gender
ggplot(dataset, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Рис. 1 - Распределение по полу", x = "Пол", y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

# 2.2. Race/Ethnicity
ggplot(dataset, aes(x = `race/ethnicity`, fill = `race/ethnicity`)) +
  geom_bar() +
  labs(title = "Рис. 2 - Распределение по этнической группе", x = "Этническая группа", y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2.3. Parental Level of Education
ggplot(dataset, aes(x = `parental level of education`, fill = `parental level of education`)) +
  geom_bar() +
  labs(title = "Рис. 3 - Распределение по уровню образования родителей", x = "Уровень образования", y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2.4. Lunch
ggplot(dataset, aes(x = lunch, fill = lunch)) +
  geom_bar() +
  labs(title = "Рис. 4 - Распределение по типу обеда", x = "Тип обеда", y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("standard" = "green", "free/reduced" = "orange"))

# 2.5. Test Preparation Course
ggplot(dataset, aes(x = `test preparation course`, fill = `test preparation course`)) +
  geom_bar() +
  labs(title = "Рис. 5 - Распределение по подготовительному курсу", x = "Подготовительный курс", y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("none" = "gray", "completed" = "purple"))

# Гистограммы для числовых переменных
# 2.6. Math Score
ggplot(dataset, aes(x = `math score`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Рис. 6 - Распределение оценок по математике", x = "Оценка по математике", y = "Частота") +
  theme_minimal()

# 2.7. Reading Score
ggplot(dataset, aes(x = `reading score`)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Рис. 7 - Распределение оценок по чтению", x = "Оценка по чтению", y = "Частота") +
  theme_minimal()

# 2.8. Writing Score
ggplot(dataset, aes(x = `writing score`)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Рис. 8 - Распределение оценок по письму", x = "Оценка по письму", y = "Частота") +
  theme_minimal()

# Боксплоты для числовых переменных по категориальным
# 2.9. Math Score по Gender
ggplot(dataset, aes(x = gender, y = `math score`, fill = gender)) +
  geom_boxplot() +
  labs(title = "Рис. 9 - Оценки по математике по полу", x = "Пол", y = "Оценка по математике") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

# 2.10. Reading Score по Test Preparation Course
ggplot(dataset, aes(x = `test preparation course`, y = `reading score`, fill = `test preparation course`)) +
  geom_boxplot() +
  labs(title = "Рис. 10 - Оценки по чтению по подготовительному курсу", x = "Подготовительный курс", y = "Оценка по чтению") +
  theme_minimal() +
  scale_fill_manual(values = c("none" = "gray", "completed" = "purple"))

# 2.11. Writing Score по Parental Level of Education
ggplot(dataset, aes(x = `parental level of education`, y = `writing score`, fill = `parental level of education`)) +
  geom_boxplot() +
  labs(title = "Рис. 11 - Оценки по письму по уровню образования родителей", x = "Уровень образования", y = "Оценка по письму") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Средние оценки по категориальным переменным
# 2.12. Средние оценки по Gender
mean_by_gender <- dataset %>%
  group_by(gender) %>%
  summarise(
    Math_Mean = mean(`math score`),
    Reading_Mean = mean(`reading score`),
    Writing_Mean = mean(`writing score`)
  ) %>%
  melt(id.vars = "gender", variable.name = "Discipline", value.name = "Mean")

ggplot(mean_by_gender, aes(x = gender, y = Mean, fill = Discipline)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Рис. 12 - Средние оценки по полу", x = "Пол", y = "Средняя оценка") +
  theme_minimal() +
  scale_fill_manual(values = c("Math_Mean" = "blue", "Reading_Mean" = "green", "Writing_Mean" = "red"))

# 2.13. Средние оценки по Test Preparation Course
mean_by_course <- dataset %>%
  group_by(`test preparation course`) %>%
  summarise(
    Math_Mean = mean(`math score`),
    Reading_Mean = mean(`reading score`),
    Writing_Mean = mean(`writing score`)
  ) %>%
  melt(id.vars = "test preparation course", variable.name = "Discipline", value.name = "Mean")

ggplot(mean_by_course, aes(x = `test preparation course`, y = Mean, fill = Discipline)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Рис. 13 - Средние оценки по подготовительному курсу", x = "Подготовительный курс", y = "Средняя оценка") +
  theme_minimal() +
  scale_fill_manual(values = c("Math_Mean" = "blue", "Reading_Mean" = "green", "Writing_Mean" = "red"))

# 2.14. Средние оценки по Race/Ethnicity
mean_by_race <- dataset %>%
  group_by(`race/ethnicity`) %>%
  summarise(
    Math_Mean = mean(`math score`),
    Reading_Mean = mean(`reading score`),
    Writing_Mean = mean(`writing score`)
  ) %>%
  melt(id.vars = "race/ethnicity", variable.name = "Discipline", value.name = "Mean")

ggplot(mean_by_race, aes(x = `race/ethnicity`, y = Mean, fill = Discipline)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Рис. 14 - Средние оценки по этнической группе", x = "Этническая группа", y = "Средняя оценка") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Math_Mean" = "blue", "Reading_Mean" = "green", "Writing_Mean" = "red"))

# 2.15. Боксплоты оценок по Lunch
ggplot(dataset, aes(x = lunch, y = `math score`, fill = lunch)) +
  geom_boxplot() +
  labs(title = "Рис. 15 - Оценки по математике по типу обеда", x = "Тип обеда", y = "Оценка по математике") +
  theme_minimal() +
  scale_fill_manual(values = c("standard" = "green", "free/reduced" = "orange"))

# 3) ======================= ОПРЕДЕЛЕНИЕ ЧИСЛА КЛАСТЕРОВ =========================
# 3.1. Метод локтя
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  labs(title = "Рис. 1 - Метод локтя для определения числа кластеров") +
  theme_minimal()

# 3.2. Метод силуэта
fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  labs(title = "Рис. 2 - Метод силуэта для определения числа кластеров") +
  theme_minimal()

# 3.3. Статистика разрыва
set.seed(123)
gap_stat <- clusGap(data_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(title = "Рис. 3 - Статистика разрыва для определения числа кластеров") +
  theme_minimal()

# 3.4. Алгоритм консенсуса
set.seed(123)
nbclust_result <- NbClust(data_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, 
                          method = "kmeans", index = "all")
print(nbclust_result)

# 4) ======================= ИЕРАРХИЧЕСКАЯ КЛАСТЕРИЗАЦИЯ =========================
# Вычисляем матрицу расстояний (евклидово расстояние)
dist_matrix <- dist(data_scaled, method = "euclidean")

# Выполняем иерархическую кластеризацию (метод Уорда)
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Строим дендрограмму
fviz_dend(hclust_result, 
          main = "Рис. 1 - Дендрограмма иерархической кластеризации", 
          xlab = "Объекты", 
          ylab = "Высота", 
          sub = "", 
          k = 3, 
          rect = TRUE, 
          rect_fill = TRUE, 
          rect_border = "black", 
          labels_track_height = 0) +
  theme_minimal()

# Вычисляем коэффициент силуэта для разного числа кластеров
silhouette_scores <- c()
for (k in 2:10) {
  clusters <- cutree(hclust_result, k = k)
  sil <- silhouette(clusters, dist_matrix)
  silhouette_scores[k-1] <- mean(sil[, 3])
}

# Создаём data.frame для визуализации
silhouette_data <- data.frame(
  Number_of_Clusters = 2:10,
  Silhouette_Score = silhouette_scores
)

# Строим график силуэта
ggplot(silhouette_data, aes(x = Number_of_Clusters, y = Silhouette_Score)) +
  geom_line() +
  geom_point() +
  labs(title = "Рис. 2 - Средний коэффициент силуэта для разного числа кластеров", 
       x = "Число кластеров", 
       y = "Средний коэффициент силуэта") +
  theme_minimal()

# Разрезаем дендрограмму на 3 кластера
clusters_hclust <- cutree(hclust_result, k = 3)

# Добавляем метки кластеров в исходный датасет (для иерархической кластеризации)
dataset$Cluster_hclust <- as.factor(clusters_hclust)

# Визуализируем кластеры в пространстве PCA
fviz_cluster(list(data = data_scaled, cluster = clusters_hclust), 
             geom = "point", 
             main = "Рис. 3 - Результаты иерархической кластеризации (PCA)") +
  theme_minimal()

# Вычисляем средние значения оценок по кластерам
cluster_summary_hclust <- dataset %>%
  group_by(Cluster_hclust) %>%
  summarise(
    Math_Mean = mean(`math score`),
    Reading_Mean = mean(`reading score`),
    Writing_Mean = mean(`writing score`),
    Count = n()
  )

# Выводим результаты
print(cluster_summary_hclust)

# Распределение категориальных переменных по кластерам (иерархическая кластеризация)
# 4.1. Распределение Gender по кластерам
ggplot(dataset, aes(x = Cluster_hclust, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 4 - Распределение пола по кластерам (иерархическая)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

# 4.2. Распределение Race/Ethnicity по кластерам
ggplot(dataset, aes(x = Cluster_hclust, fill = `race/ethnicity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 5 - Распределение этнической группы по кластерам (иерархическая)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal()

# 4.3. Распределение Lunch по кластерам
ggplot(dataset, aes(x = Cluster_hclust, fill = lunch)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 6 - Распределение типа обеда по кластерам (иерархическая)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("standard" = "green", "free/reduced" = "orange"))

# 4.4. Распределение Test Preparation Course по кластерам
ggplot(dataset, aes(x = Cluster_hclust, fill = `test preparation course`)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 7 - Распределение подготовительного курса по кластерам (иерархическая)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("none" = "gray", "completed" = "purple"))

# 5) ======================= ИЕРАРХИЧЕСКАЯ КЛАСТЕРИЗАЦИЯ С ВИЗУАЛИЗАЦИЕЙ =========================
# Столбчатые диаграммы (Рис. 1)
mean_by_cluster_hclust <- dataset %>%
  group_by(Cluster_hclust) %>%
  summarise(
    Math_Mean = mean(`math score`),
    Reading_Mean = mean(`reading score`),
    Writing_Mean = mean(`writing score`)
  ) %>%
  melt(id.vars = "Cluster_hclust", variable.name = "Discipline", value.name = "Mean")

ggplot(mean_by_cluster_hclust, aes(x = Cluster_hclust, y = Mean, fill = Discipline)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Рис. 1 - Средние оценки по кластерам (иерархическая)", 
       x = "Кластер", 
       y = "Средняя оценка") +
  scale_fill_manual(values = c("Math_Mean" = "red", "Reading_Mean" = "green", "Writing_Mean" = "blue"),
                    labels = c("Math_Mean" = "Математика", 
                               "Reading_Mean" = "Чтение", 
                               "Writing_Mean" = "Письмо")) +
  theme_minimal()

# Боксплоты (Рис. 2)
data_long_hclust <- dataset %>%
  select(Cluster_hclust, `math score`, `reading score`, `writing score`) %>%
  melt(id.vars = "Cluster_hclust", variable.name = "Discipline", value.name = "Score")

ggplot(data_long_hclust, aes(x = Discipline, y = Score, fill = Cluster_hclust)) +
  geom_boxplot() +
  labs(title = "Рис. 2 - Боксплоты оценок по кластерам (иерархическая)", 
       x = "Дисциплина", 
       y = "Оценка") +
  scale_x_discrete(labels = c("math score" = "Математика", 
                              "reading score" = "Чтение", 
                              "writing score" = "Письмо")) +
  scale_fill_manual(values = c("1" = "red", "2" = "green", "3" = "blue")) +
  theme_minimal()

# Новое: Распределение Parental Level of Education по кластерам
ggplot(dataset, aes(x = Cluster_hclust, fill = `parental level of education`)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 3 - Распределение уровня образования родителей по кластерам (иерархическая)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6) ======================= K-MEANS КЛАСТЕРИЗАЦИЯ С ВИЗУАЛИЗАЦИЕЙ =========================
# Визуализируем кластеры с изменённой цветовой палитрой (Рис. 2)
fviz_cluster(km_res, data = data_scaled, 
             ellipse.type = "norm", 
             palette = "Set2", 
             main = "Рис. 2 - Кластеры студентов, изменена палитра", 
             ggtheme = theme_minimal())

# Распределение категориальных переменных по кластерам (k-means)
# 6.1. Распределение Gender по кластерам
ggplot(dataset, aes(x = Cluster, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 3 - Распределение пола по кластерам (k-means)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

# 6.2. Распределение Test Preparation Course по кластерам
ggplot(dataset, aes(x = Cluster, fill = `test preparation course`)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 4 - Распределение подготовительного курса по кластерам (k-means)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("none" = "gray", "completed" = "purple"))

# 6.3. Распределение Race/Ethnicity по кластерам
ggplot(dataset, aes(x = Cluster, fill = `race/ethnicity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 5 - Распределение этнической группы по кластерам (k-means)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal()

# 6.4. Распределение Lunch по кластерам
ggplot(dataset, aes(x = Cluster, fill = lunch)) +
  geom_bar(position = "dodge") +
  labs(title = "Рис. 6 - Распределение типа обеда по кластерам (k-means)", 
       x = "Кластер", 
       y = "Количество") +
  theme_minimal() +
  scale_fill_manual(values = c("standard" = "green", "free/reduced" = "orange"))

# 7) ======================= SCATTERPLOT С КЛАСТЕРАМИ =========================
# Определяем цвета для кластеров
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")[dataset$Cluster]

# Функция для отображения корреляций на верхней панели
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  # Сохраняем текущие графические параметры
  usr <- par("usr"); on.exit(par(usr))
  # Устанавливаем область для текста
  par(usr = c(0, 1, 0, 1))
  # Вычисляем корреляцию
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  # Отображаем корреляцию
  text(0.5, 0.5, txt, cex = 1.5)
}

# Функция для отображения гистограмм на диагонали
panel.hist <- function(x, ...) {
  # Сохраняем текущие графические параметры
  usr <- par("usr"); on.exit(par(usr))
  # Устанавливаем область для гистограммы
  par(usr = c(usr[1:2], 0, 1.5))
  # Строим гистограмму
  h <- hist(x, plot = FALSE, breaks = 20)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  # Рисуем гистограмму
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", border = "black")
}

# Рис. 1: Диаграмма рассеяния по кластерам (k-means)
pairs(data_for_clustering, 
      main = "Рис. 1 - Диаграмма рассеяния по кластерам (k-means)", 
      col = my_cols, 
      pch = 19, 
      cex = 0.8, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor)

# Диаграмма рассеяния по Gender
my_cols_gender <- c("#00AFBB", "#FC4E07")[as.factor(dataset$gender)]
pairs(data_for_clustering, 
      main = "Рис. 2 - Диаграмма рассеяния по полу", 
      col = my_cols_gender, 
      pch = 19, 
      cex = 0.8, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor)

# Диаграмма рассеяния по Test Preparation Course
my_cols_course <- c("#E7B800", "#5684E9")[as.factor(dataset$`test preparation course`)]
pairs(data_for_clustering, 
      main = "Рис. 3 - Диаграмма рассеяния по подготовительному курсу", 
      col = my_cols_course, 
      pch = 19, 
      cex = 0.8, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor)

# Диаграмма рассеяния по Lunch
my_cols_lunch <- c("#00AFBB", "#E7B800")[as.factor(dataset$lunch)]
pairs(data_for_clustering, 
      main = "Рис. 4 - Диаграмма рассеяния по типу обеда", 
      col = my_cols_lunch, 
      pch = 19, 
      cex = 0.8, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor)

# Диаграмма рассеяния по Race/Ethnicity
my_cols_race <- c("#F99999", "#E6F900", "#5684E9", "#00AFBB", "#FC4E07")[as.factor(dataset$`race/ethnicity`)]
pairs(data_for_clustering, 
      main = "Рис. 5 - Диаграмма рассеяния по этнической группе", 
      col = my_cols_race, 
      pch = 19, 
      cex = 0.8, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor)

# Диаграмма рассеяния по Parental Level of Education
my_cols_education <- c("#F99999", "#E6F900", "#5684E9", "#00AFBB", "#FC4E07", "#FF00FF")[as.factor(dataset$`parental level of education`)]
pairs(data_for_clustering, 
      main = "Рис. 6 - Диаграмма рассеяния по уровню образования родителей", 
      col = my_cols_education, 
      pch = 19, 
      cex = 0.8, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor)

# 8) ======================= ТРЁХМЕРНАЯ КЛАСТЕРИЗАЦИЯ =========================
# Определяем цвета для кластеров
colors <- c("#F99999", "#E6F900", "#5684E9")[dataset$Cluster]
# Рис. 2: Трёхмерная диаграмма рассеяния (с легендой)
s3d <- scatterplot3d(data_for_clustering, 
                     main = "Рис. 2 - Трёхмерная кластеризация студентов (k-means)", 
                     xlab = "math score", 
                     ylab = "reading score", 
                     zlab = "writing score", 
                     pch = 16, 
                     color = colors, 
                     cex.symbols = 0.8, 
                     angle = 30)

# Добавляем легенду
legend("topright", 
       legend = levels(dataset$Cluster), 
       col = c("#F99999", "#E6F900", "#5684E9"), 
       pch = 16, 
       title = "Кластеры", 
       inset = 0.05)

# Трёхмерная диаграмма по Gender
colors_gender <- c("#00AFBB", "#FC4E07")[as.factor(dataset$gender)]
s3d <- scatterplot3d(data_for_clustering, 
                     main = "Рис. 3 - Трёхмерная диаграмма по полу", 
                     xlab = "math score", 
                     ylab = "reading score", 
                     zlab = "writing score", 
                     pch = 16, 
                     color = colors_gender, 
                     cex.symbols = 0.8, 
                     angle = 30)

# Добавляем легенду для Gender
legend("topright", 
       legend = levels(as.factor(dataset$gender)), 
       col = c("#00AFBB", "#FC4E07"), 
       pch = 16, 
       title = "Пол", 
       inset = 0.05)

# Трёхмерная диаграмма по Race/Ethnicity
colors_race <- c("#F99999", "#E6F900", "#5684E9", "#00AFBB", "#FC4E07")[as.factor(dataset$`race/ethnicity`)]
s3d <- scatterplot3d(data_for_clustering, 
                     main = "Рис. 4 - Трёхмерная диаграмма по этнической группе", 
                     xlab = "math score", 
                     ylab = "reading score", 
                     zlab = "writing score", 
                     pch = 16, 
                     color = colors_race, 
                     cex.symbols = 0.8, 
                     angle = 30)

# Добавляем легенду для Race/Ethnicity
legend("topright", 
       legend = levels(as.factor(dataset$`race/ethnicity`)), 
       col = c("#F99999", "#E6F900", "#5684E9", "#00AFBB", "#FC4E07"), 
       pch = 16, 
       title = "Этническая группа", 
       inset = 0.05)

# Трёхмерная диаграмма по Test Preparation Course
colors_course <- c("#E7B800", "#5684E9")[as.factor(dataset$`test preparation course`)]
s3d <- scatterplot3d(data_for_clustering, 
                     main = "Рис. 5 - Трёхмерная диаграмма по подготовительному курсу", 
                     xlab = "math score", 
                     ylab = "reading score", 
                     zlab = "writing score", 
                     pch = 16, 
                     color = colors_course, 
                     cex.symbols = 0.8, 
                     angle = 30)

# Добавляем легенду для Test Preparation Course
legend("topright", 
       legend = levels(as.factor(dataset$`test preparation course`)), 
       col = c("#E7B800", "#5684E9"), 
       pch = 16, 
       title = "Подготовительный курс", 
       inset = 0.05)

