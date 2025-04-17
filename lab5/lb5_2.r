# Подключаем необходимые библиотеки
library(ggplot2)      # Для визуализации
library(dplyr)        # Для манипуляции данными
library(reshape2)     # Для преобразования данных (melt)
library(corrplot)     # Для корреляционных матриц (не используется здесь)
library(cluster)      # Для кластеризации (silhouette)
library(factoextra)   # Для визуализации кластеризации
library(NbClust)      # Для алгоритма консенсуса
library(parameters)   # Для анализа параметров (не используется здесь)
library(scatterplot3d)# Для 3D-диаграмм рассеяния
library(e1071)        # Для наивного байесовского классификатора
library(rpart)        # Для деревьев решений
library(rpart.plot)   # Для визуализации деревьев решений
library(caret)        # Для разделения данных на обучающую и тестовую выборки
library(randomForest) # Для случайного леса

# 1) Импортируем датасет
dataset <- read.csv("StudentsPerformance.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE, 
                    encoding = "UTF-8", 
                    check.names = FALSE)

# Переименовываем столбцы, чтобы убрать пробелы и специальные символы
colnames(dataset) <- c("gender", "race_ethnicity", "parental_level_of_education", 
                       "lunch", "test_preparation_course", "math_score", 
                       "reading_score", "writing_score")

# Преобразуем числовые столбцы в числовой формат
dataset$math_score <- as.numeric(dataset$math_score)
dataset$reading_score <- as.numeric(dataset$reading_score)
dataset$writing_score <- as.numeric(dataset$writing_score)

# Выбираем числовые столбцы для кластеризации
data_for_clustering <- dataset[, c("math_score", "reading_score", "writing_score")]

# Удаляем строки с пропущенными значениями
data_for_clustering <- na.omit(data_for_clustering)

# Стандартизируем данные
data_scaled <- scale(data_for_clustering)

# Выполняем кластеризацию методом k-means с 3 кластерами
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 10)

# Добавляем метки кластеров в исходный датасет как фактор
dataset$Cluster <- as.factor(km_res$cluster)

# Удаляем строки с пропущенными значениями в основном датасете
dataset <- na.omit(dataset)

# 2) Разделяем данные на обучающую и тестовую выборки
set.seed(123)
trainIndex <- createDataPartition(dataset$Cluster, p = 0.7, list = FALSE)

train_data <- dataset[trainIndex, ]
test_data <- dataset[-trainIndex, ]

# 3) Наивный байесовский классификатор
nb_model <- naiveBayes(Cluster ~ math_score + reading_score + writing_score + 
                         gender + race_ethnicity + parental_level_of_education + 
                         lunch + test_preparation_course, 
                       data = train_data)

nb_pred <- predict(nb_model, newdata = test_data)

nb_conf_matrix <- table(Actual = test_data$Cluster, Predicted = nb_pred)
print("Матрица ошибок для наивного байесовского классификатора:")
print(nb_conf_matrix)

nb_accuracy <- sum(diag(nb_conf_matrix)) / sum(nb_conf_matrix)
print(paste("Точность наивного байесовского классификатора:", round(nb_accuracy, 3)))

# 4) Дерево решений (исследование с разными параметрами)
# 4.1 Дерево с параметрами по умолчанию
dt_model_default <- rpart(Cluster ~ math_score + reading_score + writing_score + 
                            gender + race_ethnicity + parental_level_of_education + 
                            lunch + test_preparation_course, 
                          data = train_data, 
                          method = "class")

# Визуализируем дерево по умолчанию
rpart.plot(dt_model_default, 
           main = "Рис. 1 - Дерево решений (по умолчанию)",
           extra = 104, 
           fallen.leaves = TRUE, 
           cex = 0.8)

# Предсказываем и оцениваем точность
dt_pred_default <- predict(dt_model_default, newdata = test_data, type = "class")
dt_conf_matrix_default <- table(Actual = test_data$Cluster, Predicted = dt_pred_default)
dt_accuracy_default <- sum(diag(dt_conf_matrix_default)) / sum(dt_conf_matrix_default)
print("Матрица ошибок для дерева решений (по умолчанию):")
print(dt_conf_matrix_default)
print(paste("Точность дерева решений (по умолчанию):", round(dt_accuracy_default, 3)))

# 4.2 Дерево с ограниченной глубиной (maxdepth = 3)
dt_model_shallow <- rpart(Cluster ~ math_score + reading_score + writing_score + 
                            gender + race_ethnicity + parental_level_of_education + 
                            lunch + test_preparation_course, 
                          data = train_data, 
                          method = "class",
                          control = rpart.control(maxdepth = 3))

# Визуализируем дерево с ограниченной глубиной
rpart.plot(dt_model_shallow, 
           main = "Рис. 2 - Дерево решений (глубина 3)",
           extra = 104, 
           fallen.leaves = TRUE, 
           cex = 0.8)

# Предсказываем и оцениваем точность
dt_pred_shallow <- predict(dt_model_shallow, newdata = test_data, type = "class")
dt_conf_matrix_shallow <- table(Actual = test_data$Cluster, Predicted = dt_pred_shallow)
dt_accuracy_shallow <- sum(diag(dt_conf_matrix_shallow)) / sum(dt_conf_matrix_shallow)
print("Матрица ошибок для дерева решений (глубина 3):")
print(dt_conf_matrix_shallow)
print(paste("Точность дерева решений (глубина 3):", round(dt_accuracy_shallow, 3)))

# 4.3 Дерево с большей глубиной (maxdepth = 10)
dt_model_deep <- rpart(Cluster ~ math_score + reading_score + writing_score + 
                         gender + race_ethnicity + parental_level_of_education + 
                         lunch + test_preparation_course, 
                       data = train_data, 
                       method = "class",
                       control = rpart.control(maxdepth = 10))

# Визуализируем дерево с большей глубиной
rpart.plot(dt_model_deep, 
           main = "Рис. 3 - Дерево решений (глубина 10)",
           extra = 104, 
           fallen.leaves = TRUE, 
           cex = 0.8)

# Предсказываем и оцениваем точность
dt_pred_deep <- predict(dt_model_deep, newdata = test_data, type = "class")
dt_conf_matrix_deep <- table(Actual = test_data$Cluster, Predicted = dt_pred_deep)
dt_accuracy_deep <- sum(diag(dt_conf_matrix_deep)) / sum(dt_conf_matrix_deep)
print("Матрица ошибок для дерева решений (глубина 10):")
print(dt_conf_matrix_deep)
print(paste("Точность дерева решений (глубина 10):", round(dt_accuracy_deep, 3)))

# 5) Случайный лес
# Обучаем модель случайного леса
rf_model <- randomForest(Cluster ~ math_score + reading_score + writing_score + 
                           gender + race_ethnicity + parental_level_of_education + 
                           lunch + test_preparation_course, 
                         data = train_data, 
                         ntree = 100,  # Число деревьев
                         importance = TRUE)

# Предсказываем и оцениваем точность
rf_pred <- predict(rf_model, newdata = test_data)
rf_conf_matrix <- table(Actual = test_data$Cluster, Predicted = rf_pred)
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
print("Матрица ошибок для случайного леса:")
print(rf_conf_matrix)
print(paste("Точность случайного леса:", round(rf_accuracy, 3)))

# Визуализируем важность признаков
varImpPlot(rf_model, 
           main = "Рис. 4 - Важность признаков в случайном лесу")

# 6) Сравнение результатов
accuracy_data <- data.frame(
  Method = c("Naive Bayes", "Decision Tree (Default)", "Decision Tree (Depth 3)", 
             "Decision Tree (Depth 10)", "Random Forest"),
  Accuracy = c(nb_accuracy, dt_accuracy_default, dt_accuracy_shallow, dt_accuracy_deep, rf_accuracy)
)

ggplot(accuracy_data, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Рис. 5 - Сравнение точности классификаторов", 
       x = "Метод", 
       y = "Точность") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Naive Bayes" = "skyblue", 
                               "Decision Tree (Default)" = "lightgreen", 
                               "Decision Tree (Depth 3)" = "lightcoral", 
                               "Decision Tree (Depth 10)" = "lightpink", 
                               "Random Forest" = "lightyellow"))

# 7) Визуализация предсказаний (для случайного леса)
test_data$Predicted_Cluster <- rf_pred

fviz_cluster(list(data = scale(test_data[, c("math_score", "reading_score", "writing_score")]), 
                  cluster = as.numeric(test_data$Predicted_Cluster)), 
             geom = "point", 
             main = "Рис. 6 - Предсказанные кластеры (случайный лес) в пространстве PCA") +
  theme_minimal()

fviz_cluster(list(data = scale(test_data[, c("math_score", "reading_score", "writing_score")]), 
                  cluster = as.numeric(test_data$Cluster)), 
             geom = "point", 
             main = "Рис. 7 - Реальные кластеры в пространстве PCA") +
  theme_minimal()
