library(e1071)
library(caret)
library(dplyr)
library(ggplot2)

svm_model_na <- svm(Performance ~ Teacher_Quality + Parental_Education_Level + Distance_from_Home
                 , data = trainData, kernel = "radial")
predictions_na <- predict(svm_model_na, testData)

conf_matrix_na <- confusionMatrix(predictions_na, testData$Performance)
print(conf_matrix_na)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data untuk Teacher Quality vs Motivation Level
data_teacher <- trainData %>%
  select(Teacher_Quality, Motivation_Level, Performance)

data_teacher$Teacher_Quality <- as.factor(data_teacher$Teacher_Quality)
data_teacher$Motivation_Level <- as.factor(data_teacher$Motivation_Level)

summary_table_teacher <- data_teacher %>%
  group_by(Teacher_Quality, Motivation_Level) %>%
  summarise(Count = n(), .groups = "drop")

# Plot untuk Teacher Quality vs Motivation Level
plot_teacher <- ggplot(data = summary_table_teacher, aes(x = Teacher_Quality, y = Motivation_Level)) +
  geom_tile(aes(fill = Count), color = "white", alpha = 0.8) +
  geom_text(aes(label = Count), size = 5, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Data Distribution: Teacher Quality vs Motivation Level",
    x = "Teacher Quality",
    y = "Motivation Level",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Data untuk Distance from Home vs Motivation Level
data_distance <- trainData %>%
  select(Distance_from_Home, Motivation_Level, Performance)

data_distance$Distance_from_Home <- as.factor(data_distance$Distance_from_Home)
data_distance$Motivation_Level <- as.factor(data_distance$Motivation_Level)

summary_table_distance <- data_distance %>%
  group_by(Distance_from_Home, Motivation_Level) %>%
  summarise(Count = n(), .groups = "drop")

# Plot untuk Distance from Home vs Motivation Level
plot_distance <- ggplot(data = summary_table_distance, aes(x = Distance_from_Home, y = Motivation_Level)) +
  geom_tile(aes(fill = Count), color = "white", alpha = 0.8) +
  geom_text(aes(label = Count), size = 5, color = "black") +
  scale_fill_gradient(low = "lightcoral", high = "darkred") +
  labs(
    title = "Data Distribution: Distance from Home vs Motivation Level",
    x = "Distance from Home",
    y = "Motivation Level",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Menampilkan plot
print(plot_teacher)
print(plot_distance)
