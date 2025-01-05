str(trainData)

data1 <- trainData %>%
  select(Hours_Studied, Motivation_Level, Performance)

data1$Motivation_Level <- as.numeric(data1$Motivation_Level)

svm_model1 <- svm(Performance ~ Hours_Studied + Motivation_Level, 
                  data = data1, kernel = "radial")

x_range <- seq(min(data1$Hours_Studied), max(data1$Hours_Studied), by = 0.1)
y_range <- seq(min(data1$Motivation_Level), max(data1$Motivation_Level), by = 0.1)
grid1 <- expand.grid(Hours_Studied = x_range, Motivation_Level = y_range)
grid1$Prediction <- predict(svm_model1, grid1)

plot1 <- ggplot() +
  geom_point(data = data1, aes(x = Hours_Studied, y = Motivation_Level, color = Performance)) +
  geom_tile(data = grid1, aes(x = Hours_Studied, y = Motivation_Level, fill = Prediction), alpha = 0.3) +
  labs(title = "SVM: Hours Studied vs Motivation Level", x = "Hours Studied", y = "Motivation Level") +
  theme_minimal()

print(plot1)

