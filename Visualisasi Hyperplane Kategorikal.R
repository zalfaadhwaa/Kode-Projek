library(dplyr)
library(ggplot2)

# Data yang digunakan
data2 <- trainData %>%
  select(Parental_Education_Level, Motivation_Level, Performance)

data2$Parental_Education_Level <- as.factor(data2$Parental_Education_Level)
data2$Motivation_Level <- as.factor(data2$Motivation_Level)

# Menghitung jumlah data pada setiap kombinasi
summary_table <- data2 %>%
  group_by(Parental_Education_Level, Motivation_Level) %>%
  summarise(Count = n(), .groups = "drop")

# Membuat visualisasi seperti tabel confusion matrix
plot2 <- ggplot(data = summary_table, aes(x = Parental_Education_Level, y = Motivation_Level)) +
  geom_tile(aes(fill = Count), color = "white", alpha = 0.8) +
  geom_text(aes(label = Count), size = 5, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Data Distribution: Parental Education vs Motivation Level",
    x = "Parental Education Level",
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
print(plot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data yang digunakan
data3 <- trainData %>%
  select(Parental_Involvement, Motivation_Level, Performance)

# Pastikan kolom kategorikal sudah berupa faktor
data3$Parental_Involvement <- as.factor(data3$Parental_Involvement)
data3$Motivation_Level <- as.factor(data3$Motivation_Level)

# Menghitung jumlah data pada setiap kombinasi
summary_table2 <- data3 %>%
  group_by(Parental_Involvement, Motivation_Level) %>%
  summarise(Count = n(), .groups = "drop")

# Membuat visualisasi seperti tabel confusion matrix
plot3 <- ggplot(data = summary_table2, aes(x = Parental_Involvement, y = Motivation_Level)) +
  geom_tile(aes(fill = Count), color = "white", alpha = 0.8) +
  geom_text(aes(label = Count), size = 5, color = "black") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(
    title = "Data Distribution: Parental Involvement vs Motivation Level",
    x = "Parental Involvement",
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
print(plot3)
