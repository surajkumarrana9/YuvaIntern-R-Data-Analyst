# ==============================================================================
# YuvaIntern R Data Analyst Track - Week 2
# Script: week2_visualization.R
# Intern: Suraj Kumar Rana
# Focus: Data Visualization and Insight Communication using ggplot2
# ==============================================================================

library(ggplot2)

# 1. Set Working Directory to Week 2
setwd("E:/Internship/YuvaIntern_R_Data_Analyst/Week_2")

# 2. Load Dataset & Prepare Variables
titanic_data <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Cleaned transformations
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Embarked[titanic_data$Embarked == "" | is.na(titanic_data$Embarked)] <- "S"

# Factor labels for clean presentation
titanic_data$Survival_Status <- factor(titanic_data$Survived, levels = c(0, 1), labels = c("Perished", "Survived"))
titanic_data$Passenger_Class <- factor(titanic_data$Pclass, levels = c(1, 2, 3), labels = c("1st Class", "2nd Class", "3rd Class"))
titanic_data$Gender <- factor(titanic_data$Sex, levels = c("male", "female"), labels = c("Male", "Female"))

# ==============================================================================
# 3. Generate Visualizations & Save Images
# ==============================================================================

# Plot 1: Survival Count by Gender
p1 <- ggplot(titanic_data, aes(x = Gender, fill = Survival_Status)) +
  geom_bar(position = "dodge", alpha = 0.85, width = 0.6) +
  scale_fill_manual(values = c("Perished" = "#E63946", "Survived" = "#2A9D8F")) +
  labs(
    title = "Survival Count by Passenger Gender",
    subtitle = "Female passengers exhibited significantly higher survival numbers",
    x = "Passenger Gender",
    y = "Number of Passengers",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p1)
ggsave("plot1_survival_gender.png", p1, width = 6.5, height = 4, dpi = 300)

# Plot 2: Survival Proportion Across Classes
p2 <- ggplot(titanic_data, aes(x = Passenger_Class, fill = Survival_Status)) +
  geom_bar(position = "fill", alpha = 0.85, width = 0.55) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_fill_manual(values = c("Perished" = "#E63946", "Survived" = "#457B9D")) +
  labs(
    title = "Survival Proportion Across Passenger Classes",
    subtitle = "1st Class passengers experienced a distinct survival advantage",
    x = "Passenger Class",
    y = "Proportion of Passengers",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p2)
ggsave("plot2_survival_pclass.png", p2, width = 6.5, height = 4, dpi = 300)

# Plot 3: Age Distribution Density
p3 <- ggplot(titanic_data, aes(x = Age, fill = Survival_Status)) +
  geom_density(alpha = 0.45) +
  scale_fill_manual(values = c("Perished" = "#E63946", "Survived" = "#2A9D8F")) +
  labs(
    title = "Age Distribution Density by Survival Outcome",
    subtitle = "Young children showed higher relative survival density",
    x = "Age (in Years)",
    y = "Density",
    fill = "Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p3)
ggsave("plot3_age_density.png", p3, width = 6.5, height = 4, dpi = 300)

# Plot 4: Ticket Fare Spread Boxplot
p4 <- ggplot(titanic_data, aes(x = Passenger_Class, y = Fare, fill = Passenger_Class)) +
  geom_boxplot(outlier.color = "#E63946", outlier.alpha = 0.5, alpha = 0.7, show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 150)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Ticket Fare Distribution Across Classes",
    subtitle = "Higher median fare and wider dispersion observed in 1st Class",
    x = "Passenger Class",
    y = "Ticket Fare (in GBP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p4)
ggsave("plot4_fare_distribution.png", p4, width = 6.5, height = 4, dpi = 300)

cat("\n=== All 4 Plots Created and Saved in Week_2 Folder ===\n")
