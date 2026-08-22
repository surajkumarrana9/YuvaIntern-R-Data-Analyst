# ==============================================================================
# YuvaIntern R Data Analyst Track - Week 3
# Script: week3_modeling.R
# Intern: Suraj Kumar Rana
# Focus: Statistical Hypothesis Testing & Logistic Regression Modeling (Base R)
# ==============================================================================

# 1. Path Setup & Dataset Load
# Week_1 se titanic dataset load kar rahe hain
data_path <- "E:/Internship/YuvaIntern_R_Data_Analyst/Week_1/titanic.csv"
if(!file.exists(data_path)) {
  data_path <- "titanic.csv"
}
titanic_data <- read.csv(data_path, stringsAsFactors = FALSE)

# 2. Data Preprocessing & Cleaning
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Embarked[titanic_data$Embarked == "" | is.na(titanic_data$Embarked)] <- "S"
titanic_data$Sex_Binary <- ifelse(titanic_data$Sex == "female", 1, 0) # Female=1, Male=0

# ==============================================================================
# 3. STATISTICAL HYPOTHESIS TESTING
# ==============================================================================
cat("\n=== HYPOTHESIS TEST 1: Chi-Square Test (Sex vs Survived) ===\n")
chi_res <- chisq.test(table(titanic_data$Sex, titanic_data$Survived))
print(chi_res)

cat("\n=== HYPOTHESIS TEST 2: Two-Sample T-Test (Fare vs Survived) ===\n")
t_res <- t.test(Fare ~ Survived, data = titanic_data)
print(t_res)

# ==============================================================================
# 4. TRAIN / TEST SPLIT (80% Train, 20% Test)
# ==============================================================================
set.seed(42)
train_indices <- sample(1:nrow(titanic_data), size = 0.8 * nrow(titanic_data))
train_data <- titanic_data[train_indices, ]
test_data  <- titanic_data[-train_indices, ]

# ==============================================================================
# 5. LOGISTIC REGRESSION MODEL TRAINING
# ==============================================================================
model <- glm(Survived ~ Pclass + Sex_Binary + Age + SibSp + Fare, 
             data = train_data, 
             family = binomial(link = "logit"))

cat("\n=== LOGISTIC REGRESSION MODEL SUMMARY ===\n")
print(summary(model))

# ==============================================================================
# 6. MODEL EVALUATION (CONFUSION MATRIX & METRICS)
# ==============================================================================
test_data$prob <- predict(model, newdata = test_data, type = "response")
test_data$pred <- ifelse(test_data$prob >= 0.5, 1, 0)

conf_matrix <- table(Actual = test_data$Survived, Predicted = test_data$pred)
cat("\n=== CONFUSION MATRIX (TEST SET) ===\n")
print(conf_matrix)

# Calculate Metric Scores
acc <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall    <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1        <- 2 * (precision * recall) / (precision + recall)

cat(sprintf("\nTest Accuracy:  %.2f%%\n", acc * 100))
cat(sprintf("Precision:      %.2f%%\n", precision * 100))
cat(sprintf("Recall:         %.2f%%\n", recall * 100))
cat(sprintf("F1-Score:       %.4f\n", f1))

# ==============================================================================
# 7. EXPORT DIAGNOSTIC PLOTS (ROC & Residuals)
# ==============================================================================
# Week 3 folder ensure karein
if(!dir.exists("E:/Internship/YuvaIntern_R_Data_Analyst/Week_3")) {
  dir.create("E:/Internship/YuvaIntern_R_Data_Analyst/Week_3", recursive = TRUE)
}
setwd("E:/Internship/YuvaIntern_R_Data_Analyst/Week_3")

# Plot 1: Logistic Regression Diagnostic Residual Plot
png("model_diagnostic_residuals.png", width = 1800, height = 1200, res = 300)
plot(model$fitted.values, residuals(model, type = "deviance"),
     pch = 19, col = rgb(0.1, 0.3, 0.6, 0.5),
     main = "Residual Deviance vs Fitted Probabilities",
     xlab = "Fitted Survival Probabilities",
     ylab = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2, lwd = 2)
dev.off()

cat("\n=== Week 3 Statistical Modeling & Diagnostics Complete ===\n")