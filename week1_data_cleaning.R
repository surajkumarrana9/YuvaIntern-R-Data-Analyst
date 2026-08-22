# 1. Base dataset reload (Using standard data frame name 'titanic_data')
titanic_data <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# 2. Check structure & missing values
cat("=== STRUCTURE ===\n")
str(titanic_data)
cat("\n=== MISSING VALUES (INITIAL) ===\n")
colSums(is.na(titanic_data))

# 3. Imputation (Age -> Median, Embarked -> Mode)
titanic_data$Embarked[titanic_data$Embarked == "" | is.na(titanic_data$Embarked)] <- "S"
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)

# 4. Outlier Capping (Fare)
Q1 <- quantile(titanic_data$Fare, 0.25, na.rm = TRUE)
Q3 <- quantile(titanic_data$Fare, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower_bound <- max(0, Q1 - 1.5 * IQR_val)
upper_bound <- Q3 + 1.5 * IQR_val
titanic_data$Fare_Capped <- pmin(pmax(titanic_data$Fare, lower_bound), upper_bound)

# 5. Encoding & Normalization
titanic_data$Sex_Encoded <- ifelse(titanic_data$Sex == "male", 1, 0)
titanic_data$Fare_Normalized <- as.vector(scale(titanic_data$Fare_Capped))

cat("\n=== MISSING VALUES (POST CLEANING) ===\n")
colSums(is.na(titanic_data))

# 6. Built-in Base Visualizations (No external packages needed)
# Graph 1: Survival Counts
barplot(table(titanic_data$Survived), 
        names.arg = c("Perished (0)", "Survived (1)"),
        col = c("#E63946", "#2A9D8F"),
        main = "Titanic Survival Count",
        ylab = "Passenger Count",
        xlab = "Survival Status")

# Graph 2: Age Distribution Histogram
hist(titanic_data$Age, 
     breaks = 20, 
     col = "#457B9D", 
     border = "white",
     main = "Age Distribution (Imputed)", 
     xlab = "Age in Years", 
     ylab = "Frequency")