# import library
library(readr)
library(randomForest)
library(caret)
library(ggplot2)
library(Metrics)

# loading dataset
Survey_AI <- read.csv("~/Documents/assignment/Data Analytics and Machine Learning/Assignment 2/Survey_AI.csv")

# Identify categorical columns
categorical_cols <- sapply(Survey_AI, is.factor)

# Label encode categorical columns
for (col in names(Survey_AI)[categorical_cols]) {
  Survey_AI[[col]] <- as.integer(factor(Survey_AI[[col]]))
}

# remove Q2,Q6 columns
Survey_AI <- subset(Survey_AI, select = -c(Q2.AI_sources, Q6.Domains))

# Modify the gpa column
gpa_mapping <- c("5.0-5.4" = 1, "5.5-5.9" = 2, "6.0-6.4" = 3, "6.5-6.9" = 4, "7.0-7.4" = 5,
                 "7.5-7.9" = 6, "8.0-8.4" = 7, "8.5-8.9" = 8, "9.0-9.4" = 9, "9.5-10" = 10)
Survey_AI$Q16.GPA <- gpa_mapping[Survey_AI$Q16.GPA]
Survey_AI$Q16.GPA

# basic information
str(Survey_AI)
summary(Survey_AI)
head(Survey_AI)
dim(Survey_AI)
names(Survey_AI)

# check for missing values
table(is.na(Survey_AI))
colSums(is.na(Survey_AI))

# remove missing values
Survey_AI <- na.omit(Survey_AI)
dim(Survey_AI)
colnames(Survey_AI) <- gsub("_", "", colnames(Survey_AI))
colnames(Survey_AI) <- gsub("/", "", colnames(Survey_AI))
head(Survey_AI)

# splitting
X <- Survey_AI[, !(names(Survey_AI) %in% 'Q16.GPA')]  
y <- Survey_AI[, 'Q16.GPA']                      

# split into training and testing
# Set seed for reproducibility
set.seed(123) 
# 70% for training, adjust as needed
train_indices <- createDataPartition(y, p = 0.7, list = FALSE) 
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Linear regression model
linear_model  <- lm(y_train ~ ., data = X_train)
summary(linear_model)
par(mfrow=c(2,2))
plot(linear_model)
linear_model <- lm(log(y_train) ~ ., data = X_train)

# Summary model
summary(linear_model)

# Predict response values for new data
lm_predict = predict(linear_model, newdata = X_test)

# Calculate RMSE
rmse <- sqrt(mean((y_test - exp(lm_predict))^2))

# View the RMSE
paste("RMSE:", rmse)

# View the predictions
lm_predict

# Random forest model

# Set tuning parameters
control <- trainControl(method = "cv", number = 5)

# train model
forest_model <- randomForest(X_train, y_train)
print(forest_model)
varImpPlot(forest_model)

# Summary model
summary(forest_model)

# Predict response values for new data
fm_predict <- predict(forest_model, newdata = X_test)

# View the predictions
fm_predict

# Calculate RMSE
rmse <- sqrt(mean((y_test - fm_predict)^2))

# View the RMSE
paste("RMSE:", rmse)

# Create a data frame for visualization
results <- data.frame(Actual = y_test, LinearRegression = lm_predict, RandomForest = fm_predict)

# Scatter plot comparing actual vs predicted values
ggplot(results, aes(x = Actual, y = LinearRegression)) +
  geom_point(color = "red") +
  geom_smooth(color = "black", method = "lm") +
  labs(x = "Actual", y = "Predicted", title = "Linear Regression Model")

ggplot(results, aes(x = Actual, y = RandomForest)) +
  geom_point(color = "yellow") +
  geom_smooth(color = "green", method = "lm") +
  labs(x = "Actual", y = "Predicted", title = "Random Forest Model")

