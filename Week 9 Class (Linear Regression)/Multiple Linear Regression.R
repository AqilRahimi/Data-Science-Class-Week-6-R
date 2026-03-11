# Multiple Linear Regression

# 1.mtcars dataset
#built in data
data(mtcars)
head(mtcars)
str(mtcars)

#model the MLR
model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
#invesitigate the properties of the model
summary(model)

#split data into train and test sets
data.train<- mtcars[1:22,]
data.test<- mtcars[23:32,]

#modelling
relation <-lm(mpg ~ hp +wt+cyl, data = data.train)
summary(relation)

# Prediction
a <- data.frame(hp = data.test$hp, wt = data.test$wt, cyl = data.test$cyl)
result <- predict(relation, a)
print(round(result, digits = 2))

# Performance Measurement
mape <- mean(abs((data.test$mpg - result)/ data.test$mpg )*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

actuals_preds <- data.frame(cbind(actuals=data.test$mpg,predicted=result))
View(actuals_preds )
correlation_accuracy <- cor(actuals_preds)

mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicted)/actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")

# Class Activity

# Load the dataset based on the table provided in the image
Ozone <- c(11, 11, 11, 12, 12, 13, 13, 13, 13, 14)
Solar_R <- c(290, 44, 320, 149, 120, 137, 112, 27, 238, 274)
Wind <- c(9.2, 9.7, 16.6, 12.6, 11.5, 10.3, 11.5, 10.3, 12.6, 10.9)
Temp <- c(66, 62, 73, 74, 73, 76, 71, 76, 64, 68)

# Combine into a single data frame
data <- data.frame(Ozone, Solar.R = Solar_R, Wind, Temp)

# ==============================================================================
# Task: Create Multiple Linear Regression Model (70% Train / 30% Test)
# ==============================================================================

# Set a seed for reproducibility so the random split is consistent
set.seed(123)

# Calculate the size for the training set (70% of 10 rows = 7 rows)
train_size <- floor(0.70 * nrow(data))

# Randomly select row indices for the training set
train_indices <- sample(seq_len(nrow(data)), size = train_size)

# Split the dataset into training and testing sets
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build the multiple linear regression model using the training data
# The formula Ozone ~ Solar.R + Wind + Temp uses all three independent variables
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = train_data)

# Display the summary of the trained model to see coefficients and p-values
cat("\n--- Multiple Regression Model Summary ---\n")
print(summary(model))

# Test the model by predicting Ozone values on the 30% unseen testing data
predictions <- predict(model, newdata = test_data)

cat("\n--- Test Set Predictions ---\n")
test_results <- data.frame(
  Actual_Ozone = test_data$Ozone,
  Predicted_Ozone = round(predictions, 2),
  Solar_R_Input = test_data$Solar.R,
  Wind_Input = test_data$Wind,
  Temp_Input = test_data$Temp
)
print(test_results)