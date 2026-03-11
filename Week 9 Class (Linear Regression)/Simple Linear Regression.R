# Muhammad Aqil Rahimi
# Simple Linear Regression

# data of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
# data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# 1.Apply the lm() function
relation <- lm(y~x)
print(relation)

# 2.Find weight of a person with height 170
x_test <- data.frame(x = 170)
result <- predict(relation,x_test)
print(round(result, digit=2))

# Find weight of a person with height 189
x_test <- data.frame(x = 189)
result <- predict(relation,x_test)
print(round(result, digit=2))

# 3.Plot the chart
plot(x,y,col = "blue",main = "Height & Weight Regression", 
     abline(lm(y~x)),pch = 16,xlab = "Height in cm", ylab = "Weight in Kg")

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# 4.Training, Testing & Performance Analysis
# data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
# Creating data frame
data1= data.frame(x,y)

# splitting data into training and testing
data1_train<-data1[1:7,]
data1_test<-data1[8:10,]

# Apply the lm() function
relation <- lm(y~x, data1_train)
print(relation)

# Make prediction
x_text <- data.frame(x= data1_test$x)
result <- predict(relation,x_text)
print(result)

# Performance Measurement using MAPE (Mean Absolute Percentage Error) 
mape <- mean(abs((data1_test$y -result)/data1_test$y)*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

# Performance Measurement by Creating a data frame to combine the actuals and predicted values
actuals_preds <- data.frame(cbind(actuals=data1_test$y,predicteds=result))
mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicteds )/ actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")

df<-read.csv("C:/Users/aqilr/Downloads/Week 9 Class (Linear Regression)/income_happiness.csv")

# Split data into training (80%) and testing (20%) sets
# Randomly select row indices for training
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Apply the lm() function
relation <- lm(happiness~income, data=train_data)
print(relation)

# Prediction
a <- data.frame(x=test_data$income)
colnames(a) <- "income"
result <- predict(relation,a)

#Plot
plot(test_data$income,test_data$happiness,col="red",abline(lm(happiness~income,
     data=train_data)),pch = 16,xlab = "income",ylab = "happiness")

# 5.Class Activity

# Load the dataset into a data frame
Experience_Years <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Monthly_Salary <- c(2500, 2700, 3000, 3400, 3900, 4400, 5000, 5600, 6200, 6900)
data <- data.frame(Experience_Years, Monthly_Salary)

# ==============================================================================
# Part (a): Create Simple Linear Regression Model (70% Train / 30% Test)
# ==============================================================================

# Set a seed so the random split gives you the exact same result every time
set.seed(42)

# Determine the size for the training set (70% of 10 rows = 7 rows)
train_size <- floor(0.70 * nrow(data))

# Randomly select indices for the training set
train_indices <- sample(seq_len(nrow(data)), size = train_size)

# Split the dataset
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build the simple linear regression model using the training data
model <- lm(Monthly_Salary ~ Experience_Years, data = train_data)

# Display the summary of the trained model
cat("\n--- Model Summary ---\n")
print(summary(model))

# Test the model on the 30% testing data
predictions <- predict(model, newdata = test_data)

cat("\n--- Test Set Predictions ---\n")
test_results <- data.frame(
  Actual_Experience = test_data$Experience_Years,
  Actual_Salary = test_data$Monthly_Salary,
  Predicted_Salary = round(predictions, 2)
)
print(test_results)

# ==============================================================================
# Part (b): Visualize the Output
# ==============================================================================

# Create a scatter plot of the full dataset
plot(data$Experience_Years, data$Monthly_Salary,
     main = "Regression Output: Salary vs Years of Experience",
     xlab = "Experience (Years)",
     ylab = "Monthly Salary (RM)",
     pch = 16,        # Solid circle for data points
     col = "blue")    # Blue color for data points

# Add the regression line predicted by the model
abline(model, col = "red", lwd = 2)

# Add a legend for clarity
legend("topleft", 
       legend = c("Actual Data", "Regression Line"),
       col = c("blue", "red"), 
       pch = c(16, NA), 
       lty = c(NA, 1), 
       lwd = c(NA, 2))