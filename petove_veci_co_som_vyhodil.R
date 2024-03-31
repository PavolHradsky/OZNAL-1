# plot of distiribution of missing values in each column
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values <- data.frame(column = names(missing_values), missing_values = missing_values)
ggplot(missing_values, aes(x = column, y = missing_values)) +
  geom_col() +
  labs(title = "Distribúcia chýbajúcich hodnôt pre jednotlivé stĺpce datasetu", x = "Column", y = "Missing Values")

# plot of distiribution of missing values in each column
missing_values <- sapply(data, function(x) sum(str_detect(x, "^\\s*$")))
missing_values <- data.frame(column = names(missing_values), missing_values = missing_values)
ggplot(missing_values, aes(x = column, y = missing_values)) +
  geom_col() +
  labs(title = "Distribúcia chýbajúcich hodnôt pre jednotlivé stĺpce datasetu", x = "Column", y = "Missing Values")




# linear regression model
model <- lm(`Life expectancy` ~ `Adult Mortality` + Alcohol  +
              `percentage expenditure` + BMI + 
              `Total expenditure` + Diphtheria + `HIV/AIDS` + GDP +
              `Income composition of resources` +
              Schooling, data = data)
summary(model)

# generalized linear model - reapir it 
model <- glm(`Life expectancy` ~ `Adult Mortality` + Alcohol  +
               `percentage expenditure` + BMI + 
               `Total expenditure` + Diphtheria + `HIV/AIDS` + GDP +
               `Income composition of resources` +
               Schooling, data = data, family = binomial)
summary(model)

model <- lm(`Alcohol` ~ `Adult Mortality` + BMI + `HIV/AIDS` + GDP +
              `Income composition of resources` +
              Schooling, data = data)
summary(model)





# svm model
install.packages("e1071")
library(e1071)
model <- svm(status_oh ~ `Life expectancy` + `Adult Mortality` + Alcohol  +
               `percentage expenditure` + BMI + 
               `Total expenditure` + Diphtheria + `HIV/AIDS` + GDP +
               `Income composition of resources` +
               Schooling, data = train_data)
summary(model)

predictions <- predict(model, test_data)
predictions

actual <- test_data$`Life expectancy`
actual

# confusion matrix
confusion_matrix <- table(actual, predictions)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy



classifier = svm(formula = `Life expectancy` ~ ., 
                 data = train_data, 
                 type = 'C-classification', 
                 kernel = 'linear') 
summary(classifier)

svm_model <- svm(`Life expectancy` ~ ., data = train_data, kernel = "radial")
svm_model

svm_pred <- predict(svm_model, test_data)



install.packages("countrycode")
library(countrycode)
data$Continent <- countrycode(data$Country, "country.name", "continent")
data
