library(tidyverse)
library(magrittr)
library(ggplot2)
library(caret)
library(ROCit)
getwd() 
# setwd("projekt1") #set working directory

data <- read_csv("Life Expectancy Data.csv", col_names = TRUE)

# to view data
View(data)
str(data)
head(data)
dim(data)
sapply(data, class)
sapply(data, function(x) sum(is.na(x)))

# class type distribution
column_types_count <- table(sapply(data, class))
column_types_count


complete_cases_count <- sum(complete.cases(data))
table(complete.cases(data))

# We are going to check Life expectancy because it is the most important column for us, prediction will be done on this value

# check for missing values in column Life expectancy
missing_values_life_axpextancy <- sum(is.na(data$`Life expectancy`))
missing_values_life_axpextancy
# there is 10 missing values in column price
# check also for empty strings
missing_values_life_axpextancy <- sum(str_detect(data$`Life expectancy`, "^\\s*$"))
missing_values_life_axpextancy

# drop rows with missing values in column Life expectancy
data <- data %>% drop_na(`Life expectancy`)
missing_values_life_axpextancy <- sum(is.na(data$`Life expectancy`))
missing_values_life_axpextancy

data %>%
  count(Status) %>%
  rename(Count = n) %>%
  ggplot(aes(x=Status, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

data %>%
  count(Year) %>%
  rename(Count = n) %>%
  ggplot(aes(x=Year, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

data %>%
  count(`Life expectancy`) %>%
  rename(Count = n) %>%
  ggplot(aes(x=`Life expectancy`, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

data %>%
  count(`Adult Mortality`) %>%
  rename(Count = n) %>%
  ggplot(aes(x=`Adult Mortality`, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()



missing_values_na <- sapply(data, function(x) sum(is.na(x)))
missing_values_na <- data.frame(column = names(missing_values_na), missing_values = missing_values_na)
missing_values_na$type <- "NA"

# Count empty strings for each column
missing_values_empty <- sapply(data, function(x) sum(str_detect(x, "^\\s*$")))
missing_values_empty <- data.frame(column = names(missing_values_empty), missing_values = missing_values_empty)
missing_values_empty$type <- "Empty String"

# Combine the two datasets
combined_missing_values <- rbind(missing_values_na, missing_values_empty)

# Plotting
ggplot(combined_missing_values, aes(x = column, y = missing_values, fill = type)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of Missing and Empty Values for Each Dataset Column",
       x = "Column", y = "Count of Missing and Empty Values") +
  scale_fill_manual(values = c("NA" = "blue", "Empty String" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-----Helper functions-----
panel.cor <- function(x,y, digits=2, prefix="", cex.cor){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r <- abs(cor(x,y,use="complete.obs"))
  txt <- format(c(r,0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt,sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2],0,1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white",...)
}

panel.lm <- function(x, y, col = par("col"), bg = NA, pch = par("pch"),
                     cex = 1, col.smooth = "blue", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(x ~ y), col = "steelblue", ...)
} 
pairs(~ `Life expectancy` + `Adult Mortality` + Alcohol  +
        `percentage expenditure` + BMI + 
        `Total expenditure` + Diphtheria + `HIV/AIDS` + GDP +
        `Income composition of resources` +
        Schooling, 
      data = data,
      upper.panel= NULL, 
      diag.panel = panel.hist,
      lower.panel = panel.lm)


#one hot encode status
data
data %<>% 
  mutate(status_oh = if_else(Status=='Developed', 1, 0)) %>%
  relocate(status_oh)


# train test split
set.seed(123)
train_index <- sample(1:nrow(data), 0.8*nrow(data))
train_data <- data[train_index,]
test_data <- data[-train_index,]
train_data
test_data

                # Vidime, ze aj po random splite je status primerane rozdeleny
train_data %>%
  count(Status) %>%
  rename(Count = n) %>%
  ggplot(aes(x=Status, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

test_data %>%
  count(Status) %>%
  rename(Count = n) %>%
  ggplot(aes(x=Status, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

                # Drop na rows for used colums, to prevent removing during prediction
test_data %<>% 
  select(status_oh, `Life expectancy`, `Adult Mortality`, Alcohol, 
         `percentage expenditure`, BMI, `Total expenditure`, 
         Diphtheria, `HIV/AIDS`, GDP, `Income composition of resources`, 
         Schooling) %>%
  drop_na()

# Experiments:

# Regression:

# 1. percentage expenditure vs GDP nieco???
model = lm(`percentage expenditure` ~ GDP, data = train_data)
summary(model)
qqnorm(model$residuals)
qqline(model$residuals)

plot(model$residuals, model$fitted.values)

shapiro.test(model$residuals)

residuals <- model$residuals
RSS <- sum((residuals-mean(residuals))^2)
RSS

RMSE <- sqrt(sum(((predict(model, test_data)) - test_data$`Life expectancy`)^2/length(test_data$`Life expectancy`)))
RMSE


# 2. Človek, ktorý ... sa dožíva dlhšieho veku. (niečo s alkoholom a choroby)
model = lm(`Life expectancy` ~ `HIV/AIDS` + Alcohol, data = train_data)
summary(model)


qqnorm(model$residuals)
qqline(model$residuals)

plot(model$residuals, model$fitted.values)

shapiro.test(model$residuals)

residuals <- model$residuals
RSS <- sum((residuals-mean(residuals))^2)
RSS

RMSE <- sqrt(sum(((predict(model, test_data)) - test_data$`Life expectancy`)^2/length(test_data$`Life expectancy`)))
RMSE

# 3. Človek, ktorý žije v krajinách s vyšším GDP sa dožíva dlhšieho veku.
model = lm(`Life expectancy` ~ GDP, data = train_data)
summary(model)
qqnorm(model$residuals)
qqline(model$residuals)

plot(model$residuals, model$fitted.values)

shapiro.test(model$residuals)

residuals <- model$residuals
RSS <- sum((residuals-mean(residuals))^2)
RSS

RMSE <- sqrt(sum(((predict(model, test_data)) - test_data$`Life expectancy`)^2/length(test_data$`Life expectancy`)))
RMSE

# 4. človek, ktorý dlhšie študuje a je viac vzdelaný sa dožíva dlhšieho veku.
model = lm(`Life expectancy` ~ Schooling + Alcohol, data = train_data)
summary(model)
qqnorm(model$residuals)
qqline(model$residuals)

plot(model$residuals, model$fitted.values)

shapiro.test(model$residuals)

residuals <- model$residuals
RSS <- sum((residuals-mean(residuals))^2)
RSS

RMSE <- sqrt(sum(((predict(model, test_data)) - test_data$`Life expectancy`)^2/length(test_data$`Life expectancy`)))
RMSE


# All :)
model = lm(`Life expectancy` ~ `Adult Mortality` + Alcohol  +
             `percentage expenditure` + BMI + 
             `Total expenditure` + Diphtheria + `HIV/AIDS` + GDP +
             `Income composition of resources` +
             Schooling, data = train_data)
summary(model)
qqnorm(model$residuals)
qqline(model$residuals)

plot(model$residuals, model$fitted.values)

shapiro.test(model$residuals)

residuals <- model$residuals
RSS <- sum((residuals-mean(residuals))^2)
RSS

RMSE <- sqrt(sum(((predict(model, test_data)) - test_data$`Life expectancy`)^2/length(test_data$`Life expectancy`)))
RMSE


# Classification:

# 1. GDP a Schooling suvisi s tym, ci je krajina rozvojova alebo rozvinuta
library(ROCit)
model = glm(status_oh ~ GDP + Schooling, data = train_data)
summary(model)

model = glm(status_oh ~ GDP + Schooling, data = train_data, family = binomial(link=logit))
summary(model)

predictions <- predict(model, test_data)
as.vector(predictions)

actual <- test_data$status_oh
as.vector(actual)

roc = rocit(predictions, actual)
plot(roc)
cutoff_index <- which.max(roc$TPR + (1 - roc$FPR) - 1)
optimal_cutoff <- roc$Cutoff[cutoff_index]
optimal_cutoff

predictions
predicted_class <- ifelse(predictions >= optimal_cutoff, 1, 0)
predicted_class

caret::confusionMatrix(as.factor(predicted_class), as.factor(actual), positive = "1")


# all :)
model = glm(status_oh ~ `Life expectancy` + `Adult Mortality` + Alcohol  +
              `percentage expenditure` + BMI + 
              `Total expenditure` + Diphtheria + `HIV/AIDS` + GDP +
              `Income composition of resources` +
              Schooling, data = train_data)
summary(model)

predictions <- predict(model, test_data)
as.vector(predictions)

actual <- test_data$status_oh
as.vector(actual)

roc = rocit(predictions, actual)
plot(roc)
cutoff_index <- which.max(roc$TPR + (1 - roc$FPR) - 1)
optimal_cutoff <- roc$Cutoff[cutoff_index]
optimal_cutoff

predictions
predicted_class <- ifelse(predictions >= optimal_cutoff, 1, 0)
predicted_class

caret::confusionMatrix(as.factor(predicted_class), as.factor(actual), positive = "1")



