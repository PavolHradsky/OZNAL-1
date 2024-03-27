library(tidyverse)
library(magrittr)
library(ggplot2)
getwd() 
setwd("projekt1") #set working directory

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

# train test split
set.seed(123)
train_index <- sample(1:nrow(data), 0.8*nrow(data))
train_data <- data[train_index,]
test_data <- data[-train_index,]
train_data
test_data

# svm model
install.packages("e1071")
library(e1071)
model <- svm(`Life expectancy` ~ `Adult Mortality` + Alcohol  +
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

classifier = svm(formula = `Life expectancy` ~ ., 
                 data = train_data, 
                 type = 'C-classification', 
                 kernel = 'linear') 
summary(classifier)







