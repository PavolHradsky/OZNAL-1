library(tidyverse)
library(magrittr)
library(ggplot2)
getwd() 
setwd("projekt1") #set working directory

data <- read_csv("AB_NYC_2019.csv", col_names = TRUE)

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

# We are going to check prices because it is the most important column for us, prediction will be done on this value

# check for missing values in column price
missing_values_price <- sum(is.na(data$price))
missing_values_price
# there is no missing values in column price

data %>%
  count(room_type) %>%
  rename(Count = n) %>%
  ggplot(aes(x=room_type, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

data %>%
  count(neighbourhood_group) %>%
  rename(Count = n) %>%
  ggplot(aes(x=neighbourhood_group, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

data %>%
  count(availability_365) %>%
  rename(Count = n) %>%
  ggplot(aes(x=availability_365, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()

data %>%
  filter(availability_365 != 0) %>%
  count(availability_365) %>%
  rename(Count = n) %>%
  ggplot(aes(x=availability_365, y=Count)) + 
  geom_histogram(stat="identity", aes(fill=Count)) + 
  scale_fill_viridis_c()


# these rows will be dropped where price is 0, because it is not possible to rent a room for free
data %>%
  filter(price == 0) %>%
  select(price, host_name, neighbourhood_group, room_type)

zero_price_count <- nrow(data %>% filter(price == 0))
zero_price_count

# we can remove rows 
data <- data %>% filter(price != 0)

# in dataset there are no rows with price 0 and also with price less than 10
nrow(data %>% filter(price < 10))


# Count empty strings for each column (there are no empty strings)
data %>%
  summarise_all(~sum(str_detect(., "^\\s*$")))

missing_values_reviews_per_month <- sum(is.na(data$reviews_per_month))
missing_values_reviews_per_month


missing_values_last_review <- sum(is.na(data$last_review))
missing_values_last_review

#What to clean? can we omit so much missing values????




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



zero_availability_count <- nrow(data %>% filter(availability_365 == 0))
zero_availability_count



ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Price Distribution Histogram", x = "Price", y = "Frequency")

ggplot(data, aes(x = neighbourhood_group, y = price, fill = neighbourhood_group)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Neighbourhood Group", x = "Neighbourhood Group", y = "Price")



# just all columns, linear model
lm_model <- lm(price ~ minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365, data = data)
summary(lm_model)

residuals <- model$residuals
RSS <- sum((residuals-mean(residuals))^2)
RSS
RMSE <- sqrt(mean(residuals^2))
RMSE

data %>% 
  select(host_name, last_review, neighbourhood_group, price) %>%
  group_by(neighbourhood_group) %>% 
  summarise(mean_price = mean(price),
            min_price = min(price),
            max_price = max(price),
            number_of_records = n())

# mean price of housing for each neighbourhood group
data %>%
  select(host_name, last_review, neighbourhood_group, price) %>%
  group_by(neighbourhood_group) %>%
  summarise(mean_price = mean(price),
            min_price = min(price),
            max_price = max(price),
            number_of_records = n()) %>%
  ggplot(aes(x = neighbourhood_group, y = mean_price)) +
  geom_col() +
  labs(title = "Mean Price by Neighbourhood Group", x = "Neighbourhood Group", y = "Mean Price")



# pokus o pair plot, opravit
# pair plot

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

panel.lm <- function(x,y,col = par("col"), bg = NA, pch = par("pch"),
                     cex = 1, col.smooth = "blue",...){
  points(x,y,pch=pch, col=col(alpha = 0.1), bg=bg, cex=cex)
  abline(stats::lm(x ~ y), col = "steelblue", ...)
} 


pairs(data[, c("price", "minimum_nights", "number_of_reviews", "reviews_per_month", "calculated_host_listings_count", "availability_365")],
      upper.panel = panel.cor, 
      diag.panel = panel.hist, 
      lower.panel = panel.lm)









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

panel.lm <- function(x,y,col = par("col"), bg = NA, pch = par("pch"),
                     cex = 1, col.smooth = "blue",...){
  points(x,y,pch=pch, col=col(alpha = 0.1), bg=bg, cex=cex)
  abline(stats::lm(x ~ y), col = "steelblue", ...)
} 

#-----Pair plot-----
data %$%
  pairs( ~ price + minimum_nights + number_of_reviews + reviews_per_month +
           calculated_host_listings_count + availability_365, # Omit skill_moves from plotting       
         upper.panel= NULL, 
         diag.panel=panel.hist,
         lower.panel=panel.smooth)


data <- read_csv("salary_prediction_data.csv", col_names = TRUE)
data

# one hot encode Gender

data <- data
data$Gender <- as.factor(data$Gender)
data <- data
data$Gender <- as.numeric(data$Gender)
data$Gender <- as.factor(data$Gender)
data

data$Gender <- as.factor(data$Gender)
data$Education <- as.factor(data$Education)

# Create the pairs plot
ggpairs(data, 
        columns = c("Salary", "Experience", "Location", "Job_Title", "Age", "Gender", "Education"),
        upper = list(continuous = "cor"),
        lower = list(combo = "box"),
        diag = list(continuous = "barDiag", discrete = "barDiag"))

# encode 
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
pairs(~ Salary + Experience + Age, 
      data = data,
      upper.panel= NULL, 
      diag.panel = panel.hist,
      lower.panel = panel.lm)
