library(tidyverse)
library(magrittr)
library(ggplot2)
getwd() 
#setwd("") #set working directory

data <- read.csv("AB_NYC_2019.csv", header = TRUE, sep = ",")


View(data)
str(data)
head(data)
dim(data)
sapply(data, class)
sapply(data, function(x) sum(is.na(x)))
column_types_count <- table(sapply(data, class))
column_types_count

complete_cases_count <- sum(complete.cases(data))
table(complete.cases(data))

ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Price Distribution Histogram", x = "Price", y = "Frequency")

ggplot(data, aes(x = neighbourhood_group, y = price, fill = neighbourhood_group)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Neighbourhood Group", x = "Neighbourhood Group", y = "Price")

# just all columns
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











# count classifier values in column transmission
data %>% count(transmission)
# remove rows where transmission is not automatic or manual and count how many rows were affected
data_filtered <- data %>% filter(transmission %in% c("automatic", "manual"))
# Count how many rows were affected
rows_affected <- nrow(data) - nrow(data_filtered)
rows_affected


data %>% group_by(year) %>% summarise(count = n()) %>% ggplot(aes(x = year, y = count)) + geom_col()
data_filtered %>% group_by(year) %>% summarise(count = n()) %>% ggplot(aes(x = year, y = count)) + geom_col()


data_filtered %>% group_by(state) %>% summarise(avg_price = mean(sellingprice)) %>% ggplot(aes(x = state, y = avg_price)) + geom_col()
data %>% group_by(state) %>% summarise(avg_price = mean(sellingprice)) %>% ggplot(aes(x = state, y = avg_price)) + geom_col()

# wait for generation little bit, influence of year on selling price
ggplot(data, aes(x = year, y = sellingprice)) +
  geom_point() +
  labs(x = "Year of production", y = "Selling Price", title = "Linear relation between year and selling price") +
  theme_minimal()


# influence of odometer on selling price
ggplot(data, aes(x = odometer, y = sellingprice)) +
  geom_point() +
  labs(x = "Odometer", y = "Selling Price") +
  theme_minimal()


# check if duplicates are in column vin and how many of them, so we are looking for cars wich were not reselled
unique_record_not_reselled <- data %>% distinct(vin, .keep_all = TRUE)
duplicates_count <-  nrow(data) - nrow(unique_record_not_reselled)
duplicates_count
unique_record_not_reselled <- data %>% distinct(vin, .keep_all = TRUE)
unique_record_not_reselled


# find the most expensive car 
most_expensive_car <- data %>%
  filter(!is.na(sellingprice)) %>%
  filter(sellingprice == max(sellingprice))
most_expensive_car

# find the cheapest car
cheapest_car <- data %>%
  filter(!is.na(sellingprice)) %>%
  filter(sellingprice == min(sellingprice))
cheapest_car

# count average selling price 
average_selling_price <- mean(data$sellingprice, na.rm = TRUE)
average_selling_price

# most common color 
data$color <- trimws(data$color)

# Count occurrences of each color
color_counts <- table(data$color)
color_counts

most_common_color <- names(color_counts)[which.max(color_counts)]
most_common_color

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

#make pair plot, not working yet

pairs(data[,c("year", "sellingprice", "odometer")], lower.panel = panel.smooth, upper.panel = NULL, diag.panel = panel.hist)

# log transformation
print(head(data["sellingprice"]))
data$sellingprice <- log(data$sellingprice)
cat("\nLog-transformed wages:\n")
print(head(data["sellingprice"]))



