
library(tidyverse)
library(dplyr)
library(lubridate)

meals <- read.csv("DataSets/mealplan.csv", skip=3, header=TRUE, check.names = FALSE)
class(meals)
dim(meals)
names(meals)
str(meals)
glimpse(meals)
summary(meals)
head(meals)
tail(meals)

#remove first row
meals1 <- meals[2:4,]

meals1 <- gather(meals1, date, value, -Meal, -Location)
head(meals1)

meals1 <- separate(meals1, value, c("entree", "price"), sep=',')
str(meals1)

meals1$date <- mdy(meals1$date)
head(meals1)

meals1$price <- gsub("\\$","",meals1$price)
head(meals1)

meals1$price <- as.numeric(meals1$price)

hist(meals1$price)
boxplot(meals1$price)

unique(meals1$price)
meals1$price[meals1$price == 400] <- 4.00
hist(meals1$price)
summary(meals1)

meals1$Meal <- str_trim(meals1$Meal)
meals1$entree <- str_trim(meals1$entree)
meals1$Location <- str_trim(meals1$Location)

meals1$entree[meals1$entree == ""] <- "NA"

meals1$Meal <- as.character(meals1$Meal)
meals1$Location <- as.character(meals1$Location)

meals2 <- meals1[,]

names(meals2) <- tolower(names(meals2))
head(meals2)

meals2$entree <- tolower(meals2$entree)

mean(meals2$price, na.rm = TRUE)

maxprice <- max(meals2$price, na.rm = TRUE)
print(maxprice)

meals2$price_imputed <- meals2$price
meals2$price_imputed[is.na(meals2$price_imputed)] <- maxprice

sum(meals2$price_imputed)

