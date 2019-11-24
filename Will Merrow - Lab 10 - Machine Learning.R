rm(list = ls(all = TRUE))

install.packages("factoextra")
install.packages("rpart") #to make decision trees
install.packages("rpart.plot") #to visualize decision trees
install.packages("rattle")
install.packages("RColorBrewer") #nice colors

library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(class)
library(factoextra)

data("txhousing")
?txhousing
summary(txhousing)
head(txhousing)

lm_housing <- lm(median ~ listings, data=txhousing)

unseen <- data.frame(listings = 800)
predict(lm_housing, unseen)
summary(lm_housing)

# we should likely add other variables. Volume (total value of sales) is a better predictor.
lm_housing1 <- lm(median ~ volume, data=txhousing)
summary(lm_housing1)

# multiple regression

street <- read_csv("Datasets/streeteasy.csv")
summary(street)

street_select6 <- street %>% select(rent, bedrooms, bathrooms, size_sqft, min_to_subway, floor, building_age_yrs)
str(street_select6)

lm_street_select6 <- lm(rent ~ ., data=street_select6)
summary(lm_street_select6)

lm_street_size <- lm(rent ~ size_sqft, data=street_select6)
summary(lm_street_size)

street_select16 <- street[,1:17]
#inspect your data
str(street_select16)
lm_street_select16 <- lm(rent ~ ., data=street_select16)
summary(lm_street_select16)
# The best model is the one that includes all variables except neighborhood, supermarket, and borough. 
# Bedrooms, bathrooms, size, and floor contribute the most to the output.


# decision trees

set.seed(1234)

train <- read.csv("DataSets/titanic/train.csv", header=TRUE)
test <- read.csv("DataSets/titanic/test.csv", header=TRUE)

str(train)
str(test)

train <- train %>%
  select(Pclass, Sex, Age, Survived)
test <- test %>%
  select(Pclass, Sex, Age, Survived)

tree <- rpart(Survived ~ Pclass + Sex + Age , train, method = "class") 
fancyRpartPlot(tree)

pred <- predict(tree, test, type = "class")

conf <- table(test$Survived, pred)
conf

# Assign TP, FN, FP and TN using conf
TP <- conf[1, 1] # this will be 212
FN <- conf[1, 2] # this will be 78
FP <- conf[2,1] 
TN <- conf[2,2] 

# Calculate and print the accuracy: acc
acc <- sum(TP,TN)/sum(TP,TN,FP,FN)
acc

# Calculate and print out the precision: prec
prec <- TP/sum(TP,FP)
prec

# Calculate and print out the recall: rec
rec <- TP/sum(TP,FN)
rec

acc <- sum(diag(conf))/sum(conf)
acc


# classification: KNN

train <- drop_na(train)
test <- drop_na(test)
train_labels <- train$Survived
test_labels <- test$Survived

knn_train <- train
knn_test <- test
knn_train$Sex <- as.factor(knn_train$Sex)
knn_test$Sex <- as.factor(knn_test$Sex)
knn_train$Sex <- as.factor(gsub("male", "1", knn_train$Sex))
knn_train$Sex <- as.factor(gsub("female", "0", knn_train$Sex))
knn_test$Sex <- as.factor(gsub("male", "1", knn_test$Sex))
knn_test$Sex <- as.factor(gsub("female", "0", knn_test$Sex))
knn_train$Sex <- as.numeric(knn_train$Sex)
knn_test$Sex <- as.numeric(knn_test$Sex)
knn_train$Survived <- NULL
knn_test$Survived <- NULL

min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age)/(max_age - min_age)

k_pred <- knn(train=knn_train, test=knn_test, cl=train_labels, k=5)
k_pred

conf <- table(test_labels, k_pred)
conf

my_titanic <- data.frame("Age" = .6, "Pclass" = .5, "Sex" = 0)
new_k_pred <- knn(train=knn_train, test=my_titanic, cl=train_labels, k=5)
new_k_pred

# clustering

set.seed(1234)
data(iris)

my_iris <- iris[,1:4]
species <- iris$Species

kmeans_iris <- kmeans(my_iris, centers=3, nstart=10)
kmeans_iris

table(species, kmeans_iris$cluster)

plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)

fviz_cluster(kmeans_iris, data = my_iris)

# Initialize total within sum of squares error: wss
wss_tot <- 0
for (i in 1:15) {
  km_out <- kmeans(my_iris, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss_tot[i] <- km_out$tot.withinss/km_out$totss
}

plot(1:15, wss_tot, type = "b",
     xlab = "Number of Clusters",
     ylab = "WSS/TSS")

data("USArrests")
str(USArrests)

#1
wss_tot <- 0
for (i in 1:15) {
  km_out <- kmeans(USArrests, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss_tot[i] <- km_out$tot.withinss/km_out$totss
}

#2
plot(1:15, wss_tot, type = "b",
     xlab = "Number of Clusters",
     ylab = "WSS/TSS")

#3
kmeans_arrests <- kmeans(USArrests, centers= 2, nstart=10)
kmeans_arrests

#4
plot(Murder ~ Assault, data = USArrests, col = kmeans_arrests$cluster)
fviz_cluster(kmeans_arrests, data = USArrests)

