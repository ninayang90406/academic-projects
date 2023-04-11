
#ALY6040 FINAL REPORT R CODE-Yi Yang
library(readxl)
library(gmodels)
library(ggplot2)
library(corrplot)
library(caret)
library(glmnet)
library(gridExtra)
library(data.table)
library(plotROC)
library(dplyr)
library(magrittr)
library(tidyr)
library(tidyverse)
library(stringr)
library(InformationValue)
library(PerformanceAnalytics)
library(rsample) 
library(ISLR)
library(gbm)          
library(xgboost)  
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(randomForestSRC)
library(h2o)          
library(pdp)          
library(lime)
library(rattle)
library(stats)
library(ggfortify)

#import the dataset
satisfaction <- read.csv("C:/Users/sheny/Desktop/ALY6040/week2/satisfaction.csv")
str(satisfaction)
summary(satisfaction)

# data cleanup
satisfaction1 <- satisfaction[-c(1)]
satisfaction2 <- na.omit(satisfaction1)
summary(satisfaction2)
str(satisfaction2)
CrossTable(satisfaction2$satisfaction_v2 ,digits = 7)

satisfaction3 <-satisfaction2 %>% select(-(2:6))
satisfaction3$satisfaction_v2 <- ifelse(satisfaction3$satisfaction_v2=="satisfied", 1,0)
summary(satisfaction3)
#EDA
# satisfaction distribution
ggplot(satisfaction2, aes(x = satisfaction_v2)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill= c("#099DD9","orange"))+
  scale_y_continuous(labels=scales::percent) +
  labs(x="Satisfaction Level",y = "Percentage",title="Satisfaction",    fill="Satisfaction Level")

# gender vs satisfaction
ggplot(satisfaction2, aes(satisfaction_v2, group = Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")+
  scale_fill_manual(values = c("#099DD9","orange"))+
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5, cex=3)+
  labs(x="Satisfaction Level",y = "Percentage",title="Gender vs Satisfaction",  fill="Satisfaction Level")+
  facet_grid(~Gender)

# Class vs satisfaction
ggplot(satisfaction2, aes(satisfaction_v2, group = Class)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")+
  scale_fill_manual(values = c("#099DD9","orange"))+
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5, cex=3)+
  labs(x="Class of Travel",y = "Percentage",title="Class vs Satisfaction", fill="Satisfaction Level")+
  facet_grid(~Class)

# Customer Type vs satisfaction
ggplot(satisfaction2, aes(satisfaction_v2, group = `Customer Type`)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")+
  scale_fill_manual(values = c("#099DD9","orange"))+
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5, cex=3)+
  labs(x="Type of Customer",y = "Percentage",title="Customer type vs Satisfaction", fill="Satisfaction Level")+
  facet_grid(~`Customer Type`)

# Type of travel vs satisfaction
ggplot(satisfaction2, aes(satisfaction_v2, group = `Type of Travel`)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")+
  scale_fill_manual(values = c("#099DD9","orange"))+
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5, cex=3)+
  labs(x="Type of Travel",y = "Percentage",title="Travel type vs Satisfaction", fill="Satisfaction Level")+
  facet_grid(~`Type of Travel`)

#Correlation Matrix
M <-cor(satisfaction3)
corrplot.mixed(M, lower.col="black")
p1 <-boxplot(satisfaction3$`Inflight entertainment`,xlab="Inflight entertainment",ylab="score",col="orange",border="brown")
p2 <-boxplot(satisfaction3$`Online support`,xlab="Online support",ylab="score",col="orange",border="brown")
p3 <-boxplot(satisfaction3$`Ease of Online booking`,xlab="Ease of Online booking",ylab="score",col="orange",border="brown")
summary(satisfaction3$`Inflight entertainment`)
summary(satisfaction3$`Ease of Online booking`)
summary(satisfaction3$`Online support`)

# Model Building
#Model Preparation
s1$Gender <- as.numeric(s1$Gender)#female = 1, male = 2
s1$`Customer Type` <- as.numeric(s1$`Customer Type`)#disloyal customer = 1, Loyal customer = 2
s1$`Type of Travel` <- as.numeric(s1$`Type of Travel` )#Business travel = 1, Personal Travel = 2
s1$Class <- as.numeric(s1$Class)#Business = 1, Eco = 2, Eco Plus = 3
str(s1)
s2 <- na.omit(s1)
s2 <- mutate(s2,score=`Seat comfort`+`Departure/Arrival time convenient`+
               `Food and drink`+`Gate location`+
               `Inflight wifi service`+`Inflight entertainment`+ 
               `Online support`+ `Ease of Online booking`+
               `On-board service`+`Leg room service`+
               `Baggage handling`+`Checkin service`+
               Cleanliness+`Online boarding`)
str(s2)
s3 <- s2[-c(8:21)]
chart.Correlation(s3, method="pearson", histogram = TRUE, pch=20)

#split dataset
set.seed(12345)
row.number <- sample(x=1:nrow(s3), size=0.7*nrow(s3))
train = s3[row.number, ]
test = s3[-row.number,]
dim(train)
dim(test)
head(train)
head(test)

#Linear Regression Model
null_model <- lm(score ~ 1, data = train)
full_model <- lm(score ~ ., data = train)
#Stepwise Selection
step(null_model,
     scope=list(upper=full_model),
     direction= "both",
     data= train)
final_model=lm(formula = score ~ satisfaction_v2 + `Type of Travel` + Class + 
                 Age + `Customer Type` + Gender + `Flight Distance` + `Arrival Delay in Minutes`, 
               data = train)
summary(final_model)
observed_s1 <- test$score
predicted_s1 <- predict(final_model, newdata = test)
summary(predicted_s1)
plot(observed_s1,predicted_s1,
     xlab="actual value", ylab="predicted value") 
abline(0,1,col="blue", lwd =2)
hist(residuals(final_model),
     col = "skyblue")
SSE <- sum((observed_s1 - predicted_s1) ^ 2)
SST<- sum((observed_s1 - mean(observed_s1)) ^ 2)
r2 <- 1 - (SSE/SST)
r2
rmse <- sqrt(mean((predicted_s1 - observed_s1)^2))
rmse

# Logistic Regression Model
s4 <- na.omit(s1)
set.seed(12345)
row.number <- sample(x=1:nrow(s4), size=0.7*nrow(s4))
train = s4[row.number, ]
test = s4[-row.number,]
dim(train)
dim(test)
head(train)
head(test)
Logistic_model<-glm (formula = satisfaction_v2 ~ ., family = "binomial", data = train)
summary(Logistic_model)
predicted_s2 <- predict(Logistic_model, newdata = test, type = "response")
sqrt(mean((predicted_s2 - test$satisfaction_v2)^2))
opt <- optimalCutoff(test$satisfaction_v2, predicted_s2)
opt
predicted_s2 <- ifelse(predicted_s2 > opt,1,0)
Classify <- mean(predicted_s2!=test$satisfaction_v2)
print(paste('Accuracy',1-Classify))
Matrix<-confusionMatrix(test$satisfaction_v2, predicted_s2,threshold=opt)
Matrix
Misclassification = (3900+2444)/(2444+15228	+3900	+17275	)
Misclassification
sensitivity(predicted_s2, test$satisfaction_v2,threshold = opt)
specificity(predicted_s2, test$satisfaction_v2,threshold = opt)
plotROC(test$satisfaction_v2, predicted_s2)

#Ridge Regression Model
set.seed(1) 
row.number <- sample(x=1:nrow(s3), size=0.7*nrow(s3))
train = s3[row.number,]
test = s3[-row.number,]
x <- model.matrix(score~., train)[,-1] 
#this transforms x from a variable within a dataframe into a matrix
y <- train$score 
#this transforms y from a variable within a dataframe into a vector
Ridge_model <- glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda
best_coeffs <- predict(Ridge_model, s=best.lambda, type="coefficients")[1:6, ]
x_test <- model.matrix(score~., test)[,-1]
y_test <- test$score
y_predicted <- predict(Ridge_model, s = best.lambda, newx = x_test)
summary(y_predicted)
summary(y_test)
SSE <- sum ((y_predicted - y_test)^2)
SST <- sum ((y_test)^2)
r2  <- 1 - (SSE/SST)
r2
rmse <- sqrt(mean(y_predicted-y_test)^2)
rmse

#Decision Tree Model
set.seed(123)
split <- initial_split(s2, prop = .7)
train <- training(split)
test  <- testing(split)
#grow tree,"class" for a classification tree
tree_model <- rpart(satisfaction_v2 ~ . ,, data= train)
# plot tree
rpart.plot(tree_model)
printcp(tree_model) # display the results
plotcp(tree_model) # visualize cross-validation results
# prune the tree
pruned_tree <- prune(tree_model, 
                     cp= tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
plotcp(pruned_tree)
p1_train = predict(tree_model, train, type = "class")
p1_test = predict(tree_model, test, type = "class")
# train confusion
table(predicted = p1_train, actual = train$satisfaction_v2)
# train acc
mean(p1_train == train$satisfaction_v2)
# test confusion
table(predicted = p1_test, actual = test$satisfaction_v2)
# test acc
mean(p1_test == test$satisfaction_v2)

# Random Forest Model
# data split:70/30
set.seed(123)
sample_data <- initial_split(s2, prop = 0.70)
train <- training(sample_data)
test <- testing(sample_data)
dim(train)
dim(test)
head(train)
head(test)

# Create a Random Forest with default parameters
rf_model <- randomForest(satisfaction_v2 ~ ., method ="rf" , data = train, importance = TRUE)
rf_model

# Predicting on train data
pred <- predict(rf_model,train)
confusionMatrix(pred,train$satisfaction_v2)

# Predicting on test data
pred <- predict(rf_model,test)
mean(pred==test$satisfaction_v2)
confusionMatrix(pred,test$satisfaction_v2)

# Error rate of Random Forest
plot(rf_model)

# Using For loop to identify the right mtry for model
a=c()
i=4
for (i in 3:15) {
  rf_model1 <- randomForest(satisfaction_v2 ~ ., data = train, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(rf_model1, test, type = "class")
  a[i-2] = mean(predValid == test$satisfaction_v2)
}
a
plot(3:15, a)

# Create a Random Forest with ntrees = 1000, mtry = 9
rf_model_optimal <- randomForest(satisfaction_v2 ~ ., method ="rf" , data = train, ntrees = 1000, 
                                 mtry = 9, importance = TRUE)
rf_model_optimal

# Predicting on train data
pred1 <- predict(rf_model_optimal,train)
confusionMatrix(pred1,train$satisfaction_v2)

# Predicting on test data
pred1 <- predict(rf_model_optimal,test)
mean(pred1==test$satisfaction_v2)
confusionMatrix(pred1,test$satisfaction_v2)

# Variable importance
importance(rf_model_optimal)
varImpPlot(rf_model_optimal)

# Number of nodes for the trees
hist(treesize(rf_model_optimal),
     main = "Number of Nodes for the Trees",
     col = "blue")

#GBM Model
s1$satisfaction_v2 <- ifelse(s1$satisfaction_v2 == 'satisfied', 1,0)
s2 <- na.omit(s1)
str(s2)
#split the dataset
set.seed(123)
split <- initial_split(s2, prop = .7)
train <- training(split)
test  <- testing(split)
#first GBM mdoel
gbm_model <- gbm(formula = satisfaction_v2 ~ .,
                 distribution = "gaussian",
                 data = train,
                 interaction.depth = 1,
                 shrinkage = 0.001,
                 cv.folds = 5,
                 n.cores = NULL, # will use all cores by default
                 verbose = FALSE)  
print(gbm_model)
cv.error = sqrt(min(gbm_model$cv.error))
cv.error
gbm.perf(gbm_model, method = "cv")
#second GBM model
set.seed(123)
gbm_model2 <- gbm( formula = satisfaction_v2 ~ .,
                   distribution = "gaussian",
                   data = train,
                   n.trees = 15000,
                   interaction.depth = 3,
                   shrinkage = 0.1,
                   cv.folds = 5,
                   n.cores = NULL, 
                   verbose = FALSE)
print(gbm_model2)
# find index for n trees with minimum CV error
idx_min_MSE <- which.min(gbm_model2$cv.error)
idx_min_MSE
# get MSE and compute RMSE
sqrt(gbm_model2$cv.error[idx_min_MSE])
# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm_model2, method = "cv")
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)
# total number of combinations
nrow(hyper_grid)
# randomize data
random_index <- sample(1:nrow(train), nrow(train))
random_train <- train[random_index, ]
# grid search 
for(i in 1:nrow(hyper_grid)) {
  set.seed(123)
  gbm_tune <- gbm(formula = satisfaction_v2 ~ .,
                  distribution = "gaussian",
                  data = random_train,
                  n.trees = 10000,
                  interaction.depth = hyper_grid$interaction.depth[i],
                  shrinkage = hyper_grid$shrinkage[i],
                  n.minobsinnode = hyper_grid$n.minobsinnode[i],
                  bag.fraction = hyper_grid$bag.fraction[i],
                  train.fraction = .75,
                  n.cores = NULL, # will use all cores by default
                  verbose = FALSE)
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm_tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm_tune$valid.error))
}
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
# modify hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)
nrow(hyper_grid)
for(i in 1:nrow(hyper_grid)) {
  set.seed(123)
  gbm_tune2 <- gbm(formula = satisfaction_v2 ~ .,
                   distribution = "gaussian",
                   data = random_train,
                   n.trees = 15000,
                   interaction.depth = hyper_grid$interaction.depth[i],
                   shrinkage = hyper_grid$shrinkage[i],
                   n.minobsinnode = hyper_grid$n.minobsinnode[i],
                   bag.fraction = hyper_grid$bag.fraction[i],
                   train.fraction = .75,
                   n.cores = NULL, # will use all cores by default
                   verbose = FALSE)
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm_tune2$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm_tune2$valid.error))
}
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
#final GBM model
set.seed(123)
gbm_final <- gbm(
  formula = satisfaction_v2 ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 14986,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE )  
par(mar = c(5, 8, 1, 1))
par(mar = c(5, 8, 1, 1))
summary(gbm_model2, 
        cBars = 10,
        method = relative.influence, # also can use permutation.test.gbm
        las = 2)
par(mar = c(5, 8, 1, 1))
summary(gbm_final, 
        cBars = 10,
        method = relative.influence, # also can use permutation.test.gbm
        las = 2)
## Predicting
p_gbm2 <- predict(gbm_model2, n.trees = gbm_model2$n.trees, test)
p_final <- predict(gbm_final, n.trees = gbm_final$n.trees, test)
# results
sqrt(mean((p_gbm2 - test$satisfaction_v2)^2))
sqrt(mean((p_final - test$satisfaction_v2)^2))
