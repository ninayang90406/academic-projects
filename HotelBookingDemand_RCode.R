#Group 5 
#Authors: Yi Yang
#course:ALY6020-Predictive Analytics
#Final Project

## Data can be downloaded from here:
## https://www.kaggle.com/jessemostipak/hotel-booking-demand. 

#import libraries
library(dplyr)
library(mlr)
library(gmodels)
library(sampling)
library(lubridate)
#import data set and sampling data set
hotel<-read.csv("C:/Users/sheny/Desktop/hotel_bookings.csv")
str(hotel)

#distribution of response variable:is_canceled
table(hotel$is_canceled)

## Step 1: data sampling
# sampling 20% of total data
n1=round(0.2*75166)
n2=round(0.2*44224)
sub_hotel=strata(hotel,stratanames=("is_canceled"),size=c(n1,n2),method="srswor")
hotel2=hotel[sub_hotel$ID_unit,]
summary(hotel2)
str(hotel2)

#distribution of response variable:is_canceled in sampled dataset
table(hotel2$is_canceled)
CrossTable(hotel2$is_canceled ,digits = 5)

## Step 2: cleaning data
#drop the null column("agent","company")
hotel2<-hotel2[,-c(24,25)]

#check the na value and drop the na value
sum(is.na(hotel2))
hotel2<-na.omit(hotel2)
summary(hotel2)

#remove the adr data which <=0
hotel2<-hotel2[-which(hotel2$adr<=0),]

#transform month columns to numeric type and remove duplicate column
hotel2$arrival_date_month<-ifelse(hotel2$arrival_date_month=="January",1,
                                  ifelse(hotel2$arrival_date_month=="February",2,
                                         ifelse(hotel2$arrival_date_month=="March",3,
                                                ifelse(hotel2$arrival_date_month=="April",4,
                                                       ifelse(hotel2$arrival_date_month=="May",5,
                                                              ifelse(hotel2$arrival_date_month=="June",6,
                                                                     ifelse(hotel2$arrival_date_month=="July",7,
                                                                            ifelse(hotel2$arrival_date_month=="August",8,
                                                                                   ifelse(hotel2$arrival_date_month=="September",9,
                                                                                          ifelse(hotel2$arrival_date_month=="October",10,
                                                                                                 ifelse(hotel2$arrival_date_month=="November",11,
                                                                                                        ifelse(hotel2$arrival_date_month=="December",12,NA))))))))))))
hotel2$reservation_status_date_month<-month(hotel2$reservation_status_date)
hotel2$reservation_status_date_day<-day(hotel2$reservation_status_date)
View(hotel2)

#remove "reservation status" and "reservation_status_date" 
hotel2<-hotel2[-c(29,30)]

#transform some columns into dummy variables 
hotel2<-createDummyFeatures(hotel2, cols =c("hotel","meal","country","market_segment","distribution_channel","reserved_room_type","assigned_room_type","deposit_type","customer_type"))
#after cleansing
View(hotel2)
str(hotel2)

#output the cleaning data set
write.csv(hotel2,file = "C:/Users/sheny/Desktop/hotel_cleaning_data.csv")

##Step 3: Data splitting
#get train data and test data
set.seed(1)
row.number <- sample(x=1:nrow(hotel2), size=0.8*nrow(hotel2))
train = hotel2[row.number,]
test = hotel2[-row.number,]
# checking the dimensions of train and test datasets
dim(train)
dim(test)

#output the cleaning training data set
write.csv(train,file = "C:/Users/sheny/Desktop/hotel_cleaning_training_data.csv")
#output the cleaning training data set
write.csv(test,file = "C:/Users/sheny/Desktop/hotel_cleaning_test_data.csv")

################################## Method 1: K-nearest neighboring algorithm#############
#load Class package
library(class)
#KNN model
knn_prediction1<-knn(train[, -1],test[, -1], cl = train[, 1],k=3)
knn_prediction2<-knn(train[, -1],test[, -1], cl = train[, 1],k=5)
knn_prediction3<-knn(train[, -1],test[, -1], cl = train[, 1],k=7)
knn_prediction4<-knn(train[, -1],test[, -1], cl = train[, 1],k=9)
knn_prediction5<-knn(train[, -1],test[, -1], cl = train[, 1],k=11)
knn_prediction6<-knn(train[, -1],test[, -1], cl = train[, 1],k=13)
knn_prediction7<-knn(train[, -1],test[, -1], cl = train[, 1],k=15)
knn_prediction8<-knn(train[, -1],test[, -1], cl = train[, 1],k=17)
knn_prediction9<-knn(train[, -1],test[, -1], cl = train[, 1],k=19)

##Check for performance/accuracy
#load Caret package
library(caret)
# confusion matrix 
knn_confusion1<-confusionMatrix(knn_prediction1,factor(test$is_canceled))
knn_confusion1
knn_confusion2<-confusionMatrix(knn_prediction2,factor(test$is_canceled))
knn_confusion2
knn_confusion3<-confusionMatrix(knn_prediction3,factor(test$is_canceled))
knn_confusion3
knn_confusion4<-confusionMatrix(knn_prediction4,factor(test$is_canceled))
knn_confusion4
knn_confusion5<-confusionMatrix(knn_prediction5,factor(test$is_canceled))
knn_confusion5
knn_confusion6<-confusionMatrix(knn_prediction6,factor(test$is_canceled))
knn_confusion6
knn_confusion7<-confusionMatrix(knn_prediction7,factor(test$is_canceled))
knn_confusion7
knn_confusion8<-confusionMatrix(knn_prediction8,factor(test$is_canceled))
knn_confusion8
knn_confusion9<-confusionMatrix(knn_prediction9,factor(test$is_canceled))
knn_confusion9

K<-c(3,5,7,9,11,13,15,17,19)
accuracy<-c(0.8256,0.8094,0.799,0.7881,0.7853,0.7794,0.7732,0.7651,0.7596)
a<-rbind(K,accuracy)
################################## Method 2:Logistic Regression #######################
#Model building
#General linear model glm is used
#is_canceled before ~ indicates it is our target variable and all other after dot are independent variables
#Binomial family indicates output would be binary either '1'(Hotel booking will not be cancelled) or '0'(Bookings will be cancelled)
#For building logistic model dropping columns with NA values as glm does not take dataframes with values NA
logistic_model <- glm(formula = is_canceled ~., data = trainlogistic,family = "binomial")

#Summary of built model
#Number of iterations and significant variables can be known
summary(logistic_model)

#Using test data to predict values of future and validation purpose
#if predicted values are >0.5 then 
predictValue <- predict(logistic_model,test[, -1])
predictValue <- ifelse(predictValue>0.5,"1","0")

#Accuracy of model is identified using mean of predicted and test data
#We find logistic model is 87% accurate
mean(predictValue == test$is_canceled)# Accuracy is 76%

#####################################################################


################################## Method 3:Decision Tree using rpart###############

#convert response variable to be factors
train$is_canceled <- factor(train$is_canceled)
test$is_canceled <- factor(test$is_canceled)

#load rpart() package 
library(rpart)

#grow our tree
rtree <- rpart(is_canceled ~.,method="class", train)

#print results
print(rtree)
printcp(rtree)
#plot our tree
library(rpart.plot)
rpart.plot(rtree, tweak=1.2)#tweak makes the text 20% larger

#prediction on test data
rtree_pred <- predict(rtree, test[, -c(1)], type="class")

### Confusion Matrix under caret package
library(caret)
confusionMatrix(table(rtree_pred, test$is_canceled))
#accuracy is 82.05%

########################## Optimize our decision tree:Random Forest algorithm ############# 
# Create a Random Forest with default parameters
library(randomForest)
library(randomForestSRC)
rf_model <- randomForest(is_canceled ~ ., method ="rf" , data = train, importance = TRUE)
rf_model

# Predicting on test data
pred <- predict(rf_model,test[, -c(1)], type="class")
library(caret)
confusionMatrix(pred,test$is_canceled)
levels(pred)
levels(test$is_canceled)
# accuracy is 88.66%

# Error rate of Random Forest
plot(rf_model)

#tune mtry
t <- tuneRF(train[,-1], train[,1],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 200,
       trace = TRUE,
       improve = 0.05)
#when mtry is 240, the lowest error rate is 5.67%

# Create a Random Forest with ntrees = 200, mtry = 240
rf_model_optimal <- randomForest(is_canceled ~ ., method ="rf" , data = train, ntrees = 200, 
                                 mtry = 240, importance = TRUE)
rf_model_optimal

# Predicting on test data
pred2 <- predict(rf_model_optimal,test)
mean(pred2==test$is_canceled)
confusionMatrix(pred2,test$is_canceled)
#accuracy is 93.78%

# Variable importance
importance(rf_model_optimal)
varImpPlot(rf_model_optimal)

