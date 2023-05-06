#author: Group 2 members-Yi Yang
#course:ALY6110 
#Assignment: Final Project-Basic Analysis

#load some libraries
library(readr)
library(stringr)
library(tidyr)

# This dataset is fetched from https://bj.lianjia.com/ershoufang/

#import the data file
originalHousePrice <- read.csv("C:/Users/yiyu/Desktop/originalBeijingHousePrice.csv",header=TRUE)
str(originalHousePrice)#318851 obs of 26 variables
View(originalHousePrice)

##############################Data Pre-processing###############################

#remove unnecessary columns:url,id, Cid 
housepricing <- originalHousePrice[, -c(1,2,5)]

#check for missing data NA's
summary(housepricing)
###############Handle missing values##############
# 6 columns with NA's: DOM, buildingType,elevator, fiveYearsProperty, subway, communityAverage
157977/318851#DOM:49.54%-do the analysis below
2021/318851#buildingType:0.63%-remove
32/318851#elevator, fiveYearsProperty, subway:0.01%-remove 
463/318851#communityAverage:0.145%-remove 

#feature analysis of DOM
library(psych)
describe(housepricing$DOM)#skew = 4.36 which is high
qqnorm(housepricing$DOM)#positively skewed
#replace the NA's in DOM with its median
housepricing$DOM<- ifelse(is.na(housepricing$DOM),median(housepricing$DOM,na.rm=TRUE),housepricing$DOM)

#remove all other 5 columns' NA's
housepricing <- na.omit(housepricing)

##############Data Subset and Format Conversion####################
#select only 2017 dataset
newhousepricing <- housepricing[format(as.Date(housepricing$tradeTime),'%Y') == "2017", ]
str(newhousepricing)#42710 obs of 23 variables

#remove error data 
newhousepricing$floor <- str_sub(newhousepricing$floor, start = 3L, end = 5L)

#convert constructionTime from char to numeric
library(RODBC)
newhousepricing$constructionTime <- suppressWarnings(as.numeric(newhousepricing$constructionTime))

#change price from RMB to USD
newhousepricing$totalPrice <- round((newhousepricing$totalPrice * 1.029 ) / 6.69, 2)
newhousepricing$price <- round((newhousepricing$price * 1.029) / 6.69, 2)
newhousepricing$communityAverage <- round((newhousepricing$communityAverage * 1.029) / 6.69, 2)

#order by time
finalhousepricing <- newhousepricing[order(as.Date(newhousepricing$tradeTime, format = "%Y-%m-%d")),]
write.csv(finalhousepricing, "~/desktop/cleanHousepricingBJ.csv")

#summary data
summary(finalhousepricing)
str(finalhousepricing)#42710 obs of 23 variables
summary(finalhousepricing)

#############Check for outliers in plot
boxplot(finalhousepricing$totalPrice,xlab="total price in usd",
        main="Boxplot for Total Price")
#identify number of outliers
OutVals = boxplot(finalhousepricing$totalPrice)$out
which(finalhousepricing$totalPrice %in% OutVals)#1189 outliers


#check for outliers for total price vs square
library(ggplot2)
ggplot(finalhousepricing, aes(x= square, y = totalPrice)) +
  geom_boxplot(outlier.color = "blue", outlier.shape = 3, outlier.size = 3)
Q <- quantile(finalhousepricing$totalPrice, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(finalhousepricing$totalPrice)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(finalhousepricing, totalPrice > (Q[1] - 1.5*iqr) & totalPrice < (Q[2]+1.5*iqr))
boxplot(eliminated$totalPrice,xlab="total price in usd",
        main="Boxplot for Total Price")

##############################EDA###############################################
#histogram of target variable:totalPrice
library(ggplot2)
ggplot(finalhousepricing, aes(totalPrice))+geom_histogram(fill = "steelblue")

library(GGally)
ggcorr(data = eliminated, palette = "RdYlGn",
       label = TRUE, label_color = "black")
## According to the correlation matrix in the figure above, we can see that the variables that have a strong positive correlation with the total price include square, the number of living rooms, the number of bathrooms, and the average price of the community.
plot(square ~ totalPrice, data = eliminated)
plot(bathRoom ~ totalPrice, data = eliminated)
plot(livingRoom ~ totalPrice, data = eliminated)
plot(drawingRoom ~ totalPrice, data = eliminated)
plot(renovationCondition ~ totalPrice, data = eliminated)
plot(subway ~ totalPrice, data = eliminated)

#totalPrice vs subway
eliminated <- eliminated %>% 
  mutate(subway = case_when(subway == 1 ~ "Has Subway",
                            subway != 1 ~ "No Subway"))
#totalPrice vs elevator
eliminated <- eliminated %>% 
  mutate(elevator = case_when(elevator == 1 ~ "Has Elevator",
                            elevator != 1 ~ "No Elevator"))

library(ggplot2)
ggplot(eliminated, aes(x= subway, y=totalPrice, color = subway))+geom_boxplot() + 
  labs(title = "House Prices on whether to have subway", y =" Total price of houses")

library(ggplot2)
ggplot(eliminated, aes(x= elevator, y=totalPrice, color = elevator))+geom_boxplot() + 
  labs(title = "House Prices on whether to have elevator", y =" Total price of houses")

library(ggplot2)
ggplot(finalhousepricing, aes(x= constructionTime, y=DOM)) +geom_col()

library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(readxl)

######import dataset#####
clean_housepricingBJ <- read.csv("~/Desktop/cleanHousepricingBJ.csv")
clean_housepricingBJ <- clean_housepricingBJ[,-c(1,4,5,8)]
clean_housepricingBJ$constructionTime <- as.numeric(clean_housepricingBJ$constructionTime)
clean_housepricingBJ <- na.omit(clean_housepricingBJ)

clean_housepricingBJ$totalPrice<-clean_housepricingBJ$totalPrice*10000
##revise column name
colnames(clean_housepricingBJ)[6] <- "Bedroom"

######change to dummy variables#####
##create totalroom column
clean_housepricingBJ$TotalRoom <- clean_housepricingBJ$Bedroom+clean_housepricingBJ$drawingRoom+clean_housepricingBJ$bathRoom+clean_housepricingBJ$kitchen
##One-Hot Encoding for RenovationCondition
clean_housepricingBJ$RevCon_IncludingOther <- ifelse(clean_housepricingBJ$renovationCondition=='1',1,0)
clean_housepricingBJ$RevCon_Rough <- ifelse(clean_housepricingBJ$renovationCondition=='2',1,0)
clean_housepricingBJ$RevCon_Simplicity <- ifelse(clean_housepricingBJ$renovationCondition=='3',1,0)
clean_housepricingBJ$RevCon_Hardcover <- ifelse(clean_housepricingBJ$renovationCondition=='4',1,0)
##One-Hot Encoding for BuildingType
clean_housepricingBJ$BuildingType_IncludingTower <- ifelse(clean_housepricingBJ$buildingType=='1',1,0)
clean_housepricingBJ$BuildingType_Bungalow <- ifelse(clean_housepricingBJ$buildingType=='2',1,0)
clean_housepricingBJ$BuildingType_Conbination <- ifelse(clean_housepricingBJ$buildingType=='3',1,0)
clean_housepricingBJ$BuildingType_Plate <- ifelse(clean_housepricingBJ$buildingType=='4',1,0)
##One-Hot Encoding for BuildingStructure
clean_housepricingBJ$BuildingStructure_IncludingUnknown <- ifelse(clean_housepricingBJ$buildingStructure=='1',1,0)
clean_housepricingBJ$BuildingStructure_Mixed <- ifelse(clean_housepricingBJ$buildingStructure=='2',1,0)
clean_housepricingBJ$BuildingStructure_BrickWood <- ifelse(clean_housepricingBJ$buildingStructure=='3',1,0)
clean_housepricingBJ$BuildingStructure_BrickConcrete <- ifelse(clean_housepricingBJ$buildingStructure=='4',1,0)
clean_housepricingBJ$BuildingStructure_Steel <- ifelse(clean_housepricingBJ$buildingStructure=='5',1,0)
clean_housepricingBJ$BuildingStructure_SteelConcrete <- ifelse(clean_housepricingBJ$buildingStructure=='6',1,0)

######change totalPrice outlier#####
qn <- quantile(clean_housepricingBJ$totalPrice, c(0.05, 0.95), na.rm = TRUE)
clean_housepricingBJ <- within(clean_housepricingBJ, {totalPrice =
  ifelse(totalPrice< qn[1], qn[1], totalPrice)
totalPrice = ifelse(totalPrice > qn[2], qn[2], totalPrice)})

######initial linear regression#####
attach(clean_housepricingBJ)
str(clean_housepricingBJ)#42009 obs of 35 variables
lm_house <- lm(totalPrice ~ .-totalPrice, data = clean_housepricingBJ)
summary(lm_house)

x <- as.matrix(cbind(Lng,Lat,followers,square,floor,constructionTime,ladderRatio,elevator,fiveYearsProperty,subway,communityAverage,RevCon_Hardcover,RevCon_IncludingOther,RevCon_Rough,RevCon_Simplicity,BuildingType_Bungalow,BuildingType_Conbination,BuildingType_IncludingTower,BuildingType_Plate,BuildingStructure_SteelConcrete,BuildingStructure_Steel,BuildingStructure_Mixed,BuildingStructure_IncludingUnknown,BuildingStructure_BrickWood,BuildingStructure_BrickConcrete,TotalRoom))
y <- as.matrix(totalPrice)
str(x)
str(y)
model_house <- lm(y~x)
summary(model_house)

######stepwise variable selection#####
##Set seed for reproducibility
set.seed(123)
##Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
##Select 10
step.model1 <- train(totalPrice~Lng+Lat+followers+square+floor+constructionTime+ladderRatio+elevator+fiveYearsProperty+subway+communityAverage+RevCon_Hardcover+RevCon_IncludingOther+RevCon_Rough+RevCon_Simplicity+BuildingType_Bungalow+BuildingType_Conbination+BuildingType_IncludingTower+BuildingType_Plate+BuildingStructure_SteelConcrete+BuildingStructure_Steel+BuildingStructure_Mixed+BuildingStructure_IncludingUnknown+BuildingStructure_BrickWood+BuildingStructure_BrickConcrete+TotalRoom, data = clean_housepricingBJ,
                     method = "leapSeq", 
                     tuneGrid = data.frame(nvmax = 1:10),
                     trControl = train.control
)
step.model1$results
step.model1$bestTune
summary(step.model1$finalModel)
###followers, square,  constructionTime, fiveYearsProperty, communityAverage, RevCon_Hardcover, BuildingStructure_SteelConcrete, TotalRoom
##Select 15
step.model2 <- train(totalPrice~Lng+Lat+followers+square+floor+constructionTime+ladderRatio+elevator+fiveYearsProperty+subway+communityAverage+RevCon_Hardcover+RevCon_IncludingOther+RevCon_Rough+RevCon_Simplicity+BuildingType_Bungalow+BuildingType_Conbination+BuildingType_IncludingTower+BuildingType_Plate+BuildingStructure_SteelConcrete+BuildingStructure_Steel+BuildingStructure_Mixed+BuildingStructure_IncludingUnknown+BuildingStructure_BrickWood+BuildingStructure_BrickConcrete+TotalRoom, data = clean_housepricingBJ,
                     method = "leapSeq", 
                     tuneGrid = data.frame(nvmax = 1:15),
                     trControl = train.control
)
step.model2$results
step.model2$bestTune
summary(step.model2$finalModel)
###followers, square, ...
##Select 8
step.model3 <- train(totalPrice~Lng+Lat+followers+square+floor+constructionTime+ladderRatio+elevator+fiveYearsProperty+subway+communityAverage+RevCon_Hardcover+RevCon_IncludingOther+RevCon_Rough+RevCon_Simplicity+BuildingType_Bungalow+BuildingType_Conbination+BuildingType_IncludingTower+BuildingType_Plate+BuildingStructure_SteelConcrete+BuildingStructure_Steel+BuildingStructure_Mixed+BuildingStructure_IncludingUnknown+BuildingStructure_BrickWood+BuildingStructure_BrickConcrete+TotalRoom, data = clean_housepricingBJ,
                     method = "leapSeq", 
                     tuneGrid = data.frame(nvmax = 1:15),
                     trControl = train.control
)
step.model1$results
step.model1$bestTune
summary(step.model1$finalModel)

##########eight variables: followers, square,  constructionTime
# fiveYearsProperty, communityAverage, RevCon_Hardcover
# BuildingStructure_SteelConcrete, TotalRoom

######final dataset#####
newdata <- clean_housepricingBJ[,c(3,4,5,12,17,20,21,25,35)]

train_newdataset <- sort(sample(x = nrow(newdata), size = nrow(newdata) * 0.7))
newdataset_train <- newdata[train_newdataset,]
newdataset_test<- newdata[-train_newdataset,]
str(newdataset_train)
str(newdataset_test)

###########################################
######Linear Regression Modeling###########
###########################################
lm_model <- lm(data=newdataset_train, totalPrice ~ .-totalPrice)
lm_pred <- predict(lm_model,newdataset_test)
summary(lm_model)
AIC(lm_model)
anova(lm_model)
RMSE(lm_pred,newdataset_test$totalPrice)
mean((newdataset_test$totalPrice - predict(lm_model,newdataset_test))^2)

###########################################
######Random Forest Regression Modeling####
###########################################
X <- clean_housepricingBJ %>%
  dplyr::select(followers, square,  constructionTime, fiveYearsProperty, communityAverage, RevCon_Hardcover, BuildingStructure_SteelConcrete, TotalRoom)
y <- clean_housepricingBJ$totalPrice

# Split data into training and test sets
index <- createDataPartition(y, p=0.7, list=FALSE)
X_train <- X[ index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

# Train the model
library(randomForest)
regr <- randomForest(x = X_train, y = y_train , importance = TRUE)
print(regr)#default parameters: mtree = 500, mtry = 2
# accuracy:92.5%

# Make prediction
predictions <- predict(regr, X_test)

result <- X_test
result['total_Price'] <- y_test
result['prediction']<-  predictions
head(result)

# Import library for visualization
library(ggplot2)
# Build scatterplot
ggplot(  ) + 
  geom_point( aes(x = X_test$square, y = y_test, color = 'red', alpha = 0.5) ) + 
  geom_point( aes(x = X_test$square , y = predictions, color = 'blue',  alpha = 0.5)) + 
  labs(x = "square", y = "Total Price", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 
# most predicted values coincide with real values

# estimate model more precisely
# Import library for Metrics
library(Metrics)
print(paste0('MAE: ' , mae(y_test,predictions) ))
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))

## [1] "MAE: 63251.512328329"
## [1] "MSE: 7394079110.67613"
## [1] "R2: 0.932488296038324"

# Tune the parameters
# set a bit higher than default 500
N=600 #length(X_train)
X_train_ = X_train[1:N , ]
y_train_ = y_train[1:N]

seed <-7
metric<-'RMSE'

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), class = rep("numeric", 2), label = c("maxnodes", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Set grid search parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')

# Outline the grid of parameters
tunegrid <- expand.grid(.maxnodes=c(70,80,90,100), .ntree=c(600,800,1000))
set.seed(seed)

# Train the model
rf_gridsearch <- train(x=X_train_, y=y_train_, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

plot(rf_gridsearch)

rf_gridsearch$bestTune
# Best Tune parameters: mtry:12, maxnodes:100, ntree:1000

tuned_regr <- randomForest(x = X_train, y = y_train , mtry = 12, maxnodes = 100, ntree = 1000)
print(tuned_regr)#92.62% accuracy

# Make prediction on test data
tuned_predictions <- predict(tuned_regr, X_test)

result2 <- X_test
result2['total_Price'] <- y_test
result2['prediction']<-  tuned_predictions
head(result2)

# estimate model more precisely
# Import library for Metrics
library(Metrics)
print(paste0('MAE: ' , mae(y_test,tuned_predictions) ))
print(paste0('MSE: ' ,caret::postResample(tuned_predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(tuned_predictions , y_test)['Rsquared'] ))

## [1] "MAE: 61656.1470066257"
## [1] "MSE: 7365654519.98312"
## [1] "R2: 0.926805571725542"

# Variable Importance
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')

