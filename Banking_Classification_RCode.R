# ALY 6015 Assignment 3
# Yi Yang and 06/26/2020

## Part 1: Load the banking data
bank_data <- read.csv("C:/Users/sheny/Desktop/ALY6015/banking_data.csv")
View(bank_data)
bank_data<- data.frame(bank_data)
head(bank_data)

## Part 2: Pre-processing
# The outcome y: 0 for no, 1 for yes
bank_data$y <- ifelse(bank_data$y == "no", 0, 1)

# Calculate the probability of y = 1(sign up for a term deposit)
prop.table(bank_data$y)[bank_data$y == "1"] #p=0.0215% when y = 1

# Create an Intercept Model
intercept_model <-glm(y ~ 1,family = binomial(link = "logit"),data = bank_data)

# Obtain the estimate for the intercept
intercept_model$coefficients[1]#-2.063912
summary(intercept_model)

# Use the function to calculate the probability
(exp(intercept_model$coefficients[1]))/(1+exp(intercept_model$coefficients[1]))
exp(-2.063912)/(1+exp(-2.063912))#p = 0.1126542

# Use gmodels package to create a Cross Table
# To verify with the probability, 0.1126542
library(gmodels)
CrossTable(bank_data$y)
CrossTable(bank_data$y, digits = 7)

## Part 3:Logistic Regression for education
# check the NAs under education
education1 <- bank_data[-which(is.na(bank_data$education)),]
str(education1$education)

# logistic regression model 
# covert education values from int to factor
education1$education <- as.factor(education1$education)
education_model <-glm(y ~ education,family = binomial(link = "logit"),data = education1)
summary(education_model)

# log odds
summary(education_model)$coeff

# Odds ratios
exp(coef(education_model))

# odds ratio for education=2 compared to education=0
exp(education_model$coefficients[3])#1.564949

#probability when education=0
(exp(education_model$coefficients[1]))/(1+exp(education_model$coefficients[1]))

#predict function to verify
pred_ex<-predict(education_model,newdata =education1[education1$education==0,],type="response")
head(pred_ex,1)

#probability when education=2
pred_ex<-predict(education_model,newdata =education1[education1$education==2,],type="response")
head(pred_ex,1)

## Part 4:
# remove missing values in bank data
bank_omit <- na.omit(bank_data)
View(bank_omit)

bank_new <- data.frame(as.numeric(as.factor(bank_omit$X)),
                       as.numeric(as.factor(bank_omit$age)),
                       as.numeric(as.factor(bank_omit$marital)),
                       as.numeric(as.factor(bank_omit$education)),
                       as.numeric(as.factor(bank_omit$occupation)),
                       as.numeric(as.factor(bank_omit$default)),
                       as.numeric(as.factor(bank_omit$housing)),
                       as.numeric(as.factor(bank_omit$contact)),
                       as.numeric(as.factor(bank_omit$quarter)),
                       as.numeric(as.factor(bank_omit$day)),
                       as.numeric(as.factor(bank_omit$duration)),
                       as.numeric(as.factor(bank_omit$campaign)),
                       as.numeric(as.factor(bank_omit$pdays)),
                       as.numeric(as.factor(bank_omit$previous)),
                       as.numeric(as.factor(bank_omit$poutcome)),
                       bank$y)

# Split the bank data into train and test data
set.seed(12345) 
row.number <- sample(x=1:nrow(bank_omit), size=0.8*nrow(bank_omit))
train = bank_omit[row.number,]
test = bank_omit[-row.number,]
head(train)
head(test)

# random forest
library(randomForest)
fitRF <- randomForest(y~., train)
fitRF

# logistic regression model using train data
train_model <-glm(y ~ factor(education).,family = binomial(link = "logit"),data = train)
summary(train_model)

# predict function of test data
predicted <- predict(train_model, data=test, type="response")
head(predicted,1)
CrossTable(predicted)

#predicted probabilities as either Y=1 or Y=0 based on some cutoff value 0.05
y_pred_num <- ifelse(predicted > 0.5, 1, 0)
library(InformationValue)
optCutOff <- optimalCutoff(test$y, predicted, )[1] 
y_pred_num <- ifelse(predicted >= optCutOff, 1, 0)

table(y_pred_num,test$y)


