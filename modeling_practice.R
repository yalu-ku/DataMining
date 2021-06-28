#######################################################
## Lecture Material                                  ##
## Convergence Informatics                           ##
## Minseok Seo                                       ##
## Nov/29/2020                                       ##
#######################################################

## Set Environment
setRepositories(ind = c(1:8))

library(tidyverse)
library(datarium) # for data
library(caret) # for cross-validation algorithms
library(dplyr)
library(rpart)
library(rpart.plot)
library(kknn)

## Set Working Dir.
WORK_DIR <- "E:\\ABC\\2.Lecture\\2.강의자료\\2020년2학기\\3.융합정보학\\3.Code\\Week14"
setwd(WORK_DIR)

## Practice for Multiple Linear Regression Model
data("marketing", package = "datarium")
head(marketing)

# Automatic way
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)

RSE <- sigma(model)/ mean(marketing$sales)
RSE

# Different models can be considered
model <- lm(sales ~., data = marketing)
model <- lm(sales ~. -newspaper, data = marketing)



## Practice for Multiple Linear Regression Model with cross validation
data(swiss)
dim(swiss)
head(swiss)

# Random sampling test
sample_n(swiss, 3)

# Representative performance measures for linear model
# =====================================================================
#R-squared (R2), representing the squared correlation between the observed outcome values and the predicted values by the model. The higher the adjusted R2, the better the model.

#Root Mean Squared Error (RMSE), which measures the average prediction error made by the model in predicting the outcome for an observation. That is, the average difference between the observed known outcome values and the values predicted by the model. The lower the RMSE, the better the model.

#Mean Absolute Error (MAE), an alternative to the RMSE that is less sensitive to outliers. It corresponds to the average absolute difference between observed and predicted outcomes. The lower the MAE, the better the model
# =====================================================================



# Approach 1: The Validation(test) set Appraoch
trainingSampleIndex <- swiss$Fertility %>%
  createDataPartition(p = 0.5, list = F)

trainData  <- swiss[trainingSampleIndex, ]
testData <- swiss[-trainingSampleIndex, ]

# Builidng linear model using training data only
model <- lm(Fertility ~., data = trainData)

# Make predictions and compute the R2, RMSE and MAE
prediction <- model %>% predict(testData)

result <- data.frame( R2 = R2(prediction, testData$Fertility),
                      RMSE = RMSE(prediction, testData$Fertility),
                      MAE = MAE(prediction, testData$Fertility))

# Prediction Error Rate (%) Calculation
RMSE(prediction, testData$Fertility) / mean(testData$Fertility)
result$MAE / mean(testData$Fertility)


# Approach 2: Leave-one-out cross validation (LOOCV)

# Define training control
trainControl <- trainControl(method = "LOOCV")

# Building model using linear model with training data-set derived from LOOCV partitions
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = trainControl)

# Summarize the results
print(model)
model$results
model$results$RMSE / mean(testData$Fertility)



# Approach 3: K-fold cross validation
trainControl <- trainControl(method = "cv", number = 10)

# Building model using linear model with training data-set derived from 10-fold partitions
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = trainControl)
# Summarize the results
print(model)
model$results
model$results$RMSE / mean(testData$Fertility)


# Approach 4: Repeated K-fold cross validation

trainControl <- trainControl(method = "repeatedcv", 
                             number = 10, repeats = 3)

# Building model using linear model with training data-set derived from 10-fold partitions $ repeat 3 times
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = trainControl)

# Summarize the results
print(model)
model$results
model$results$RMSE / mean(testData$Fertility)




## Decision tree

# Step1: Loading data (Tatanic data)
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv")

dim(data)
head(data)
tail(data)

shuffleIndex <- sample(1:nrow(data))
data <- data[shuffleIndex, ]
head(data)

data$age <- as.numeric(data$age)
data$fare <- as.numeric(data$fare)

# Step2 : Data Cleansing
# Drop variables home.dest,cabin, name, X and ticket
# Create factor variables for pclass and survived
# Drop the NA values

cleanData <- data %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')), survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()

glimpse(cleanData)


## Generating function
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row <- nrow(data)
  total_row <- size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


dataTrain <- create_train_test(cleanData, 0.5, train = TRUE)
dataTest <- create_train_test(cleanData, 0.5, train = FALSE)

dim(dataTrain)
dim(dataTest)

prop.table(table(dataTrain$survived))
prop.table(table(dataTest$survived))

model <- rpart(survived~., data = dataTrain, method = 'class')
rpart.plot(model, extra = 106)


predictResult <-predict(model, dataTest, type = 'class')

confusion <- table(dataTest$survived, predictResult)
confusion

sum(diag(confusion)) / sum(confusion)



## KNN approach
knnModel_1 <- kknn(survived~., train = dataTrain, test = dataTest, k=3)

knnResult <- table(Predicted = as.integer(fitted(knnModel_1)), Diagnosis=dataTest$survived)
sum(diag(knnResult)) / sum(knnResult)






