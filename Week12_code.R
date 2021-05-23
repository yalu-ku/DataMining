##################################################
## Lecture of Week12        
## Data mining
## Aro Yun
## 05-19-2021
##################################################

## Set Env.
setRepositories(ind = c(1:8))

# Install <- c("datarium", "caret", "rpart.plot", "kknn", "ROCR", "kernlab")
# install.packages(Install)

library(tidyverse)
library(datarium) # several data
library(caret)    # ML algorithmes
library(dplyr)    # classifier
library(rpart)    # decision tree?
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)

## Set working Dir.
WORK_DIR <- "D:\\2021-1학기\\DataMining\\Week12"
setwd(WORK_DIR)
getwd()

## Practice for data subsetting
data <- swiss
nrow(data)

## randomly choosing
#sample_n(data, 10)  #random

# Approach 1: Test(validation)-set approach
trainSampleIndex <- data$Fertility %>% 
  createDataPartition(p = 0.7, list = F)  #randomly choosing 70%

trainData <- data[trainSampleIndex,]
testData <- data[-trainSampleIndex,]


## Check well organize
dim(data)
dim(trainData)
dim(testData)

rownames(trainData)
rownames(testData) 
intersect(rownames(trainData), rownames(testData)) ## character(0) -> well organize


# lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data)
# == lm(Fertility ~., data)

model <- lm(Fertility ~., data = trainData)  #cause 'data' is divided
summary(model)

## Feature selection (forward selection, backward elimination, stepwise regression)
model_forward <- step(model, direction = "forward")
model_backward <- step(model, direction = "backward")
model_stepwise <- step(model, direction = "both")

model2 <- lm(Fertility ~ Agriculture, data = trainData)
model3 <- lm(Fertility ~ Agriculture + Education, data = trainData)
model4 <- lm(Fertility ~ Agriculture + Education + Examination, data = trainData)

## Model usage Step.
prediction <- model %>% predict(testData)

predict_forward <- model_forward %>% predict(testData)
predict_backward <- model_backward %>% predict(testData)
predict_stepwise <- model_stepwise %>% predict(testData)

prediction2 <- model2 %>% predict(testData)
prediction3 <- model3 %>% predict(testData)
prediction4 <- model4 %>% predict(testData)

## Full model
result_M1 <- data.frame(R2 = R2(prediction, testData$Fertility), 
           RMSE = RMSE(prediction, testData$Fertility), 
           MAE = MAE(prediction, testData$Fertility))

result_M2 <- data.frame(R2 = R2(prediction2, testData$Fertility), 
                        RMSE = RMSE(prediction2, testData$Fertility), 
                        MAE = MAE(prediction2, testData$Fertility))

result_M3 <- data.frame(R2 = R2(prediction3, testData$Fertility), 
                        RMSE = RMSE(prediction3, testData$Fertility), 
                        MAE = MAE(prediction3, testData$Fertility))

result_M4 <- data.frame(R2 = R2(prediction4, testData$Fertility), 
                        RMSE = RMSE(prediction4, testData$Fertility), 
                        MAE = MAE(prediction4, testData$Fertility))

result <- rbind(result_M1, result_M2, result_M3, result_M4)
result


## Leave-one-out corss validation (LOOCV)

# How divided
trainControl <- trainControl(method = "LOOCV")

model <- train(Fertility ~., data, trControl = trainControl, method = "lm")

## LOOCV 구현
predicted_Fertility <- c()
for(i in 1:nrow(data)){
  testSet <- data[i,]
  trainSet <- data[-i,]
  
  model <- lm(Fertility ~., data = trainSet)

  predicted_Fertility[i] <- model %>% predict(testSet)
  
}

result_LOOCV <- data.frame(R2 = R2(predicted_Fertility, data$Fertility), 
                        RMSE = RMSE(predicted_Fertility, data$Fertility), 
                        MAE = MAE(predicted_Fertility, data$Fertility))


## Approach 3: k-fold cross validation
# trainControl <- trainControl(method = "cv")
trainControl <- trainControl(method = "repeatedccv", number = 10, repeats = 3)

model <- train(Fertility ~., data, trControl = trainControl, method = "lm")

for(i in 1:nrow(data)){
  testSet <- data[i,]
  trainSet <- data[-i,]
  
  model <- lm(Fertility ~., data = trainSet)
  
  predicted_Fertility[i] <- model %>% predict(testSet)
}

###########################################
testMethodKfoldCV <- function(data, k){
  
  output <- list()
  for(i in 1:k){
    output[[i]] <- data[((nrow(data)%/%k)*(i-1)+1):((nrow(data)%/%k)*i),] 
  }
  
  for(i in 1:(nrow(data)%%k)){
    output[[i]] <- rbind(output[[i]],data[(((nrow((data))%/%k)*k)+i),])   
  }
  
  return(output)
}
a <- testMethodKfoldCV(data,10)
a
a[[1]]

data
data[47,]
a
k <- 10
i <- 1
a[[1]]<- rbind(data[(nrow(data)%/%k)*k+i,],a[[1]])
a[[1]]

a[[1]]
data[41,]+

class(output[[1]])
class(output[1])
output[[1]]
output[1]
testMethodKfoldCV(data,10)

############################################

a <- testMethodKfoldCV(data,10)
a[1]
a[[1]][1]
for(i in 1:nrow(data)){
  
  
  
  output[i] = nrow(data)%/%k
  # idx[i] = nrow(data)%/%10
  test <- data[i*10,]
  train <- data[-i,]
}

t <- cut()
# share = nrow(data)%/%k
# share(i-1)+1
# test <- data[i+(k-1),]
# train <- data[-i*k,]
# }



for(i in 1:nrow(data)){
  length(data)
  print(data[i,])
  output[i] = nrow(data)%/%10
}

for(i in 1:k){
  nrow(data)%/%k
  i = 4(i-1)+1
}


(nrow(data)%/%10) ==4

for(i in 1:10){
  
  
  
  
  # data[i,]*
  # 
  output[i,] <- data[(i-1)*4+(3+i),]
  # print(output)
  # output[i] <- cut(row)
  # 
  # 
  # data[i] <- (nrow(data[i,])%/%10)(i-1)+1
  # # output[i] <- data[i,]/4*(i-1)+1
  # print(data[i])
  
  print(output[2,])
}





  #   share = nrow(data)%/%10
  # for (j in 1:share)
  #   output[j] = share(i-1)+i%/%share
  #   print(output[j])
  # }



  