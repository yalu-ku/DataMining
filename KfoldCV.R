##############################
## DataMining HW2
## 2018270679 윤아로
##############################


## Set Env.
setRepositories(ind = c(1:8))

## Set libraries
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
setwd("E:\\2021-1학기\\데이터마이닝")
getwd()

## Get dataSet
data <- swiss

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
summary(result_LOOCV)

## Impelement Function
## Q1
KfoldCV <- function(data, k) {
  predicted_Fertility_k_fold <- c()
  result_K_fold <- c()
  output <- list()
  copy <- data
  each <- as.integer(nrow(data)/k)
  rest <- nrow(data) - (k*each)
  
  count <- each+1
  for(i in 1:(rest-1)){
    count <- c(count,each+1)
  }
  for(i in rest:(k-1)){
    count <- c(count,each)
  }
  
  for(i in 1:k){
    output[[i]] <- copy[1:count[i],]
    copy <- copy[-1:-count[i],]
  }
  return(output)
}

testMethodKfoldCV <- function(data, k){
  predicted_Fertility_k_fold <- c()
  result_K_fold <- c()
  output <- list()
  copy <- data
  each <- as.integer(nrow(data)/k)
  rest <- nrow(data) - (k*each)
  total_res <- c()
  count <- each+1
  for(i in 1:(rest-1)){
    count <- c(count,each+1)
  }
  for(i in rest:(k-1)){
    count <- c(count,each)
  }
  
  for(i in 1:k){
      output[[i]] <- copy[1:count[i],]
      copy <- copy[-1:-count[i],]
  }
  
  for(i in 1:k){
    testSet <- output[[i]]
    traindata <- output[-i]
    trainSet <- c()
    for(j in traindata){
      trainSet <- rbind(trainSet,j)
    }
    
    model <- lm(Fertility ~., data = trainSet)
    predicted_Fertility_k_fold[[i]] <- model %>% predict(testSet)
    result_k_fold <- data.frame(R2 = R2(as.numeric(predicted_Fertility_k_fold[[i]]), testSet$Fertility),
                                RMSE = RMSE(as.numeric(predicted_Fertility_k_fold[[i]]), testSet$Fertility),
                                MAE = MAE(as.numeric(predicted_Fertility_k_fold[[i]]), testSet$Fertility))

    total_res <- rbind(total_res,result_k_fold)
  }

  for (i in 1:k){
    if(i == 1)
      sum_res <- total_res[i,]
    else{
      sum_res <- sum_res + total_res[i,]
    }
  }
  return(sum_res/k)
}

result_k_fold <- testMethodKfoldCV(data,10)
result_k_fold

## vs LOOCV
result_LOOCV <- data.frame(R2 = R2(predicted_Fertility, data$Fertility), 
                           RMSE = RMSE(predicted_Fertility, data$Fertility), 
                           MAE = MAE(predicted_Fertility, data$Fertility))
result_LOOCV


## Check well organize
dim(data)

dim(trainData)
dim(testData)

rownames(trainData)
rownames(testData) 
intersect(rownames(trainData), rownames(testData))


## Q2
testMethodKfoldCVRepeat <- function(data, k, rep){
  result <- c()
  final <- c()
  set.seed(NULL)
  # result <- append(result, KfoldCV(shuffle, k))
  for(i in 1:rep) {
    rows <- sample(1:nrow(data))
    shuffle <- data[rows, ]
    result[[i]] <- KfoldCV(shuffle, k)
  }
  
  for(i in 1:rep){
    predict_Fert_Kfold_rep <- c()
    result_KfoldRepeat <- c()
    total_res <- c()
    for(j in 1:k){
      trainSet <- c()
      testSet <- result[[i]][[j]]
      traindata <- result[[i]][-j]
      for(l in traindata){
        trainSet <- rbind(trainSet,l)
      }
      
      model <- lm(Fertility ~., data = trainSet)
      predict_Fert_Kfold_rep[[j]] <- model %>% predict(testSet)
      result_KfoldRepeat <- data.frame(R2 = R2(as.numeric(predict_Fert_Kfold_rep[[j]]), testSet$Fertility),
                                       RMSE = RMSE(as.numeric(predict_Fert_Kfold_rep[[j]]), testSet$Fertility),
                                       MAE = MAE(as.numeric(predict_Fert_Kfold_rep[[j]]), testSet$Fertility))
      total_res <- rbind(total_res,result_KfoldRepeat)
    }
    for (j in 1:k){
      if(j == 1)
        sum_res <- total_res[j,]
      else{
        sum_res <- sum_res + total_res[j,]
      }
    }
    
    final <- rbind(final, (sum_res/k))
    
  }
  
  for (i in 1:rep){
    if(i == 1)
      real_final <- final[i,]
    else{
      real_final <- real_final + final[i,]
    }
  }
  return(real_final/rep)
}
  
result_k_fold_repeat <- testMethodKfoldCVRepeat(data,10,3)
result_k_fold_repeat

## Q3

result_LOOCV

result_k_fold <- testMethodKfoldCV(data,20)
result_k_fold

result_k_fold_repeat <- testMethodKfoldCVRepeat(data, 20, 2000)
result_k_fold_repeat


