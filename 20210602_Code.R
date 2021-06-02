## DM - Week 14
## Aro Yun
## 06-02-2021

## Set Env.
setRepositories(ind = 1:8)

library(tidyverse)
library(dastarium)
# install.packages("datarium")
library(caret) # Important package
library(dplyr)
library(rpart)
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)

## Set working Dir.
WORK_DIR <- "E:\\2021-1학기\\데이터마이닝\\Week14"
setwd(WORK_DIR)

## Decision tree

# Step1: Loading Titanic data
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv")
View(data)
str(data)
# install.packages("titanic")
# library(titanic)
# View(titanic.raw)

### Cleansing
# Raw Data shuffling
randomIdx <- sample(1:nrow(data))
data <- data[randomIdx,]

# Data cleansing
str(data) # char -> num [age, fare]

data$age <- as.numeric(data$age)
data$fare <- as.numeric(data$fare)

### check
# sum(is.na(data$fare))
# sum(is.na(data$age))
# str(data) #check their types

cleanData <- data %>% 
  dplyr:::select(-c(home.dest, cabin, name, x, ticket)) %>% 
  mutate(pclass = factor(pclass, levels = c(1,2,3), labels = c("Upper", "Middle", "Lower")),
         survived = factor(survived, levels = c(0,1), labels = c("No", "YES"))) %>%
  na.omit()

glimpse(cleanData)


# Generating Cross-validation data (Train and Test set)
createTrainTestSet <- function(data, propTrain = 0.8){
  propTest = 1 - propTrain # remaining 20% is TestSet
  
  total_row <- propTest * nrow(data)
  testSetIdx <- 1:total_row
  
  output <- list(testSet = data.frame(data[testSetIdx,]),
                 trainSet = data.frame(data[-testSetIdx,]))
  return(output)
}


foldData <- createTrainTestSet(cleanData, 0.5)

dim(cleanData)

dim(foldData$testSet)
dim(foldData$trainSet)

prop.table(table(foldData$trainSet$survived))
prop.table(table(foldData$testSet$survived))
prop.table(table(cleanData$survived))


# Modeling
model <- rpart(survived~., data = foldData$trainSet, method = "class")
model

rpart.plot(model, extra = 106)


# Prediction & Error estimation
predictResult <- predict(model, foldData$testSet, type = "class")
data.frame(predictResult)

### Compare
confusion <- table(foldData$testSet$survived, predictResult)
confusion

### Evaluation
Accuracy <- sum(diag(confusion)) / sum(confusion) * 100
Sensitivity <- confusion[2,2] / sum(confusion[,2]) # model power
Specificity <- confusion[1,1] / sum(confusion[,1])

############################################

## Data Loading
webAddress <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

Data <- read.csv(webAddress, head = F, stringsAsFactors = T)

View(Data)

features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "convavity", "concave_points", "symmetry", "fractal_dimension")

colnames(Data) <- c("id", "diagnosis", paste0(features, "_mean"), paste0(features, "_se"), paste0(features, "_worst"))

View(Data)

cleanData2 <- Data[,3:ncol(Data)]
# 3:ncol(Data)
rownames(cleanData2) <- Data$id # or Data[,1]
# View(cleanData)

cleanData2 <- cbind(cleanData2, Data$diagnosis) # Final goal


# Raw Data shuffling
randomIdx2 <- sample(1:nrow(cleanData2))
cleanData2 <- cleanData2[randomIdx2,]


# Generate Train and Test set
foldData2 <- createTrainTestSet(cleanData2, 0.5)

dim(foldData2$testSet)
dim(foldData2$trainSet)

prop.table(table(foldData2$trainSet$Data.diagnosis))
prop.table(table(foldData2$testSet$Data.diagnosis))
prop.table(table(cleanData2$'Data$diagnosis'))

# Modeling
ldaModel <- lda(Data.diagnosis ~ ., data = foldData2$trainSet)
qdaModel <- qda(Data.diagnosis ~ ., data = foldData2$trainSet)
knnModel_3 <- kknn(Data.diagnosis ~ ., train = foldData2$trainSet, test = foldData2$testSet, k = 3) ### Modeling and Prediction 한번에 끝냄
knnModel_5 <- kknn(Data.diagnosis ~ ., train = foldData2$trainSet, test = foldData2$testSet, k = 5)
knnModel_7 <- kknn(Data.diagnosis ~ ., train = foldData2$trainSet, test = foldData2$testSet, k = 7)

svmModel <- ksvm(Data.diagnosis ~ ., data = foldData2$trainSet, Kernel = "rdf",
                 type = "C-svc")

# Prediction
prediction_lda <- predict(ldaModel, newdata = foldData2$testSet)
prediction_lda #likelihood function
prediction_qda <- predict(qdaModel, newdata = foldData2$testSet)

prediction_svm <- predict(svmModel, newdata = foldData2$testSet)

prediction_lda$class

# Model performance check
confusion_lda <- table(Predicted = prediction_lda$class, 
                   Diagnosis = foldData2$testSet$Data.diagnosis)
confusion_qda <- table(Predicted = prediction_qda$class, 
                       Diagnosis = foldData2$testSet$Data.diagnosis)
confusion_3NN <- table(Predicted = fitted(knnModel_3),
                       Diagnosis = foldData2$testSet$Data.diagnosis)
confusion_5NN <- table(Predicted = fitted(knnModel_5),
                       Diagnosis = foldData2$testSet$Data.diagnosis)
confusion_7NN <- table(Predicted = fitted(knnModel_7),
                       Diagnosis = foldData2$testSet$Data.diagnosis)

confusion_svm <- table(Predicted = prediction_svm,
                       Diagnosis = foldData2$testSet$Data.diagnosis)

confusion_lda
confusion_qda
confusion_7NN
confusion_svm

Accuracy <- sum(diag(confusion)) / sum(confusion) * 100
Accuracy
Sensitivity <- confusion[2,2] / sum(confusion[,2])
Sensitivity
Specificity <- confusion[1,1] / sum(confusion[,1])
Specificity




