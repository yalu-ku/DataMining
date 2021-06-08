#########################
## DataMining HW3
## Implemented by ARO
## due to 210611
#########################

## Set Env.
setRepositories(ind = 1:8)

# install.packages("datarium")

library(tidyverse)
library(datarium)
library(caret) ## Classification And Regression Training
library(dplyr)
library(rpart)
library(rpart.plot)
library(kknn)
library(ROCR)
library(kernlab)
library(MASS)


## Set working Dir.
WORK_DIR <- "E:\\2021-1학기\\데이터마이닝"
setwd(WORK_DIR)


##### Decision tree #####

# Step1: Loading Titanic data


data01 <- data(oil)
View(data01)

data01 <- data(oil) %>% data.frame(oil)
View(data01)
# data01 <- read.csv(scat)
# View(data)
# str(data)

# install.packages("titanic")
# library(titanic)
# View(titanic.raw)

# install.packages("titanic")
# library(titanic)
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv")
# View(titanic.raw)


test <- data(GermanCredit)
test <- str(GermanCredit[, 1:10])
test <- data.frame(GermanCredit)

dim(test)
View(test)



##### Cleansing #####

## Raw Data shuffling

randomIdx <- sample(1:nrow(test))
test <- test[randomIdx,]


## Data cleansing
str(data01) # char -> num [age, fare]

# data01$age <- as.numeric(data$age)
# data01$fare <- as.numeric(data$fare)

## CHECK
# sum(is.na(data$fare))
# sum(is.na(data$age))
# str(data) #check their types


cleanTest <- test %>% 
  dplyr:::select(c(1:10)) %>% 
  mutate(Telephone = factor(Telephone, levels=c(0,1), labels = c("Have", "Haven't")),
         ForeignWorker = factor(ForeignWorker, levels = c(0,1), labels = c("NO", "YES"))) 
  # rename(test, "InstallmentR")
glimpse(cleanTest)

#############################################################

cleanData <- data %>% 
  dplyr:::select(-c(home.dest, cabin, name, x, ticket)) %>% 
  mutate(pclass = factor(pclass, levels = c(1,2,3), labels = c("Upper", "Middle", "Lower")),
         survived = factor(survived, levels = c(0,1), labels = c("No", "YES"))) %>%
  na.omit()

glimpse(cleanData)


#############################################################
# Generating Cross-validation data (Train and Test set)
createTrainTestSet <- function(data, propTrain = 0.8){
  propTest = 1 - propTrain # remaining 20% is TestSet
  
  total_row <- propTest * nrow(data)
  testSetIdx <- 1:total_row
  
  output <- list(testSet = data.frame(data[testSetIdx,]),
                 trainSet = data.frame(data[-testSetIdx,]))
  return(output)
}


#############################################################

testMethodKfoldCV <- function(data, k) {
  output <- list()
  ## Divided folds
  folds <- cut(seq(1,nrow(data)), breaks = k, labels = FALSE)
  
  for(i in 1:k) {
    ## Data splicing
    testSet <- data %>% filter(folds==i)
    trainSet <- data %>% filter(folds!=i)
    ## Training
    # model <- lm(Duration ~., data = trainSet)
    ## Testing
    # output[[i]] <- model %>% predict(testSet)
    output <- list(testSet = data.frame(testSet),
                   trainSet = data.frame(trainSet))
  }
  return(output)
}

foldTest <- testMethodKfoldCV(cleanTest, 20)
foldTest

dim(cleanTest)
dim(foldTest$testSet)
dim(foldTest$trainSet)



foldData <- createTrainTestSet(cleanData, 0.5)

dim(cleanData)

dim(foldData$testSet)
dim(foldData$trainSet)

prop.table(table(foldData$trainSet$survived))
prop.table(table(foldData$testSet$survived))
prop.table(table(cleanData$survived))
#-----------------------------------------------------------#
prop.table(table(foldTest$trainSet$Class))
prop.table(table(foldTest$testSet$Class))
prop.table(table(cleanTest$Class))



#############################################################
## Modeling
model <- rpart(survived~., data = foldData$trainSet, method = "class")
model

rpart.plot(model, extra = 106)

#-----------------------------------------------------------#

testModel <- rpart(Class~., data = foldTest$trainSet, method = "class")
testModel


?rpart
?lm

?lda

testLDA <- lda(Class ~ ., data = foldTest$trainSet)
testQDA <- qda(Class ~ ., data = foldTest$trainSet)
testSVM <- ksvm(Class ~ ., data = foldTest$trainSet, Kernel = "rdf", type = "C-svc")
## Prediction
testPredict_lda <- predict(testLDA, newdata = foldTest$testSet)
testPredict_qda <- predict(testQDA, newdata = foldTest$testSet)
testPredict_svm <- predict(testSVM, newdata = foldTest$testSet)
testPredict_lda #likelihood function
testPredict_qda
testPredict_svm
### Modeling and Prediction 한번에 끝냄
testKNN3 <- kknn(Class ~ ., train = foldTest$trainSet, test = foldTest$testSet, k = 3) 
testKNN5 <- kknn(Class ~ ., train = foldTest$trainSet, test = foldTest$testSet, k = 5)
testKNN7 <- kknn(Class ~ ., train = foldTest$trainSet, test = foldTest$testSet, k = 7)

testLDA
testQDA
testKNN3
testKNN5
testKNN7
testSVM
#############################################################
#############################################################
# Prediction & Error estimation
predictResult <- predict(model, foldData$testSet, type = "class")
data.frame(predictResult)
#-----------------------------------------------------------#
testResult <- predict(testModel, foldTest$testSet, type = "class")
data.frame(testResult)

#############################################################
#############################################################
### Compare
confusion <- table(foldData$testSet$survived, predictResult)
confusion
#-----------------------------------------------------------#
testConf <- table(foldTest$testSet$Class, testResult)
testConf

# Model performance check
confusion_lda <- table(Predicted = testPredict_lda$class, 
                       Evaluate = foldTest$testSet$Class)
confusion_qda <- table(Predicted = testPredict_qda$class, 
                       Evaluate = foldTest$testSet$Class)
confusion_3NN <- table(Predicted = fitted(testKNN3), 
                       Evaluate = foldTest$testSet$Class)
confusion_5NN <- table(Predicted = fitted(testKNN5), 
                        Evaluate = foldTest$testSet$Class)
confusion_7NN <- table(Predicted = fitted(testKNN7), 
                        Evaluate = foldTest$testSet$Class)
confusion_svm <- table(Predicted = testPredict_svm,
                       Diagnosis = foldTest$testSet$Class)

confusion_lda
confusion_qda
confusion_3NN
confusion_5NN
confusion_7NN
confusion_svm


#############################################################
#############################################################
### Evaluation
Accuracy <- sum(diag(confusion)) / sum(confusion) * 100
Sensitivity <- confusion[2,2] / sum(confusion[,2]) # model power
Specificity <- confusion[1,1] / sum(confusion[,1])





#############################################################
#############################################################
#############################################################

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

#############################################################
#############################################################
# Raw Data shuffling
randomIdx2 <- sample(1:nrow(cleanData2))
cleanData2 <- cleanData2[randomIdx2,]

#############################################################
# Generate Train and Test set
foldData2 <- createTrainTestSet(cleanData2, 0.5)

dim(foldData2$testSet)
dim(foldData2$trainSet)

prop.table(table(foldData2$trainSet$Data.diagnosis))
prop.table(table(foldData2$testSet$Data.diagnosis))
prop.table(table(cleanData2$'Data$diagnosis'))
#-----------------------------------------------------------#
foldData3 <- testMethodKfoldCV(cleanData2, 10)

dim(foldData3$testSet)
dim(foldData3$trainSet)

prop.table(table(foldData3$trainSet$Class))


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




