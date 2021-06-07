######################
## K-fold CV
## Implemented by ARO
######################

## Set Env.
setRepositories(ind = c(1:8))

Packages <- c("datarium", "dplyr")
install.packages("Packages")

library(datarium) # A lots of data
library(dplyr) # 



## Set working Dir.
WORK_DIR <- "Code your working path" # have to use / or \\
setwd(WORK_DIR)



## Function of K-fold
KfoldCV <- function(data, k) {
  output <- list()
  ## Divided folds
  folds <- cut(seq(1,nrow(data)), breaks = k, labels = FALSE)
  
  for(i in 1:k) {
    ## Data splicing
    testSet <- data %>% filter(folds==i)
    trainSet <- data %>% filter(folds!=i)
    ## Training 
    ## first_col_name :: Put on the first column name of data
    model <- lm(first_col_name ~., data = trainSet)
    ## Testing
    output[[i]] <- model %>% predict(testSet)
  }
  return(output)
}

## Example
test <- KfoldCV(swiss, 10) # first_col_name -> Fertility
test <- KfoldCV(marketing, 5) # first_col_name -> youtube
test <- KfoldCV(Boston, 24) # first_col_name -> crim
rm(KfoldCV)
?marketing

## Model Evaluation
result <- data.frame(R2 = R2(unlist(test), data$first_col_name),
                     RMSE = RMSE(unlist(test), data$first_col_name),
                     MAE = MAE(unlist(test), data$first_col_name))
result

