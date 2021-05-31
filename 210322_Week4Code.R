install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")

library(tidyverse)
library(lubridate)
library(dplyr)
library(rvest)
library(tidyr)


setwd("C:\\Users\\YaluAro\\2021-1학기\\데이터마이닝\\Week4")
getwd()

mydata_df <- read.csv("C:\\Users\\YaluAro\\2021-1학기\\데이터마이닝\\Week4\\data\\small_data.csv")

class(mydata_df)
View(mydata_df)

## tibble은 _ 언더바
mydata_tib <- read_csv("C:\\Users\\YaluAro\\2021-1학기\\데이터마이닝\\Week4\\data\\small_data.csv")

class(mydata_tib)
mydata_tib



mydata <- read_table("C:\\Users\\YaluAro\\2021-1학기\\데이터마이닝\\Week4\\data\\small_data.csv")

mydata


mydata <- read.table("C:\\Users\\YaluAro\\2021-1학기\\데이터마이닝\\Week4\\data\\small_data.csv", sep=",")
mydata

?read_table

## 반드시 Check Up


###############################
untidy_data <- tibble(
  name = c("Ana", "Bob", "Cara"),
  meds = c("advil 600mg 2xday", "tylenol 650mg 4xday", "advil 200mg 3xday")
)
untidy_data

dim(untidy_data)


untidy_data %>% 
  separate(col = meds, into = c("med_name", "dose_mg", "times_per_day"), sep = " ") %>% 
  mutate(times_per_day = as.numeric(str_remove(times_per_day, "xday"))) %>% 
  mutate(dose_mg = as.numeric(str_remove(dose_mg, "mg"))) 


?separate
?mutate


untidy_data2 <- tibble(
  name = c("Ana", "Bob", "Cara"),
  wt_07_01_2018 = c(100, 150, 140),
  wt_08_01_2018 = c(104, 155, 138),
  wt_09_01_2018 = c(NA, 160, 142)
)

untidy_data2
untidy_data2 %>% 
  gather(key = "date", value = "weight", -name) %>% 
  mutate(date = str_remove(date, "wt_"), date = dmy(date))



demo_data <- read_csv("data/yrbss_demo.csv")
demo_data

head(demo_data)
tail(demo_data)
glimpse(demo_data)


bmi20_data <- demo_data %>% filter(bmi > 20)

## 아래 보다는 위의 filter 사용하는게 BETTER
idx <- which(demo_data$bmi > 20)
bmi20_data <- demo_data[idx,]

dim(bmi20_data)



