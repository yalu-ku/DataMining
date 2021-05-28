## Week7

setwd("C:\\Users\\YaluAro\\2021-1학기\\데이터마이닝")
getwd()

setRepositories(ind = 1:8)

install.packages("car")
install.packages("mosaic")
install.packages("tidyverse")
install.packages("ggfortify")
install.packages("huxtable")
install.packages("jtools")
install.packages("latex2exp")
install.packages("pubh")
install.packages("sjlabelled")
install.packages("sjPlot")
install.packages("sjmisc")

# rm(list=ls())

library(MASS)
library(car)
library(mosaic)
library(tidyverse)
library(ggfortify)
library(huxtable)
library(jtools)
library(latex2exp)
library(pubh)
library(sjlabelled)
library(sjPlot)
library(sjmisc)

## Linear Regression Application
data(birthwt)
# view(birthwt)
?birthwt
str(birthwt)

birthwt <- birthwt %>% 
  # 알아보기 쉽게 변형 (ex. 0/1 -> Non/Smoker)
  mutate(smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),  
        race = factor(race, label = c("white", "African American", "Other"))) %>% 
  var_labels(bwt = "Birth Weight(g)",
            smoke = "Smoking status",
            race = "Race")
birthwt %>% 
  group_by(race, smoke) %>% 
  summarise(
    n = n(), #How many observe in this option
    Mean = mean(bwt),
# birthwt$bwt #Continous variable
    SD = sd(bwt),
    Median = median(bwt),
    CV = rel_dis(bwt) #pubh Package
  ) %>% 
  as_hux() %>% 
  theme_pubh(1) #How many volume can be Header

birthwt %>% 
  box_plot(bwt ~ smoke, fill = ~ race) + theme_classic() 
# ~ smoke : depending on the smoke

birthwt %>% 
  gen_bst_df(bwt ~ race|smoke) %>% 
  as_hux() %>% theme_pubh(1) #Acdemic General 표기법

model_norm <- lm(bwt ~ smoke + race, data = birthwt)
model_norm

model_norm %>% Anova() #P-value는 0에 가까울 수록 좋음


model_norm %>% 
  summ(confint = T, model.info = T)

model_norm %>% 
  glm_coef(label = model_labels(model_norm)) %>% 
  as_hux() %>% 
  set_align(everywhere, 2:3, "right") %>% 
  theme_pubh(1) %>% 
  add_footnote(get_r2(model_norm), font_size = 12)

model_norm %>% 
  glance()

?rel_dis

head(birthwt)

get_labels(birthwt)

x <- 1:100
print(x)
sum(x>50)

