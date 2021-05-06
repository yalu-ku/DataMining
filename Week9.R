## Week7

setwd("D:\\2021-1학기\\데이터마이닝")
getwd()

setRepositories(ind = 1:8)

# packages <- c("","")
# install.packages(packages)
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

rm(list=ls())

## FOR library(car)
install.packages("curl")
install.packages("openxlsx")
install.packages("cellranger")

## FOR library(mosaic)
install.packages("gtable")
install.packages("yaml")
install.packages("munsell")
install.packages("ggforce")
install.packages("labelled")
install.packages("crosstalk")
install.packages("htmlwidgets")

## For library(tidyverse)
install.packages("DBI")
install.packages("fs")

## For library(pubh)
install.packages("emmeans")


## For library(sjPlot)
install.packages("xfun")
install.packages("statmod")
install.packages("minqa")
install.packages("nloptr")

# library(MASS)
library(car)
library(broom)
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


## FOR copy_labels Function
install.packages("labeling")
library(labeling)

## Linear Regression Application
data(birthwt, package = "MASS")
View(birthwt) # v : error
??birthwt
?birthwt
str(birthwt)

## Change easy to read (ex. 0/1 -> Non/Smoker)
## mutate {dplyr}, var_labels {sjlabelled}
birthwt %>% 
  mutate(smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),  
         race = factor(race, label = c("white", "African American", "Other"))) %>% 
  var_labels(bwt = "Birth Weight(g)",
             smoke = "Smoking status",
             race = "Race")
## How can Check? 
get_labels(birthwt)

head(birthwt)
View(birthwt)
tibble(birthwt)

??var_labels

birthwt
birthwt %>% 
  group_by(race, smoke)
?group_by

birthwt %>% 
  group_by(race, smoke) %>% 
  summarise(
    n = n(), #How many observe in each cases
    Mean = mean(bwt), #One of the targeted data(goal) 
    #?birthwt, birthwt$bwt, Continous variable, Regression modeling
    SD = sd(bwt),
    Median = median(bwt),
    CV = rel_dis(bwt) #{pubh}
  ) %>% 
  as_hux() %>% 
  theme_pubh(1)

??theme_pubh

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
  summ(confint = T, model.info = T) #{jtools}

model_norm %>% 
  glm_coef(label = model_labels(model_norm)) %>% 
  as_hux() %>% 
  set_align(everywhere, 2:3, "right") %>% 
  theme_pubh(1) %>% 
  add_footnote(get_r2(model_norm), font_size = 12) #add R square

model_norm %>% glance()

?rel_dis

head(birthwt)

get_labels(birthwt)

# x <- 1:100
# print(x)
# sum(x>50)

model_norm %>% 
  plot_model("pred", terms = ~race|smoke, dot.size =1.5, title = "")

emmip(model_norm, smoke ~ race) %>% 
  gf_labs(y = get_label(birthwt$bwt), x = "", col = "Smoking status") + them_classic()


####################################
## Week 9

## Logistic Regression 

# install.packages("Epi")
# library(Epi)
# ?diet

data(diet, package = "Epi")
View(diet)

factor(diet$chd)

diet <- diet %>% 
  mutate(chd = factor(chd, labels = c("No CHD", "CHD"))) %>% 
  var_labels(
    chd = "Coronary heart disease",
    fibre = "Fibre intake (10 g/day)"
  )


diet %>% 
  estat(~fibre|chd) %>% # depending on CHD information investigate FIBRE
  as_hux() %>% 
  theme_pubh(1)

diet %>% 
  na.omit() %>% 
  # 위 var_labels에서 가져옴
  copy_labels(diet) %>% # 여길 주석한다면?
  box_plot(fibre ~ chd)
# axis_labs() %>% # takes labels from labelled data 
# theme_classic()

## glm(outcome ~ )
model_binom1 <- glm(chd ~ fibre, data = diet, family = binomial)
model_binom2 <- glm(chd ~ fibre + height + weight, data = diet, family = binomial)

model_binom1
model_binom2
## What model will be good? ***Final Exam***
## R adR AIC BIC 


model_binom %>% 
  summ(confint = T, model.info = T, exp = T)



