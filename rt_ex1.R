library(foreign)
library(randomForest)
library(Hmisc)
library(gdata) 
library(glmnet)
## change the working directory
getwd()
setwd('/Users/shill/documents/R scripts')
load("titanic3.sav")
attach(titanic3)
titanic3.train <- titanic3[sample(1:nrow(titanic3), 300, replace=FALSE),] ##take a sample
titanic3.survival.train = glm(survived ~ pclass + sex + pclass:sex + age + sibsp,family = binomial(logit), data = titanic3.train)
summary(titanic3.survival.train)

