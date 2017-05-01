# Title     : model
# Objective : Build a Genric Linear Regression Model
# Created by: pawan.lamba
# Created on: 5/1/2017
## Assumes that dataset is the csv file with column names at the top and all the variables are numerical variable \
source("conf.R")

# To read data into R
data <- read.csv(dataFileName,sep=',',header=T,stringsAsFactors=F,na.strings=c("NULL","NA","","null"))

# Formula String is the dependent variable(DV) you want to model across the independent variable (IDV): Feel Free to edit it according to your requirement.
idvVariables <- variableColumns
dvVariable <- c(dependendentColumns)
# this makes the formula string
formulaString <- as.formula(paste(dvVariable," ~ ", paste(idvVariables,sep="",collapse="+")))

#Only select cases which has the complete data
dataSet <- data[complete.cases(data[,c(idvVariables,dvVariable)]),]

# model the data
model <- lm(formulaString ,data = dataSet)
print("************************************** Model with Random variables ******************************************")
print(summary(model))


## automated pruning of the model
print("************************************** Run Model Pruning ***************************************")
library(MASS)
model1 =  step(model)
print("************************************** Model without Random variables ***************************************")
print(summary(model1))


## print the model Equations

coeff = model1$coefficients
modelString =  paste(names(coeff),coeff,sep=" * ",collapse =" + " )
print("************************************** Model Equations ******************************************")
print(modelString)