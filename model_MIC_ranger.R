setwd("/labs/syang5/Nikhil/for_Kristel/Single_Cell_Tracking/complete_ts_raw/pooled_ID_groups_12/finer_concs/iteration-2") ## setwd with the normalized data

library(plyr)
library(readxl)
library(tidyverse)
library(data.table)
library(anytime)
library(stringr)
library(ggplot2)
library(ggridges)
library(pastecs)
library(reshape2)
library(nnet)
library(pROC)
library(caret)
library(doParallel)
library(rsample)

x <- read.csv("normalized_pooled_ID_6R_5S_1I_train.csv", header=TRUE) ## read in normalized data
x <- na.omit(x)
x[x$strain=="KP_0010","MIC"] <- 1
x[x$strain=="KP_0016","MIC"] <- 0.5
x[x$strain=="KP_0120","MIC"] <- 8
x[x$strain=="KP_0142","MIC"] <- 2
x[x$strain=="KP_0147","MIC"] <- 0.5
x[x$strain=="KP_028","MIC"] <- 0.5
x[x$strain=="KP_160","MIC"] <- 8
x[x$strain=="KP_1705","MIC"] <- 8
x[x$strain=="KP_0153","MIC"] <- 8
x[x$strain=="KP_0140","MIC"] <- 2
x[x$strain=="KP_700603","MIC"] <- 0.5

x[x$resistance=="I", "weights"] <- 1  ## weights calculated by dividing the smallest ratio 'I' by the others
x[x$resistance=="R", "weights"] <- 0.048
x[x$resistance=="S", "weights"] <- 0.08

set.seed(123)
cl <- makePSOCKcluster(16)
registerDoParallel(cl)

for (i in 1:8){
  compiled.sub <- x[x$Time_Point<=i,]
  
  x.folds <- createMultiFolds(compiled.sub$ID, k = 5, times = 5)
  
  fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 5, classProbs=T,
                              savePredictions = T, allowParallel = TRUE,
                              index = x.folds)
  
  y <- compiled.sub[,c(1:19,22,23,26)]
  w <- compiled.sub$weights
  rf <- train(MIC ~ ., data = y, method = "ranger", trControl = fit.control,  weights = w)
  save(rf, file=paste("ranger_MIC_iter-2_",i,".RData", sep=""))
}

stopCluster(cl)
