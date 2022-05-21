install.packages(c("mplot","ISLR","glmnet","MASS","readr","dplyr"))
install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(lattice)
library(Matrix)
library(caret)
library(glmnet)
library(dplyr)
library(tidyr)
library(faux)

rm(list=ls())

#Example 1:60 data sets/(30,30,300) sets/beta=beta_1,12 variables/sigma=4/corr=0.6^(i-j)
#Construct the corvariance for X_i
cor_max <- matrix(nrow = 12, ncol = 12)

for(i in 1:12){
  for(j in 1:12){
    cor_max[i,j] <- 0.6^(abs(i - j))
  }
}

#Simulate X for Ex1_trainning set
set.seed(123)
sigma_1 <- 4
beta_1 <- as.matrix(c(0,4,2,3,0,0,0,1,5.5,7.5,0,0))

lambda <- 10^seq(-3, 3, length = 100)

#Simulate 60 times
MSE_lao <- c()
MSE_rid <- c()
MSE_en <- c()
non_zero_lao <- c()
non_zero_en <- c()

for (i in 1:60){
#Simulate the data
  dat_mc_1_train_x <- as.matrix(rnorm_multi(30, 12, 0, 1, cor_max, 
                                            varnames = letters[1:12]))
  class(dat_mc_1_train_x[,1])
  dat_frame_mc_1_train_x <- as.data.frame(dat_mc_1_train_x)
  
  #value of Y for Ex1_trainning set
  dat_mc_1_train_y <- dat_mc_1_train_x %*% beta_1 + sigma_1*rnorm(30)
  
  #Ex_1 test set
  dat_mc_1_test_x <- as.matrix(rnorm_multi(300, 12, 0, 1, cor_max, 
                                           varnames = letters[1:12]))
  
  
  dat_mc_1_test_y <- dat_mc_1_test_x %*% beta_1 + sigma_1*rnorm(300)
  
###LASSO regression
model_lao <- train(
  dat_frame_mc_1_train_x, dat_mc_1_train_y[,1], method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

outcome_lao <- coef(model_lao$finalModel, model_lao$bestTune$lambda)
non_zero_lao[i] <- colSums(outcome_lao != 0)


# Make predictions
pred_lao <- model_lao %>% predict(dat_mc_1_test_x)
# Model prediction performance
MSE_lao[i] <- RMSE(pred_lao, dat_mc_1_test_y)

###Ridge regression
  model_rid <- train(
    dat_frame_mc_1_train_x, dat_mc_1_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  # Make predictions
pred_rid <- model_rid %>% predict(dat_mc_1_test_x)
  # Model prediction performance
MSE_rid[i] <- RMSE(pred_rid, dat_mc_1_test_y)

###Elastic Net Method
model_en <- train(
  dat_frame_mc_1_train_x, dat_mc_1_train_y[,1], method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
outcome_en <- coef(model_en$finalModel, model_en$bestTune$lambda)
non_zero_en[i] <- colSums(outcome_en != 0)

# Make predictions on the test data
pred_en <- model_en %>% predict(dat_mc_1_test_x)
# Model performance metrics
MSE_en[i] <- RMSE(pred_en, dat_mc_1_test_y)
}

print(median(non_zero_lao))
print(median(non_zero_en))

print(median(MSE_lao))
print(median(MSE_rid))           
print(median(MSE_en))  

MSE_lao_boot_med <- c()
MSE_rid_boot_med <- c()
MSE_en_boot_med <- c()
for(j in 1:600){
  MSE_lao_boot <- sample(MSE_lao, 60, replace = TRUE)
  MSE_rid_boot <- sample(MSE_rid, 60, replace = TRUE)
  MSE_en_boot <- sample(MSE_en, 60, replace = TRUE)
  MSE_lao_boot_med <- c(MSE_lao_boot_med,median(MSE_lao_boot))
  MSE_rid_boot_med <- c(MSE_rid_boot_med,median(MSE_rid_boot))
  MSE_en_boot_med <- c(MSE_en_boot_med,median(MSE_en_boot))
}

print(sd(MSE_lao_boot_med))
print(sd(MSE_rid_boot_med))
print(sd(MSE_en_boot_med))

#print(median(non_zero_lao))
#[1] 9
# print(median(non_zero_en))
#[1] 10

#print(median(MSE_lao))
#[1] 4.858841
# print(median(MSE_rid))           
#[1] 4.994676
# print(median(MSE_en))  
#[1] 4.886042

#print(sd(MSE_lao_boot_med))
#[1] 0.06968987
# print(sd(MSE_rid_boot_med))
#[1] 0.1015829
# print(sd(MSE_en_boot_med))
#[1] 0.07321801

#sd(MSE_lao)
#[1] 0.4746382
#sd(MSE_rid)
#[1] 0.544789
#sd(MSE_en)
#[1] 0.5088323

