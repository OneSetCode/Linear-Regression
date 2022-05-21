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

###Example 5:Group selection of elastic net
#beta is a vector contains 50 factors, first 30 factors all equal 6.5, and last 20 all equal to 0
beta_5 <- c()
beta_5[1:30] <- 6.5
beta_5[31:50] <- 0

sigma_5 <- 15.5

lambda <- 10^seq(-3, 3, length = 100)

#Simulate 60 times
MSE_lao <- c()
MSE_rid <- c()
MSE_en <- c()
non_zero_lao <- c()
non_zero_en <- c()


#Simulate the data
  #60 data sets
  #Training set:60 data; Testing set:480 data
  #Simulate X for Ex5_trainning set
  for(s in 1:60){
  dat_mc_5_train_x <- matrix(nrow = 60, ncol = 50)

  for(j in 1:60){
  x_5_train <- c()
  z_1 <- rnorm(1)
  z_2 <- rnorm(1)
  z_3 <- rnorm(1)
  #xi =Z1 +ei , Z1 ¡«N(0, 1), i=1, . . . , 10, ei ~ N(0, 0.05)
  for(i in 1:10){
    x_5_train[i] = z_1 + rnorm(1, 0, 0.05)
  }
  #xi =Z1 +ei , Z1 ¡«N(0, 1), i=11, . . . , 20, ei ~ N(0, 0.05)
  for(i in 11:20){
    x_5_train[i] = z_2 + rnorm(1, 0, 0.05)
  }
  #xi =Z1 +ei , Z1 ¡«N(0, 1), i=21, . . . , 30, ei ~ N(0, 0.05)
  for(i in 21:30){
    x_5_train[i] = z_3 + rnorm(1, 0, 0.05)
  }
  #xi~N(0,1), i = 31,...,50
  for(i in 31:50){
    x_5_train[i] = rnorm(1)  
  }
  
  dat_mc_5_train_x[j,] <- x_5_train 
  }
 
  dat_frame_mc_5_train_x <- as.data.frame(dat_mc_5_train_x)
  
  #value of Y for Ex1_trainning set
  dat_mc_5_train_y <- dat_mc_5_train_x %*% beta_5 + sigma_5*rnorm(60)
  
 
  #Ex_5 test set
  dat_mc_5_test_x <- matrix(nrow = 480, ncol = 50)
  
  for(k in 1:480){
    x_5_test <- c()
    z_1 <- rnorm(1)
    z_2 <- rnorm(1)
    z_3 <- rnorm(1)
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=1, . . . , 10, ei ~ N(0, 0.05)
    for(i in 1:10){
      x_5_test[i] = z_1 + rnorm(1, 0, 0.05)
    }
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=11, . . . , 20, ei ~ N(0, 0.05)
    for(i in 11:20){
      x_5_test[i] = z_2 + rnorm(1, 0, 0.05)
    }
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=21, . . . , 30, ei ~ N(0, 0.05)
    for(i in 21:30){
      x_5_test[i] = z_3 + rnorm(1, 0, 0.05)
    }
    #xi~N(0,1), i = 31,...,50
    for(i in 31:50){
      x_5_test[i] = rnorm(1)  
    }
    
    dat_mc_5_test_x[k,] <- x_5_test 
  }
  
  dat_frame_mc_5_test_x <- as.data.frame(dat_mc_5_test_x)
  
  #value of Y for Ex5_test set
  dat_mc_5_test_y <- dat_mc_5_test_x %*% beta_5 + sigma_5*rnorm(480)
  
###LASSO regression
model_lao <- train(
  dat_frame_mc_5_train_x, dat_mc_5_train_y[,1], method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

outcome_lao <- coef(model_lao$finalModel, model_lao$bestTune$lambda)
non_zero_lao[s] <- colSums(outcome_lao != 0)


# Make predictions
pred_lao <- model_lao %>% predict(dat_frame_mc_5_test_x)
# Model prediction performance
MSE_lao[s] <- RMSE(pred_lao, dat_mc_5_test_y)

###Ridge regression
  model_rid <- train(
    dat_frame_mc_5_train_x, dat_mc_5_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  # Make predictions
pred_rid <- model_rid %>% predict(dat_frame_mc_5_test_x)
  # Model prediction performance
MSE_rid[s] <- RMSE(pred_rid, dat_mc_5_test_y)

###Elastic Net Method
model_en <- train(
  dat_frame_mc_5_train_x, dat_mc_5_train_y[,1], method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
outcome_en <- coef(model_en$finalModel, model_en$bestTune$lambda)
non_zero_en[s] <- colSums(outcome_en != 0)

# Make predictions on the test data
pred_en <- model_en %>% predict(dat_frame_mc_5_test_x)
# Model performance metrics
MSE_en[s] <- RMSE(pred_en, dat_mc_5_test_y)
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


# print(median(non_zero_lao))
#[1] 16
# print(median(non_zero_en))
#[1] 34

#print(median(MSE_lao))
#[1] 17.34469
# print(median(MSE_rid))           
#[1] 18.29436
# print(median(MSE_en))  
#[1] 17.04736

#print(sd(MSE_lao_boot_med))
#[1] 0.130407
# print(sd(MSE_rid_boot_med))
#[1] 0.1283487
# print(sd(MSE_en_boot_med))
#[1] 0.1628731
