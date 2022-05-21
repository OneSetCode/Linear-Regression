# 2 Simulation
## simulation 1
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



## simulation 2
#Same as example 0, only changing beta to c(0.8, ..., 0.8)
#Example 2:60 data sets/(30,30,300) sets/beta=beta_1,12 variables/sigma=4/corr=0.6^(i-j)
#Construct the corvariance for X_i
cor_max <- matrix(nrow = 12, ncol = 12)

for(i in 1:12){
  for(j in 1:12){
    cor_max[i,j] <- 0.6^(abs(i - j))
  }
}

#Simulate X for Ex2_trainning set
set.seed(123)
sigma_2 <- 4
beta_2 <- as.matrix(c(0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8))

lambda <- 10^seq(-3, 3, length = 100)

#Simulate 60 times
MSE_lao <- c()
MSE_rid <- c()
MSE_en <- c()
non_zero_lao <- c()
non_zero_en <- c()

for (i in 1:60){
  #Simulate the data
  dat_mc_2_train_x <- as.matrix(rnorm_multi(30, 12, 0, 1, cor_max, 
                                            varnames = letters[1:12]))
  
  dat_frame_mc_2_train_x <- as.data.frame(dat_mc_2_train_x)
  
  #value of Y for Ex2_trainning set
  dat_mc_2_train_y <- dat_mc_2_train_x %*% beta_2 + sigma_2*rnorm(30)
  
  #Ex_2 test set
  dat_mc_2_test_x <- as.matrix(rnorm_multi(300, 12, 0, 1, cor_max, 
                                           varnames = letters[1:12]))
  
  
  dat_mc_2_test_y <- dat_mc_2_test_x %*% beta_2 + sigma_2*rnorm(300)
  
  ###LASSO regression
  model_lao <- train(
    dat_frame_mc_2_train_x, dat_mc_2_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  )
  
  outcome_lao <- coef(model_lao$finalModel, model_lao$bestTune$lambda)
  non_zero_lao[i] <- colSums(outcome_lao != 0)
  
  
  # Make predictions
  pred_lao <- model_lao %>% predict(dat_mc_2_test_x)
  # Model prediction performance
  MSE_lao[i] <- RMSE(pred_lao, dat_mc_2_test_y)
  
  ###Ridge regression
  model_rid <- train(
    dat_frame_mc_2_train_x, dat_mc_2_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  # Make predictions
  pred_rid <- model_rid %>% predict(dat_mc_2_test_x)
  # Model prediction performance
  MSE_rid[i] <- RMSE(pred_rid, dat_mc_2_test_y)
  
  ###Elastic Net Method
  model_en <- train(
    dat_frame_mc_2_train_x, dat_mc_2_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  )
  outcome_en <- coef(model_en$finalModel, model_en$bestTune$lambda)
  non_zero_en[i] <- colSums(outcome_en != 0)
  
  # Make predictions on the test data
  pred_en <- model_en %>% predict(dat_mc_2_test_x)
  # Model performance metrics
  MSE_en[i] <- RMSE(pred_en, dat_mc_2_test_y)
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



## simulation 3
cor_max <- matrix(nrow = 20, ncol = 20)

for(i in 1:20){
  for(j in 1:20){
    if (i==j){
      cor_max[i,j] <- 1
    }
    else{
      cor_max[i,j] <- 0.6
    }
  }
}

#Simulate X for Ex3_trainning set
set.seed(123)
sigma_3 <- 16
beta_3 <- as.matrix(c(0,0,0,0,0,0,0,0,0,0,
                      3,3,3,3,3,3,3,3,3,3))

lambda <- 10^seq(-3, 3, length = 100)

#Simulate 60 times
MSE_lao <- c()
MSE_rid <- c()
MSE_en <- c()
non_zero_lao <- c()
non_zero_en <- c()

for (i in 1:60){
  #Simulate the data
  dat_mc_3_train_x <- as.matrix(rnorm_multi(80, 20, 0, 1, cor_max, 
                                            varnames = letters[1:20]))
  
  dat_frame_mc_3_train_x <- as.data.frame(dat_mc_3_train_x)
  
  #value of Y for Ex3_trainning set
  dat_mc_3_train_y <- dat_mc_3_train_x %*% beta_3 + sigma_3*rnorm(80)
  
  #Ex_3 test set
  dat_mc_3_test_x <- as.matrix(rnorm_multi(400, 20, 0, 1, cor_max, 
                                           varnames = letters[1:20]))
  
  
  dat_mc_3_test_y <- dat_mc_3_test_x %*% beta_3 + sigma_3*rnorm(400)
  
  ###LASSO regression
  model_lao <- train(
    dat_frame_mc_3_train_x, dat_mc_3_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  )
  
  outcome_lao <- coef(model_lao$finalModel, model_lao$bestTune$lambda)
  non_zero_lao[i] <- colSums(outcome_lao != 0)
  
  
  # Make predictions
  pred_lao <- model_lao %>% predict(dat_mc_3_test_x)
  # Model prediction performance
  MSE_lao[i] <- RMSE(pred_lao, dat_mc_3_test_y)
  
  ###Ridge regression
  model_rid <- train(
    dat_frame_mc_3_train_x, dat_mc_3_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  # Make predictions
  pred_rid <- model_rid %>% predict(dat_mc_3_test_x)
  # Model prediction performance
  MSE_rid[i] <- RMSE(pred_rid, dat_mc_3_test_y)
  
  ###Elastic Net Method
  model_en <- train(
    dat_frame_mc_3_train_x, dat_mc_3_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  )
  outcome_en <- coef(model_en$finalModel, model_en$bestTune$lambda)
  non_zero_en[i] <- colSums(outcome_en != 0)
  
  # Make predictions on the test data
  pred_en <- model_en %>% predict(dat_mc_3_test_x)
  # Model performance metrics
  MSE_en[i] <- RMSE(pred_en, dat_mc_3_test_y)
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



## simulation 4
###Example 4:Group selection of elastic net
#beta is a vector contains 50 factors, first 30 factors all equal 6.5, and last 20 all equal to 0
beta_4 <- c()
beta_4[1:30] <- 6.5
beta_4[31:50] <- 0

sigma_4 <- 15.5

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
#Simulate X for Ex4_trainning set
for(s in 1:60){
  dat_mc_4_train_x <- matrix(nrow = 60, ncol = 50)
  
  for(j in 1:60){
    x_4_train <- c()
    z_1 <- rnorm(1)
    z_2 <- rnorm(1)
    z_3 <- rnorm(1)
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=1, . . . , 10, ei ~ N(0, 0.05)
    for(i in 1:10){
      x_4_train[i] = z_1 + rnorm(1, 0, 0.05)
    }
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=11, . . . , 20, ei ~ N(0, 0.05)
    for(i in 11:20){
      x_4_train[i] = z_2 + rnorm(1, 0, 0.05)
    }
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=21, . . . , 30, ei ~ N(0, 0.05)
    for(i in 21:30){
      x_4_train[i] = z_3 + rnorm(1, 0, 0.05)
    }
    #xi~N(0,1), i = 31,...,50
    for(i in 31:50){
      x_4_train[i] = rnorm(1)  
    }
    
    dat_mc_4_train_x[j,] <- x_4_train 
  }
  
  dat_frame_mc_4_train_x <- as.data.frame(dat_mc_4_train_x)
  
  #value of Y for Ex1_trainning set
  dat_mc_4_train_y <- dat_mc_4_train_x %*% beta_4 + sigma_4*rnorm(60)
  
  
  #Ex_5 test set
  dat_mc_4_test_x <- matrix(nrow = 480, ncol = 50)
  
  for(k in 1:480){
    x_4_test <- c()
    z_1 <- rnorm(1)
    z_2 <- rnorm(1)
    z_3 <- rnorm(1)
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=1, . . . , 10, ei ~ N(0, 0.05)
    for(i in 1:10){
      x_4_test[i] = z_1 + rnorm(1, 0, 0.05)
    }
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=11, . . . , 20, ei ~ N(0, 0.05)
    for(i in 11:20){
      x_4_test[i] = z_2 + rnorm(1, 0, 0.05)
    }
    #xi =Z1 +ei , Z1 ¡«N(0, 1), i=21, . . . , 30, ei ~ N(0, 0.05)
    for(i in 21:30){
      x_4_test[i] = z_3 + rnorm(1, 0, 0.05)
    }
    #xi~N(0,1), i = 31,...,50
    for(i in 31:50){
      x_4_test[i] = rnorm(1)  
    }
    
    dat_mc_4_test_x[k,] <- x_4_test 
  }
  
  dat_frame_mc_4_test_x <- as.data.frame(dat_mc_4_test_x)
  
  #value of Y for Ex5_test set
  dat_mc_4_test_y <- dat_mc_4_test_x %*% beta_4 + sigma_4*rnorm(480)
  
  ###LASSO regression
  model_lao <- train(
    dat_frame_mc_4_train_x, dat_mc_4_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  )
  
  outcome_lao <- coef(model_lao$finalModel, model_lao$bestTune$lambda)
  non_zero_lao[s] <- colSums(outcome_lao != 0)
  
  
  # Make predictions
  pred_lao <- model_lao %>% predict(dat_frame_mc_4_test_x)
  # Model prediction performance
  MSE_lao[s] <- RMSE(pred_lao, dat_mc_4_test_y)
  
  ###Ridge regression
  model_rid <- train(
    dat_frame_mc_4_train_x, dat_mc_4_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  # Make predictions
  pred_rid <- model_rid %>% predict(dat_frame_mc_4_test_x)
  # Model prediction performance
  MSE_rid[s] <- RMSE(pred_rid, dat_mc_4_test_y)
  
  ###Elastic Net Method
  model_en <- train(
    dat_frame_mc_4_train_x, dat_mc_4_train_y[,1], method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  )
  outcome_en <- coef(model_en$finalModel, model_en$bestTune$lambda)
  non_zero_en[s] <- colSums(outcome_en != 0)
  
  # Make predictions on the test data
  pred_en <- model_en %>% predict(dat_frame_mc_4_test_x)
  # Model performance metrics
  MSE_en[s] <- RMSE(pred_en, dat_mc_4_test_y)
}

MSE_box <- as.data.frame(rbind(MSE_lao, MSE_rid, MSE_en))
jpeg(file="msebox_4.jpg")
boxplot(t(MSE_box), main = "Example 4")
dev.off()

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



## simulation 5
set.seed(123)

mse_rid=c()
mse_en=c()
mse_lao=c()

for (k in 1:60){
  ###Example 5:Case when p>>n
  #a training set:200 observations\test set :100 observations
  #1000 variables:300 non-zero/700 zero
  
  n = 300
  p = 1000
  p.nonzero = 10
  x = matrix(rnorm(n*p),nrow=n,ncol=p)
  b.nonzero = rexp(p.nonzero)*(sign(rnorm(p.nonzero)))
  b.nonzero
  beta = c(b.nonzero,rep(0,p-p.nonzero))
  y = x%*%beta + rnorm(n)
  
  #Build training set and test set
  train = sample(1:n,2*n/3)
  x.train = x[train,]
  x.test  = x[-train,]
  y.train = y[train]
  y.test  = y[-train]
  
  #Run three regressions and fit the model
  lasso.fit = glmnet(x.train, y.train, family="gaussian", alpha=1)
  ridge.fit = glmnet(x.train, y.train, family="gaussian", alpha=0)
  enet.fit  = glmnet(x.train, y.train, family="gaussian", alpha=.5)
  
  fit = list()
  fit[[1]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/2,family="gaussian")
  fit[[2]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/2,family="gaussian")
  fit[[3]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/2,family="gaussian")
  names(fit) = 0:2
  
  yhat0  = predict(fit[["0"]], s=fit[["0"]]$lambda.1se, newx=x.test)
  yhat1  = predict(fit[["1"]], s=fit[["1"]]$lambda.1se, newx=x.test)
  yhat2  = predict(fit[["2"]], s=fit[["2"]]$lambda.1se, newx=x.test)
  
  #Calculate the MSE
  MSE_rid[k]  = mean((y.test - yhat0)^2)
  MSE_en[k]  = mean((y.test - yhat1)^2)
  MSE_lao[k]  = mean((y.test - yhat2)^2)
}

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



################################################################################
# 3. Application
## 3.1 House price prediction
library(tidyverse)
library(Amelia) #missmap
library(MASS)
library(stats)
library(mice) #mice
library(VIM) #aggr
library(fastDummies) #dummy_cols
library(glmnet) #glmnet, cv.glmnet -> ridge, lasso
library(gridExtra) #grid.arrange
library(caret) #Elastic Net: trainControl, train, findCorrelation
library(ggplot2)
library(reshape2)

usedata <- read.csv("train.csv")
predict <- read.csv("test.csv")
data <- usedata
set.seed(1000)
pick <- sample(1:nrow(data), 1314)
train <- data[pick,]
test <- data[-pick,]

#ID remove and separate dependent variable
trainSalePrice <- train$SalePrice
testSalePrice <- test$SalePrice

id<-data[1]
head(data[1])

#Checking which variables have missing values
na <- numeric(length(data))

for (i in 1:length(data)){
  na[i] <- ifelse(sum(is.na(data[i]))>0, sum(is.na(data[i])), 0)
  if (na[i]>0) { cat(names(data[i]), '=', na[i], '\n')}
}

#34 variables with missing values
sum(na>0)

#13965 missing values in training and test set
sum(is.na(data))

#Variables with about or more than 50% missing data in data set are PoolQC, MiscFeature, Alley, Fence and FireplaceQu
aggr(data, sortVars=TRUE)

z<-c(which(colnames(data)=="PoolQC" ), which(colnames(data)=="MiscFeature" ), 
     which(colnames(data)=="Alley" ), which(colnames(data)=="Fence" ), 
     which(colnames(data)=="FireplaceQu" ))
#Removing variables with mostly missing values
data<-data[,-z]


## Change to factors
for(i in 1:ncol(data)) {
  if(is.character(data[1,i])) {
    data[,i] <- factor(data[,i])
  }
}

missmap(data)

#-------------------------MULTIPLE IMPUTATION
#Predictive mean matching imputation for numeric, binary and ordered variables
set.seed(1000)
mice_mod <- mice(data[, c("LotFrontage",
                          "GarageType",
                          "GarageYrBlt",
                          "GarageFinish",
                          "GarageQual",
                          "GarageCond",
                          "BsmtExposure",
                          "BsmtFinType2",
                          "BsmtQual",
                          "BsmtCond",
                          "BsmtFinType1",
                          "MasVnrType",
                          "MasVnrArea",
                          "Electrical")], method='pmm')
mice_complete <- complete(mice_mod)

## give to factors
data$LotFrontage <- mice_complete$LotFrontage 
data$GarageType <- mice_complete$GarageType 
data$GarageYrBlt <- mice_complete$GarageYrBlt 
data$GarageFinish <- mice_complete$GarageFinish 
data$GarageQual <- mice_complete$GarageQual 
data$GarageCond <- mice_complete$GarageCond 
data$BsmtExposure <- mice_complete$BsmtExposure 
data$BsmtFinType2 <- mice_complete$BsmtFinType2 
data$BsmtQual <- mice_complete$BsmtQual 
data$BsmtCond <- mice_complete$BsmtCond 
data$BsmtFinType1 <- mice_complete$BsmtFinType1 
data$MasVnrType <- mice_complete$MasVnrType 
data$MasVnrArea <- mice_complete$MasVnrArea
data$Electrical <- mice_complete$Electrical

#Two sets - numeric and factor
data_factor <- select_if(data, is.factor)
data_numeric<- select_if(data, is.numeric)
data_numeric <- data_numeric[-1]

##Correlation matrix heat map
library(ggcorrplot)
model.matrix(~0+., data=data_numeric) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#F numeric variables
train_numeric <- data_numeric[pick,]
test_numeric <- data_numeric[-pick,]

#Creating DUMMY VARIABLES for factors
data_factor_dummy <- dummy_cols(data_factor, remove_first_dummy = TRUE)
data_factor_dummy <- select_if(data_factor_dummy, is.numeric)

data_num <- cbind(id, data_numeric, data_factor_dummy) #set without standarization numeric variables
#data_scale <- cbind(id, data_numeric_scale, data_factor_dummy) #set with standardization numeric variables

data_train_new <- cbind(train_numeric[-37], data_factor_dummy[pick,]) #set with standardization numeric variables
data_test_new <- cbind(test_numeric[-37], data_factor_dummy[-pick,])

#train
train_sc <- data_train_new#data_scale_train
train_set_sc<- cbind(train_sc, trainSalePrice) 

#test scale
test_sc <- data_test_new#data_scale_test
test_set_sc <- cbind(test_sc, testSalePrice)


##Ridge
train_y <- trainSalePrice#train_numeric_scale[,37]
train_x <- as.matrix(train_sc)


test_y <- testSalePrice#test_numeric_scale[,37]
test_x <- as.matrix(test_sc)
#lambda=0 is like linear regression so higher values are better. But also lambda must minimize the cross-validated sum of squared residuals so it can not be to high -> cross-validation 

#finding the optimal lambda, using cross validation glmnet

ridge.cv <- cv.glmnet(train_x, train_y, alpha = 0, type="mse")
ridge.cv$lambda.min
plot(ridge.cv, main='ridge')

ridge <- glmnet(train_x,train_y, alpha = 0)
coef(ridge.cv, s= "lambda.min")

ridge.opt <- glmnet(train_x,train_y, alpha = 0,lambda = ridge.cv$lambda.min)
length(coef(ridge.opt))

plot(ridge, xvar="lambda", main="ridge")

ridge.pred <- ridge.opt %>% predict(test_x)

ridge_mse <- sum((test_y - ridge.pred)^2) / nrow(test_set_sc)

ridge_mse
sqrt(ridge_mse)

plot(ridge.cv)

##lasso

# Setting alpha = 1 ---> implements lasso regression
lasso.cv <- cv.glmnet(train_x, train_y, alpha = 1, type = "mse")
lasso.cv
# Finding the optimal lambda 
lambda_best <- lasso.cv$lambda.min 
lambda_best

plot(lasso.cv, main='lasso')


lasso <- glmnet(train_x, train_y, alpha = 1)
plot(lasso, xvar = "lambda", main='lasso')

lasso.opt <- glmnet(train_x, train_y, alpha = 1, lambda = lambda_best)
plot(lasso.opt, xvar='lambda', main='lasso')

lasso.pred <- predict(lasso, s = lambda_best, newx = test_x)

lasso_mse <- sum((test_y - lasso.pred)^2) / nrow(test_set_sc)

lasso_mse
sqrt(lasso_mse)
sum(coef(lasso.opt)!=0)
##elastic net
# Set training control, how the repeated cross validation will take place
set.seed(1000)
train_control <- trainControl(method = "cv", number = 10,  
                              search = "grid", verboseIter = TRUE)

# Train the model
model_net2 <- train(trainSalePrice~., data = train_set_sc, method = "glmnet", 
                    tuneLength = 10, trControl = train_control, metric = "RMSE")
model_net2

#min(model_net2$results$RMSE)
model_net2$bestTune

model_net.pred <- model_net2 %>% predict(test_x) %>% as.vector()

coef(model_net2$finalModel, model_net2$bestTune$lambda)
sum(coef(model_net2$finalModel, model_net2$bestTune$lambda)!=0)

en_mse <- sum((test_y - model_net.pred)^2) / nrow(test_set_sc)

en_mse
sqrt(en_mse)

coef(model_net2, model_net2$bestTune)

##OLS
ols <- lm(trainSalePrice~., data = train_set_sc)
summary(ols)
#train_set_sc
ols.pred <- predict(ols, test_set_sc)
ols_mse <- sum((test_y - ols.pred)^2) / nrow(test_set_sc)
ols_mse
sqrt(ols_mse)
length(ols$coefficients)

res <- c(ols_mse,ridge_mse, lasso_mse, en_mse)
names(res) <- c("ols","ridge", "lasso", "elastic net")
res

sum(predict(model_net$finalModel, type = "coefficients", s = model_net$bestTune$lambda)!=0)
sum(coef(lasso.cv, s="lambda.min")!=0)
predict(model_net$finalModel, type = "coefficients", s = model_net$bestTune$lambda)



## 3.2 Classification
library(caret)
library(glmnet)
library(e1071) #svm & naive bayes
library(ISLR) #data

data(Default, package = "ISLR")
str(Default)
table(Default$default)
# checking missing values
sum(is.na(Default))
# separate train & test set
set.seed(42)
idx <- createDataPartition(Default$default, p = 0.75, list = FALSE)
trn <- Default[idx, ]
tst <- Default[-idx, ]
table(trn$default)
table(tst$default)

# logistic
mdl.log <- glm(default~., trn, family = 'binomial')
summary(mdl.log)
pr.log <- as.numeric(predict(mdl.log, tst, type = 'response')>0.5)
pr.log[which(pr.log==0)] <- c('No') 
pr.log[which(pr.log==1)] <- c('Yes') 
confusionMatrix(factor(pr.log),factor(tst$default))
# svm
mdl.svm <- svm(default~., data=trn)
mdl.svm
pr.svm <- predict(mdl.svm, tst)
confusionMatrix(factor(pr.svm),factor(tst$default))
# naive Bayes
mdl.nb <- naiveBayes(default~., trn)
pr.nb <- predict(mdl.nb, tst)
confusionMatrix(pr.nb,factor(tst$default))
# elastic net
mdl.elnet = train(
  default~., data = trn,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)
mdl.elnet$bestTune
pr.elnet <- predict(mdl.elnet, tst)
confusionMatrix(factor(pr.elnet),factor(tst$default))

