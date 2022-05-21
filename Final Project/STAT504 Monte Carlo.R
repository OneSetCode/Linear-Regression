install.packages("faux")

library(ggplot2)
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
dat_mc_1_train_x <- as.matrix(rnorm_multi(30, 12, 0, 1, cor_max, 
                              varnames = letters[1:12]))

#value of beta for Ex1
beta_1 <- as.matrix(c(0,4,2,3,0,0,0,1,5.5,7.5,0,0))

#value of Y for Ex1_trainning set
sigma_1 <- 4
dat_mc_1_train_y <- dat_mc_1_train_x %*% beta_1 + sigma_1*rnorm(30)

#Ex_1 Validation set
dat_mc_1_valid_x <- as.matrix(rnorm_multi(30, 12, 0, 1, cor_max, 
                                          varnames = letters[1:12]))


dat_mc_1_vaild_y <- dat_mc_1_valid_x %*% beta_1 + sigma_1*rnorm(30)


#Ex_1 test set
dat_mc_1_test_x <- as.matrix(rnorm_multi(300, 12, 0, 1, cor_max, 
                                          varnames = letters[1:12]))


dat_mc_1_test_y <- dat_mc_1_test_x %*% beta_1 + sigma_1*rnorm(300)

