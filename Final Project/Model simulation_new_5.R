library(Matrix)
library(glmnet)

set.seed(123)

mse_rid=c()
mse_en=c()
mse_lao=c()

for (k in 1:60){
  n = 300
  p = 1000
  p.nonzero = n
  x = matrix(rnorm(n*p),nrow=n,ncol=p)
  b.nonzero = rexp(p.nonzero)*(sign(rnorm(p.nonzero)))
  b.nonzero
  beta = c(b.nonzero,rep(0,p-p.nonzero))
  y = x%*%beta + rnorm(n)
  
  train = sample(1:n,2*n/3)
  x.train = x[train,]
  x.test  = x[-train,]
  y.train = y[train]
  y.test  = y[-train]
  
  fit = list()
  fit[[1]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,family="gaussian")
  fit[[2]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=0.5,family="gaussian")
  fit[[3]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=1,family="gaussian")
  names(fit) = 0:2
  
  yhat0  = predict(fit[["0"]], s=fit[["0"]]$lambda.1se, newx=x.test)
  yhat1  = predict(fit[["1"]], s=fit[["1"]]$lambda.1se, newx=x.test)
  yhat2  = predict(fit[["2"]], s=fit[["2"]]$lambda.1se, newx=x.test)
  
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

MSE_box <- as.data.frame(rbind(MSE_lao, MSE_rid, MSE_en))
jpeg(file="msebox_5.jpg")
boxplot(t(MSE_box), main = "Example 5")
dev.off()

#output:
# > print(median(MSE_lao))
# [1] 495.0403
# > print(median(MSE_rid))           
# [1] 550.91
# > print(median(MSE_en))  
# [1] 488.3686
# # 
# > print(sd(MSE_lao_boot_med))
# [1] 8.55754
# > print(sd(MSE_rid_boot_med))
# [1] 15.71227
# > print(sd(MSE_en_boot_med))
# [1] 10.3283



