


library(forecast)
library(forecTheta)
library(parallel)
library(tseries)
library(formatR)

#############################################################################################################################

scale_TS <- function(timeLength, length) {
  variance <- NULL
  std <- NULL
  for (col in 1:ncol(timeLength)) {
    var <- var(timeLength[, col]) * (10 - length) / (11 - length)
    variance <- cbind(variance, var)
    std <- cbind(std, sqrt(var))
  }
  mean <- colMeans(timeLength)
  
  TS <- NULL
  for (col in 1:ncol(timeLength)) {
    TS <- cbind(TS, (timeLength[, col] - mean[col]) / std[, col])
  }
  return(TS)
}

results <- function(ind, method,horizon) {
  h<-horizon+11
  timeSeries <- read.xls(paste0("~/Desktop/First_project_modification/Case/Income/Generated_data/sample_",ind,"/Synthetic.xls"))
  original <-read.csv(paste0("~/Desktop/First_project_modification/Case/Income/Generated_data/sample_",ind,"/insample_",h,".csv"))
  original <- original - 10
  
  timeLength <- timeSeries[-c(12:14),]
  sample <- scale_TS(timeLength, l = 0) + 10
  
  if (identical(method, "Theta")) {
    myfunction <- function(x) {
      fit <- stheta(ts(x, frequency = 1), h = horizon, s = NULL)       #Theta
      return(fit$mean)
    }
    
  } else if (identical(method, "ETS")) {
    myfunction <- function(x) {
      f <- ets(ts(x, frequency = 1),
               model = "ZZN",
               lambda = BoxCox.lambda(x))  #ETS
      fit <- forecast(f, h = horizon)
      return(fit$mean)
    }
  } else if (identical(method, "Damped")) {
    myfunction <- function(x) {
      f <-ets(x, model = "ZZZ",damped = TRUE,lambda = BoxCox.lambda(x))   ###Damped
      fit <- forecast(f, h = horizon)
      return(fit$mean)
    }
  } else if (identical(method, "RW")) {
    myfunction <- function(x) {
      fit <- naive(x, h = horizon)
      return(fit$mean)
    }
  }else if(identical(method,"Drift")) {
    myfunction <- function(x) {
      fit <- rwf(x, drift = TRUE, h = horizon)      #Drift
      return(fit$mean)
    }
  }
  # myfunction<-function(x){
  #
  #   # f<-ets(x,model="ZZZ",damped=TRUE,lambda=BoxCox.lambda(x))   ###Damped
  #
  #   # f<-ets(ts(x,frequency=1),model="ZZN",lambda=BoxCox.lambda(x))  #ETS
  #   # fit<-forecast(f,h=6)
  #
  #   # fit<-rwf(x,drift=TRUE,h=6)      #Drift
  #      fit<-stheta(ts(x,frequency=1),h=6,s=NULL)       #Theta
  #
  #    return(fit$mean)
  #  }
  
  ETS <- matrix(apply(sample, 2, myfunction), 208 * horizon, 1) - 10
  # MSE<-mean(abs(original[,1]-ETS)/(abs(original[,1])+abs(ETS))*200)
  
   MSE <- mean((original[, 1] - ETS) ^ 2)
  return(MSE)
  
  # f<-ets(livestock2,model="ZZN",lambda=0)
  
}



##########################################################################################################################
######         CALL FUNCTIONS FOR DIFFERENT METHODS
##########################################################################################################################

agg_md <- NULL
for (j in 1:5) {
  if (j == 1) {
    md = "Damped"
    
  } else if (j == 2) {
    md = "Drift"
    
  } else if (j == 3) {
    md = "ETS"
    
  } else if (j == 4) {
    md = "RW"
  } else{
    md = "Theta"
  }
  value <- results(ind = 1, method = md,horizon=3)
  agg_md<-rbind(agg_md,value)

}
 rownames(agg_md)<-c("Damped","Drift","ETS","RW","Theta")
write.csv(agg_md,paste0("~/Desktop/First_project_modification/Case/Income/pam/NEW_Addmodels_14_MSE.csv"))


#####################################Calculate MSKF###########################################

myfunction <- function(ind) {
  all <- NULL
  for (r in 1:30)
  {
    agg_l <- NULL
    for (l in seq(5, 0,-1)) {
      fcst <-read.xls(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind, "/MSKF/rep_", r,"/length",l, "/forecast.xls",sep = "") )
      fcst <- fcst - 10
      MSE <- (fcst[, 1] - fcst[, 2]) ^ 2
      
      # MSE<-abs(fcst[,1]-fcst[,2])/(abs(fcst[,1])+abs(fcst[,2]))*200
      
      mse <- mean(MSE)
      agg_l <- rbind(agg_l, mse)
    }
    all <- cbind(all, agg_l)
    
  }
  return (mean(apply(all, 1, mean)))
}


all <- NULL
for (ind in 13:18) {
  value <- myfunction(ind)
  all <- cbind(all, value)
  write.csv(all, "~/Desktop/summary_MSKF_13_17_MSE.csv")
  
}







####################################################################################################################################################################
f <-
  ets(timeLength, model = "ZZN", lambda = BoxCox.lambda(livestock2))  #automatic selection of exponential model with parameters
fit1 <- forecast(f, h = 6)


fit5 <-
  ets(
    livestock2,
    model = "ZZZ",
    damped = TRUE,
    lambda = BoxCox.lambda(livestock2)
  )
fit6 <-
  stheta(ts(livestock2, frequency = 1), h = 6, s = NULL)             #theta model

plot(livestock, type = "o", ylab = "Livestock")
#       flwd=1, plot.conf=FALSE)
lines(livestock, type = "o")

lines(fitted(fit1), col = 2)
lines(fit1$mean, col = 2)

# lines(fitted(fit2),col=3)
# lines(fit2$mean,col=3)
#
# lines(fitted(fit3),col=4)
# lines(fit3$mean,col=4)

lines(fitted(fit4), col = 5)
lines(fit4$mean, col = 5)

lines(fitted(fit5), col = 6)
lines(fit5$mean, col = 6)

lines(fitted(fit6), col = 7)
lines(fit6$mean, col = 7)

legend(
  "topleft",
  lty = 1,
  pch = 1,
  col = c(1, 2, 5, 6, 7),
  c(
    "Data",
    "ETS",
    "Additive Damped",
    "Multiplicative Damped",
    "Theta"
  )
)