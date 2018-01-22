



##############################################################
#            ME is working
##############################################################
require(foreach)
library(parallel)

sMAPEcalculator <- function(ind) {
  library(gdata)
  library(cluster)
  library(nnet)
  library(mclust)
  
  
  sMAPEpos <- function(ind,rep, l) {
      pos <- NULL
      sMAPE <- NULL
      
      for (w in 0:10){
        forecast <-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_17/SilFcst_23/rep_",rep,"/length", l,"/weight_", w,"/forecast.xls"))
        forecast <- forecast - 10
        x <- (forecast[, 1] - forecast[, 2]) ^ 2
        y <- mean(x)
        sMAPE <- rbind(sMAPE, y)
      }

      vector <- which(sMAPE == min(sMAPE))
      if (length(vector) == 1)
      {
        pos <- vector

      } else{
        pos <- sample(vector, 1, replace = FALSE)
      }
      return(pos)
    }
  
  
  all_k<-NULL
  
  for(rep in 1:30){
    
    # agg_l_ari<-NULL
    combine<-NULL
    for(l in 0:5){
      
      selectW<-sMAPEpos(ind,rep,l)-1
      
      forecast<-read.xls(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_17/SilFcst_23/rep_",rep,"/length",l,"/weight_",selectW,"/forecast.xls",sep=""))
      forecast<-forecast-10
      
      f1<-forecast[seq(1,nrow(forecast),6),]
      f2<-forecast[seq(2,nrow(forecast),6),]
      f3<-forecast[seq(3,nrow(forecast),6),]
      f4<-forecast[seq(4,nrow(forecast),6),]
      f5<-forecast[seq(5,nrow(forecast),6),]
      f6<-forecast[seq(6,nrow(forecast),6),]
      
      x1<-colMeans(matrix(f1[,1]-f1[,2],ncol=1))
      x2<-colMeans(matrix(f2[,1]-f2[,2],ncol=1))
      x3<-colMeans(matrix(f3[,1]-f3[,2],ncol=1))
      x4<-colMeans(matrix(f4[,1]-f4[,2],ncol=1))
      x5<-colMeans(matrix(f5[,1]-f5[,2],ncol=1))
      x6<-colMeans(matrix(f6[,1]-f6[,2],ncol=1))
      
      
      f1<-x1
      f2<-colMeans(rbind(x1,x2))
      f3<-colMeans(rbind(x1,x2,x3))
      f4<-colMeans(rbind(x1,x2,x3,x4))
      f5<-colMeans(rbind(x1,x2,x3,x4,x5))
      f6<-colMeans(rbind(x1,x2,x3,x4,x5,x6))
      x<-rbind(f1,f2,f3,f4,f5,f6)
      combine<-rbind(combine, x)
      
    }
    
    all_k<-cbind(all_k,combine)
    write.csv(all_k,paste0("~/Desktop/Model_selection/Synthetic/hypothesis/MC/hypothesis_",ind,"_ME.csv"),row.names = FALSE)
    
  }
}

no_cores <- 3
# detectCores() -1
cl <- makeCluster(no_cores)
parLapply(cl,1:5,sMAPEcalculator)
stopCluster(cl)






#################################################################################
#      MASE for exp_1-5
#################################################################################
require(foreach)
library(parallel)


sMAPEcalculator <- function(ind) {
  library(gdata)
  library(cluster)
  library(nnet)
  library(mclust)
  

  sMAPEpos <- function(ind,rep, l) {
    pos <- NULL
    sMAPE <- NULL
    for (w in 0:10) {
      forecast <-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_17/SilFcst_23/rep_",rep,"/length", l,"/weight_", w,"/forecast.xls"))
      forecast <- forecast - 10
      x <- (forecast[, 1] - forecast[, 2]) ^ 2
      y <- mean(x)
      sMAPE <- rbind(sMAPE, y)
    }

    vector <- which(sMAPE == min(sMAPE))
    if (length(vector) == 1)
    {
      pos <- vector

    } else{
      pos <- sample(vector, 1, replace = FALSE)
    }
    return(pos)
  }

  all_k<-NULL
  
  for(rep in 1:30){
    
    combine<-NULL
    
    for(l in 0:5){
      
      selectW<-sMAPEpos(ind,rep,l)-1
      Q<-read.csv(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/MASE/SFactor_18/length",l,"/rep_",rep,"/SF.csv"))
      # ct<-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/SilClust/rep_",rep,"/length",l,"/weight_",selectW,"/Category.xls"))
      # category<-ct[order(ct[,2],ct[,1]),]
      # Q_category<-matrix(rep(Q[category[,1],],each=1))
      Q_category<-matrix(unlist(Q),ncol=1)
      forecast<-read.xls(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_17/SilFcst_23/rep_",rep,"/length",l,"/weight_",selectW,"/forecast.xls",sep=""))
      forecast<-forecast-10
      f1<-abs(forecast[,1]-forecast[,2])
      
      f<-matrix(f1,ncol=1)
      x1<-matrix(f[seq(1,nrow(f),6),],ncol=1)
      x2<-matrix(f[seq(2,nrow(f),6),],ncol=1)
      x3<-matrix(f[seq(3,nrow(f),6),],ncol=1)
      x4<-matrix(f[seq(4,nrow(f),6),],ncol=1)
      x5<-matrix(f[seq(5,nrow(f),6),],ncol=1)
      x6<-matrix(f[seq(6,nrow(f),6),],ncol=1)
      
      horizon_1<-mean(x1/Q_category)
      horizon_2<-mean(x2/Q_category)
      horizon_3<-mean(x3/Q_category)
      horizon_4<-mean(x4/Q_category)
      horizon_5<-mean(x5/Q_category)
      horizon_6<-mean(x6/Q_category)
      
      f1<-horizon_1
      f2<-colMeans(rbind(horizon_1,horizon_2))
      f3<-colMeans(rbind(horizon_1,horizon_2,horizon_3))
      f4<-colMeans(rbind(horizon_1,horizon_2,horizon_3,horizon_4))
      f5<-colMeans(rbind(horizon_1,horizon_2,horizon_3,horizon_4,horizon_5))
      f6<-colMeans(rbind(horizon_1,horizon_2,horizon_3,horizon_4,horizon_5,horizon_6))
      x<-rbind(f1,f2,f3,f4,f5,f6)
      
      combine<-rbind(combine, x)
      
    }
    
    all_k<-cbind(all_k,combine)
    write.csv(all_k,paste0("~/Desktop/Model_selection/Synthetic/hypothesis/MC/hypothesis_",ind,"_MASE.csv"),row.names = FALSE)
    
  }
  
  
}

no_cores <- 3
# detectCores() -1
cl <- makeCluster(no_cores)
parLapply(cl,1:5,sMAPEcalculator)
stopCluster(cl)










