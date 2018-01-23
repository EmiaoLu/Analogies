


############################################################################################################################

require(foreach)
library(parallel)

  for(ind in 5){
   # for(h in 15:17){
  
  for (rep in 1:30) {
    # for(num in c(2,10:29)){
      for (l in 0:5) {
        for (w in 0:10) {
        #  
           file.copy("~/Desktop/forecast.xls",paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/history/rep_",rep,"/length",l,"/weight_",w))
        #dir.create,showWarnings = TRUE, recursive = FALSE, mode = "0777"
         # dir.create(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/SilFcst/rep_",rep,"/length",l,"/weight_",w),showWarnings = TRUE,recursive = TRUE,mode = "0777")
        }
      
    }
  }
}
   

no_cores <- detectCores() - 3
cl <- makeCluster(no_cores)
parLapply(cl,1:5,createFolder)
stopCluster(cl)

  ##################################################################################################################################
  # This pice of code makes use of results from silhouette values to determine the cluster number and weight was selected based on the historical forecasting
  # performance. This method suffers when time series is short and volatile as historical weight might shiftting.
  
  
  ############################
  
  #Based on min of the forecasting
  #results on future points
  #############################
  
  
  sMAPEpos <- function(ind, rep, l) {
    pos <- NULL
    sMAPE <- NULL
    y <- NULL
    
    for (w in 0:10) {
      forecast <-
        read.xls(paste( "~/Desktop/Model_selection/Synthetic/Exp_", ind,"/Hclust/SW_39/Clustered_17/SilFcst/rep_",rep,"/length", l,"/weight_", w, "/forecast.xls",sep = ""),1)
      forecast <- forecast - 10
      x <- (forecast[, 1] - forecast[, 2]) ^ 2
      
      y <- mean(x)
      
      sMAPE <- rbind(sMAPE, y)
    }
    print(sMAPE)
    
    vector <- which(sMAPE == min(sMAPE))
    if (length(vector) == 1)
    {
      pos <- vector
      
    } else{
      pos <- sample(vector, 1, replace = FALSE)
    }
    
    return(pos)
  }
  
  
  for(rep in 1:30){
    for(l in 0:5){
  W<-sMAPEpos(ind=1,rep,l)-1
  
  file.copy(paste0("~/Desktop/Model_selection/Synthetic/Exp_1/Hclust/SW_39/Clustered_17/SilFcst/rep_",rep,"/length",l,"/weight_",W,"/forecast.xls"),paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Hclust/SW_39/Clustered_17/selectW_future/rep_",rep,"/length",l))
    }
  }
  
  
  
  
  weightSelect<-function(ind){
  agg_r <- NULL
  for (rep in 1:30) {
    agg_l <- NULL
    for (l in seq(5, 0, -1)) {
      agg_l <- rbind(agg_l, sMAPEpos(ind, rep, l) - 1)
    }
    agg_r <- cbind(agg_r, agg_l)
    write.csv(agg_r,paste0("~/Desktop/Model_selection/Synthetic/weights/summary_",ind,"_weights.csv")
    )
  }
}

# for(i in 15){
weightSelect(ind = i)
}
######################################################################################################################################




sMAPEpos <- function(ind, num, r, l) {
  pos <- NULL
  sMAPE <- NULL
  
  for (w in 0:10) {
    forecast <-
      read.xls(
        paste0(
          "~/Desktop/First_project_modification/Case/Income/pam/random/history/cluster_",
          num,
          "/weight_",
          w,
          "/forecast.xls"
        ),
        1
      )
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


##########################################################################################
#########            Num_calculation
##########################################################################################


require(foreach)
library(parallel)

sMAPEcalculator <- function(horizon) {
  library(gdata)
  sMAPEpos <- function(ind, num) {
    pos <- NULL
    sMAPE <- NULL
    y <- NULL
    
    for (w in 0:10) {
      no <- NULL
      for (horizon in 17) {
        forecast <-read.xls( paste0( "~/Desktop/First_project_modification/Case/Income/Noise_data/history_",ind,"/cluster_",num,"/weight_",w,"/forecast.xls"),1)
        forecast <- forecast - 10
        x <- (forecast[, 1] - forecast[, 2]) ^ 2
        no <- rbind(no, x)
      }
      y <- mean(no)
      
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
  
  ind=1
  MSE <- NULL
  SMAPE <- NULL
    for (num in 2:40) {
      all_mse <- NULL
      all_smape <- NULL
      for(w in 0:10){
      forecast <-read.xls(paste0("~/Desktop/First_project_modification/Case/Income/Noise_data/Forecasts_",ind,"/Fcst_",horizon,"/cluster_", num, "/weight_",w,"/forecast.xls"),1)
      forecast <- forecast - 10
      mse <- (forecast[, 1] - forecast[, 2]) ^ 2
      smape <-abs(forecast[, 1] - forecast[, 2]) / (abs(forecast[, 1]) + abs(forecast[, 2])) *200
      all_mse <- rbind(all_mse, mean(mse))
      all_smape <-rbind(all_smape, mean(smape))
      }
      MC_mse<-all_mse[sMAPEpos(ind,num)]
      MC_smape<-all_smape[sMAPEpos(ind,num)]
      MSE <- cbind(MSE, rbind(all_mse,MC_mse))
      SMAPE <- cbind(SMAPE, rbind(all_smape,MC_smape))
      write.csv(MSE, paste("~/Desktop/First_project_modification/Case/Income/Noise_data/Results_",ind,"/history/summary_",horizon,"_MSE1.csv", sep = ""))
      write.csv(SMAPE, paste("~/Desktop/First_project_modification/Case/Income/Noise_data/Results_",ind,"/history/summary_",horizon,"_sMAPE1.csv", sep = ""))

    }
}


no_cores <- detectCores() -1
cl <- makeCluster(no_cores)
parLapply(cl,c(13),sMAPEcalculator)
stopCluster(cl)



################################################################



for (ind in 1) {
  table <- NULL
  all <- NULL
  # agg_ARI<-NULL
  for (num in 2:40) {
    mse <-read.csv( paste( "~/Desktop/First_project_modification/Case/Income/pam/12_14/Results/Clustered_17/summary_",num, "_MSE.csv",sep = "") )
    smape <- read.csv(paste0( "~/Desktop/First_project_modification/Case/Income/pam/12_14/Results/Clustered_17/summary_", num, "_sMAPE.csv"))
    # ari<-read.csv(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Historical/Clustered_17/summary_",num,"_ARI.csv",sep=""))
    
    MSE <- rowMeans(mse[, -1])
    SMAPE <- rowMeans(smape[, -1])
    # ARI<-rowMeans(ari[,-1])
    table <- cbind(table, MSE)
    all <- cbind(all, SMAPE)
    # agg_ARI<-cbind(agg_ARI,ARI)
  }
  
  write.csv( table,paste0( "~/Desktop/First_project_modification/Case/Income/pam/12_14/Historical/Clustered_17/summary_3_9_MSE.csv"))
  write.csv( all, paste("~/Desktop/First_project_modification/Case/Income/pam/12_14/Historical/Clustered_17/summary_3_9_sMAPE.csv") )
  # write.csv(agg_ARI,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Historical/Clustered_17/summary_3_9_ARI.csv",sep=""))
}

###############################################################################################################
# AcrossK history
##############################################################################################################
sMAPEpos <- function(r, l, start_number, end_number) {
  agg_num <- NULL
  for (num in start_number:end_number) {
    sMAPE <- NULL
    for (w in 0:10) {
      forecast <-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_1/pam/Historical/Clustered_17/cluster_",
            num,"/17/rep_",r,"/length", l,"/weight_", w, "/forecast.xls"),1 )
      forecast <- forecast - 10
      x <- (forecast[, 1] - forecast[, 2]) ^ 2
      y <- mean(x)
      sMAPE <- rbind(sMAPE, y)
    }
    agg_num <- cbind(agg_num, sMAPE)
    
    vector <- which(agg_num == min(agg_num), arr.ind = TRUE)
    if (length(vector) == 2)
    {
      pos <- vector
      
    } else{
      pos <- sample(vector, 1, replace = FALSE)
    }
  }
  return(pos)
}




for (r in 1:30) {
  for (l in 0:5) {
    parameter <- sMAPEpos(r, l, start_number = 2, end_numbe = 29)
    Iter <- length(parameter) / 2
    
    K <- parameter[2] - 3
    W <- parameter[1] - 1
    forecast <- read.xls( paste0( "~/Desktop/Model_selection/Synthetic/Exp_", ind,"/pam/Historical/Clustered_17/cluster_",K, "/length", l,"/weight_",W, "/forecast.xls") )
    x <- (forecast[, 1] - forecast[, 2]) ^ 2
    y <- mean(x)
    print(y)
  }
}


###########################################################################################################################




for (ind in 15:16) {
  agg <- NULL
  for (num in 3:9) { dat <-read.csv(paste0( "~/Desktop/Model_selection/Synthetic/Exp_4/pam/Historical/Clustered_17/", ind,"/summary_", num, "_MSE.csv"))
    x <- rowMeans(dat[, -1])
    agg <- cbind(agg, x)
    write.csv( agg,paste0( "~/Desktop/Model_selection/Synthetic/Exp_4/pam/Historical/Clustered_17/",ind,"/summary_3_9_MSE.csv")
    )
    
  }
}

##############################################################
#            Silhouette_calculation: SW_hist
##############################################################
require(foreach)
library(parallel)


sMAPEcalculator <- function(ind) {
  library(gdata)
  library(cluster)
  library(nnet)
  library(mclust)
  
  # ClusterPurity <- function(clusters, classes) {
  #   sum(apply(table(classes, clusters), 2, max)) / length(clusters)
  # }
  
  sMAPEpos <- function(ind,rep, l) {
    pos <- NULL
    sMAPE <- NULL
    for (w in 0:10) {
      forecast <-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/history/rep_",rep,"/length", l,"/weight_", w,"/forecast.xls"))
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
  
  
    # all3<-NULL
    all_k<-NULL
    
  for(rep in 1:30){
   
    # agg_l_ari<-NULL
    agg_l_k<-NULL
  for(l in 0:5){
   
    # agg_purity<-NULL
    # agg_k<-NULL
    agg1_ME<-NULL
    for(f in 18:23){  
      agg_ME<-NULL
      for(w in 0:10){
    # clust<-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_17/SilClust/rep_",rep,"/length",l,"/weight_",w,"/Category.xls"))
    forecast<-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/SilFcst_",f,"/rep_",rep,"/length",l,"/weight_",w,"/forecast.xls"))
    # oriIndex<-read.csv("~/Desktop/Ideal.csv")
    # purity<-ClusterPurity(clust[,2],oriIndex[,2])
    # K<-max(clust[,2])
    fcst<-forecast-10
    ME<-fcst[,1]-fcst[,2]
   # agg_purity<-rbind(agg_purity,purity)
   # agg_k<-rbind(agg_k, K)
    agg_ME<-rbind(agg_ME,mean(ME))
      }    
      
      MC_ME<-agg_ME[sMAPEpos(ind,rep,l)]
      
      agg1_ME<-rbind(agg1_ME,MC_ME)

    }
    # MC_purity<-agg_purity[sMAPEpos(ind,rep,l)]
  #   MC_k<-agg_k[sMAPEpos(ind,rep,l)]
  # agg_l_ari<-rbind(agg_l_ari,MC_purity)
  agg_l_k<-rbind(agg_l_k,agg1_ME)
      }

    # all3<-cbind(all3,agg_l_ari)
    all_k<-cbind(all_k,agg_l_k)
    # write.csv(all3,paste0("~/Desktop/Model_selection/Synthetic/hypothesis/MC_SilHist_15/hypothesis_",ind,"_purity.csv"),row.names = FALSE)
    write.csv(all_k,paste0("~/Desktop/Model_selection/Synthetic/hypothesis/MC_SilHist_15/hypothesis_",ind,"_ME.csv"),row.names = FALSE)
  }
    
  }

no_cores <- 1
# detectCores() -1
cl <- makeCluster(no_cores)
parLapply(cl,5,sMAPEcalculator)
stopCluster(cl)


##############################################################
#            Silhouette_calculation: SW_MS
##############################################################

start.time<-Sys.time()

sMAPEcalculator <- function(ind,rep,l) {
    forecast <-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_MS/Forecasts/rep_",rep,"/length",l,"/forecast.xls"),1)
    forecast <- forecast - 10
    mse <- (forecast[, 1] - forecast[, 2]) ^ 2
    smape <- abs(forecast[, 1] - forecast[, 2]) /(abs(forecast[, 1]) + abs(forecast[,2])) * 200
    agg_mse <- mean(mse)
    agg_smape <-mean(smape)
    errors<-rbind(agg_mse,agg_smape)
    return(errors)
  # write.csv(MC_mse,paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Results/future/summary_MSE.csv"))
  # write.csv(MC_smape,paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Results/future/summary_sMAPE.csv"))
}


for (i in 1:5) {
    all_mse<-NULL
    all_smape<-NULL
    for (j in 1:30) {
    agg_mse<-NULL
    agg_smape<-NULL
    for(k in seq(5,0,-1)){
    error<-sMAPEcalculator(ind=i,rep=j,l=k)
    agg_mse<-cbind(agg_mse,error[1])
    agg_smape<-cbind(agg_smape,error[2])
  }
    all_mse<-cbind(all_mse,agg_mse)
    all_smape<-cbind(all_smape,agg_smape)
    write.csv(all_mse,paste0("~/Desktop/Model_selection/Synthetic/Exp_",i,"/SW_MS/summary_MSE.csv"))
    write.csv(all_smape,paste0("~/Desktop/Model_selection/Synthetic/Exp_",i,"/SW_MS/summary_sMAPE.csv"))  
   
    }
   
   
   
}

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken


##########################################################################


all<-NULL
for(ind in 1:5){
data<-read.csv(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Results/history/summary_sMAPE.csv"))
dat<-data[,-1]
 all<-cbind(all,rowMeans(dat))

}




#################################################################################################################
#   Peformance Split-up
# ################################################OVERALL PERFORMANCE#############################################

results<-function(ind, method){
  agg_quality<-NULL
  agg_mse<-NULL
  agg_smape<-NULL
  for(rep in 1:30){
    agg_l<-NULL
    all_mse<-NULL
    all_smape<-NULL
    for(length in seq(5,0,-1)){
      
      fcst<-read.xls(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/",method,"/FcstNond/length",length,"/rep_",rep,"/forecast.xls",sep=""),1)
      forecast<-fcst-10
      mse<-(forecast[,1]-forecast[,2])^2
      smape<-abs(forecast[,1]-forecast[,2])/(abs(forecast[,1])+abs(forecast[,2]))*200
      clust<-read.xls(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/",method,"/ClustNond/length",length,"/rep_",rep,"/category.xls",sep=""),1)
      oriIndex<-read.csv("~/Desktop/Ideal.csv")
      quality<-adjustedRandIndex(oriIndex[,2],clust[,2])
      
      all_mse<-rbind(all_mse,mean(mse))
      all_smape<-rbind(all_smape,mean(smape))
      agg_l<-rbind(agg_l,quality)
    }
    agg_quality<-cbind(agg_quality,agg_l)
    agg_mse<-cbind(agg_mse,all_mse)
    agg_smape<-cbind(agg_smape,all_smape)
    # write.csv(agg_mse,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/summary_",method,"_MSE.csv",sep=""))
    
    # write.csv(agg_smape,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/summary_",method,"_sMAPE.csv",sep=""))
    
    write.csv(agg_quality,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/summary_",method,"_ARI.csv",sep=""))
    
  }
}





##############################################################################################
#####                              split up performance
############################################################################################



error="sMAPE"
aggre<-NULL

for(ind in 1:5){

    all<-NULL
    for(l in 0:5){
      
    agg_l<-NULL
    for(rep in 1:30){
      clust<-read.xls(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/ParetoHist/Clustered_17/15/ClustNond/length",l,"/rep_",rep,"/category.xls"),1)
      oriIndex<-read.csv("~/Desktop/Ideal.csv")
      quality<-adjustedRandIndex(oriIndex[,2],clust[,2])
      
      
     #  fcst<-fcst-10
     #  if(error=="MSE"){
     #  MSE <- (fcst[1:180, 1] - fcst[1:180,2]) ^ 2
     #  }else{
     # MSE<-abs(fcst[1:180,1]-fcst[1:180,2])/(abs(fcst[1:180,1])+abs(fcst[1:180,2]))*200
     #  }
      
  agg_l<-cbind(agg_l,quality)
  }
  all<-cbind(all,agg_l)
    }
    dat<-rowMeans(all)
    # h1<-mean(dat[seq(1,length(dat),6)])  #horizon 1
    # h2<-mean(dat[seq(2,length(dat),6)])  #horizon 2
    # h3<-mean(dat[seq(3,length(dat),6)])  #horizon 3
    # h4<-mean(dat[seq(4,length(dat),6)])  #horizon 4
    # h5<-mean(dat[seq(5,length(dat),6)])  #horizon 5
    # h6<-mean(dat[seq(6,length(dat),6)])  #horizon 6
    # f1<-h1
    # f2<-mean(cbind(h1,h2))
    # f3<-mean(cbind(h1,h2,h3))
    # f4<-mean(cbind(h1,h2,h3,h4))
    # f5<-mean(cbind(h1,h2,h3,h4,h5))
    # f6<-mean(cbind(h1,h2,h3,h4,h5,h6))
    # table<-round(cbind(f1,f2,f3,f4,f5,f6),2)
    # 
   # write.csv(table,paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/hypothesis_",method,"/hypothesis_",ind,"_",error,".csv"),row.names=FALSE)
    aggre<-rbind(aggre,dat)

  write.csv(aggre,paste0("~/Desktop/Summary_15_ARI.csv"))
  
    # row.names(aggre)<-c("MC_39","MC_maxmax","MC_maxmin","MC_maxsum")
}



##############################################################################################
#####                              split up performance for Additional Models 
############################################################################################

error="ARI"
method="ParetoHist_16"


all<-NULL
for(ind in 1:5){
data<-read.csv(paste0("~/Desktop/Model_selection/Synthetic/hypothesis/",method,"/hypothesis_",ind,"_",error,".csv"))
dat<-rowMeans(data)
h1<-mean(dat[seq(1,length(dat),6)])  #horizon 1
h2<-mean(dat[seq(2,length(dat),6)])  #horizon 2
h3<-mean(dat[seq(3,length(dat),6)])  #horizon 3
h4<-mean(dat[seq(4,length(dat),6)])  #horizon 4
h5<-mean(dat[seq(5,length(dat),6)])  #horizon 5
h6<-mean(dat[seq(6,length(dat),6)])  #horizon 6
f1<-h1
f2<-mean(cbind(h1,h2))
f3<-mean(cbind(h1,h2,h3))
f4<-mean(cbind(h1,h2,h3,h4))
f5<-mean(cbind(h1,h2,h3,h4,h5))
f6<-mean(cbind(h1,h2,h3,h4,h5,h6))
table<-round(cbind(f6),2)
all<-rbind(all,table)
}




#################################################
#b    Split Up for Bagging Results
################################################


error="MSE"

  aggre<-NULL
  

    method="SW_39"
    all<-NULL
    for(l in 0:5){
      
      agg_l<-NULL
    for(rep in 1:30){

  order<-c(1:30)
  order_rep<-rep(order,each=6)*10
  vector<-rep(c(1,2,3,4,5,6),30)
  order_ind<-order_rep+vector
  origin<-read.csv(paste("~/Desktop/Model_selection/Synthetic/Exp_1/Generated_data/replicate_",rep,"/insample.csv",sep=""))   ##ordered 
  forecast<-read.xls(paste("~/Desktop/Bagging/Synthetic/BagFcst/Hclust/",method,"/Forecasts/replicate_",rep,"/length",l,"/Aggregated_forecast.xls",sep=""),1)
  index<-read.xls(paste("~/Desktop/Bagging/Synthetic/BagFcst/Hclust/",method,"/Forecasts/replicate_",rep,"/length",l,"/Aggregated_ind.xls",sep=""),1)
  matrix<-matrix(data=NA,nrow=nrow(forecast),ncol=ncol(forecast))
  
  forecast<-as.matrix(forecast)
  index<-as.matrix(index)
  for(i in 1:ncol(forecast)){
    for(j in 1:nrow(forecast)){
      for(k in 1:nrow(forecast)){
        if(index[j,i]==order_ind[k]){
          matrix[k,i]=forecast[j,i]
          
        }}
    }}
  
  ordered_forecast<-as.matrix(rowMeans(matrix,na.rm=TRUE))-10   ##PROBLEM
  original<-as.matrix(origin)-10

  if(error=="MSE"){
    MSE<-(original[,l+1]-ordered_forecast)^2
  }else if(error=="sMAPE"){
    MSE<-abs(original[,l+1]-ordered_forecast)/(abs(original[,l+1])+abs(ordered_forecast))*200
  }
      
      
      agg_l<-cbind(agg_l,MSE)
    }
    

    all<-cbind(all,agg_l)
    
  }
  dat<-rowMeans(all)
  h1<-mean(dat[seq(1,length(dat),6)])  #horizon 1
  h2<-mean(dat[seq(2,length(dat),6)])  #horizon 2
  h3<-mean(dat[seq(3,length(dat),6)])  #horizon 3
  h4<-mean(dat[seq(4,length(dat),6)])  #horizon 4
  h5<-mean(dat[seq(5,length(dat),6)])  #horizon 5
  h6<-mean(dat[seq(6,length(dat),6)])  #horizon 6
  f1<-h1
  f2<-mean(cbind(h1,h2))
  f3<-mean(cbind(h1,h2,h3))
  f4<-mean(cbind(h1,h2,h3,h4))
  f5<-mean(cbind(h1,h2,h3,h4,h5))
  f6<-mean(cbind(h1,h2,h3,h4,h5,h6))
  table<-round(cbind(f1,f2,f3,f4,f5,f6),2)
  # write.csv(table,paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/Angles/hypothesis_",method,"/hypothesis_",ind,"_",error,".csv"),row.names=FALSE)
  aggre<-rbind(aggre,table)
  

write.csv(aggre,paste0("~/Desktop/Bagging/Synthetic/Split_up/SW_39_split_up_",error,".csv"))

 