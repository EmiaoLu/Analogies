CFlibrary(base)
library(boot)
library(boa)
library(cluster)
library(devtools)
library(dplyr)
library(ifultools)
library(gdata)
library(ggplot2)
library(ggrepel)
library(lmtest)
library(Matrix)
library(matrixStats)
library(MASS)
library(mclust)
library(nnet)
library(pdc)
library(sandwich)
library(scrime)
library(stats)
library(splus2R)
library(tibble)
library(TSclust)
library(forecast)
library(wmtsa)
library(xlsx)
library(WriteXLS)
library(zoo)
library(nsga2R)
library(ggplot2)

createFolder<-function(){
    #for(f in 1:6){
    pos<-read.xls(paste("~/Desktop/Bagging/Synthetic/BagForecast/Generated_data/R.xls",sep=""),1)
    
     for(ind in 15){
      for(r in 1:30){
       # for(num in 3:9){
          for(l in 0:5){
             for(w in 0:10){

     #
        file.copy("~/Desktop/forecast.xls",paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/SW_39/Clustered_17/history/rep_",r,"/length",l,"/weight_",w,sep=""))
             # file.remove(paste("~/Desktop/Bagging/Synthetic/BagClust/Pareto_39/Results/replicate_",r,"/length",l,"/sample_",sap,"/Original_ind.xls",sep=""))
        
        #unlink(paste("~/Desktop/First_project_modification/Case/Forecasts_MC/rep_",rep, sep=""),recursive=TRUE)
        
        # dir.create(paste("~/Desktop/Boosting/First_project_modification/Generated_data/replicate_",r,sep=""),showWarnings = TRUE, recursive = FALSE, mode = "0777")
           # 
               #  dir.create(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/SW_39/Clustered_17/history/rep_",r,"/length",l,"/weight_",w,sep=""),showWarnings = TRUE, recursive = TRUE, mode = "0777")
          }
        }
    }
      }
    
    
    
    
    
}




#############################################################################################################################################################


TSgenerator<-function(funct){
    model<-diff(funct)
    ma<-NULL
    analogy<-NULL
    TS<-matrix(NA,nrow=23,ncol=10)
    rand<-NULL
    for(r in 1:10){
        rand<-qnorm(runif(23,0,1),model,0.95)
        analogy<-cbind(rand,analogy)
    }
    new<-funct[1]+analogy[1,]
    TS[1,]<-new
    
    for(i in 2:23){
        TS[i,]<-analogy[i,]+TS[i-1,]
    }
    
    return(TS)
}


timeGenerator<-function(ind,end_r){
    linear<-NULL
    log<-NULL
    piece<-NULL
    for(r in 1:end_r){
        t<-c(1:24)
        linear<-0.8*t+2.8
        log<-4*log(t)+2
        t1<-c(1:14)
        t2<-c(15:24)
        piece1<-rbind(0.7*t1+2.8)
        piece2<-rbind(-0.9*t2+25)
        piece<-as.numeric(cbind(piece1,piece2))
        
        linear_model<-TSgenerator(linear)
        log_model<-TSgenerator(log)
        piece_model<-TSgenerator(piece)
        Timeseries<-cbind(linear_model,log_model,piece_model)
        write.csv(Timeseries,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Generated_data/replicate_",r,"/timeseries.csv",sep=""),row.names=FALSE)
    }
    
}

dataGenerate<-function(ind,end_r){
    a<-NULL
    b<-NULL
    c<-NULL
    for(r in 1:end_r){
        a<-cbind(rnorm(10,mean=1,sd=0.35))
        b<-cbind(rnorm(10,mean=2,sd=0.35))
        c<-cbind(rnorm(10,mean=3,sd=0.35))
        z<-rbind(a,b,c)
        write.csv(z,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Generated_data/replicate_",r,"/causal.csv",sep=""),row.names=FALSE)
    }
}

index=15
timeGenerator(ind=index,end_r=30)

dataGenerate(ind=index,end_r=30)



scale_TS<-function(timeLength,length,t){
  
  variance<-NULL
  std<-NULL
  for(col in 1:ncol(timeLength)){
    var<-var(timeLength[,col])*(16-length)/(17-length)
    variance<-cbind(variance,var)
    std<-cbind(std,sqrt(var))
  }
  mean<- colMeans(timeLength)
  
  TS<-NULL
  for(col in 1:ncol(timeLength)){
    
    TS<-cbind(TS,(t[,col]-mean[col])/std[,col])
  }
  
  return(TS)
}









insampleGenerator<-function(ind){
  
  for(rep in 1:30){
    insample<-NULL
    timeSeries<-read.csv(paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Generated_data/replicate_",rep,"/timeseries.csv",sep=""))
    for(length in seq(0,5,1)){
      if(length==0){
        timeLength<-timeSeries[-c(18:23),]
        t<-timeSeries
      }else{
        timeLength<-timeSeries[-c(1:length,18:23),]   ##Delete historical and predicted points
        t<-timeSeries[-c(1:length),]
      }
      
      length_sample<-scale_TS(timeLength,length,t)+10
      combine<-NULL
      for(col in 1:ncol(length_sample)){
        l<-NULL
        
        # for(i in seq( (nrow(length_sample)-5),nrow(length_sample),1  )){
          # for(i in seq( (nrow(length_sample)-5),nrow(length_sample)-1,1  )){
          #   
          # for(i in seq( (nrow(length_sample)-5),nrow(length_sample)-2,1  )){
          #     
           # for(i in seq( (nrow(length_sample)-5),nrow(length_sample)-3,1  )){
          #       
               # for(i in seq( (nrow(length_sample)-5),nrow(length_sample)-4,1  )){
          #         
         for(i in seq( (nrow(length_sample)-5),nrow(length_sample)-5,1  )){
                    
          l<- rbind(l,length_sample[i,col])
        }
        
        combine<-rbind(combine,l)
        
      }
      
      
      insample<-cbind(insample,combine)
      write.csv(insample,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Generated_data/replicate_",rep,"/insample_18.csv",sep=""),row.names=FALSE)
    }
  }
  
}
for(i in 1:7){
insampleGenerator(ind=i)
}
