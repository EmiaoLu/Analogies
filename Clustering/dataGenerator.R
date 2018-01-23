library(zoo)
library(forecast)
library(wmtsa)
library(splus2R)
library(ifultools)
library(MASS)
library(pdc)
library(cluster)
library(dplyr)
library(boot)
library(WriteXLS)
library(base)
library(TSclust)
library(stats)
library(nnet)
library(gdata)
library(XLConnect)
library(devtools)
library(mclust)
library(boa)
library(matrixStats)
library(scrime)
library(Matrix)
library(xlsx)

##############################################################################################



for(no in 1:5){
  
  
  y<-read.csv(paste("~/Desktop/First_project_modification/Case/OH_SD/Final_SD_TS.csv"))
  all_row<-nrow(y)
  all_col<-ncol(y)
  
  index<-c(1:ncol(y))
  causal_index<-sample(index,size=ncol(y),replace=FALSE)
  pos<-as.data.frame(causal_index)
  TS<-y[,causal_index]
  
  WriteXLS(TS,paste("~/Desktop/First_project_modification/Case/OH_SD/Generated_data/sample_",no,"/Synthetic.xls",sep=""))
  
  WriteXLS(pos,paste("~/Desktop/First_project_modification/Case/OH_SD/Generated_data/sample_",no,"/pos.xls",sep=""))
  
  x<-read.csv(paste("~/Desktop/First_project_modification/Case/OH_SD/Final_SD_CF.csv"))
  States<-x[causal_index,1]
  CF<-as.data.frame(States)
  WriteXLS(CF,paste("~/Desktop/First_project_modification/Case/OH_SD/Generated_data/sample_",no,"/causal.xls",sep=""))
    
  
causal<-read.xls(paste("~/Desktop/First_project_modification/Case/OH_SD/Generated_data/sample_",no,"/causal.xls",sep=""))

a<-as.vector(causal[,1])
causalDist<-function(a){
  
  x<-a
  y<-x
  dist<-matrix(NA,nrow=length(x),ncol=length(y))
  for(row in 1:length(x))
    for(col in 1:length(y))
      if(x[row]==y[col]){
        dist[row,col]<-0
      }else{
        dist[row,col]<-1
      }
  
  return(dist)
}


dist<-causalDist(a)
dist<-as.data.frame(dist)
WriteXLS(dist,paste("~/Desktop/First_project_modification/Case/OH_SD/Generated_data/sample_",no,"/dist.xls",sep=""))

}





scale_TS<-function(timeLength,length,t){
  
  variance<-NULL
  std<-NULL
  for(col in 1:ncol(timeLength)){
    var<-var(timeLength[,col])*(10-length)/(11-length)
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


insampleGenerator<-function(){
  
  for(rep in 1:25){
    insample<-NULL
    timeSeries<-read.xls(paste0("~/Desktop/First_project_modification/Case/Income/Generated_data/sample_",rep,"/Synthetic.xls"))
    for(length in 0){
      if(length==0){
        timeLength<-timeSeries[-c(12:14),]
        t<-timeSeries
      }else{
        timeLength<-timeSeries[-c(1:length,12:14),]   ##Delete historical and predicted points
        t<-timeSeries[-c(1:length),]
      }
      
      length_sample<-scale_TS(timeLength,length,t)+10
      combine<-NULL
      for(col in 1:ncol(length_sample)){
        l<-NULL
        
        # for(i in seq( (nrow(length_sample)-2),nrow(length_sample)-2,1  )){  ###carefully check this 12
          
          # for(i in seq( (nrow(length_sample)-2),nrow(length_sample)-1,1  )){  ###carefully check this   13
          for(i in seq( (nrow(length_sample)-2),nrow(length_sample),1  )){  ###carefully check this   14
              
          
          l<- rbind(l,length_sample[i,col])
        }
        
        combine<-rbind(combine,l)
        
      }
      
      
      insample<-cbind(insample,combine)
      write.csv(insample,paste("~/Desktop/First_project_modification/Case/Income/Generated_data/sample_",rep,"/insample_14.csv",sep=""),row.names=FALSE)
    }
  }
  
}
insampleGenerator()


