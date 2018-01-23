library(zoo)#
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
library(devtools)
library(mclust)
library(boa)
library(matrixStats)
library(Matrix)
library(clue)
library(factoextra)
library(cluster)
library(NbClust)

########################################################Insample################################################################

##Silhouette width#####################################ClUSTERING###########################################################################
require(foreach)
library(parallel)


Clustering<-function(ind){
  library(gdata)
  library(nnet)
  library(base)
  library(mclust)
  library(cluster)
  library(WriteXLS)
  
  normal<-function(dist,num_TS){
    dist_norm<-as.matrix(dist)
    m<-matrix(data=NA,nrow=num_TS^2,ncol=1)
    dist<-matrix(data=NA,nrow=num_TS,ncol=num_TS)
    for(i in 1:num_TS^2){
      m[i]<-(dist_norm[i]-min(dist_norm))/(max(dist_norm)-min(dist_norm))
    }
    dist<-matrix(m,nrow=num_TS,ncol=num_TS,byrow=FALSE)
    return (dist)
  }
  
   for(rep in 1:30){
     # for(sap in 1:50){
     causal<-read.csv(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Generated_data/replicate_",rep,"/causal.csv"))
     Scale_causal<-scale(causal)
     timeSeries<-read.csv(paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/Generated_data/replicate_",rep,"/timeseries.csv"))
    for(length in 0:5){
      
      if(length==0){
        timeLength<-timeSeries[-c(18:23),]
      }else{
        timeLength<-timeSeries[-c(1:length,18:23),]   ##Delete historical and predicted points
      }
      num_TS<-ncol(timeSeries)
     timeLength<-scale(timeLength)
     data<-cor(timeLength,method="pearson")
     dissimilarity<-1-as.matrix(data)
     dTS<-normal(dissimilarity,num_TS)
        
      d2 <- dist(Scale_causal,"euclidean")
      dCF<-normal(d2,num_TS)
      for(weight in seq(0,1,0.1)){
        distMatrix_1<-weight*dTS
        distMatrix_2<-(1-weight)*dCF

        overal_distMatrix<-distMatrix_1+distMatrix_2
        # hc<- hclust(as.dist(overal_distMatrix),"average")
        k=num_TS-1
        sil = rep(0, k)
        for (i in 3:9) {
           ct = pam(as.dist(overal_distMatrix),i)

          aa = silhouette(ct$clustering,overal_distMatrix)
          sil[i] = mean(aa[, 3])
        }

       #
        # si = cutree(hc,num)
        hc<-pam(as.dist(overal_distMatrix),which.is.max(sil))
        si=hc$clustering
        
         order<-c(1:num_TS)
         class<-cbind(order,si)
        for(r in 1:which.is.max(sil)){
          count=0;
          for(i in 1:num_TS){
            if(class[i,2]==r){

              count=count+1;
            }
          }
          newData<-matrix(data=NA,nrow=nrow(timeSeries),ncol=count)
          index<-matrix(data=NA,nrow=count,ncol=1)
          k=1;
          for(i in 1:num_TS){
            if(class[i,2]==r){

              index[k]<-class[i,1]
              k=k+1;

            } }

          newData<-timeSeries[,index]
          output<-as.data.frame(newData)
          category<-as.data.frame(class)
           WriteXLS("category",paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/SilClust/rep_",rep,"/length",length,"/weight_",weight*10,"/Category.xls"))
           WriteXLS("output",paste0("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/SW_hist/Clustered_15/SilClust/rep_",rep,"/length",length,"/weight_",weight*10,"/Group_",r,".xls"))
          
       }
      }
    }
      
    }
   }
# }
 

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
parLapply(cl,1:3,Clustering)
stopCluster(cl)





controlGenerator<-function(causal,timeSeries,length,weight){
  
  agg_CF<-NULL
  for(i in 1:100){

conCF<-matrix(runif(30,min(causal),max(causal)),nrow=30,ncol=1)
agg_CF<-cbind(agg_CF,conCF)

  }
 controlCF<- matrix(apply(agg_CF,1,mean),nrow=30,ncol=1)


Scale_causal<-scale(controlCF)

new_TS<-t(timeSeries)



table_TS<-NULL
for(i in 1:23){
  
  agg_colTS<-NULL
  
  for(j in 1:100){
    
  colTS<-runif(30,min(new_TS[,i]),max(new_TS[,i]))
  agg_colTS<-cbind(agg_colTS,colTS)
  }
  column<-rowMeans(agg_colTS)
  table_TS<-cbind(table_TS,column)
}
  

controlTS<-t(table_TS)

  if(length==0){
    timeLength<-controlTS[-c(18:23),]
  }else{
    timeLength<-controlTS[-c(1:length,18:23),]   ##Delete historical and predicted points
  }
  num_TS<-ncol(controlTS)
  timeLength<-scale(timeLength)
  
  
  data<-cor(timeLength,method="pearson")
  dissimilarity<-1-as.matrix(data)
  dTS<-normal(dissimilarity,num_TS)
  
  d2 <- dist(Scale_causal,"euclidean")
  dCF<-normal(d2,num_TS)

    
    distMatrix_1<-weight*dTS
    distMatrix_2<-(1-weight)*dCF

    overal_distMatrix<-distMatrix_1+distMatrix_2
    hc<- hclust(as.dist(overal_distMatrix),"average")
    
    k =num_TS-1
    sil = rep(0, k)
    for (i in 2:k) {
      ct = cutree(hc, i)
      aa = silhouette(ct,overal_distMatrix)
      sil[i] = mean(aa[, 3])
    }
    
    return(sil)
    
    


}


lines(plot(controlGenerator(causal,timeSeries,length,weight)))




      
    
for(length in seq(5,0,-1)){
  agg_rep<-NULL
  
  for(rep in 1:30){
rep<-Clustering(rep=rep,length=length,start_weight=0,end_weight=10)  ##length,run,rep,weight
agg_rep<-cbind(agg_rep,rep)
write.csv(agg_rep,paste("~/Desktop/Model_selection/Synthetic/Historical/Clustering_results/Number_",length,".csv",sep=""))
  }
  
  }

###################################################################################################################################################
MSEcalculator<-function(){
  
  for(i in 1:3){
    if (i==1){method="CF"}
    if(i==2){method="TS"}
    if(i==3){method="MC"}
    
    
    table<-NULL
    for(rep in 1:2){
      length<-NULL
      for(l in seq(5,0,-1)){
        # MSE<-NULL
         # for(weight in 0:10){
        #forecast<-read.xls(paste("~/Desktop/First_project_modification/Experiments/",method,"/rep_",rep,"/length",l,"/forecast.xls",sep=""))
        
        forecast<-read.xls(paste("~/Desktop/Model_selection/Synthetic/MSKF/rep_",rep,"/length",l,".xls",sep=""),1)
        forecast<-forecast-10
         x<-(forecast[,1]-forecast[,2])^2
        
       # x<-abs(forecast[,1]-forecast[,2])/(abs(forecast[,1])+abs(forecast[,2]))*200
        y<-mean(x)
        # MSE<-rbind(MSE,y)
        length<-rbind(length,y)
        
         }
        
      table<-cbind(table,length)
      write.csv(table,paste("~/Desktop/Model_selection/Synthetic/summary_MSKF_MSE.csv",sep=""))
    }
    
    
    
    
    # MC<-apply(length,2,min)
    # combine<-rbind(length,MC)
    # replicate<-rowMeans(combine)
    
    
    
    }
    
    
}



#################################################################################################################################################


sMAPEpos<-function(rep,l){
  sMAPE<-NULL  
  for(w in 0:10){
    forecast<-read.xls(paste("~/Desktop/Extended/pam/distance/SW_39/history/rep_",rep,"/length",l,"/weight_",w,"/forecast.xls",sep=""),1)
    
    forecast<-forecast-10
    x<-(forecast[,1]-forecast[,2])^2
    # x<-abs(forecast[,1]-forecast[,2])/(abs(forecast[,1])+abs(forecast[,2]))*200
    
    y<-mean(x)
    sMAPE<-rbind(sMAPE,y)
  } 
  vector<-which(sMAPE==min(sMAPE))
  if(length(vector)==1){
    pos<-vector
  }else{
    pos<-sample(vector,1,replace=FALSE)
  }
  
  return(pos)
}


sMAPEcalculator<-function(ind){
  overall<-NULL
  overall_smape<-NULL
  for(rep in 1:30){
    cbind_mse<-NULL
    cbind_smape<-NULL
    for(l in seq(from=5,to=0,by=-1)){
      
      MSE<-NULL
      sMAPE<-NULL
      for(w in 0:10){
        forecast<-read.xls(paste("~/Desktop/Extended/pam/distance/SW_39/SilFcst/rep_",rep,"/length",l,"/weight_",w,"/forecast.xls",sep=""),1)
        forecast<-forecast-10
        x1<-(forecast[,1]-forecast[,2])^2
        x2<-abs(forecast[,1]-forecast[,2])/(abs(forecast[,1])+abs(forecast[,2]))*200
        
        MSE<-rbind(MSE,mean(x1))
        sMAPE<-rbind(sMAPE,mean(x2))
      } 
      MC_mse<-MSE[sMAPEpos(ind,rep,l)]
      MC_smape<-sMAPE[sMAPEpos(ind,rep,l)]
      combine_mse<-rbind(MSE,MC_mse)
      combine_smape<-rbind(sMAPE,MC_smape)
      cbind_mse<-cbind(cbind_mse,combine_mse)
      cbind_smape<-cbind(cbind_smape,combine_smape)
    }
    overall<-cbind(overall,cbind_mse)
    overall_smape<-cbind(overall_smape,cbind_smape)
    write.csv(overall,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/SW_39/Clustered_17/summary_sil_MSE.csv",sep=""))
    write.csv(overall_smape,paste("~/Desktop/Model_selection/Synthetic/Exp_",ind,"/pam/SW_39/Clustered_17/summary_sil_sMAPE.csv",sep=""))
    
    
  }    
}
for(ind in 8){
  sMAPEcalculator(ind)
}




