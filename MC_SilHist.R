





library(foreach)
library(parallel)


Clustering<-function(ind){
  
  library(gdata)
  library(nnet)
  library(base)
  library(mclust)
  library(cluster)
  library(WriteXLS)
  library(gdata)
  
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
    

    causal<-read.csv(paste0("~/Desktop/Analogies/Simulated_data/Forecasting_analogies/Exp_",ind,"/Generated_data/replicate_",rep,"/causal.csv"))
    Scale_causal<-scale(causal)
    timeSeries<-read.xls(paste0("~/Desktop/Analogies/Simulated_data/Forecasting_analogies/Exp_",ind,"/Generated_data/replicate_",rep,"/timeseries.xls"))
    
    for(l in 0:5){
      if(l==0){
        timeLength<-timeSeries[-c(18:23),]
      }else{
        timeLength<-timeSeries[-c(1:l,18:23),]   ##Delete historical and predicted points
      }
      num_TS<-ncol(timeLength)
      timeLength<-scale(timeLength)
      data<-cor(timeLength,method="pearson")
      d1<-1-as.matrix(data)
      dTS<-normal(d1,num_TS)
      
      d2 <- dist(Scale_causal,"euclidean")
      dCF<-normal(d2,num_TS)
      
      for(weight in seq(0,1,0.1)){
        
        distMatrix_1<-weight*dTS
        distMatrix_2<-(1-weight)*dCF
        overal_distMatrix<-distMatrix_1+distMatrix_2
        
        k_num =9
        sil = rep(0, k_num)
        agg_pos=rep(0,k_num)
        
        
        for (i in 3:k_num) {
          record=rep(0,30)
          for(times in 1:30){
            set.seed(times)
            med=sample(c(1:num_TS),i,replace=FALSE)
            hc<-pam(as.dist(overal_distMatrix),diss=TRUE,k=i,medoids=med)
            record[times]=hc$objective[2]
          }
          
          vector<-which(record==min(record))
          
          if(length(vector)==1)
          {
            pos<-vector
            
          }else{
            pos<-sample(vector,1,replace=FALSE)
          }
          
          set.seed(pos)                     ########Which initialization should be randomly determined. Otherwise, it causes the problem to K=2
          agg_pos[i]=pos
          hc<-pam(as.dist(overal_distMatrix),k=i,medoids=sample(c(1:num_TS),i,replace=FALSE))
          labs=hc$clustering
          aa = silhouette(labs,overal_distMatrix)
          sil[i] = mean(aa[, 3])
        }
        
        set.seed(agg_pos[which.is.max(sil)])
        hc<-pam(as.dist(overal_distMatrix),k=which.is.max(sil),medoids=sample(c(1:num_TS),which.is.max(sil),replace=FALSE))
        
        order<-c(1:num_TS)
        
        class<-cbind(order,hc$clustering)
        
        for(r in 1:which.is.max(sil)){
          count=0;
          for(i in 1:num_TS){
            if(class[i,2]==r){
              count=count+1;
            }
          }
          
          newData<-matrix(data=NA,nrow=nrow(timeSeries),ncol=count)
          index<-matrix(data=NA,nrow=count,ncol=1)
          k=1
          for(i in 1:num_TS){
            if(class[i,2]==r){
              index[k]<-class[i,1]
              k=k+1
              
            } }
          newData<-timeSeries[,index]
          output<-as.data.frame(newData)
          category<-as.data.frame(class)    ##dont' output sortSilhouette as this doesn't correspond to the original 30 TS. It is changing every replicate and not easy for calculating the Aggregated_MAPE
          
          WriteXLS("category",paste0("~/Desktop/Analogies/Simulated_data/Forecasting_analogies/Exp_",ind,"/MC_SilHist/SilClust/rep_",rep,"/length",l,"/weight_",weight*10,"/Category.xls",sep=""))
          
          WriteXLS("output",paste0("~/Desktop/Analogies/Simulated_data/Forecasting_analogies/Exp_",ind,"/MC_SilHist/SilClust/rep_",rep,"/length",l,"/weight_",weight*10,"/Group_",r,".xls",sep=""))
        }
      }
    }   
  }
}

no_cores <- detectCores() - 1

cl <- makeCluster(no_cores)

parLapply(cl,1,Clustering)

stopCluster(cl)
