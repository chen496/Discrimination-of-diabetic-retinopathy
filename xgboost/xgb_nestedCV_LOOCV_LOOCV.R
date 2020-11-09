
### clear object
#rm(list = ls())

args=commandArgs(trailingOnly=TRUE)
##setwd("/root/Downloads/Projects_clinic/prepare_results_for_papers/paper2_DR/nested")
### refer to Gelman et al (2008), "A weakly informative default prior distribution for logistic & other regression models",

library(xgboost)

### loading data
##HC_DWT_path<-"/root/Downloads/Projects_clinic/Pprojects_clinic_diabetic retinopathy patients/Rename_sample/HC/HC_DWT/HC_"
##DR_DWT_path<-"/root/Downloads/Projects_clinic/Pprojects_clinic_diabetic retinopathy patients/Rename_sample/DR/DR_DWT/DR_"

HC_DWT_path<-"../../HC/HC_DWT/HC_"
DR_DWT_path<-"../../DR/DR_DWT/DR_"

Vname<-"bior1.1"
vname<-Vname

HC_Data<-list()
DR_Data<-list()

HC_DWT_engy_file_D<-paste0(HC_DWT_path,vname,"_D.csv")
HC_DWT_engy_D<-read.csv(HC_DWT_engy_file_D,header = FALSE)
dim(HC_DWT_engy_D)
HC_DWT_engy_file_R<-paste0(HC_DWT_path,vname,"_R.csv")
HC_DWT_engy_R<-read.csv(HC_DWT_engy_file_R,header = FALSE)
dim(HC_DWT_engy_R)
HC_DWT_engy_file_S<-paste0(HC_DWT_path,vname,"_S.csv")
HC_DWT_engy_S<-read.csv(HC_DWT_engy_file_S,header = FALSE)
dim(HC_DWT_engy_S)

HC_Data[[1]]<-HC_DWT_engy_D
HC_Data[[2]]<-HC_DWT_engy_R
HC_Data[[3]]<-HC_DWT_engy_S

DR_DWT_engy_file_D<-paste0(DR_DWT_path,vname,"_D.csv")
DR_DWT_engy_D<-read.csv(DR_DWT_engy_file_D,header = FALSE)
dim(DR_DWT_engy_D)
DR_DWT_engy_file_R<-paste0(DR_DWT_path,vname,"_R.csv")
DR_DWT_engy_R<-read.csv(DR_DWT_engy_file_R,header = FALSE)
dim(DR_DWT_engy_R)
DR_DWT_engy_file_S<-paste0(DR_DWT_path,vname,"_S.csv")
DR_DWT_engy_S<-read.csv(DR_DWT_engy_file_S,header = FALSE)
dim(DR_DWT_engy_S)

DR_Data[[1]]<-DR_DWT_engy_D
DR_Data[[2]]<-DR_DWT_engy_R
DR_Data[[3]]<-DR_DWT_engy_S

data_name<-args[1]
data_name<-strsplit(data_name,"\\.")[[1]]
## data_name<-"D"##c("D","R","S")
tt<-length(data_name)
if(tt==3){
  HC_DWT_engy<-cbind(HC_DWT_engy_D,HC_DWT_engy_R,HC_DWT_engy_S)
  DR_DWT_engy<-cbind(DR_DWT_engy_D,DR_DWT_engy_R,DR_DWT_engy_S)
  feature<-c("Da",paste0("Dh",8:1),paste0("Dv",8:1),paste0("Dd",8:1),
             "Ra",paste0("Rh",8:1),paste0("Rv",8:1),paste0("Rd",8:1),
             "Sa",paste0("Sh",8:1),paste0("Sv",8:1),paste0("Sd",8:1))
  
}
if(tt==1){
  if(data_name=="D"){
    HC_DWT_engy<-(HC_DWT_engy_D)
    DR_DWT_engy<-(DR_DWT_engy_D)
    feature<-c("Da",paste0("Dh",8:1),paste0("Dv",8:1),paste0("Dd",8:1))
  }
  if(data_name=="R"){
    HC_DWT_engy<-(HC_DWT_engy_R)
    DR_DWT_engy<-(DR_DWT_engy_R) 
    feature<-c("Ra",paste0("Rh",8:1),paste0("Rv",8:1),paste0("Rd",8:1))
  }
  if(data_name=="S"){
    HC_DWT_engy<-(HC_DWT_engy_S)
    DR_DWT_engy<-(DR_DWT_engy_S) 
    feature<-c("Sa",paste0("Sh",8:1),paste0("Sv",8:1),paste0("Sd",8:1))
  }
}
if(tt==2){
  if(data_name[1]=="D"){
    if(data_name[2]=="R"){
      HC_DWT_engy<-cbind(HC_DWT_engy_D,HC_DWT_engy_R)
      DR_DWT_engy<-cbind(DR_DWT_engy_D,DR_DWT_engy_R)
      feature<-c("Da",paste0("Dh",8:1),paste0("Dv",8:1),paste0("Dd",8:1),
                 "Ra",paste0("Rh",8:1),paste0("Rv",8:1),paste0("Rd",8:1))
    }else{
      HC_DWT_engy<-cbind(HC_DWT_engy_D,HC_DWT_engy_S)
      DR_DWT_engy<-cbind(DR_DWT_engy_D,DR_DWT_engy_S)
      feature<-c("Da",paste0("Dh",8:1),paste0("Dv",8:1),paste0("Dd",8:1),
                 "Sa",paste0("Sh",8:1),paste0("Sv",8:1),paste0("Sd",8:1))
    }
  }else{
    HC_DWT_engy<-cbind(HC_DWT_engy_R,HC_DWT_engy_S)
    DR_DWT_engy<-cbind(DR_DWT_engy_R,DR_DWT_engy_S)
    feature<-c("Ra",paste0("Rh",8:1),paste0("Rv",8:1),paste0("Rd",8:1),
               "Sa",paste0("Sh",8:1),paste0("Sv",8:1),paste0("Sd",8:1))
  }
}
print(data_name)
print(paste0("DR_data_dim:",dim(DR_DWT_engy)))



A_engy<-HC_DWT_engy
dim(A_engy)
A_engy[1,1]

B_engy<-DR_DWT_engy
dim(B_engy)

B_engy[1,1]



data0<-A_engy
data1<-B_engy
nsample<-nrow(data0)+nrow(data1)

X<-rbind(data0,data1)
X<-matrix(unlist(X),nrow=nrow(X),ncol=ncol(X))
data<-X
Y<-c(rep(0,nrow(data0)),rep(1,nrow(data1)))
X<-scale(X,center = TRUE,scale = TRUE)

nfolds<-nsample-1
nrounds<-1


data<-cbind.data.frame(X,Y)
names(data)<-c(feature,"label")
colnames(X)<-feature



#### hyperparameter selection -cv
set.seed(123)



#### run xgboost / split data into 10 folds and train 9 folds and test 1 fold
#### repeat 30 times

SS<-matrix(0,nrounds,ncol=2)
Labels<-matrix(NA,nrounds,nsample)
Pred.Prob<-matrix(0,nrounds,nsample)
TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)

SS_Valid<-matrix(0,nrounds,ncol=2)
SS_Valid_pred.Prob<-matrix(0,nrounds,nsample)
SS_Valid_TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)

M<-list()
set.seed(123)
foldID<-1:nfolds
for(r in 1:nrounds){
  
  test_pred.Prob<-rep(NA,nsample)
  Valid_pred.Prob<-list()
  ### leave one out
  for(sample_i in 1:nrow(X)){
    
    test_X<-X[sample_i,]
    test_X<-matrix(test_X,nrow=1)
    temp_X<-X[-sample_i,]
    temp_Y<-Y[-sample_i]
    
    ###  10-fold cv 
    foldID<-sample(rep(seq(nfolds), length =nsample-1))
    Eta<-seq(0.1,0.6,by=0.1)
    Nrounds<-c(3:8)
    Max_depth<-c(3:6)
    Mse<-list()
    
    
    for(i in 1:length(Eta)){
      mse<-matrix(NA,length(Nrounds),length(Max_depth))
      for(j in 1:length(Nrounds)){
        for(k in 1:length(Max_depth)){
          
          xgb.fit<-xgb.cv(data = temp_X, label =temp_Y, eta=Eta[i],nrounds=Nrounds[j],max_depth=Max_depth[k],
                          nfold=nfolds, objective = "binary:logistic",prediction = TRUE)
          mse[j,k]<- mean(xgb.fit$pred)
        }
      }
      Mse[[i]]<-mse
    }
    
    temp<-rep(NA,length(Eta)) 
    for(i in 1:length(Eta)){
      temp[i]<-min(Mse[[i]])
    }
    which.min(temp)
    Mse[[which.min(temp)]]
    
    ### find the best hyper parameters
    best_eta<-Eta[which.min(temp)]
    index<-which(Mse[[which.min(temp)]]==min(temp),arr.ind = TRUE)
    best_nrounds<-Nrounds[index[1,1]]
    best_max_depth<-Max_depth[index[1,2]]
    
    
    ### count the validation error
    valid_pred.Prob<-rep(NA,nsample-1)
    glm.fit<-NA
    test_t<-rep(NA,nfolds)
    
    ##foldID<-sample(rep(seq(nfolds), length =nsample-1))
    pred.Prob<-rep(NA,nsample)
    pred.Labels<-rep(NA,nsample)
    for(fold_i in 1:nfolds){
      train_X<-temp_X[which(foldID!=fold_i),]
      train_Y<-temp_Y[which(foldID!=fold_i)]
      valid_X<-temp_X[which(foldID==fold_i),]
      valid_X<-matrix(valid_X,nrow=length(which(foldID==fold_i))) 
      train<-which(foldID!=fold_i)
      test<-which(foldID==fold_i)
      xgb.fit<-xgboost(data = train_X, label = train_Y, eta=best_eta,nrounds=best_nrounds,max_depth=best_max_depth,
                       nfold=1, objective = "binary:logistic")
      valid_pred.Prob[which(foldID==fold_i)]<-predict(xgb.fit,valid_X)
      test_t[fold_i]<-predict(xgb.fit,test_X)
      
      
    }
    
    a<-rep(NA,nsample)
    a[sample_i]<-mean(test_t)
    a[-sample_i]<-valid_pred.Prob
    Valid_pred.Prob[[sample_i]]<-a
    ### use all data excpet the leave out one to train the model , then calculate the test error
    final_xgb.fit<-xgboost(data = temp_X, label = temp_Y, eta=best_eta,nrounds=best_nrounds,max_depth=best_max_depth,
                           nfold=1, objective = "binary:logistic")
    test_pred.Prob[sample_i]<-predict(final_xgb.fit,test_X)
    M[[sample_i]]<-final_xgb.fit
    write.csv(a,file=paste0("xgb_Valid_pred.Prob_sample",sample_i,".csv"))
    saveRDS(final_xgb.fit,file=paste0("xgb_HC_DR_best_model_sample",sample_i,".rds"))
    
  }
  
  
  write.csv(Valid_pred.Prob,file=paste0("xgb_Valid_pred.Prob_r",r,".csv"))
  test_pred.Labels<-ifelse(test_pred.Prob >0.5, 1, 0)
  TPR<-length(intersect(which(test_pred.Labels==1),which(Y==1)))/length(which(Y==1))
  TNR<-length(intersect(which(test_pred.Labels==0),which(Y==0)))/length(which(Y==0))
  SS[r,]<-c(TPR,TNR)
  Pred.Prob[r,]<-test_pred.Prob
  tp<-length(intersect(which(test_pred.Labels==1),which(Y==1)))
  fp<-length(which(test_pred.Labels==1))-length(intersect(which(test_pred.Labels==1),which(Y==1)))
  fn<-length(which(test_pred.Labels==0))-length(intersect(which(test_pred.Labels==0),which(Y==0)))
  tn<-length(intersect(which(test_pred.Labels==0),which(Y==0)))                     
  TP_FP_FN_TN[r,]<-c(tp,fp,fn,tn)
  
  
  ### count validation error
  valid_TPR<-rep(NA,nsample)
  valid_TNR<-rep(NA,nsample)
  temp_valid_pred.Prob<-matrix(NA,nsample,nsample)
  for(valid_i in 1:nsample){
    temp_valid_pred.Prob[valid_i,]<-rep(NA,nsample)
    temp_valid_pred.Prob[valid_i,]<-Valid_pred.Prob[[valid_i]]
  }
  valid_pred.Prob<-colMeans(temp_valid_pred.Prob)
  valid_pred.Labels<-ifelse(valid_pred.Prob >0.5, 1, 0)
  valid_TPR<-length(intersect(which(valid_pred.Labels==1),which(Y==1)))/length(which(Y==1))
  valid_TNR<-length(intersect(which(valid_pred.Labels==0),which(Y==0)))/length(which(Y==0))
  SS_Valid[r,]<-c(valid_TPR,valid_TNR)
  SS_Valid_pred.Prob[r,]<-valid_pred.Prob
  valid_tp<-length(intersect(which(valid_pred.Labels==1),which(Y==1)))
  valid_fp<-length(which(valid_pred.Labels==1))-length(intersect(which(valid_pred.Labels==1),which(Y==1)))
  valid_fn<-length(which(valid_pred.Labels==0))-length(intersect(which(valid_pred.Labels==0),which(Y==0)))
  valid_tn<-length(intersect(which(valid_pred.Labels==0),which(Y==0)))                     
  SS_Valid_TP_FP_FN_TN[r,]<-c(valid_tp,valid_fp,valid_fn,valid_tn)
  
 
  
}



write.csv(Pred.Prob,file=paste0("xgb_HC_DR_test_prob.csv"))
write.csv(SS,file=paste0("xgb_HC_DR_test_TPR_TNR.csv"))
write.csv(TP_FP_FN_TN,file=paste0("xgb_HC_DR_test_TP_FP_FN_TN.csv"))


write.csv(SS_Valid_pred.Prob,file=paste0("xgb_HC_DR_valid_prob.csv"))
write.csv(SS_Valid,file=paste0("xgb_HC_DR_rounds_valid_TPR_TNR.csv"))
write.csv(SS_Valid_TP_FP_FN_TN,file=paste0("xgb_HC_DR_valid_TP_FP_FN_TN.csv"))
write.csv(M,file=paste0("xgb_HC_DR_CVmodel.rds"))
# importance_matrix <- xgb.importance(model = xgb.fit)
# print(importance_matrix)
# 
# 
# xgb.plot.deepness(xgb.fit)

