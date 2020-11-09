
args=commandArgs(trailingOnly=TRUE)
### clear object
# rm(list = ls())

##setwd("/root/Downloads/Projects_clinic/prepare_results_for_papers/paper2_DR/nested")
### refer to Gelman et al (2008), "A weakly informative default prior distribution for logistic & other regression models",

library(kernlab)
library(e1071)
### loading data
##HC_DWT_path<-"/root/Downloads/Projects_clinic/Pprojects_clinic_diabetic retinopathy patients/Rename_sample/HC/HC_DWT/HC_"
##DR_DWT_path<-"/root/Downloads/Projects_clinic/Pprojects_clinic_diabetic retinopathy patients/Rename_sample/DR/DR_DWT/DR_"

HC_DWT_path<-"../../HC/HC_DWT/HC_"
DR_DWT_path<-"../../DR/DR_DWT/DR_"


# HC_DWT_path<-"/media/root/10AF08F010AF08F03/root/Imputation/Trajectory/Script/temp/HC_DR_1102/Projects_clinic_HC_DR_20201102_mae/HC/HC_DWT/HC_"
# DR_DWT_path<-"/media/root/10AF08F010AF08F03/root/Imputation/Trajectory/Script/temp/HC_DR_1102/Projects_clinic_HC_DR_20201102_mae//DR/DR_DWT/DR_"

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

# data_name<-"D"

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
Y<-c(rep("A",nrow(data0)),rep("B",nrow(data1)))

X<-scale(X,center = TRUE,scale = TRUE)

nrounds<-1
nfolds<-(nsample-1)

Data<-cbind.data.frame(X,Y)
names(Data)<-c(feature,"label")

SS<-matrix(0,nrounds,ncol=2)
Pred.Prob<-matrix(0,nrounds,ncol=nsample)
TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)

SS_Valid<-matrix(0,nrounds,ncol=2)
SS_Valid_pred.Prob<-matrix(0,nrounds,nsample)
SS_Valid_TP_FP_FN_TN<-matrix(0,nrounds,ncol=4)

set.seed(123)
foldID<-1:nfolds
M<-list()
for(r in 1:nrounds){
  
  test_pred.Prob<-rep(NA,nsample)
  Valid_pred.Prob<-list()
  ### leave one out
  for(sample_i in 1:nrow(X)){
    
    test_X<-X[sample_i,]
    test_X<-matrix(test_X,nrow=1)
    temp_X<-X[-sample_i,]
    temp_Y<-Y[-sample_i]
    
    data<-cbind.data.frame(temp_X,temp_Y)
    names(data)<-c(feature,"label")
    ###  10-fold cv 
    # foldID<-sample(rep(seq(nfolds), length =nsample-1))
    
    
    
    C<-10^(-3:2)
    Sigma<-c(10^(-4:-1),1/ncol(X),0.1/ncol(X))
    Mse<-matrix(0,length(C),length(Sigma))
    for(C_i in 1:length(C)){
      for(Sigma_j in 1:length(Sigma)){
        svm.fit<-ksvm(label~.,data=data,kernel="rbfdot",cross=nrow(data),verbose = FALSE,
             prob.model=TRUE,C=C[C_i],kpar=list(sigma=Sigma[Sigma_j]))
        #### save the cross-validation error
        Mse[C_i,Sigma_j]<-cross(svm.fit)
      }
    }
   
    
    ### find the best hyper parameters
    index<-which(Mse==min(Mse),arr.ind = TRUE)
    best_c<-C[index[1,1]]
    best_sigma<-Sigma[index[1,2]]
    
    ### leave one out, use (nsample-1) samples to fit the model with the best hyperparameters best_c and best_sigma
    best.model<-ksvm(label~.,data=data,kernel="rbfdot",cross=nrow(data),verbose = FALSE,
                     prob.model=TRUE,C=best_c,kpar=list(sigma=best_sigma))
    ### save best model into M
    M[[sample_i]]<-best.model
    ### predict the test sample
    test_pred.Prob[sample_i]<- predict(best.model,Data[sample_i,],type="probabilities")[,2]
    write.csv(cbind.data.frame(sigma=best_sigma,cost=best_c),file=paste0("svm_sigma_cost_",sample_i,".csv")) 
    ### calculate the inner leave one out validation error
    test_t<-rep(NA,nfolds)
    valid_pred.Prob<-rep(NA,nsample-1)
    for(fold_i in 1:nfolds){
      valid<-fold_i
      train<-c(1:nrow(data))[-valid]
      # svm.fit<-svm(label ~.,data=data[train,],gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost,probability=TRUE,kernel="radial")
      svm.fit<-ksvm(label~.,data=data[train,],kernel="rbfdot",cross=1,verbose = FALSE,prob.model=TRUE,C=best_c,kpar=list(sigma=best_sigma))
    
      valid_pred.Prob[which(foldID==fold_i)]<-predict(best.model,data[valid,],type="probabilities")[,2]
      test_t[fold_i]<-predict(best.model,Data[sample_i,],type="probabilities")[,2]
    }
    a<-rep(NA,nsample)
    a[sample_i]<-mean(test_t)
    a[-sample_i]<-valid_pred.Prob
    Valid_pred.Prob[[sample_i]]<-a
    
     write.csv(a,file=paste0("svm_Valid_pred.Prob_sample",sample_i,".csv"))
     saveRDS(best.model,file=paste0("svm_HC_DR_best_model_sample",sample_i,".rds"))
  }
  
  
  
  write.csv(Valid_pred.Prob,file=("svm_Valid_pred.Prob.csv"))
  test_pred.Labels<-ifelse(test_pred.Prob>0.5,"B","A")
  TPR<-length(intersect(which(test_pred.Labels=="B"),which(Y=="B")))/length(which(Y=="B"))
  TNR<-length(intersect(which(test_pred.Labels=="A"),which(Y=="A")))/length(which(Y=="A"))
  SS[r,]<-c(TPR,TNR)
  Pred.Prob[r,]<-test_pred.Prob
  tp<-length(intersect(which(test_pred.Labels=="B"),which(Y=="B")))
  fp<-length(which(test_pred.Labels=="B"))-length(intersect(which(test_pred.Labels=="B"),which(Y=="B")))
  fn<-length(which(test_pred.Labels=="A"))-length(intersect(which(test_pred.Labels=="A"),which(Y=="A")))
  tn<-length(intersect(which(test_pred.Labels=="A"),which(Y=="A")))                     
  TP_FP_FN_TN[r,]<-c(tp,fp,fn,tn)
  
  
  
  ### count validation error
  valid_TPR<-rep(NA,nsample)
  valid_TNR<-rep(NA,nsample)
  temp_valid_pred.Prob<-matrix(NA,nsample,nsample)
  for(valid_i in 1:nsample){
    temp_valid_pred.Prob[valid_i,]<-rep(NA,nsample)
    # if(valid_i<=nrow(data0)){
    #   temp_valid_pred.Prob[valid_i,valid_i]<-0
    # }else{
    # 
    #   temp_valid_pred.Prob[valid_i,valid_i]<-1
    #   }
    #valid_pred.Lables[-valid_i]<-ifelse(Valid_pred.Prob[[valid_i]]>0.5,1,0)
    #valid_TPR[valid_i]<-length(intersect(which(valid_pred.Lables==1),which(Y==1)))/length(which(Y==1))
    #valid_TNR[valid_i]<-length(intersect(which(valid_pred.Lables==0),which(Y==0)))/length(which(Y==0))
    temp_valid_pred.Prob[valid_i,]<-Valid_pred.Prob[[valid_i]]
  }
  valid_pred.Prob<-colMeans(temp_valid_pred.Prob)
  valid_pred.Labels<-ifelse(valid_pred.Prob >0.5,"B","A")
  valid_TPR<-length(intersect(which(valid_pred.Labels=="B"),which(Y=="B")))/length(which(Y=="B"))
  valid_TNR<-length(intersect(which(valid_pred.Labels=="A"),which(Y=="A")))/length(which(Y=="A"))
  SS_Valid[r,]<-c(valid_TPR,valid_TNR)
  SS_Valid_pred.Prob[r,]<-valid_pred.Prob
  valid_tp<-length(intersect(which(valid_pred.Labels=="B"),which(Y=="B")))
  valid_fp<-length(which(valid_pred.Labels=="B"))-length(intersect(which(valid_pred.Labels=="B"),which(Y=="B")))
  valid_fn<-length(which(valid_pred.Labels=="A"))-length(intersect(which(valid_pred.Labels=="A"),which(Y=="A")))
  valid_tn<-length(intersect(which(valid_pred.Labels=="A"),which(Y=="A")))                     
  SS_Valid_TP_FP_FN_TN[r,]<-c(valid_tp,valid_fp,valid_fn,valid_tn)
  
}




write.csv(Pred.Prob,file=paste0("svm_HC_DR_test_prob.csv"))
write.csv(SS,file=paste0("svm_HC_DR_test_TPR_TNR.csv"))
write.csv(TP_FP_FN_TN,file=paste0("svm_HC_DR_test_TP_FP_FN_TN.csv"))


write.csv(SS_Valid_pred.Prob,file=paste0("svm_HC_DR_valid_prob.csv"))
write.csv(SS_Valid,file=paste0("svm_HC_DR_rounds_valid_TPR_TNR.csv"))
write.csv(SS_Valid_TP_FP_FN_TN,file=paste0("svm_HC_DR_valid_TP_FP_FN_TN.csv"))
saveRDS(M,file=paste0("svm_HC_DR_CVmodel.rds"))


