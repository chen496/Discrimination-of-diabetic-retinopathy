library(stringr)
##create directories
##root_name<-"/scratch/tmp/cxw496/Projects_clinic_HC_DR_20201102/3images_DRS/xgb/"

data<-"D"
loss<-"class"
##write jobs
for(seed in c(1)){

  dir_name<-paste0("logit_",loss,"_",data)
  x1<-c("#!/bin/bash")
  x2<-paste0("#BSUB -J logit_",loss,"_",data)
  x3<-c("#BSUB -q general
#BSUB -n 1
#BSUB -W 100:00")
  x4<-paste0("#BSUB -e ", dir_name,".err")
  x5<-paste0("#BSUB -o ", dir_name,".out")
  ##x6<-paste0("cd ",root_name)
  x7<-c("module switch python/2.7.3 python/3.7.1 ")
  x8<-paste0("$HOME/software/local/R-3.6.3/bin/Rscript logit_logistic_nestedCV_LOOCV_LOOCV.R ",data," ",loss)

write.table(c(x1,x2,x3,x4,x5,x7,x8),paste0(dir_name,".job"),
              row.names = F,col.names=F,
              quote = F)
  dir1<-paste0(dir_name,".job")
  print(dir1)
  cat(sprintf("bsub < %s\n",dir1))
  system(sprintf("bsub < %s",dir1))
# for(tt in 24165341:24165490)
# system(sprintf("bkill %s", tt))
}
