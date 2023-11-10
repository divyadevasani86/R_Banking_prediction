getwd()
setwd("D:/Data Science_Classes/data science class/projects/Banking/data")
bank_full_test=read.csv("bank-full_test.csv",stringsAsFactors = F)
bank_full_train=read.csv("bank-full_train.csv",stringsAsFactors = F)

View(bank_full_train)
bank_full_test$y=NA
bank_full_test$data="test"
bank_full_train$data="train"

full=rbind(bank_full_train,bank_full_test)


library(dplyr)

library(tidyr)
glimpse(full)
View(full)
  
   full=full%>%
        mutate(default=as.numeric(if_else(default=="yes",1,0)),
               housing=as.numeric(if_else(housing=="yes",1,0)),
               loan=as.numeric(if_else(loan=="yes",1,0)),
               y=as.factor(if_else(y=="yes",1,0)))
   
   CreateDummies=function(data,var,freq_cutoff=0){
     t=table(data[,var])
     t=t[t>freq_cutoff]
     t=sort(t)
     categories=names(t)[-1]
     
     for( cat in categories){
       name=paste(var,cat,sep="_")
       name=gsub(" ","",name)
       name=gsub("-","_",name)
       name=gsub("\\?","Q",name)
       name=gsub("<","LT_",name)
       name=gsub("\\+","",name)
       name=gsub("\\/","_",name)
       name=gsub(">","GT_",name)
       name=gsub("=","EQ_",name)
       name=gsub(",","",name)
       name=gsub("[()]","",name)
       name=gsub("`","",name)
       data[,name]=as.numeric(data[,var]==cat)
     }
     
     data[,var]=NULL
     return(data)
   }
   
glimpse(full)


var_dummy=c("job","marital","education","contact","month","poutcome")


for (cat in var_dummy){
  
  full=CreateDummies(full,cat,50)
}
   
bank_train=full %>% filter(data=="train") %>% select(-data)
bank_test= full %>% filter(data=="test")  %>% select(-data,-y)


set.seed(10)

C=sample(1:nrow(bank_train),0.8*nrow(bank_train))

bank_train1=bank_train[C,]
bank_train2=bank_train[-C,]

library(tree)
bank.tree=tree(y~.-ID,data=bank_train1)


bank.pred=predict(bank.tree,newdata = bank_train2,type = "vector")[,2]
library(pROC)

auc(roc(bank_train2$y,bank.pred))

final.bank.tree=tree(y~.-ID,data=bank_train)

final.test.pred=predict(final.bank.tree,newdata=bank_test,type="vector")[,2]

# making confusion matrix

pred.total.train=predict(final.bank.tree,newdata = bank_train,type="vector")[,2]

real=bank_train$y


cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(pred.total.train>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]


#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=Sp))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

predicted_bank_test=as.numeric(final.test.pred>my_cutoff)

any(is.na(predicted_bank_test))

write.csv(predicted_bank_test,"divya_devasani_p2_project5.csv",row.names = F)

View(predicted_bank_test)
table(predicted_bank_test)
table(bank_full_train$y)

