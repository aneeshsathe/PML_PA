rm(list=ls())
library(data.table)
library(ggplot2)
library(caret)
library(doParallel)
library(randomForest)

pro_in_dat=read.csv("pml-training.csv",nrows =2)
head(pro_in_dat)

keywords=c("X","user_name","min","max","amplitude","window","kurtosis","user","*timestamp*","*skewness*","var","avg","stddev")
strings=colnames(pro_in_dat)
# colnames(pro_in_dat)[!apply(sapply(keywords, grepl, strings, ignore.case=TRUE),1,any)]

mat=apply(sapply(keywords, grepl, strings, ignore.case=TRUE),1,any)
mat[mat]="NULL"
mat[mat==FALSE]=NA


pro_in_dat=read.csv("pml-training.csv",colClasses = mat)
levels(pro_in_dat$classe)

inTrain <- createDataPartition(y=pro_in_dat$classe,p=0.7, list=FALSE)
training <- pro_in_dat[inTrain,]
testing <- pro_in_dat[-inTrain,]
# pred_training <- pro_in_dat[inTrain,41]

# cl <- makePSOCKcluster(2)
# registerDoParallel(cl)
ctrl <- trainControl(preProcOptions = list(pcaComp=4))
# modFit=train(Classe~.,data=training, method="rf",prox=TRUE,preProcess=c("pca"),trControl = ctrl)
modFit=randomForest(classe~.,data=training,ntree=500)
# stopCluster(cl)
modFit

pred_out=predict(modFit,testing[,1:40])

confusionMatrix(testing$classe,pred_out)
# 
# cs_testing=read.csv("pml-testing.csv",colClasses = mat)
# 
# cs_pred_out=predict(modFit,cs_testing[,1:40])
# 
# confusionMatrix(cs_testing$classe,cs_pred_out)
# 
# 
# save(modFit,file="rand_forest_formula.RData")
save.image()
unlink("rand_forest_formula.RData")
unlink(".RData")


load("rand_forest_formula.RData")

cs_testing=read.csv("pml-testing.csv")

cs_pred_out=predict(modFit,cs_testing[,1:40])

confusionMatrix(cs_testing$classe,cs_pred_out)


pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

