
library("usethis")
library("devtools")
library("keras")
library("ISLR")
library("ggplot2")
library("lattice")
library("caret")
library("class")
library("MASS")
library("e1071")
library("ROSE")
library("grid")
library("DMwR")
library("MASS") 
library("corrplot")
library("leaps")
library("tree")
library("ROCR")
library("rpart")
library("tensorflow")
library("tidyverse")
library("data.table")
library("clustMixType")
library("tfruns")
library("kernlab")

set.seed(1000)
data<-read.csv("diabetic_data.csv",header=TRUE)
data<-data[,-(1:2)]
str(data)
head(data)
summary(data)

# Data Cleaning ----------------------------------------------------------
sum(data$weight=="?")
sum(data$payer_code=="?")
sum(data$medical_specialty=="?")

data<-data[,-c(4,9,10)]
sum(data=="?")
dim(data)
data[data=="?"]<-NA
data<-na.omit(data)
dim(data)

diag<-data[,14:16]
diag$diag_1[which(diag$diag_1>="390"&diag$diag_1<="459"|diag$diag_1=="785")]="Circulatory"
diag$diag_2[which(diag$diag_2>="390"&diag$diag_2<="459"|diag$diag_2=="785")]="Circulatory"
diag$diag_3[which(diag$diag_3>="390"&diag$diag_3<="459"|diag$diag_3=="785")]="Circulatory"

diag$diag_1[which(diag$diag_1>="460"&diag$diag_1<="519"|diag$diag_1=="786")]="Respiratory"
diag$diag_2[which(diag$diag_2>="460"&diag$diag_2<="519"|diag$diag_2=="786")]="Respiratory"
diag$diag_3[which(diag$diag_3>="460"&diag$diag_3<="519"|diag$diag_3=="786")]="Respiratory"

diag$diag_1[which(diag$diag_1>="520"&diag$diag_1<="579"|diag$diag_1=="787")]="Digestive"
diag$diag_2[which(diag$diag_2>="520"&diag$diag_2<="579"|diag$diag_2=="787")]="Digestive"
diag$diag_3[which(diag$diag_3>="520"&diag$diag_3<="579"|diag$diag_3=="787")]="Digestive"

diag$diag_1[which(diag$diag_1>="250.01"&diag$diag_1<="250.99")]="Diabetes"
diag$diag_2[which(diag$diag_2>="250.01"&diag$diag_2<="250.99")]="Diabetes"
diag$diag_3[which(diag$diag_3>="250.01"&diag$diag_3<="250.99")]="Diabetes"

diag$diag_1[which(diag$diag_1>="800"&diag$diag_1<="999")]="Injury"
diag$diag_2[which(diag$diag_2>="800"&diag$diag_2<="999")]="Injury"
diag$diag_3[which(diag$diag_3>="800"&diag$diag_3<="999")]="Injury"

diag$diag_1[which(diag$diag_1>="710"&diag$diag_1<="739")]="Musculoskeletal"
diag$diag_2[which(diag$diag_2>="710"&diag$diag_2<="739")]="Musculoskeletal"
diag$diag_3[which(diag$diag_3>="710"&diag$diag_3<="739")]="Musculoskeletal"

diag$diag_1[which(diag$diag_1>="580"&diag$diag_1<="629"|diag$diag_1=="788")]="Genitourinary"
diag$diag_2[which(diag$diag_2>="580"&diag$diag_2<="629"|diag$diag_2=="788")]="Genitourinary"
diag$diag_3[which(diag$diag_3>="580"&diag$diag_3<="629"|diag$diag_3=="788")]="Genitourinary"

diag$diag_1[which(diag$diag_1>="1"&diag$diag_1<="139")]="Neoplasms"
diag$diag_2[which(diag$diag_2>="1"&diag$diag_2<="139")]="Neoplasms"
diag$diag_3[which(diag$diag_3>="1"&diag$diag_3<="139")]="Neoplasms"

diag$diag_1[which(diag$diag_1>="140"&diag$diag_1<="239")]="Neoplasms"
diag$diag_2[which(diag$diag_2>="140"&diag$diag_2<="239")]="Neoplasms"
diag$diag_3[which(diag$diag_3>="140"&diag$diag_3<="239")]="Neoplasms"

diag$diag_1[which(diag$diag_1>="790"&diag$diag_1<="799"|diag$diag_1=="780"|diag$diag_1=="781"|diag$diag_1=="784")]="Neoplasms"
diag$diag_2[which(diag$diag_2>="790"&diag$diag_2<="799"|diag$diag_2=="780"|diag$diag_2=="781"|diag$diag_2=="784")]="Neoplasms"
diag$diag_3[which(diag$diag_3>="790"&diag$diag_3<="799"|diag$diag_3=="780"|diag$diag_3=="781"|diag$diag_3=="784")]="Neoplasms"

diag$diag_1[which(diag$diag_1>="680"&diag$diag_1<="709"|diag$diag_1=="782")]="Neoplasms"
diag$diag_2[which(diag$diag_2>="680"&diag$diag_2<="709"|diag$diag_2=="782")]="Neoplasms"
diag$diag_3[which(diag$diag_3>="680"&diag$diag_3<="709"|diag$diag_3=="782")]="Neoplasms"

diag$diag_1[which(diag$diag_1>="240"&diag$diag_1<="279"&diag$diag_1!="250")]="Neoplasms"
diag$diag_2[which(diag$diag_2>="240"&diag$diag_2<="279"&diag$diag_2!="250")]="Neoplasms"
diag$diag_3[which(diag$diag_3>="240"&diag$diag_3<="279"&diag$diag_3!="250")]="Neoplasms"

diag$diag_1[which(diag$diag_1!="Neoplasms"&diag$diag_1!="Genitourinary"&diag$diag_1!="Musculoskeletal"&diag$diag_1!="Diabetes"&diag$diag_1!="Digestive"&diag$diag_1!="Injury"&diag$diag_1!="Respiratory"&diag$diag_1!="Circularory")]="Other"
diag$diag_2[which(diag$diag_2!="Neoplasms"&diag$diag_2!="Genitourinary"&diag$diag_2!="Musculoskeletal"&diag$diag_2!="Diabetes"&diag$diag_2!="Digestive"&diag$diag_2!="Injury"&diag$diag_2!="Respiratory"&diag$diag_2!="Circularory")]="Other"
diag$diag_3[which(diag$diag_3!="Neoplasms"&diag$diag_3!="Genitourinary"&diag$diag_3!="Musculoskeletal"&diag$diag_3!="Diabetes"&diag$diag_3!="Digestive"&diag$diag_3!="Injury"&diag$diag_3!="Respiratory"&diag$diag_3!="Circularory")]="Other"

data$diag_1<-diag$diag_1
data$diag_2<-diag$diag_2
data$diag_3<-diag$diag_3

#-----Outlier for numerical attributes
num<-c(7:13,17)
num_data<-data[,num]
colnames(num_data)

outlier_set<-c()
for (i in 1:ncol(num_data)) {
  First_Quantile<-quantile(num_data[,i], probs = c(0.25))
  Third_Quantile<-quantile(num_data[,i], probs = c(0.75))
  bench_25<-First_Quantile-1.5*(Third_Quantile-First_Quantile)
  bench_75<-Third_Quantile+1.5*(Third_Quantile-First_Quantile)
  
  outlier_set_new<-which(num_data[,i]<bench_25|num_data[,i]>bench_75)
  outlier_set<-c(outlier_set,outlier_set_new)
}  
outlier_set<-unique(outlier_set)
length(outlier_set)

summary(num_data)
for (i in 1:ncol(num_data)){
  hist(num_data[,i],main=colnames(num_data[,i]))
}

outlier_set<-c()
for (i in 1:4) {
  First_Quantile<-quantile(num_data[,i], probs = c(0.25))
  Third_Quantile<-quantile(num_data[,i], probs = c(0.75))
  bench_25<-First_Quantile-1.8*(Third_Quantile-First_Quantile)
  bench_75<-Third_Quantile+1.8*(Third_Quantile-First_Quantile)
  
  outlier_set_new<-which(num_data[,i]<bench_25|num_data[,i]>bench_75)
  outlier_set<-c(outlier_set,outlier_set_new)
}  
outlier_set<-unique(outlier_set)
length(outlier_set)

cate_data<-data[,c(1:6,14:16,18:44)]

colnames(cate_data)
num_data<-num_data[-outlier_set,]
cate_data<-cate_data[-outlier_set,]

#-----scale the numerical variables
n<-nrow(num_data)
m<-ncol(num_data)

for (i in 1:m){
  a<-max(num_data[,i])
  b<-min(num_data[,i])
  for (j in 1:n){
    num_data[j,i]<-(num_data[j,i]-b)/(a-b)
  }
}
summary(num_data)

#-----multi-collinearity
all_cor<-cor(num_data)
corrplot(all_cor,type="upper")
highCorrelation<-findCorrelation(all_cor,cutoff = 0.75,names = TRUE)
highCorrelation<-findCorrelation(all_cor,cutoff = 0.75)
highCorrelation

p<-ncol(cate_data)
less_than_two<-c()

for (q in 1:p){
  cate_data[,q]<-as.factor(cate_data[,q])
  lels<-nlevels(cate_data[,q])
  less_than_two<-c(less_than_two,lels)
}
dele_col<-c(which(less_than_two<=1))
cate_data<-cate_data[,-dele_col]

cov_data<-cate_data
p<-ncol(cov_data)
for (q in 1:p){
  cov_data[,q]<-as.numeric(cov_data[,q])
}

dataCate_cor<-cor(cov_data)
corrplot(dataCate_cor,type="upper")
highCorrelation_cate<-findCorrelation(dataCate_cor,cutoff = 0.6,names = TRUE)
highCorrelation_cate<-findCorrelation(dataCate_cor,cutoff = 0.6)
highCorrelation_cate<-as.vector(highCorrelation_cate)
highCorrelation_cate

p<-ncol(cate_data)
for (q in 1:p){
  cate_data[,q]<-as.factor(cate_data[,q])
}

#-----finalized data set
summary(data$readmitted)
data1<-cbind(cate_data,num_data)
data<-data[-outlier_set,]
data1$readmitted<-data$readmitted
str(data1)

# Undersampling, training, testing and cross-validation data sets------------------------------------------------------------
data1$readmitted<-as.factor(data1$readmitted)
summary(data1$readmitted)
barplot(prop.table(table(data1$readmitted)),col=rainbow(3),ylim = c(0,0.6),main="Class distribution")

data1$readmitted<-as.numeric(data1$readmitted)
data1$readmitted[which(data1$readmitted=="3")]="0"
data1$readmitted[which(data1$readmitted=="2")]="1"
data1$readmitted[which(data1$readmitted=="1")]="1"

data1$readmitted<-as.factor(data1$readmitted)
summary(data1$readmitted)
barplot(prop.table(table(data1$readmitted)),col=rainbow(2),ylim = c(0,0.65),main="Class distribution")

#-----undersampling
balance_data<-ovun.sample(readmitted~.,data=data1,method = "under",N=85444)$data
table(balance_data$readmitted)
total<-nrow(balance_data)

cov_data<-balance_data[,1:32]
names(cov_data)<-c(seq(1,32,by=1))
p<-ncol(cov_data)
for (q in 1:p){
  cov_data[,q]<-as.numeric(cov_data[,q])
}

dataCate_cor<-cor(cov_data)
corrplot(dataCate_cor,type="upper")

highCorrelation_cate<-findCorrelation(dataCate_cor,cutoff = 0.6,names = TRUE)
highCorrelation_cate<-findCorrelation(dataCate_cor,cutoff = 0.6)
highCorrelation_cate<-as.vector(highCorrelation_cate)
highCorrelation_cate

#-----Sampling, training and testing set
Second<-sample(1:nrow(balance_data),42722)
Second_data<-balance_data[Second,]

train<-sample(1:nrow(Second_data),29905)
train_set<-Second_data[train,]  
test_set<-Second_data[-train,]   

table(train_set$readmitted)
train_set<-ovun.sample(readmitted~.,data=train_set,method = "over",N=30050)$data
table(train_set$readmitted)

vali<-sample(1:nrow(train_set),6010)
vali_data<-train_set[vali,]
train_vali<-train_set[-vali,]
summary(train_set)
str(train_set)

# Logistic Regression ----------------------------------------------------
#-----model
p<-ncol(train_set[,-c(33:41)])
a<-c()
for (q in 1:p){
  Dele<-table(train_set[,q])
  dele<-as.data.frame(Dele)
  if (dele$Freq[1] ==30050){
    a<-c(a,q)
  }
}

LR_fit<-glm(readmitted~.,data =train_set[,-a],family = "binomial")
summary(LR_fit)

Test_set<-test_set[,-a]

factor_new<-c(which(test_set$discharge_disposition_id=="10"|test_set$discharge_disposition_id=="27"|test_set$admission_source_id=="13"|test_set$acarbose=="Down"|test_set$miglitol=="Up"))
Test_set<-test_set[-factor_new,]

pred_LR_fit<-predict(LR_fit,newdata=Test_set,type="response") #if showing error, please change line 260 and 261 based on the error 
plot(pred_LR_fit, ylab="Prediction of Logistic Regression")

#-----Performance Measurements
prediction<-rep("0",length(pred_LR_fit))
prediction[pred_LR_fit>0.5]="1"
conf_LR<-table(prediction,Test_set$readmitted)
conf_LR

sum(prediction=="1")
sum(prediction=="0")

ACC<-sum(prediction==Test_set$readmitted)/nrow(Test_set)
ACC
SEN_tpr<-conf_LR[2,2]/sum(conf_LR[,2])
SPE_tnr<-conf_LR[1,1]/sum(conf_LR[,1])
fpr<-1-SEN_tpr

#-----Selection
p<-ncol(vali_data[,-c(33:41)])
a<-c()
for (q in 1:p){
  Dele<-table(vali_data[,q])
  dele<-as.data.frame(Dele)
  if (dele$Freq[1] ==6010){
    a<-c(a,q)
  }
} 
a<-unique(a)

LR_vali_fit<-glm(readmitted~.,data =vali_data[,-a],family = "binomial")
summary(LR_vali_fit)
backwards<-step(LR_vali_fit)

LR_train_fit1<-glm(readmitted ~ age + admission_type_id + discharge_disposition_id + 
                     admission_source_id + diag_1 + diag_3 + metformin + miglitol + 
                     insulin + diabetesMed + time_in_hospital + num_lab_procedures + 
                     num_procedures + num_medications + number_outpatient + number_emergency + 
                     number_inpatient + number_diagnoses,data =train_vali,family = "binomial")
summary(LR_train_fit1)

pred_LR_fit1<-predict(LR_train_fit1,newdata=Test_set,type="response")
prediction1<-rep("0",length(pred_LR_fit1))
prediction1[pred_LR_fit1>0.5]="1"
conf_LR1<-table(prediction1,Test_set$readmitted)
conf_LR1

ACC1<-sum(prediction1==Test_set$readmitted)/nrow(Test_set)
SEN_tpr1<-conf_LR1[2,2]/sum(conf_LR1[,2])
SPE_tnr1<-conf_LR1[1,1]/sum(conf_LR1[,1])
fpr1<-1-SEN_tpr1

LR_ROC1<-prediction(pred_LR_fit1,Test_set$readmitted)
pp1<-performance(LR_ROC1,measure="tpr",x.measure  ="fpr")
plot(pp1)
abline(0,1)

# SVM ---------------------------------------------------------------------
#-----Model (radial)
svm.fit<-svm(readmitted~.,data=train_set,kernel="radial",gamma=1, cost=1)
svm.fit
summary(svm.fit)

train_svm_pred<-predict(svm.fit,newdata=test_set)
svm_conf_matrix<-table(train_svm_pred,test_set$readmitted)
svm_conf_matrix

#-----Performance Measurements
ACC_svm<-sum(train_svm_pred==test_set$readmitted)/nrow(test_set)
SEN_svm<-svm_conf_matrix[1,2]/sum(svm_conf_matrix[,2])
SPE_svm<-svm_conf_matrix[2,1]/sum(svm_conf_matrix[,1])
fpr_svm<-1-SEN_svm

#-----Selection
sele<-vali_data
x<-sele[,-41]

for (q in 1:32){
  x[,q]<-as.numeric(x[,q])
}
summary(x)

x<-scale(x)
x<-as.data.frame(x[,-c(28:29)])
dim(x)

#y<-as.factor(sele$readmitted)

#Selection_svm<-rfe(x,y,sizes=c(10,15,20),rfeControl = rfeControl(functions = caretFuncs,verbose = FALSE,number = 5),method = "svmRadial")
#Selection_svm

#-----Cross-validation
svm_fit_tune<-tune(svm,readmitted~.,data=vali_data,kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(svm_fit_tune)

svm_best<-svm(readmitted~num_medications+num_procedures+discharge_disposition_id+diag_1+age+diag_2+time_in_hospital+number_inpatient+diag_3+
                number_diagnoses+num_lab_procedures+admission_source_id+admission_type_id+insulin,data=train_vali,kernel="radial",gamma=0.5, cost=10)
svm_best
summary(svm_best)

best_svm_pred<-predict(svm_best,newdata=test_set)
svm_best_matrix<-table(best_svm_pred,test_set$readmitted)
svm_best_matrix

#-----Performance Measurements
ACC_svm1<-sum(best_svm_pred==test_set$readmitted)/nrow(test_set)
SEN_svm1<-svm_best_matrix[1,2]/sum(svm_best_matrix[,2])
SPE_svm1<-svm_best_matrix[2,1]/sum(svm_best_matrix[,1])
fpr_svm1<-1-SEN_svm1

# Decision Tree -----------------------------------------------------------
#-----Model
tree_fit<-tree(readmitted~.,data=train_set)
summary(tree_fit)

plot(tree_fit)
text(tree_fit,pretty=1)

tree_fit_pred<-predict(tree_fit,newdata=test_set,type="class")
confu_matrix_tree<-table(tree_fit_pred, test_set$readmitted)
confu_matrix_tree

ACC_tree<-sum(confu_matrix_tree[1,1]+confu_matrix_tree[2,2])/nrow(test_set)
SEN_tree<-confu_matrix_tree[1,2]/sum(confu_matrix_tree[,2])
SPE_tree<-confu_matrix_tree[2,1]/sum(confu_matrix_tree[,1])
fpr_tree<-1-SEN_tree

#-----Selection
tree_2<-tree(readmitted~.,data=vali_data,control=tree.control(6010,mincut=200),split = "gini")
plot(tree_2)
#text(tree_2,pretty=1)

cv_tree_fit<-cv.tree(tree_2,FUN=prune.misclass )
plot(cv_tree_fit$size,cv_tree_fit$dev ,type="b",ylab="Dev",xlab="Tree Size", main="Tuning Tree Size")
cv_tree_fit$dev
cv_tree_fit$size

tree_best<-tree(readmitted~.,data=train_vali,control = tree.control(24040))
prune_cv_tree_fit<-prune.misclass(tree_2,best=4)
plot(prune_cv_tree_fit)
text(prune_cv_tree_fit,pretty =0)

tree_best_pred<-predict(prune_cv_tree_fit,newdata=test_set,type="class")
best_matrix_tree<-table(tree_best_pred, test_set$readmitted)
best_matrix_tree

ACC_best_tree<-sum(best_matrix_tree[1,1]+best_matrix_tree[2,2])/nrow(test_set)
SEN_best_tree<-best_matrix_tree[1,2]/sum(best_matrix_tree[,2])
SPE_best_tree<-best_matrix_tree[2,1]/sum(best_matrix_tree[,1])
fpr_best_tree<-1-SEN_best_tree

# Random Forest -----------------------------------------------------------
#-----Model
detach("dplyr", unload = TRUE)
detach("ggplot2", unload = TRUE)
library("randomForest")

rf.fit<-randomForest(readmitted~.,data = train_set)
print(rf.fit)
rf.fit$confusion

rf.fit_pred<-predict(rf.fit,newdata=test_set,type="class")
rf_confu<-confusionMatrix(rf.fit_pred,test_set$readmitted)
rf_confu
plot(rf.fit,main="Random Forest")

#-----Performance
ACC_rf<-sum(rf.fit_pred==test_set$readmitted)/nrow(test_set)
ACC_rf 

#-----feature selection
Feat<-importance(rf.fit)
Feat
varImpPlot(rf.fit, main="Importance of Attributes")

#-----Tuning
rf_tune<-tuneRF(vali_data[,1:40],vali_data[,41], stepFactor = 3, plot = T, ntreeTry = 500,
                trace = T,improve = 0.05)

#-----New Model
rf.fit_best<-randomForest(readmitted~num_medications+num_procedures+discharge_disposition_id+diag_1+age+diag_2+time_in_hospital+number_inpatient+diag_3+
                            number_diagnoses+num_lab_procedures+admission_source_id+admission_type_id+insulin,data = train_vali,ntree=400,
                          mtry=6)
print(rf.fit_best)

rf.fit_pred_best<-predict(rf.fit_best,newdata=test_set,type="class")
rf_confu_best<-confusionMatrix(rf.fit_pred_best,test_set$readmitted)
rf_confu_best

#-----Performance with optimal values
ACC_rf_best<-sum(rf.fit_pred_best==test_set$readmitted)/nrow(test_set)
ACC_rf_best

library("dplyr")
library("ggplot2")

# Neural Network------------------------------------------------------------
#-----Sampling, training and testing set
summary(Second_data)
new_data<-Second_data

for (q in 1:32){
  new_data[,q]<-as.numeric(new_data[,q])
}
summary(new_data)

new_data[,41]<-as.numeric(new_data[,41])
new_data<-as.matrix(new_data)

dimnames(new_data)<-NULL
new_data[,1:32]<-normalize(new_data[,1:32])

tri<-sample(1:nrow(new_data),21361)
train<-new_data[tri,1:40]
test<-new_data[-tri,1:40]
train_y<-new_data[tri,41]-1
test_y<-new_data[-tri,41]-1

trainlable<-to_categorical(train_y)
testlable<-to_categorical(test_y)

model<-keras_model_sequential()
model<-keras_model_sequential()
model %>%
  layer_dense(unit=80,activation='relu',input_shape = c(40)) %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(unit=50,activation='relu') %>%
  layer_dropout(rate=0.3)%>%
  layer_dense(unit=30,activation='relu') %>%
  layer_dropout(rate=0.2)%>%
  layer_dense(unit=2,activation='sigmoid')
summary(model)

model %>%
  compile(loss='binary_crossentropy',
          optimizer='adam',
          metrics='accuracy')
set.seed(42)
history<-model %>%
  fit(train,
      trainlable,
      epoch=30,
      batch_size=32,
      validation_split=0.2)

classes<-model %>% predict_classes(test)

table(test_y, classes)

#-----tuning
runs<-tuning_run("experiment.R",
                 flags=list(dense_units1=c(80,60),
                            dense_units2=c(50,30),
                            dense_units3=c(30,20),
                            dropout1=c(0.4,0.3),
                            dropout2=c(0.3,0.2),
                            dropout3=c(0.2,0.1)))

head(runs)
runs

#----Model with optimal hyperparameters
model<-keras_model_sequential()
model %>%
  layer_dense(unit=80,activation='relu',input_shape = c(40)) %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(unit=30,activation='relu') %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(unit=30,activation='relu') %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(unit=2,activation='sigmoid')
summary(model)

model %>%
  compile(loss='binary_crossentropy',
          optimizer='adam',
          metrics='accuracy')
set.seed(42)
history<-model %>%
  fit(train,
      trainlable,
      epoch=30,
      batch_size=32,
      validation_split=0.2)

classes<-model %>% predict_classes(test)

table(test_y, classes)

# K-prototype-------------------------------------------------------------------------
#-----Modeling
TES<-test_set
View(TES)

TES$readmitted<-as.numeric(TES$readmitted)

kpro<-kproto(train_set[1:40],k=2)
kpro$cluster
summary(kpro)

library(wesanderson)
par(mfrow=c(5,8))
clprofiles(kpro, train_set[1:40], col = wes_palette("Royal1", 2, type = "continuous")) 
par(mfrow=c(1,1))
#----Performance 
K_pred<-predict(kpro,TES[1:40])
K_pred

confusionMatrix_K<-table(K_pred$cluster,TES$readmitted)
confusionMatrix_K

ACC<-sum(K_pred$cluster==TES$readmitted)/nrow(TES)
ACC





