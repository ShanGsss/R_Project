#Data Set https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity
library(MASS) 
library(ISLR)
library(ggplot2)
library(corrplot)
library(lattice)
library(caret)
library(leaps)
set.seed(1000)
data<-read.csv("OnlineNewsPopularity.csv",header=TRUE)  
data<-data[,-1]
data<-data[,-1]

# Missing value and outliers ----------------------------------------------
#Apply a simple	method to identify missing points.	
data<-na.omit(data)
summary(data)
str(data)
names(data)
dim(data)

#Apply a simple	method to identify outlier
  #I have enough samples to do analysis. So, I simply discard those outliers. 
dataA<-data[,-c(12:17,30:37)]
data_NoOutlier<-data
dim(dataA)
n<-nrow(dataA)
for (i in 1:n) {
  First_Quantile<-quantile(dataA[,i], probs = c(0.25))
  Third_Quantile<-quantile(dataA[,i], probs = c(0.75))
  bench_25<-First_Quantile-1.5*(Third_Quantile-First_Quantile)
  bench_75<-Third_Quantile-1.5*(Third_Quantile-First_Quantile)
  
  j<-nrow(dataA)
  for (k in 1:j){
    a<-dataA[k,i]
    if (a<bench_25|a>bench_75) {
      data_NoOutlier<-data_NoOutliers[-k,]
      dataA<-dataA[-k,]
    } 
  }
}  

dim(data_NoOutlier)
  
#List 5 important insightful questions about the data set.
# Insightful question #1 --------------------------------------------------
  #Is there any multi-collinearity in the analysis?
dele_col<-c(12:17,30:37,59)
dataA<-data[,-dele_col]
str(dataA)
names(dataA)
dim(dataA)
require(caret)
all_cor<-cor(dataA)

corrplot(all_cor,type="upper")
highCorrelation<-findCorrelation(all_cor,cutoff = 0.75,names = TRUE)
highCorrelation<-findCorrelation(all_cor,cutoff = 0.75)
highCorrelation

highCorrelation<-as.vector(highCorrelation)
colnames(dataA[highCorrelation])
dataB<-dataA[,-as.vector(highCorrelation)]

# Insightful question #2 --------------------------------------------------
  #PCA 
dele_col<-c(12:17,30:37,59)
dataA<-data[,-dele_col]
dim(dataA)
names(data)
PCA<-prcomp(dataA[,-44], scale=TRUE,center = TRUE)
summary(PCA)

plot(PCA$sdev^2,xlab="Principal component",ylab="Variance",type = "o")
abline(h=1,col="2")

pre<-PCA$sdev^2/sum(PCA$sdev^2)
plot(pre,xlab="Principal component",ylab="Proportion of Variance",type="b")  

pve<-pre/sum(pre)
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative PVE",
     ylim=c(0,1),type = "o")
abline(h=0.78,col="3")
#If I have time, I will map the categorical variable with PCA. 

# Insightful question #3 --------------------------------------------------
  #Is there any correlation between those categorical variables?
Cate_Var<-c(12:17,30:37)
dataCate<-data[,Cate_Var]
dim(dataCate)
str(dataCate)

variables<-as.vector(names(dataCate)) 
n<-length(variables)
#colnames(dataCate)<-c(seq(from=1,to=14,by=1))

dataCate_cor<-cor(dataCate)
corrplot(dataCate_cor,type="upper")
highCorrelation_cate<-findCorrelation(dataCate_cor,cutoff = 0.7,names = TRUE)
highCorrelation_cate<-findCorrelation(dataCate_cor,cutoff = 0.7)
highCorrelation_cate<-as.vector(highCorrelation_cate)
highCorrelation_cate
colnames(dataCate[highCorrelation_cate])
dataCate<-dataCate[,-as.vector(highCorrelation_cate)]

# Insightful question #4 --------------------------------------------------
#4. When is the article often published?
Week_Col<-c(30:36)
Week_data<-data[,Week_Col]
Count_Matrix<-matrix(nrow=7,ncol=1)
rownames(Count_Matrix)<-c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")
colnames(Count_Matrix)<-c("Counts")

for (a in 1:ncol(Week_data)) {
  Count_Matrix[a,1]<-sum(Week_data[,a])
}

View(Count_Matrix)

barplot(Count_Matrix[1:7,"Counts"], main="Articles Published on Different Days ", horiz=TRUE,
        names.arg=c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"),col=c("2","3","4","5","6","7","8"))


# Insightful question #5 --------------------------------------------------
#5. How many articles published about different topics? 
Topic_Col<-c(12:17)
Topic_data<-data[,Topic_Col]
Count_Matrix_Topic<-matrix(nrow=6,ncol=1)
rownames(Count_Matrix_Topic)<-c("Lifestyle","Entertainment","Business","Social Media","Tech","World")
colnames(Count_Matrix_Topic)<-c("Counts")

for (a in 1:ncol(Topic_data)) {
  Count_Matrix_Topic[a,1]<-sum(Topic_data[,a])
}

View(Count_Matrix_Topic)
barplot(Count_Matrix_Topic[1:6,"Counts"], main="Articles Published about Different Topics ", horiz=F,
        col=c("2","3","4","5","6","7"),ylim = c(-0.5,10000),
        legend=TRUE,args.legend = list(margin(6, 6, 6, 6),bty="n",x ="top", ncol = 3,pt.cex = 0.5, cex = 0.7))
 

# #Insightful question #6 -------------------------------------------------
#Linear regression model for all predictors
data_reg<-dataB
data_reg<-cbind(data_reg,dataCate)
names(data_reg)
data_reg$shares<-data[,59]
names(data_reg)
dim(data_reg)
lmfit<-lm(shares~.,data=data_reg)
summary(lmfit)

par(mfrow = c(2, 2))
plot(lmfit)

# Adjust the predictors --------------------------------------------------
Cate_Var<-c(12:17,30:37)
dataCate<-data[,Cate_Var]
dim(dataCate)
str(dataCate)

data_reg<-dataB
dim(data_reg)
names(data_reg)
category<-c(12:17)
data_reg<-cbind(data_reg,data[,category])
names(data_reg)
data_reg$week<-data[,37]
data_reg$shares<-data[,59]
dim(data_reg)
names(data_reg)
dele<-c(10,14,17:21)
data_reg<-data_reg[,-(dele)]
names(data_reg)

#QQ plot (running time is long)
par(mfcol=c(4,6))
for (i in 1:24){
 qqnorm(data_reg[,i],plot.it = TRUE)
 qqline(data_reg[,i])
}
par(mfcol=c(1,1))

data_reg<-data_reg[,-(3)]
names(data_reg)

#new linear regression model
lmfit<-lm(log(shares)~.,data=data_reg)
summary(lmfit)

par(mfrow = c(2, 2))
plot(lmfit)

# Forward selection and backward selection --------------------------------
#Use forward and backward selection strategy and discuss your results.	
  #forward selection
lmfit_forward<-regsubsets(log(shares)~.,data=data_reg,nvmax=35,method="forward")
summary(lmfit_forward)

n<-which.max(summary(lmfit_forward)$adjr2)
n
par(mfcol=c(1,1))
plot(summary(lmfit_forward)$adjr2,xlab="Number of Variables",ylab="adjusted RSq")
points(n,summary(lmfit_forward)$adjr2[n],col="2")
coef(lmfit_forward,n)
summary(lmfit_forward)$adjr2[n]

lm_forward<-lm(log(shares)~n_tokens_title+n_tokens_content+num_hrefs+num_self_hrefs+num_imgs +
                 num_videos+average_token_length+num_keywords+ kw_min_max+kw_max_max+kw_avg_max +
                 kw_max_avg+self_reference_avg_sharess+global_subjectivity +global_sentiment_polarity+
                 global_rate_positive_words+rate_positive_words + min_positive_polarity+
                 avg_negative_polarity+title_subjectivity +title_sentiment_polarity+abs_title_subjectivity+
                 abs_title_sentiment_polarity +data_channel_is_lifestyle+data_channel_is_entertainment+
                 data_channel_is_bus +data_channel_is_socmed+data_channel_is_tech+data_channel_is_world +week,data=data_reg)

summary(lm_forward) 

par(mfrow=c(2,2))
plot(lm_forward)

#backward selection
lmfit_backward<-regsubsets(log(shares)~.,data=data_reg,nvmax=35,method="backward")
summary(lmfit_backward)

m<-which.max(summary(lmfit_backward)$adjr2)
m
par(mfcol=c(1,1))
plot(summary(lmfit_backward)$adjr2,xlab="Number of Variables",ylab="adjusted RSq")
points(m,summary(lmfit_backward)$adjr2[m],col="2")

coef(lmfit_backward,m)
summary(lmfit_backward)$adjr2[m]

lm_backward<-lm(log(shares)~n_tokens_title+n_tokens_content+num_hrefs+num_self_hrefs+num_imgs +
                 num_videos+average_token_length+num_keywords+ kw_min_max+kw_max_max+kw_avg_max +
                 kw_max_avg+self_reference_avg_sharess+global_subjectivity +global_sentiment_polarity+
                 global_rate_positive_words+rate_positive_words + min_positive_polarity+
                 avg_negative_polarity+title_subjectivity +title_sentiment_polarity+abs_title_subjectivity+
                 abs_title_sentiment_polarity +data_channel_is_lifestyle+data_channel_is_entertainment+
                 data_channel_is_bus +data_channel_is_socmed+data_channel_is_tech+data_channel_is_world +week,data=data_reg)

summary(lm_backward) 

par(mfrow=c(2,2))
plot(lm_forward)

# Mixed selection ---------------------------------------------------------
#Use a mix of forward and backward selection strategy and discuss your results.	
lmfit_best<-regsubsets(log(shares)~.,data=data_reg,nvmax=36)
summary(lmfit_best)

b<-which.max(summary(lmfit_best)$adjr2)
b
par(mfcol=c(1,1))
plot(summary(lmfit_best)$adjr2,xlab="Number of Variables",ylab="adjusted RSq")
points(b,summary(lmfit_best)$adjr2[b],col="2")

coef(lmfit_best,b)
summary(lmfit_best)$adjr2[b]

# Accuracy ----------------------------------------------------------------
#Develop a method to find the least complex model with a reasonable accuracy.	
#transformation: because most of the variables do not follow normal distribution, I should try to transform them. 
lmfit_best<-regsubsets(log(shares)~.,data=data_reg,nvmax=17)
summary(lmfit_best)

b<-which.max(summary(lmfit_best)$adjr2)
b
par(mfcol=c(1,1))
plot(summary(lmfit_best)$adjr2,xlab="Number of Variables",ylab="adjusted RSq")
points(b,summary(lmfit_best)$adjr2[b],col="2")

coef(lmfit_best,b)
summary(lmfit_best)$adjr2[b]
