install.packages("caret")
install.packages("DMwR")
install.packages("readxl")
install.packages("tidyverse")
install.packages("ROCR")
install.packages("ModelMetrics")
install.packages("corrplot")
install.packages("ineq")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("grobs")
install.packages("car")
install.packages("VIF")
install.packages("lmtest")

library(grid)
library(gridExtra)
library(lattice)
library(ModelMetrics)
library(corrplot)
library(ineq)
library(ROCR)
library(caret)
library(tidyverse)
library(readxl)
library(dplyr)
library(rpart)
library(ggplot2)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(VIF)
library(lmtest)
library(car)
library(e1071)
library(class)
library(MASS)


#Data Preparation
#Read the dataset
mydata = read_xlsx("D:/GreatLakes/PredictiveModelling/Project/Cellphone.xlsx")
View(mydata)

#Structure of data
dim(mydata)
head(mydata)
names(mydata)
#To check missing data
any(is.na(mydata)) 
sum(is.na(mydata)) #found that there is no missing data
str(mydata) #All the variables are numeric data

##check for correlation between variables

cor.mydata = cor(mydata,)
round(cor.mydata,2)
corrplot(cor.mydata)
corrplot(cor.mydata,method = "number",number.cex = .6)

#Data Usage and Data Plan are highly correlated. Monthly Charge is also highly correlated with Data Usage, Data Plan and Day Mins. 

cor(mydata[,-c(5,9)],)
round(cor(mydata[,-c(5,9)],),2)
corrplot(round(cor(mydata[,-c(5,9)],),2))
corrplot(round(cor(mydata[,-c(5,9)],),2),method = "number",number.cex = .6)

#Converting Churn, ContractRenewal and DataPlan into factors
mydata$Churn=as.factor(mydata$Churn)
mydata$ContractRenewal=as.factor(mydata$ContractRenewal)
mydata$DataPlan=as.factor(mydata$DataPlan)

summary(mydata) 

#Exploratory Data Analysis and Outlier Treatment

boxplot(mydata$AccountWeeks,main ="Boxplot of Account Weeks",ylab = "No. of Account weeks") #outliers exist in the upper end
quantile(mydata$AccountWeeks,probs = seq(0,1,0.05))
subset1=mydata[which(mydata$AccountWeeks>200),]  # Capping in the upper value
subset1$AccountWeeks=200
mydata[which(mydata$AccountWeeks>200),]=subset1
boxplot(mydata$AccountWeeks,main ="Boxplot of Account Weeks",ylab = "No. of Account weeks") 
hist(mydata$AccountWeeks,main = "Histogram of Account Weeks",xlab = "No. of Account weeks")
ggplot(mydata,aes(AccountWeeks,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g1=ggplot(mydata,aes(AccountWeeks,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata$AccountWeeks,data=mydata,fill=Churn)


boxplot(mydata$DataUsage,main ="Boxplot of Monthly Data Usage",ylab = "Monthly Data usage in gigabytes")#outliers exist in the upper end
quantile(mydata$DataUsage,probs = seq(0,1,0.05))
subset2=mydata[which(mydata$DataUsage>4.20),]  # Capping in the upper value
subset2$DataUsage=4.20
mydata[which(mydata$DataUsage>4.20),]=subset2
boxplot(mydata$DataUsage,main ="Boxplot of Monthly Data Usage",ylab = "Monthly Data usage in gigabytes") 
hist(mydata$DataUsage,main = "Histogram of Monthly Data Usage",xlab = "Monthly Data usage in gigabytes")
ggplot(mydata,aes(DataUsage,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g2=ggplot(mydata,aes(DataUsage,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata$DataUsage,data=mydata,fill=Churn)


boxplot(mydata$CustServCalls,main ="Boxplot of Customer Service calls",ylab = "No. of customer service calls")#outliers exist in the upper end
quantile(mydata$CustServCalls,probs = seq(0,1,0.05))
subset3=mydata[which(mydata$CustServCalls>4),]  # Capping in the upper value
subset3$CustServCalls=4
mydata[which(mydata$CustServCalls>4),]=subset3
boxplot(mydata$CustServCalls,main ="Boxplot of Customer Service calls",ylab = "No. of customer service calls") 
hist(mydata$CustServCalls,main ="histogram of Customer Service calls",xlab = "No. of customer service calls")
qplot(mydata$CustServCalls,data=mydata,fill=Churn)
prop.table(table(mydata$CustServCalls,mydata$Churn),1)*100
ggplot(mydata,aes(CustServCalls,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g3=ggplot(mydata,aes(CustServCalls,fill=Churn,color=Churn))+geom_density(alpha=0.1)

boxplot(mydata$DayMins,main ="Boxplot of Average Daytime minutes/month",ylab = "Average Daytime minutes/month")#outliers exist in both sides
quantile(mydata$DayMins,probs = seq(0,1,0.05))
subset4=mydata[which(mydata$DayMins>320),]  # Capping in the upper value
subset4$DayMins=320
mydata[which(mydata$DayMins>320),]=subset4
subset5=mydata[which(mydata$DayMins<40),]  # Capping in the lower value
subset5$DayMins=40
mydata[which(mydata$DayMins<40),]=subset5
boxplot(mydata$DayMins,main ="Boxplot of Average Daytime minutes/month",ylab = "Average Daytime minutes/month")
hist(mydata$DayMins,main ="Histogram of Average Daytime minutes/month",xlab = "Average Daytime minutes/month")
qplot(mydata$DayMins,data=mydata,fill=Churn)
ggplot(mydata,aes(DayMins,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g4=ggplot(mydata,aes(DayMins,fill=Churn,color=Churn))+geom_density(alpha=0.1)


boxplot(mydata$DayCalls,main ="Boxplot of Average number of daytime calls",ylab = "Average number of daytime calls")#outliers exist in both sides
quantile(mydata$DayCalls,probs = seq(0,1,0.05))
subset6=mydata[which(mydata$DayCalls>150),]  # Capping in the upper value
subset6$DayCalls=150
mydata[which(mydata$DayCalls>150),]=subset6
subset7=mydata[which(mydata$DayCalls<50),]  # Capping in the lower value
subset7$DayCalls=50
mydata[which(mydata$DayCalls<50),]=subset7
boxplot(mydata$DayCalls,main ="Boxplot of Average number of daytime calls",ylab = "Average number of daytime calls")
hist(mydata$DayCalls,main ="Histogram of Average number of daytime calls",ylab = "Average number of daytime calls")
qplot(mydata$DayCalls,data=mydata,fill=Churn)
ggplot(mydata,aes(DayCalls,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g5=ggplot(mydata,aes(DayCalls,fill=Churn,color=Churn))+geom_density(alpha=0.1)


boxplot(mydata$MonthlyCharge,main ="Boxplot of Average monthly bill",ylab = "Average monthly bill")#outliers exist in the upper end
quantile(mydata$MonthlyCharge,probs = seq(0,1,0.05))
subset8=mydata[which(mydata$MonthlyCharge>90),]  # Capping in the upper value
subset8$MonthlyCharge=90
mydata[which(mydata$MonthlyCharge>90),]=subset8
boxplot(mydata$MonthlyCharge,main ="Boxplot of Average monthly bill",ylab = "Average monthly bill")
hist(mydata$MonthlyCharge,main ="histogram of Average monthly bill",xlab = "Average monthly bill")
qplot(mydata$MonthlyCharge,data=mydata,fill=Churn)
ggplot(mydata,aes(MonthlyCharge,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g6=ggplot(mydata,aes(MonthlyCharge,fill=Churn,color=Churn))+geom_density(alpha=0.1)


boxplot(mydata$OverageFee,main ="Boxplot of largest overage fee in last 12 months",ylab = "largest overage fee in last 12 months")#outliers exist in both sides
quantile(mydata$OverageFee,probs = seq(0,1,0.05))
subset9=mydata[which(mydata$OverageFee>16.5),]  # Capping in the upper value
subset9$OverageFee=16.5
mydata[which(mydata$OverageFee>16.5),]=subset9
subset10=mydata[which(mydata$OverageFee<3.5),]  # Capping in the lower value
subset10$OverageFee=3.5
mydata[which(mydata$OverageFee<3.5),]=subset10
boxplot(mydata$OverageFee,main ="Boxplot of largest overage fee in last 12 months",ylab = "largest overage fee in last 12 months")
hist(mydata$OverageFee,main ="Histogram of largest overage fee in last 12 months",xlab = "largest overage fee in last 12 months")
qplot(mydata$OverageFee,data=mydata,fill=Churn)
ggplot(mydata,aes(OverageFee,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g7=ggplot(mydata,aes(OverageFee,fill=Churn,color=Churn))+geom_density(alpha=0.1)


boxplot(mydata$RoamMins,main ="Boxplot of average number of roaming minutes",ylab = "average number of roaming minutes")#outliers exist in both sides
quantile(mydata$RoamMins,probs = seq(0,1,0.05))
subset11=mydata[which(mydata$RoamMins>17.5),]  # Capping in the upper value
subset11$RoamMins=17.5
mydata[which(mydata$RoamMins>17.5),]=subset11
subset12=mydata[which(mydata$RoamMins<3.5),]  # Capping in the lower value
subset12$RoamMins=3.5
mydata[which(mydata$RoamMins<3.5),]=subset12
boxplot(mydata$RoamMins,main ="Boxplot of average number of roaming minutes",ylab = "average number of roaming minutes")
hist(mydata$RoamMins,main ="Histogram of average number of roaming minutes",xlab = "average number of roaming minutes")
qplot(mydata$RoamMins,data=mydata,fill=Churn)
ggplot(mydata,aes(RoamMins,fill=Churn,color=Churn))+geom_density(alpha=0.1)
g8=ggplot(mydata,aes(RoamMins,fill=Churn,color=Churn))+geom_density(alpha=0.1)

ggplot(mydata,aes(DataUsage,fill=DataPlan,color=DataPlan))+geom_density(alpha=0.1)
g9=ggplot(mydata,aes(DataUsage,fill=DataPlan,color=DataPlan))+geom_density(alpha=0.1)
ggplot(mydata,aes(MonthlyCharge,fill=DataPlan,color=DataPlan))+geom_density(alpha=0.1)
g10=ggplot(mydata,aes(MonthlyCharge,fill=DataPlan,color=DataPlan))+geom_density(alpha=0.1)
ggplot(mydata,aes(DayMins,fill=DataPlan,color=DataPlan))+geom_density(alpha=0.1)
g11=ggplot(mydata,aes(DayMins,fill=DataPlan,color=DataPlan))+geom_density(alpha=0.1)
grid.arrange(g1, g2, g3, g4, g5, g6,g7,g8,g9,g10, ncol =5 ,nrow=2)


qplot(mydata$ContractRenewal,data=mydata,fill=Churn)
counts1 = table(mydata$ContractRenewal, mydata$Churn)
barplot(counts1, main="Contract Renewal Status vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts1), beside=TRUE)
prop.table(table(mydata$ContractRenewal,mydata$Churn),1)*100
table(mydata$Churn, mydata$ContractRenewal)


qplot(mydata$DataPlan,data=mydata,fill=Churn)
counts2 = table(mydata$DataPlan, mydata$Churn)
barplot(counts2, main="Data Plan Status vs Churn Status",
      xlab="Churn Status No vs Yes", legend = rownames(counts2), beside=TRUE)
prop.table(table(mydata$DataPlan,mydata$Churn),1)*100
table(mydata$Churn, mydata$DataPlan)

#The probability of an account churning is higher if the account has not subscribed to a data plan.


table(mydata$ContractRenewal, mydata$DataPlan)


tab1=table(mydata$Churn) 
prop.table(tab1)
prop.table(table(mydata$Churn))*100
## In the given dataset,customers who has canceled service vs not canceled service is 14.49% and 85.51% respectively

#splitting the dataset into train and test dataset
set.seed(100)
train=createDataPartition(mydata$Churn,p=0.7,list=FALSE,times=1)
traindata=mydata[train,1:length(mydata)]
testdata=mydata[-train,1:length(mydata)]
dim(testdata)
dim(traindata)
prop.table(table(traindata$Churn))#85.48% of train data has not canceled and 14.52% has canceled service 
prop.table(table(testdata$Churn))#85.59% of train data has not canceled and 14.41% has canceled service 
table(traindata$Churn)

#Logistic Regression model

glm(traindata$Churn~.,data = traindata,family="binomial")
logmodel=glm(traindata$Churn~.,data = traindata,family="binomial")
summary(logmodel)

#AccountWeeks,Data Plan,DayMins,DayCalls and OverageFee seems to be less significant
#There could be multicollinearity
exp(coef(logmodel)) #Odds ratio
exp(coef(logmodel))/(1+exp(coef(logmodel))) #Probability

#Will create a null model
glm(traindata$Churn~1,data = traindata,family="binomial")
summary(glm(traindata$Churn~1,data = traindata,family="binomial"))

#To compare null model and the created model
lrtest(logmodel)
#p value is 2.2e-16. Null hypothesis is rejected. Hence the model is a valid one.

#Check for multicollinearity


vif(logmodel) #Heteroscedasticity test
#vif values of DataPlan,DataUsage,DayMins,MonthlyCharge,OverageFee are too high
#Hence there is multicollinearity
#Data Usage and Data Plan are highly correlated. Monthly Charge is also highly correlated with Data Usage, Data Plan and Day Mins. 


#Create a model after dropping DataUsage and Monthly Charge
glm(traindata$Churn~.,data = traindata[,-c(5,9)],family="binomial")
logmodel1=glm(traindata$Churn~.,data = traindata[,-c(5,9)],family="binomial")
summary(logmodel1)
exp(coef(logmodel1)) #Odds ratio
exp(coef(logmodel1))/(1+exp(coef(logmodel1))) #Probability

#AccountWeeks and DayCalls seem to be less significant

vif(logmodel1)
#The values are less than 5, hence there is no multicollinearity

logmodel1$coefficients
likelihood=exp(logmodel1$coefficients)
print(likelihood)
#If there is 1 unit change in CustServCalls, there is 1.64863516 units change in the odds of Churn being '1' 
#Probability=1.679939003/1+1.679939003 = 0.6268572
#If there is 1 unit increase in CustServCalls, probability of customer canceling the service increases by 62.24% 


#We shall predict on test data

predictTest=predict(logmodel1,newdata = testdata,type="response")
table(testdata$Churn,(predictTest>0.16))
#Confusion matrix with threshold of 0.5
table(testdata$Churn,(predictTest>0.5))
head(predictTest)

#Accuracy
sum(diag(table(testdata$Churn,(predictTest>0.5))))/nrow(testdata) #0.8648649
#This predicts well on test data
table(testdata$Churn)
prop.table(table(testdata$Churn))

#Test set AUC
ROCRPredictTest=prediction(predictTest,testdata$Churn)
perf1=performance(ROCRPredictTest,"tpr","fpr")
plot(perf1)
as.numeric(performance(ROCRPredictTest,"auc")@y.values) #AUC is 0.8189327
#If I build a model on my training dataset & then look at a new set of data, & pick from it 
#random customers who cancelled and not cancelled the service, then 82% of the time, the churned customers
#will have higher predicted churn and the non churn customers will have low predicted churn


#Step AIC Regression method

step_both=stepAIC(logmodel1,direction = "both")
summary(step_both)
#Given all the variables that are significant - ContractRenewal,DataPlan,CustServCalls,DayMins,OverageFee and RoamMins
#and gives the best model with AIC: 1595.9

#Model tuning and building model using balanced data
control=trainControl(method = "repeatedcv",number = 5,repeats = 3,sampling = "up")
model=train(Churn~.,data=traindata,trControl=control,method='glm')
model #Accuracy : 0.8517466
summary(model)
varImp(model)


#We shall predict on the test data

pred=predict(model,newdata = testdata)
table(testdata$Churn,pred)
sum(diag(table(testdata$Churn,pred)))/sum(table(testdata$Churn,pred))

#Accuracy of 75.77578
#Specificity and Sensitivity also shows that it is a good model


##KNN


set.seed(1)

scale=preProcess(traindata,method="range")
train.norm.data=predict(scale,traindata)
test.norm.data=predict(scale,testdata)


KNN.Status=knn(traindata[,-c(5,9)],testdata[,-c(5,9)],traindata$Churn,k=7)
table(testdata$Churn,KNN.Status)
t=table(testdata$Churn,KNN.Status)
sum(diag(t))/sum(t)
#Overall Accuracy of 85.49%

KNN.Status=knn(traindata,testdata,traindata$Churn,k=5)
table(testdata$Churn,KNN.Status)
t1=table(testdata$Churn,KNN.Status)
sum(diag(t1))/sum(t1)
#Overall Acuracy of 87.59%

control=trainControl(method = "repeatedcv",number = 5,repeats = 3,sampling = "up")
model2=train(Churn~.,data=traindata,trControl=control,method="knn")
model2 
summary(model2)
knn.fit=train(Churn~.,data=train.norm.data,method="knn",trControl=trainControl(method = "cv",number = 3),tuneLength=10)
knn.fit
#Accuracy was used to select the optimal model using the largest value.

#predict on test data
pred=predict(knn.fit,newdata=test.norm.data[-1],type="raw")
confusionMatrix(pred,test.norm.data$Churn,positive = "1")

#Naive Bayes

naiveBayes(Churn~.,data=traindata)
NB.Status=naiveBayes(Churn~.,data=traindata)

#Output gives prior probabilities   
#   0         1 
#0.8547558 0.1452442

#Churned customers have 102.4189 number of active account weeks with std deviation of 39.56451, 
#0.5861652 gigabytes of monthly data usage with std dev of 1.202507, 2.050147 calls made to customer service with std dev of 1.511443,
#206.1029 average daytime mins/month with std dev of 68.66804,100.7876 average daytime calls with std dev of 21.29996, 59.27109 of monthly charge with std dev of 15.74741, 
#10.612183 of largest overage fee in last 12 months with std dev of 2.432404, 10.80029 average roaming mins with std dev of 2.748679 

predict(NB.Status,type = "raw",newdata = testdata)
plot(testdata$Churn,predict(NB.Status,type = "raw",newdata = testdata)[,2])

predict(NB.Status,newdata = testdata)
p=predict(NB.Status,newdata = testdata)
table(testdata$Churn,p)
t=table(testdata$Churn,p)
sum(diag(t))/sum(t) #Accuracy - 86.58

