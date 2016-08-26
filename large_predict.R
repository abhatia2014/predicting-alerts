library(ggplot2)
library(data.table)
library(doParallel)
library(dplyr)
library(lattice)
library(caret)

#list all files available
list.files('.')
#if you want to load the RData rather than running the models again
load("predict.RData")
#registerDoParallel(4)
#getDoParWorkers()
#load up the file in memory
alerts=fread("alert_data_aankur.Q1_3months.2016.08.23.csv")
str(alerts)
head(alerts,3)
tail(alerts,3)
#change the industry to factor
alerts$industry=factor(alerts$industry)
#81 levels of indstries
levels(alerts$industry)

#change the src_geo and dst_geo to factors
alerts$src_geo=factor(alerts$src_geo)
alerts$dst_geo=factor(alerts$dst_geo)
#change the event_vendors  to factors
alerts$event_vendors=factor(alerts$event_vendors)
#remove event group 
alerts$event_groups=NULL
#change remedy_customer_id to factor
alerts$remedy_customer_id=factor(alerts$remedy_customer_id)
#total 243331 observations
#789 customers


str(alerts)

#data cleaning
#change it back to dataframe
names(alerts)
alerts=data.frame(alerts)
#change all date types to posixclt and factors to factor data type

alerts$attack_start_time=as.POSIXct(strptime(alerts$attack_start_time,"%Y-%m-%d %H:%M:%S"))

alerts$xps_alert_create=as.POSIXct(strptime(alerts$xps_alert_create,"%Y-%m-%d %H:%M:%S"))

alerts$rule_name=as.factor(alerts$rule_name)
#total 145 rules
alerts$ai_alert_soc_status=as.factor(alerts$ai_alert_soc_status)
# 8 levels of status


#find the total number of unique alerts
names(alerts)

rm(unique1)
#243331 unique alerts
#over 3 months of data
243331/90
#with an average of 2703 unique alerts per day
unique1=alerts
#extract the date out of the unique1 data set
unique1$day=format(unique1$xps_alert_create,format = '%d')
#find a table of alerts per day
table(unique1$day)
mean.y=round(mean(table(unique1$day)),0)
plot(table(unique1$day))

ggplot(unique1,aes(day,fill='red'))+geom_bar(stat='count')+
  geom_hline(yintercept = mean.y)+
  geom_text(aes(12,mean.y+200,label=paste("Average # alerts per day:",mean.y)))+
  labs(x="days for April 2016",y="Count",title="Average Alerts per day for April 2016")+theme(legend.position="none")
head(alerts)

#find distribution of alerts by attack start time

rm(unique1,mean.hour,mean.y)

##******************** Analysis***********************************************
##****************************************************************************

#1. time distribution between start of attack (logs recieved from devices) and alert created in the UC database

names(alerts)

#alerts$QRadar_Register.Time=as.numeric(difftime(alerts$attack_end_time,alerts$attack_start_time,units = "secs"))
#alerts$Total.alert.create.Time=as.numeric(difftime(alerts$xps_alert_create,alerts$attack_start_time,units="secs"))
alerts$xps.alert.create.Time=as.numeric(difftime(alerts$xps_alert_create,alerts$attack_start_time,units="secs"))

#remove time to create variable
alerts$time_to_create=NULL
#remove attack start time and xps alert create time variables
alerts$attack_start_time=NULL
alerts$xps_alert_create=NULL

names(alerts)
#examine each variable
summary(alerts$ai_alert_id)
#change alerts_id to factor
alerts$ai_alert_id=factor(alerts$ai_alert_id)
str(alerts)
summary(alerts)
#remove alerts that are suppressed
alerts=alerts[!alerts$ai_alert_soc_status=="SUPPRESSED",]

#remaining 175830 alerts, removed
243331-175830
#67501 alerts

rm(high_alert)




levels(alerts$ai_alert_soc_status)


# remove SUPRESSED, AuTO-ESCALATED, AUTO - ASSOciATED entries

alerts.trim=alerts%>%
  filter(ai_alert_soc_status=="ASSOCIATED" |ai_alert_soc_status=="CLOSED" |ai_alert_soc_status=="COMMENTED" |ai_alert_soc_status=="ESCALATED" )%>%
  mutate(ai_alert_soc_status=as.character(ai_alert_soc_status))

175830-140759
#removed 35071 alerts that were auto escalated, auto-associated, suppressed or auto_escalation_pending


#substitue Associated with Escalated and Commented with closed

alerts.trim$ai_alert_soc_status=sub("ASSOCIATED",replacement = "ESCALATED",x=alerts.trim$ai_alert_soc_status)
alerts.trim$ai_alert_soc_status=sub("COMMENTED",replacement = "CLOSED",x=alerts.trim$ai_alert_soc_status)
table(alerts.trim$ai_alert_soc_status)/140759*100
#63.51 % closed while 36.48% escalated
#put it back as a factor
alerts.trim$ai_alert_soc_status=factor(alerts.trim$ai_alert_soc_status)
levels(alerts.trim$ai_alert_soc_status)

str(alerts.trim)

names(alerts.trim)
t=data.frame(table(alerts.trim$ai_alert_soc_status))
ggplot(alerts.trim,aes(ai_alert_soc_status))+geom_bar(stat='count',aes(fill=ai_alert_soc_status))+
  labs(x="Alert Status",y="Alert Count",title='140K Unique Alert Status')+theme(legend.position="none")+
  geom_text(data=t,aes(Var1,Freq,label=Freq))

unique.alerts.trim.analysis=alerts.trim%>%
  group_by(rule_name,ai_alert_soc_status)%>%
  summarise(count=n())


#let's also do the same analysis by customers on the unique alert trim dataframe

unique.alerts.trim.cust=alerts.trim%>%
  group_by(remedy_customer_id,ai_alert_soc_status)%>%
  summarise(count=n())
rm(unique.alerts.trim.cust,unique.alerts.trim.analysis)
rm(t)

str(alerts.trim)
#lets look at the levels for src_geo
levels(alerts.trim$src_geo)
summary(alerts.trim$src_geo)

#where ever there is a ',' in the src_geo- means there are multiple regions for origination of attack

#first convert the field to character

alerts.trim$src_geo=as.character(alerts.trim$src_geo)

#find entries with ',' in between

commarecords=grep(",",x = alerts.trim$src_geo)

#replace all of these enteries by 'multiple'

alerts.trim$src_geo[commarecords]="MULTIPLE"

alerts.trim$src_geo[12:20]

#put the src_geo back as factor

alerts.trim$src_geo=factor(alerts.trim$src_geo)
str(alerts.trim)

#work similarly on the destination geo

levels(alerts.trim$dst_geo)
head(alerts.trim$dst_geo,20)
#where ever there is a ',' in the dst_geo- means there are multiple regions for origination of attack

#first convert the field to character

alerts.trim$dst_geo=as.character(alerts.trim$dst_geo)

#find entries with ',' in between

commarecords=grep(",",x = alerts.trim$dst_geo)

#replace all of these enteries by 'multiple'

alerts.trim$dst_geo[commarecords]="MULTIPLE"

alerts.trim$dst_geo[23:30]

#convert back to factor

alerts.trim$dst_geo=factor(alerts.trim$dst_geo)
str(alerts.trim)
#101 levels for the destination geo

###############**************************Model to predict False Positives**********************#####################
####################################################################################################################



rm(commarecords)

#load caret library

library(caret)

#create data partition- 65% training data, 35% test data

ID =createDataPartition(alerts.trim$ai_alert_soc_status,p = .65,list = FALSE)

#create a training and test dataset

trainalert=alerts.trim[ID,]
#training dataset has 91494 observations

#testing dataset has 49265 observations

testalert=alerts.trim[-ID,]

#setup the training control

ctrl=trainControl(method="repeatedcv",number=10,summaryFunction = twoClassSummary,
                  classProbs = TRUE,allowParallel = TRUE)

#we will not use any grid tuning parameters as of now

#using the XGBOOST package to build the first model, remove the xps alert creation time

library(pROC)
library(xgboost)
trainalert$xps.alert.create.Time=NULL
testalert$xps.alert.create.Time=NULL
trainalert$ai_alert_id=NULL
testID=testalert$ai_alert_id
testalert$ai_alert_id=NULL
#lets remove the customer_ID as the first try
train_customer_ID=trainalert$remedy_customer_id
trainalert$remedy_customer_id=NULL
test_customer_ID=testalert$remedy_customer_id
testalert$remedy_customer_id=NULL
#check for any missing values
sum(is.na(trainalert))
which(is.na(trainalert$dst_geo))
#32333 record is NA, replace with NULL
trainalert$dst_geo[32333]="NULL"
# also test for NA in test alert dataframe
sum(is.na(testalert))
#no NA in test alert
#we begin training the model
xgb.alert=train(ai_alert_soc_status~.,data=trainalert,
                method="xgbTree",
                metric="ROC",
                trControl=ctrl)
xgb.predict.prob=predict(xgb.alert,newdata=testalert,type="prob")
xgb.predict=predict(xgb.alert,newdata=testalert)
confusionMatrix(xgb.predict,testalert$ai_alert_soc_status)
xgb.predict.prob$alert_id=testID
xgb.predict.prob$actual=testalert$ai_alert_soc_status
xgb.predict.prob$predicted=xgb.predict
names(xgb.predict.prob)
xgb.predict.prob=xgb.predict.prob[,c(3,1,2,4,5)]

#try the same model with customer ID and see changes in accuracy
names(alerts.trim)
trainalert$remedy_customer_id=train_customer_ID
testalert$remedy_customer_id=test_customer_ID

xgb.alert2=train(ai_alert_soc_status~.,data=trainalert,
                method="xgbTree",
                metric="ROC",
                trControl=ctrl)
xgb.predict.prob2=predict(xgb.alert2,newdata=testalert,type="prob")
xgb.predict2=predict(xgb.alert2,newdata=testalert)
confusionMatrix(xgb.predict2,testalert$ai_alert_soc_status)
xgb.roc=roc(predictor=xgb.predict.prob2$CLOSED,response=testalert.y)
plot(xgb.roc)
names(xgb.alert2)
xgb.alert2$bestTune
xgb.alert2$results

# area under the curve is 84.62% which is awesome

#marginal increase in accuracy from 76.28% to 77.46%

#lets see the effect of xps alert creation time with customer ID

trainID=trainalert$ai_alert_id
trainalert$ai_alert_id=NULL
testID=testalert$ai_alert_id
testalert$ai_alert_id=NULL
#lets remove the customer_ID as the first try
train_customer_ID=trainalert$remedy_customer_id
#trainalert$remedy_customer_id=NULL
test_customer_ID=testalert$remedy_customer_id
#testalert$remedy_customer_id=NULL
#check for any missing values
sum(is.na(trainalert))
summary(trainalert)
which(is.na(trainalert$dst_geo))

#32333 record is NA, replace with NULL
trainalert$dst_geo[which(is.na(trainalert$dst_geo))]="NULL"
#all negative xps alert time should be =0
trainalert$xps.alert.create.Time[trainalert$xps.alert.create.Time<0]=0
#do the same for test alert
testalert$xps.alert.create.Time[testalert$xps.alert.create.Time<0]=0
#remove NAs in trainalert
trainalert=trainalert[!is.na(trainalert$xps.alert.create.Time),]
# also test for NA in test alert dataframe
sum(is.na(testalert))
summary(testalert)
#remove NAs fro test alert
testalert=testalert[!is.na(testalert$xps.alert.create.Time),]
#no NA in test alert
#we begin training the model
xgb.alert3=train(ai_alert_soc_status~.,data=trainalert,
                method="xgbTree",
                metric="ROC",
                trControl=ctrl)
xgb.predict3=predict(xgb.alert3,newdata=testalert)

confusionMatrix(xgb.predict3,testalert$ai_alert_soc_status)


#the accuracy has improved from 77.46% to 77.48% which is not a significant increase in accuracy- 
#sensitivity has improved from 89.99% to 90.64% which is not a significant increase in accuracy

#therefore we go back to the simpler model- xgb.predict2 as the best alternative

#save this work as RData

save.image("predict.RData")

#now predict using the GBM method

grid=expand.grid(interaction.depth=c(1,2),
                 n.trees=c(10,20),
                 shrinkage=c(0.01,0.1),
                 n.minobsinnode=20)

# get the x , y separately for gbm

trainalert.y=trainalert$ai_alert_soc_status
trainalert$ai_alert_soc_status=NULL
testalert.y=testalert$ai_alert_soc_status
testalert$ai_alert_soc_status=NULL

#remove the customer id for both training and test data
trainalert$remedy_customer_id=NULL
testalert$remedy_customer_id=NULL
gbm.alert=train(x=trainalert,y=trainalert.y,
                method="gbm",
                metric="ROC",
                tuneGrid=grid,
                trControl=ctrl)


predict.gbm=predict(gbm.alert,newdata=testalert)
predict.gbm.prob=predict(gbm.alert,newdata=testalert,type="prob")

confusionMatrix(testalert.y,predict.gbm)
#gbm accuracy is much worst than XGBoost accuracy here there is only 71.3% accuracy

#adding the customer ID back and checking for accuracy
trainalert$remedy_customer_id=train_customer_ID
testalert$remedy_customer_id=test_customer_ID
gbm.alert2=train(x=trainalert,y=trainalert.y,
                method="gbm",
                metric="ROC",
                tuneGrid=grid,
                trControl=ctrl)
predict.gbm2=predict(gbm.alert2,newdata=testalert)
confusionMatrix(testalert.y,predict.gbm2)
#accuracy for the GBM model improved from 71.3% to 75.7% and the sensitivity improved from 70.85% to 76.82%

#let us try and add more trees and see if the accuracy of the gbm model changes

grid2=expand.grid(interaction.depth=c(1,5,9),
                 n.trees=(1:30)*50,
                 shrinkage=c(0.01,0.1),
                 n.minobsinnode=20)

gbm.alert3=train(x=trainalert,y=trainalert.y,
                 method="gbm",
                 metric="ROC",
                 tuneGrid=grid2,
                 trControl=ctrl)
predict.gbm3=predict(gbm.alert3,newdata=testalert)
predict.gbm3.prob=predict(gbm.alert3,newdata=testalert,type='prob')
confusionMatrix(testalert.y,predict.gbm3)
roc(predictor=predict.gbm3.prob$CLOSED,response=testalert.y)
#area under the curve for the gbm modified model is 86.71% which is higher than xgb.alert2 model (84.62%)- meaning both models are compatible
#by adding more trees (grid1) the accuracy improved to 79.46% from 75.7% and the sensitivity improved from 76.8% to 82.25% which is a significant gain
#another gbm alert model, base only predicting on customer ID, rules, industry

names(trainalert)
trainalert$remedy_customer_id=train_customer_ID
testalert$remedy_customer_id=test_customer_ID
trainalert$src_geo=NULL
testalert$src_geo=NULL

trainalert$dst_geo=NULL
testalert$dst_geo=NULL

trainalert$remedy_customer_id=NULL
testalert$event_vendors=NULL

gbm.alert4=train(x=trainalert,y=trainalert.y,
                 method="gbm",
                 metric="ROC",
                 tuneGrid=grid,
                 trControl=ctrl)

predict.gbm4=predict(gbm.alert4,newdata=testalert)
predict.gbm4.prob=predict(gbm.alert4,newdata=testalert,type='prob')
confusionMatrix(testalert.y,predict.gbm4)
roc(predictor=predict.gbm4.prob$CLOSED,response=testalert.y)

# save the models
save.image("predict.RData")
ggplot(xgb.alert2)
names(xgb.alert2)
xgb.alert2$bestTune
xgb.alert2$results
xgb.alert2$finalModel
#check the ROC and area under curve
varImp(gbm.alert)
varImp(xgb.alert2)
varImp(xgb.alert3)

#predict using adaboost and with customer ID as a variable
trainalert.y=trainalert$ai_alert_soc_status
trainalert$ai_alert_soc_status=NULL
testalert.y=testalert$ai_alert_soc_status
testalert$ai_alert_soc_status=NULL

boosttree.alert=train(x=trainalert,y=trainalert.y,
                      method="ada",
                      metric="ROC",
                      trControl=ctrl)

predict.boostree=predict(boosttree.alert,newdata=testalert)
confusionMatrix(predict.boostree,testalert.y)
varImp(boosttree.alert)
plot(varImp(boosttree.alert))
names(boosttree.alert)
boosttree.alert$bestTune
boosttree.alert$results
plot(boosttree.alert)
ggplot(boosttree.alert)+theme(legend.position="bottom")

library(randomForest)

#try out the C5 

C5.alert=train(x=trainalert,y=trainalert.y,
               method='C5.0',
               metric="ROC",
               trControl = ctrl)
predict.c5=predict(C5.alert,newdata = testalert)
predict.c5.prob=predict(C5.alert,newdata = testalert,type="prob")
confusionMatrix(predict.c5,testalert.y)
#prediction accuracy of 79.39% and a sensitivity of 87.77% which is great

names(C5.alert)
C5.alert$results
C5.alert$bestTune
library(pROC)
C5.roc=roc(predictor=predict.c5.prob$CLOSED,response=testalert.y)
plot(C5.roc)
#area under the curve is 86.61%
plot(C5.alert)

#finally we'll try and plot the C5 model again with preprocessing
#also removed the event vendor status
# and added preprocess- PCA to the mix
trainalert$event_vendors=NULL
testalert$event_vendors=NULL
C5.alert2=train(x=trainalert,y=trainalert.y,
               method='C5.0',
               metric="ROC",
               preProcess = "pca",
               trControl = ctrl)
predict.c5.2=predict(C5.alert2,newdata = testalert)
predict.c5.2.prob=predict(C5.alert2,newdata = testalert,type="prob")
confusionMatrix(predict.c5.2,testalert.y)
varImp(C5.alert2)
#prediction accuracy of 79.18% and a sensitivity of 89.23% which is great

#let's try one more model- adabag
adabag.alert=train(x=trainalert,y=trainalert.y,
                method='AdaBag',
                metric="ROC",
                tuneLength = 30,
                trControl = ctrl)

rvalues=resamples(list(xgb.no.customer=xgb.alert,xgb.customer=xgb.alert2,xgb.alertcreate.time=xgb.alert3,
                       gbm.no.customer=gbm.alert,gbm.customer=gbm.alert2,gbm.more.trees=gbm.alert3,gbm.cust.rule.ind=gbm.alert4,
                       C5=C5.alert,C5.preprocess=C5.alert2))


dotplot(rvalues,metric="ROC",main="Prediction Capabilities: \n xgboost, GBM, AdaBoost, C5")
names(rvalues)
rvalues$metrics
dotplot(rvalues,metric = "Sens")
dotplot(rvalues,metric='Spec')
gbm.alert$results
bwplot(rvalues,metric="ROC")
ggplot(rvalues)
rm(boosttree.alert)
gc()
