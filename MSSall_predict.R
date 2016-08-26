library(ggplot2)
library(data.table)
library(doParallel)
library(dplyr)
library(lattice)
library(caret)

#we can load Rdata directly or proceed through the analysis
load("Mydata.RData")
#registerDoParallel(4)
#getDoParWorkers()
alerts=fread("alert_worklogs_aankur.april2016.csv")
head(alerts,3)
tail(alerts,3)


#total 678,821 observations

str(alerts)

#data cleaning
#change it back to dataframe

alerts=data.frame(alerts)
#change all date types to posixclt and factors to factor data type

alerts$attack_start_time=as.POSIXct(strptime(alerts$attack_start_time,"%Y-%m-%d %H:%M:%S"))
alerts$attack_end_time=as.POSIXct(strptime(alerts$attack_end_time,"%Y-%m-%d %H:%M:%S"))
alerts$xps_alert_create=as.POSIXct(strptime(alerts$xps_alert_create,"%Y-%m-%d %H:%M:%S"))
alerts$log_date=as.POSIXct(strptime(alerts$log_date,"%Y-%m-%d %H:%M:%S"))
alerts$rule_name=as.factor(alerts$rule_name)
alerts$ai_alert_soc_status=as.factor(alerts$ai_alert_soc_status)
alerts$submitter=as.factor(alerts$submitter)
alerts$sla_metric_type=as.factor(alerts$sla_metric_type)
alerts$remedy_customer_id=as.factor(alerts$remedy_customer_id)

#find the total number of unique alerts
names(alerts)
unique1=alerts[, c(1:7)]
unique1=unique(unique1)
#79194 unique alerts
#over 30 days
#with an average of 2639.8 unique alerts per day

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
unique1$hour=format(unique1$attack_start_time,format="%H")

#lets find the distribution of alerts by hour of the day
mean.hour=round(mean(table(unique1$hour)),0)
table(unique1$hour)
#and plot it
ggplot(unique1,aes(hour))+geom_bar(stat='count')+
  geom_hline(yintercept = mean.hour)+
  geom_text(aes(12,mean.hour+200,label=paste("Average # alerts started per hour:",mean.hour)))+
  labs(x="hours for April 2016",y="Count",title="Average Alerts (Attacks) started per hour for April 2016")

#find the number of unique customers for the month of April

length(unique(unique1$remedy_customer_id))

#631 unique customers

#79194 unique alerts

#number of unique rules
length(unique(unique1$rule_name))

#110 unique rules

# average number of rules fired 
79194/110
table(unique1$rule_name)

#find the top 5 rules fired frequency

rules.count=unique1%>%
  select(rule_name)%>%
  group_by(rule_name)%>%
  summarise(count=n())%>%
  arrange(desc(count))
median(rules.count$count)

table(unique1$ai_alert_soc_status)
rm(unique1,rules.count,mean.hour,mean.y)

##******************** Analysis***********************************************
##****************************************************************************

#1. time distribution between start of attack (logs recieved from devices) and alert created in the UC database

names(alerts)

alerts$QRadar_Register.Time=as.numeric(difftime(alerts$attack_end_time,alerts$attack_start_time,units = "secs"))
alerts$Total.alert.create.Time=as.numeric(difftime(alerts$xps_alert_create,alerts$attack_start_time,units="secs"))
alerts$xps.alert.create.Time=as.numeric(difftime(alerts$xps_alert_create,alerts$attack_end_time,units="secs"))

#summarize and plot the distribution of time for QRadar response for all 20 alerts

#remove all Suppressed SOC Status
#using Data Table

alerts=alerts[!alerts$ai_alert_soc_status=="SUPPRESSED",]

#remaining 558714 observations, removed
678821-558714
#120107 observations

#find all alerts that have response time >3600 secs (1 hr)

high_response=alerts%>%
  select(ai_alert_id,ai_alert_soc_status,rule_name,QRadar_Register.Time,Total.alert.create.Time)%>%
  rename(system.response.time=QRadar_Register.Time)%>%
  filter(system.response.time>3600)%>%
  arrange(desc(system.response.time))

#total 64597 observations- find the unique ones

high_response=unique(high_response)
#5372 unique alerts

#find the total number of unique alerts
str(alerts$ai_alert_id)
u=length(alerts$ai_alert_id)

#%tage of observations that have very response time in excess of 3600 secs (6.78% of the alerts have response time in excess of 3600 sec (1 hr))

length(high_response$ai_alert_id)/u


#find the outliers in the distribution that have alert creation time > 1 hr (3600 sec)

high_alert=alerts%>%
  select(ai_alert_id,ai_alert_soc_status,rule_name,QRadar_Register.Time,Total.alert.create.Time)%>%
  rename(system.alert.time=Total.alert.create.Time)%>%
  filter(system.alert.time>3600)%>%
  arrange(desc(system.alert.time))

# total 25005 observations. How many unique alerts ?
high_alert=unique(high_alert)

#2657 unique alerts that have high alert creation time
#out of a total of 79194 alerts, that constitues 3.35% 
2657/u


#we  remove the observations with high response 

highres=high_response$ai_alert_id

#removed all high response alerts
alerts=alerts[!alerts$ai_alert_id %in% highres,]
# entries removed
558714-494117
#64597 entries removed

#remove all entries related to high alert creation
highalert=high_alert$ai_alert_id
alerts=alerts[!alerts$ai_alert_id %in% highalert,]

494117-474386

#19731 enteries removed, rm high_alert and high_response variables
rm(highalert,highres)
rm(high_alert,high_response)

length(unique(alerts$rule_name))
#total 96 rules
names(alerts)
unique.alerts=alerts%>%
  select(ai_alert_id,remedy_customer_id,rule_name,ai_alert_soc_status)
unique.alerts=unique(unique.alerts)

str(unique.alerts)

levels(unique.alerts$ai_alert_soc_status)

# remove SUPRESSED, AuTO-ESCALATED, AUTO - ASSOciATED entries

unique.alerts.trim=unique.alerts%>%
  filter(ai_alert_soc_status=="ASSOCIATED" |ai_alert_soc_status=="CLOSED" |ai_alert_soc_status=="COMMENTED" |ai_alert_soc_status=="ESCALATED" )%>%
  mutate(ai_alert_soc_status=as.character(ai_alert_soc_status))

str(unique.alerts.trim) 

#substitue Associated with Escalated and Commented with closed

unique.alerts.trim$ai_alert_soc_status=sub("ASSOCIATED",replacement = "ESCALATED",x=unique.alerts.trim$ai_alert_soc_status)
unique.alerts.trim$ai_alert_soc_status=sub("COMMENTED",replacement = "CLOSED",x=unique.alerts.trim$ai_alert_soc_status)

#put it back as a factor
unique.alerts.trim$ai_alert_soc_status=factor(unique.alerts.trim$ai_alert_soc_status)
head(unique.alerts.trim)
str(unique.alerts.trim)
round(table(unique.alerts.trim$ai_alert_soc_status)/length(unique.alerts.trim$ai_alert_soc_status)*100,2)
names(unique.alerts.trim)
t=data.frame(table(unique.alerts.trim$ai_alert_soc_status))
ggplot(unique.alerts.trim,aes(ai_alert_soc_status))+geom_bar(stat='count',aes(fill=ai_alert_soc_status))+
  labs(x="Alert Status",y="Alert Count",title='38K Unique Alert Status')+theme(legend.position="none")+
  geom_text(data=t,aes(Var1,Freq,label=Freq))
                        
unique.alerts.trim.analysis=unique.alerts.trim%>%
  group_by(rule_name,ai_alert_soc_status)%>%
  summarise(count=n())
names(unique.alerts.trim.analysis)
unique.alerts.trim.analysis$ai_alert_soc_status=as.character(unique.alerts.trim.analysis$ai_alert_soc_status)
unique.alerts.trim.analysis$ai_alert_soc_status=sub("CLOSED","FALSE_POSITIVE",unique.alerts.trim.analysis$ai_alert_soc_status)
levels(unique.alerts.trim.analysis$ai_alert_soc_status)
unique.alerts.trim.analysis$ai_alert_soc_status=factor(unique.alerts.trim.analysis$ai_alert_soc_status,levels=c("FALSE_POSITIVE","ESCALATED"))
table(unique.alerts.trim.analysis$ai_alert_soc_status)
ggplot(unique.alerts.trim.analysis[sample(nrow(unique.alerts.trim.analysis),50,replace=FALSE),],aes(rule_name,count,fill=ai_alert_soc_status))+geom_bar(stat="identity")+
  coord_flip()+theme(legend.position="bottom")+scale_color_manual(values = c("blue","red"))

#let's also do the same analysis by customers on the unique alert trim dataframe

unique.alerts.trim.cust=unique.alerts.trim%>%
  group_by(remedy_customer_id,ai_alert_soc_status)%>%
  summarise(count=n())
names(unique.alerts.trim.cust)
unique.alerts.trim.cust$ai_alert_soc_status=as.character(unique.alerts.trim.cust$ai_alert_soc_status)
unique.alerts.trim.cust$ai_alert_soc_status=sub("CLOSED","FALSE_POSITIVE",unique.alerts.trim.cust$ai_alert_soc_status)
levels(unique.alerts.trim.cust$ai_alert_soc_status)
unique.alerts.trim.cust$ai_alert_soc_status=factor(unique.alerts.trim.cust$ai_alert_soc_status,levels=c("FALSE_POSITIVE","ESCALATED"))
table(unique.alerts.trim.cust$ai_alert_soc_status)
ggplot(unique.alerts.trim.cust[sample(nrow(unique.alerts.trim.cust),30,replace=FALSE),],aes(remedy_customer_id,count,fill=ai_alert_soc_status))+geom_bar(stat="identity")+
  coord_flip()+theme(legend.position="bottom")+scale_color_manual(values = c("blue","red"))

###############**************************Model to predict False Positives**********************#####################
####################################################################################################################

#use the unique alerts trim dataframe removing the auto-escalated, auto closed and suppressed status

#get the total alert create time from the alerts database

names(alerts)
names(unique.alerts.trim)
rm(unique.alerts.trim.analysis,unique.alerts.trim.cust)

unique.alerts.trim$alert_create_time=alerts$Total.alert.create.Time[match(unique.alerts.trim$ai_alert_id,alerts$ai_alert_id)]

str(unique.alerts.trim)

#remove the alert id from the dataframe
unique.alerts.trim$ai_alert_id=NULL
length(unique(unique.alerts.trim$remedy_customer_id))

#load caret library

library(caret)

ID =createDataPartition(unique.alerts.trim$ai_alert_soc_status,p = .65,list = FALSE)

#create a training and test dataset

trainalert=unique.alerts.trim[ID,]
#training dataset has 25301 observations

#testing dataset has 13622 observations

testalert=unique.alerts.trim[-ID,]

#setup the training control

ctrl=trainControl(method="repeatedcv",number=10,summaryFunction = twoClassSummary,
                  classProbs = TRUE,allowParallel = TRUE)

#we will not use any grid tuning parameters as of now

#using the XGBOOST package to build the first model

library(pROC)
library(xgboost)

grid=expand.grid(interaction.depth=c(1,2),
                 n.trees=c(10,20),
                 shrinkage=c(0.01,0.1),
                 n.minobsinnode=20)

# get the x , y separately for gbm

trainalert.y=trainalert$ai_alert_soc_status
trainalert$ai_alert_soc_status=NULL

gbm.alert=train(x=trainalert,y=trainalert.y,
               method="gbm",
               metric="ROC",
               tuneGrid=grid,
               trControl=ctrl)

testalert.y=testalert$ai_alert_soc_status
testalert$ai_alert_soc_status=NULL

predict.gbm=predict(gbm.alert,newdata=testalert)
predict.gbm.prob=predict(gbm.alert,newdata=testalert,type="prob")

confusionMatrix(testalert.y,predict.gbm)

predict.gbm.prob$actual=testalert.y
predict.gbm.prob$predicted=predict.gbm
roc.gbm=roc(predictor=predict.gbm.prob$CLOSED,response = testalert.y)

plot(roc.gbm)
plot(gbm.alert)
table(predict.gbm.prob$actual)
table(predict.gbm.prob$predicted)


#using xgboost

trainalert$ai_alert_soc_status=trainalert.y
testalert$ai_alert_soc_status=testalert.y
xgb.alert=train(ai_alert_soc_status~.,data=trainalert,
                method="xgbTree",
                metric="ROC",
                trControl=ctrl)


predict.xgb.prob=predict(xgb.alert,testalert,type="prob")
predict.xgb.prob$actual=testalert.y
predict.xgb.prob$predicted=predict.xgb
predict.xgb.prob$remedy_customer_id=testalert$remedy_customer_id
names(predict.xgb.prob)
predict.xgb.prob=predict.xgb.prob[,c(5,1:4)]

predict.xgb=predict(xgb.alert,testalert)
confusionMatrix(predict.xgb,testalert.y)
library(pROC)
xgb.roc=roc(predictor=predict.xgb.prob$CLOSED,response=testalert$ai_alert_soc_status)
plot(xgb.roc,main="XGBoost prediction ROC Curve")
auc(xgb.roc)

#once check for predicting using Bagged Ada Boost

trainalert.y=trainalert$ai_alert_soc_status
trainalert$ai_alert_soc_status=NULL
testalert.y=testalert$ai_alert_soc_status
testalert$ai_alert_soc_status=NULL

boosttree.alert=train(x=trainalert,y=trainalert.y,
               method="ada",
               metric="ROC",
               trControl=ctrl)
predict.boosttree=predict(boosttree.alert,newdata=testalert)
predict.boosttree.prob=predict(boosttree.alert,newdata=testalert,type="prob")
cm.boost=confusionMatrix(predict.boosttree,testalert.y,positive="CLOSED")
?sensitivity

names(cm.boost)
cm.boost$table
cm.boost$overall[[1]]
cm.boost$byClass
bsttree.roc=roc(predictor=predict.boosttree.prob$CLOSED,response=testalert.y)
plot(bsttree.roc,main="Boosted Tree ROC")

names(boosttree.alert)
boosttree.alert$bestTune
plot(boosttree.alert)

#comparing the different models
rvalues=resamples(list(xgb=xgb.alert,gbm=gbm.alert,adaboost=boosttree.alert))
names(rvalues)

rvalues$values
rvalues$models
rvalues$metrics

summary(rvalues)
bwplot(rvalues,metric="ROC",main="Prediction Capabilities: \n XGBOOST Vs GBM Vs AdaBoost")
dotplot(rvalues,metric="ROC",main="Prediction Capabilities: \n XGBOOST Vs GBM Vs AdaBoost")

bwplot(rvalues,layout=c(1,3))
splom(rvalues)







#***********************check how many are predicted correctly with an existing dataset*****************************

alerts_13.07=read.csv("C:/Users/IBM_ADMIN/Documents/Datasets/WatsonSec/alert_worklogs_aankur.2016.07.13.csv")
str(alerts_13.07)
names(alerts_13.07)
alerts_13.07=alerts_13.07[,c(2,3,4,5,7)]
alerts_13.07$attack_start_time=as.POSIXct(strptime(alerts_13.07$attack_start_time,"%Y-%m-%d %H:%M:%S"))

alerts_13.07$xps_alert_create=as.POSIXct(strptime(alerts_13.07$xps_alert_create,"%Y-%m-%d %H:%M:%S"))
alerts_13.07$Total.alert.create.Time=as.numeric(difftime(alerts_13.07$xps_alert_create,alerts_13.07$attack_start_time,units="secs"))
names(trainalert)
names(alerts_13.07)
alerts_13.07=alerts_13.07[,c(1,2,3,6)]
alerts_13.07=unique(alerts_13.07)
testalert2=alerts_13.07$ai_alert_soc_status
testalert2=as.character(testalert2)
testalert2=sub("ASSOCIATED",replacement = "ESCALATED",x=testalert2)
testalert2=sub("COMMENTED",replacement = "CLOSED",x=testalert2)
testalert2=factor(testalert2)


alerts_13.07$ai_alert_soc_status=NULL
names(trainalert)
colnames(alerts_13.07)=names(trainalert)

levels(alerts_13.07$remedy_customer_id)=levels(trainalert$remedy_customer_id)
levels(alerts_13.07$rule_name)=levels(trainalert$rule_name)
predict.boosttree2=predict(boosttree.alert,newdata=alerts_13.07)

confusionMatrix(predict.boosttree2,testalert2)
#predicting using gbm
predict.gbm2=predict(gbm.alert,newdata=alerts_13.07)
confusionMatrix(predict.gbm2,testalert2)
