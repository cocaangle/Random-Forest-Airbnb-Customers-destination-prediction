users=read.csv("train_users_2.csv")
str(users)
dim(users)
head(users)
summary(users)

## I.Data preprocessing
# 1.1 find missing values(unknown, NA, blank)
table(is.na(users))
colSums(is.na(users))

count_missing=function(x)return(sum(is.na(x)))
df_missing=lapply(users,count_missing)
df_missing
df_missing=df_missing[df_missing>0]
df_missing

#1.2 Data cleaning

summary(users$country_destination)
prop.table(table(users$country_destination))
library(ggplot2)
ggplot(users,aes(users$country_destination))+geom_bar(fill="red")

#1.2.1 create a new country including only US NDF Outside of US
users$country=users$country_destination
users$country=factor(ifelse(users$country_destination=="US","US",
                            ifelse(users$country_destination=="NDF","NDF","Outside US")))
unique(users$country)
prop.table(table(users$country))



#1.2.2 pre-process Gender
class(users$gender)
#convert unknown to blank, otherwise it will effect visualization result
summary(users$gender)
users$gender=factor(ifelse(users$gender=="-unknown-","",as.character(users$gender)))
unique(users$gender)
# country=NDF
users%>%count(country,gender)%>%filter(gender!=""&country=="NDF")%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
df_ndf=users[users$gender==""&users$country=="NDF",]
set.seed(1)
df_ndf_f=users%>%filter(gender==""&country=="NDF")%>%sample_n(round(66670*0.54,0))
female=df_ndf_f$id
male=setdiff(df_ndf$id,df_ndf_f$id)
users$gender=ifelse(users$id%in%female,"FEMALE",
                    ifelse(users$id%in%male,"MALE",as.character(users$gender)))
users%>%filter(country=="NDF")%>%count(gender)
#country=US
users%>%count(country,gender)%>%filter(gender!=""&country=="US")%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
df_ndf=users[users$gender==""&users$country=="US",]
set.seed(1)
df_ndf_f=users%>%filter(gender==""&country=="US")%>%sample_n(round(20109*0.54,0))
female=df_ndf_f$id
male=setdiff(df_ndf$id,df_ndf_f$id)
users$gender=ifelse(users$id%in%female,"FEMALE",
                    ifelse(users$id%in%male,"MALE",as.character(users$gender)))
users%>%filter(country=="US")%>%count(gender)
# country=outside US
users%>%count(country,gender)%>%filter(gender!=""&country=="Outside US")%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
df_ndf=users[users$gender==""&users$country=="Outside US",]
set.seed(1)
df_ndf_f=users%>%filter(gender==""&country=="Outside US")%>%sample_n(round(8909*0.53,0))
female=df_ndf_f$id
male=setdiff(df_ndf$id,df_ndf_f$id)
users$gender=ifelse(users$id%in%female,"FEMALE",
                    ifelse(users$id%in%male,"MALE",as.character(users$gender)))
users%>%filter(country=="Outside US")%>%count(gender)
summary(users$country)
#1.2.3 create a new language including en and non¡ªen
summary(users$language)
users$lang=users$language
users$lang=factor(ifelse(users$language=="en","en","non-en"))
unique(users$lang)
ggplot(users,aes(users$lang))+geom_bar(fill="red")

#1.2.4 create a new afiliate_prov including:
#direct,google,craigslist,bing,facebook and others
summary(users$affiliate_provider)
users$affiliate_prov=users$affiliate_provider
users$affiliate_prov=factor(ifelse(users$affiliate_provider=="direct","direct",
                                   ifelse(users$affiliate_provider=="google","google",
                                          ifelse(users$affiliate_provider=="craigslist","craigslit",
                                                 ifelse(users$affiliate_provider=="bing","bing",
                                                        ifelse(users$affiliate_provider=="facebook","facebook","others"))))))
unique(users$affiliate_prov)
summary(users$affiliate_prov)

#1.2.5 pre-processing age
summary(users$age)
#Max age and min age from what we see now is unrealistic,so I predict age>1924 are birth years
users$age=ifelse((users$age>=1924)&(users$age<=2014),2018-users$age,users$age)
summary(users$age)
#convert any age is below 10 and 100 to mean
users$age=ifelse((users$age>=10)&(users$age<=100),users$age,mean(users$age))
summary(users$age)
#convert all NA values to mean
users$age[is.na(users$age)]=round(mean(users$age,na.rm=TRUE))
summary(users$age)
colSums(is.na(users))

#Age distribution
users$age_dis=users$age
users$age_dis=factor(ifelse((users$age>=15)&(users$age<26),"15-25",
                            ifelse((users$age>=26)&(users$age<37),"26-36",
                            ifelse((users$age>=37)&(users$age<48),"37-47",
                            ifelse((users$age>=48)&(users$age<59),"48-58",
                            ifelse((users$age>=59)&(users$age<70),"59-69",
                            ifelse((users$age>=70)&(users$age<81),"70-81",
                            ifelse((users$age>=81)&(users$age<92),"81-91",
                            ifelse((users$age>=92),"92+","others")))))))))
unique(users$age_dis)
head(users)

#age distribution when country destination = NDF
 users%>%count(age_dis,country)%>%filter(age_dis!=""&country=="NDF")%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
#age distribution when country destination = US
 users%>%count(age_dis,country)%>%filter(age_dis!=""&country=="US")%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
#age distribution when country destination = Outside US
 users%>%count(age_dis,country)%>%filter(age_dis!=""&country=="Outside US")%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
 
# 1.2.6 Break out data_account_created by year,month and date
 library(stringr)
 users$date_account_created
 class(users$date_account_created)
 doc=as.data.frame(str_split_fixed(users$date_account_created,"/",3))
 users$doc=as.data.frame(lapply(doc,as.factor))
 users["doc_year"]=doc[,3]
 users["doc_date"]=doc[,2]
 users["doc_month"]=doc[,1]
 users$doc=NULL
 
# 1.2.7 Break out data_first_booking
 dfb=as.data.frame(str_split_fixed(users$date_first_booking,"/",3))
 users["dfb_year"]=dfb[,3]
 users["dfb_date"]=dfb[,2]
 users["dfb_month"]=dfb[,1]
 
 #write CSV into your desktop
 write.csv(users,file="cleandatausers.csv")
 
##II. EDA
#2.1 signup_method and signup_app vs country
 summary(users$signup_method)
 summary(users$signup_app)
 library(dplyr)
 
 g1=users%>%count(country,signup_app)%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
 ggplot(g1,aes(country,pct))+geom_bar(stat="identity",aes(fill=signup_app))+theme_bw()
 #Insights: there's no difference among different countries on signup_app, the use of signup_app is
 #Web>>IOS>Andriod>Moweb
 
 g2=users%>%count(country,signup_method)%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
 ggplot(g2,aes(country,pct))+geom_bar(stat="identity",aes(fill=signup_method))+theme_bw()
 #Insights: there's no difference among different countries on signup_method, the use frequency is
 # basic>>facebook>>google

#2.2 age distribution VS country
 g3=users%>%count(age_dis,country)%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
 g3
 ggplot(g3, aes(age_dis,pct))+geom_bar(stat="identity",aes(fill=country))
#insight: the age distribution is concentrated between 26-48; for US,the largest proportion of age_dis
#is 26-36, for NDF and outside US, is 37-47, which proves that young adults are the main consumers for airbnb

#2.3 Gender VS country
 g4=users%>%count(gender,country)%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
 g4
 ggplot(g4, aes(country,pct))+geom_bar(stat="identity",aes(fill=gender))
#Insight:there's no difference on the gender of users in different country destinations. Usually, the pct of female
 # is larger than that of male.

 #2.4 lang VS country
 g5=users%>%count(lang,country)%>%ungroup%>%group_by(country)%>%mutate(pct=round(n/sum(n),2))
 g5
 ggplot(g5, aes(country,pct))+geom_bar(stat="identity",aes(fill=lang))
#Insight: People who speak English >>people who speak other languages in all country desinations, especially
#for US

 #2.5 doc month VS doc year
 g6=users%>%count(doc_month,doc_year)%>%ungroup%>%group_by(doc_year)%>%mutate(pct=round(n/sum(n),2))
 g6
 ggplot(g6,aes(doc_year,pct))+geom_bar(stat="identity",aes(fill=doc_month))
#Insight: from year 10-13, the people who created account was concentrated from month 10-12 and month 7-9;
#while in year of 14, this has been changed to 4-6, and the pct for 10-12 is almost 0.
 
#2.6 dfb month VS dfb year
 g7=users%>%count(dfb_month,dfb_year)%>%ungroup%>%group_by(dfb_year)%>%mutate(pct=round(n/sum(n),2))
 g7
 ggplot(g7,aes(dfb_year,pct))+geom_bar(stat="identity",aes(fill=dfb_month))
 
#Insight: the distribution of first time booking is the same as that of date create account;
#from 10-13,the people who made first booking was concentrated from month 10-12 and month 7-9;
#while in year of 14 and 15, this has been changed to month 3-5, and the pct for 10-12 is almost 0.

#III Modeling
#3.1 Logistic Regression

data=read.csv("cleandatausers.csv")

da=data.frame(data)
colSums(is.na(da))
summary(da)
#filter columns
library(dplyr)
data1=select(da,gender:dfb_month)
str(data)
del_col=c("age_dis","dfb_year","dfb_month","dfb_date","affiliate_prov","lang","country")
data1=data1[,-which(names(data1)%in%del_col)]
names(data1)
#check whether there is any missing or NA values
colSums(is.na(data1))
attach(data1)
str(data1);summary(data1)
#convert data format of each column
data1$signup_flow=as.factor(data1$signup_flow)
data1$doc_date=as.factor(data1$doc_date)
data1$doc_month=as.factor(data1$doc_month)
data1$doc_year=as.factor(data1$doc_year)
data1$signup_method=as.factor(data1$signup_method)
data1$age=as.numeric(data1$age)
str(data1)
colSums(is.na(data1))
data1$countrylg=factor(ifelse(data1$country=="US","US","Outside US"))
data1$countrymodel=ifelse(data1$countrylg=="US",1,0)
unique(data1$countrymodel)
prop.table(table(data1$countrymodel))
str(data1)
data1$countrylg=NULL
str(data1)
data1$countrymodel=as.factor(data1$countrymodel)
str(data1)
#modeling
fit=glm(countrymodel~.,data=data1, family="binomial")
summary(fit)
pred_prob = predict(fit, newdata = data1, type = "response")
pred_prob
pred_US = ifelse(pred_prob > 0.5,1,0)
table = table(pred_US, data1$countrymodel) #confusion matrix
table
sum(diag(table))/sum(table) #Accurancy
rate = 1 - sum(diag(table))/sum(table)
rate

#Load test data set
test=read.csv("test_users.csv")
library(dplyr)
test1=test%>%select(-id,-timestamp_first_active,-date_account_created,-date_first_booking,-timestamp_first_active)
library(stringr)
test_doc=as.data.frame(str_split_fixed(test$date_account_created,"/",3))
test1$doc=as.data.frame(lapply(test_doc,as.factor))
test1["doc_year"]=test_doc[,3]
test1["doc_month"]=test_doc[,1]
test1["doc_date"]=test_doc[,2]
test1$doc=NULL

test1$age=as.numeric(test1$age)
test1$signup_flow=as.factor(test1$signup_flow)
test1$country_destination=as.factor(test1$country_destination)
pred_prob=predict(fit,newdata=test1,type="class")
table1=table(pred,test1$country_destination)
1-sum(diag(table1))/sum(table1)
#3.2 Random forest

data=read.csv("cleandatausers.csv")
da=data.frame(data)
colSums(is.na(da))
summary(da)
#filter columns
library(dplyr)
data1=select(da,gender:dfb_month)
str(data)
del_col=c("age_dis","dfb_year","dfb_month","dfb_date","affiliate_prov","lang","country")
data1=data1[,-which(names(data1)%in%del_col)]
names(data1)
#check whether there is any missing or NA values
colSums(is.na(data1))
attach(data1)
str(data1);summary(data1)
#convert data format of each column
data1$signup_flow=as.factor(data1$signup_flow)
data1$doc_date=as.factor(data1$doc_date)
data1$doc_month=as.factor(data1$doc_month)
data1$doc_year=as.factor(data1$doc_year)
data1$signup_method=as.factor(data1$signup_method)
data1$age=as.numeric(data1$age)
str(data1)
colSums(is.na(data1))

#modeling
library(randomForest)
set.seed(123)
rf=randomForest(country_destination~.,data=data1,ntree=100,importance=TRUE,na.action = na.omit)
rf
rf$err.rate
importance(rf)
varImPlot(rf)
pred=predict(rf,newdata=data1,type="class")
pred##score
table=table(datda1$country_destination,pred)
table=as.data.frame(table)
table$Freq=as.numeric(table$Freq)
str(table)
agrregate(table$Freq,by=list(pred=table$pred),FUN=sum)
false_negative_rate_us=34899/54066 #accuracy

#missclassification rate
table=table(pred,data1$country_destination)
1-sum(diag(table))/sum(table)

#Load test dataset

test=read.csv("test_users.csv")
library(dplyr)
test1=test%>%select(-id,-timestamp_first_active,-date_account_created,-date_first_booking,-timestamp_first_active)
library(stringr)
test_doc=as.data.frame(str_split_fixed(test$date_account_created,"/",3))
test1$doc=as.data.frame(lapply(test_doc,as.factor))
test1["doc_year"]=test_doc[,3]
test1["doc_month"]=test_doc[,1]
test1["doc_date"]=test_doc[,2]
test1$doc=NULL

test1$age=as.numeric(test1$age)
test1$signup_flow=as.factor(test1$signup_flow)
test1$country_destination=as.factor(test1$country_destination)
pred_prob=predict(rf,newdata=test1,type="class")
table1=table(pred,test1$country_destination)
1-sum(diag(table1))/sum(table1)

#the misclassification rate of logistic regression is smaller than that of random forest, so using logistic
#model for this data is a good choice.