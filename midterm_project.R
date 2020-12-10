library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(lme4)
library(corrplot)
library(arm)
library(gridExtra)
library(performance)
library(see)

#import data
raw<-read.csv("/Users/wanghaoqi/Desktop/2020 Fall/MA678/midterm project/raw_sample.csv")
user<-read.csv("/Users/wanghaoqi/Desktop/2020 Fall/MA678/midterm project/user_profile.csv")
ad<-read.csv("/Users/wanghaoqi/Desktop/2020 Fall/MA678/midterm project/ad_feature.csv")
#raw-cleaning data
raw$date<-as.Date(as.POSIXct(raw$time_stamp, origin="1970-01-01"))
raw<-raw[,c(-2,-4,-5)]
#user-cleaning data 
user1<-na.omit(user)
user2<-user1[,c(-2,-3)]
names(user2)[1]<-"user"

#combine two tables into one table
df1<-merge(raw,user2,by="user",all=F) 
df2<-merge(df1,ad,by="adgroup_id",all=F) 


#delete outliers
quantile(df2$price,0.95)
df2$price<-ifelse(df2$price>quantile(df2$price,0.95),quantile(df2$price,0.95),df2$price)
quantile(df2$price,0.05)
df2$price<-ifelse(df2$price<quantile(df2$price,0.05),quantile(df2$price,0.05),df2$price)
df2 <-df2[-which(df2$price==19.8|df2$price==1299),]


dim(df2)
str(df2)
df<-df2[,-c(1,2,7,11:14)]
str(df)
names(df)[3]<-"gender"
table(df$new_user_class_level)
click<-subset(df,new_user_class_level==2)
table(click$shopping_level)
click<-subset(click,shopping_level==2)

fwrite(click,"/Users/wanghaoqi/Desktop/2020 Fall/MA678/midterm project/click.csv")
click<-read.csv("/Users/wanghaoqi/Desktop/2020 Fall/MA678/midterm project/click.csv")


#EDA
#date
a<-click%>% group_by(date)%>% summarise(percent=sum(clk)/n()*100)
p1<-ggplot(data = a,aes(x=date,y=percent,group = 1))+
  geom_point()+
  geom_line()+
  labs(x = "date",y = "click percent") +
  labs(title = "distribution of click rate by date",x="Date",ylab="Click percentage(%)")

  
#gender
click$gender[click$gender==1]<-"0"
click$gender[click$gender==2]<-"1"
click$gender<-as.factor(click$gender)
b<-click%>% group_by(gender)%>% summarise(percent=sum(clk)/n()*100)
p2<-ggplot(data = b,aes(x=gender,y=percent))  + 
  geom_point()+
  labs(title = "distribution of click rate by gender",x="Gender",ylab="Click percentage(%)")

#age
c<-click%>% group_by(age_level)%>% summarise(percent=sum(clk)/n()*100)
p3<-ggplot(data = c,aes(x=age_level,y=percent))  + 
  geom_point()+
  geom_line()+
  labs(title = "distribution of click rate by age",x="Age",ylab="Click percentage(%)")

#occupation
click$occupation<-as.factor(click$occupation)
d<-click%>% group_by(occupation)%>% summarise(percent=sum(clk)/n()*100)
p4<-ggplot(data = d,aes(x=occupation,y=percent))  + 
  geom_point()+
  labs(title = "distribution of click rate by occupation",x="Occupation",ylab="Click percentage(%)")

#price
e<-click%>% group_by(price)%>% summarise(percent=sum(clk)/n()*100)
p5<-ggplot(data = e,aes(x=price,y=percent))  + 
  geom_smooth()+
  labs(title = "distribution of click rate by price",x="Price",ylab="Click percentage(%)")

grid.arrange(p1,p2,p3,p4,p5,ncol=3)

click$gender<-as.numeric(click$gender)
click$occupation<-as.numeric(click$occupation)

click<-click[,-c(2,5,7)]

C <- cor(click,method = "spearman")
corrplot(C, method = "square")

grid.arrange(p1,p2,p3,p4,p5,ncol=3)


#General Linear Model
str(click)
click<-na.omit(click)
as.numeric(click$gender)
click$gender[click$gender==1]<-"0"
click$gender[click$gender==2]<-"1"

click$occupation[click$occupation==1]<-"0"
click$occupation[click$occupation==2]<-"1"
as.numeric(click$occupation)

fit1<-glm(clk~factor(gender)+factor(age_level)+log(price)+factor(occupation),data=click,family=binomial(link="logit"))
summary(fit1)
binnedplot(fitted(fit1), resid(fit1))
binned_residuals(fit1)

fit_1<-glm(clk~factor(gender)+log(price),data=click,family=binomial(link="logit"))
summary(fit_1)
binnedplot(fitted(fit_1), resid(fit_1))



#varing intercept
fit2<-glmer(clk~factor(gender)+(1|age_level)+log(price),data=click, family=binomial(link="logit"))
summary(fit2)
plot(fit2)
binnedplot(fitted(fit2), resid(fit2,type="response"))
binnedplot(predict(fit2), resid(fit2,type="response"))
#varing slope
fit3<-glmer(clk~factor(gender)+(price-1|age_level),data=click, family=binomial(link="logit"))
summary(fit4)
binnedplot(fitted(fit3), resid(fit3))

fit4<-glmer(clk~(1|gender)+(1|age_level)+log(price),data=click,family=binomial(link = 'logit'))
summary(fit4)
binnedplot(fitted(fit4), resid(fit4))

fit_2<-glm(clk~(1|gender)+log(price),data=click,family=binomial(link="logit"))
summary(fit_2)
binnedplot(fitted(fit_2), resid(fit_2))






