library(ggplot2)

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')

qplot(x = age, y = friend_count, data = pf)

ggplot(aes(x = age, y = friend_count),data=pf)+
  geom_point(alpha = 1/20)+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

ggplot(aes(x = age, y = friend_count),data=pf)+
  geom_point(alpha = 1/20,position = position_jitter(h=0))+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

ggplot(aes(x= age, y=friendships_initiated),data=pf)+
  geom_jitter(alpha=1/20)+
  xlim(13,90)

ggplot(aes(x= age, y=friendships_initiated),data=pf)+
  geom_jitter(alpha=1/20)+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

ggplot(aes(x= age, y=friendships_initiated),data=pf)+
  geom_point(alpha=1/20,position = position_jitter(h=0))+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

library('dplyr')

age_groups <- group_by(pf,age)

pf.fc_by_age <- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())

pf.fc_by_age <- arrange(pf.fc_by_age,age)

head(pf.fc_by_age)

ggplot(aes(x= age, y=friend_count_mean),data=pf.fc_by_age)+
  geom_point()+
  xlim(13,90)

ggplot(aes(x= age, y=friend_count_median),data=pf.fc_by_age)+
  geom_point()+
  xlim(13,90)

ggplot(aes(x= age, y=friend_count_median),data=pf.fc_by_age)+
  geom_line()+
  xlim(13,90)

ggplot(aes(x = age, y = friend_count),data=pf)+
  geom_point(alpha = 1/20,position = position_jitter(h=0),color = 'orange')+
  coord_cartesian(xlim = c(13, 90),ylim = c(0,1000))+
  geom_line(stat = 'summary',fun.y = mean)+
  geom_line(stat = 'summary',fun.y = quantile,fun.args = list(probs = .9),linetype = 2)+
  geom_line(stat = 'summary',fun.y = quantile,fun.args = list(probs = .5), color = 'blue')+
  geom_line(stat = 'summary',fun.y = quantile,fun.args = list(probs = .1),color = 'red', linetype = 2)
library(Hmisc)
rcorr(pf$age,pf$friend_count,type="pearson")

cor.test(pf$age,pf$friend_count,type="method")

with(pf,cor.test(age,friend_count,method = 'pearson'))

with(subset(pf,age<=45),cor.test(age,friend_count,method = 'pearson'))
names(pf)
ggplot(aes(x = www_likes_received, y = likes_received),data=pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,.95))+
  ylim(0,quantile(pf$likes_received,.95))+
  geom_smooth(method = 'lm',color = 'red')

cor.test(pf$www_likes_received,pf$likes_received)

install.packages('alr3')
library(alr3)
?Mitchell

data(Mitchell)
summary(Mitchell)

ggplot(aes(x = Month, y = Temp), data=Mitchell)+
  geom_point()

cor.test(Mitchell$Month,Mitchell$Temp)

ggplot(aes(x = Month, y = Temp), data=Mitchell)+
  geom_point()+
  scale_x_continuous(limits = c(0, 203), breaks =  seq(0, 203, 12)) 



ggplot(aes(x = Month%%12, y = Temp), data=Mitchell)+
  geom_point()+
  scale_x_continuous(limits = c(0, 12), breaks =  seq(0, 12, 1)) 

pf$age_with_month <- pf$age + (1 - pf$dob_month/12)

age_groups_months <- group_by(pf,age_with_month)

pf.fc_by_age_months <- summarise(age_groups_months, 
                                 friend_count_mean = mean(friend_count),
                                 friend_count_median = median(friend_count),
                                 n=n())

pf.fc_by_age_months <- group_by(pf.fc_by_age_months,age_with_month)

head(pf.fc_by_age_months)



p1 <- ggplot(aes(x= age, y=friend_count_mean),data=subset(pf.fc_by_age,age<71))+
  geom_line()
  

p2 <- ggplot(aes(x=age_with_month,y=friend_count_mean),data = subset(pf.fc_by_age_months,age_with_month <71))+
  geom_line()

p3 <- ggplot(aes(x= round(age/5)*5, y=friend_count),data=subset(pf,age<71))+
  geom_line(stat = 'summary',fun.y = mean)


library(gridExtra)

grid.arrange(p1,p2,p3,ncol=1)

