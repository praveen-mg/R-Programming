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