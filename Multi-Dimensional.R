names(pf)
library(ggplot2)
ggplot(aes(x=gender,y=age),data = subset(pf,!is.na(gender)))+
  geom_boxplot()+
  stat_summary(fun.y = mean,geom = 'point',shape = 4)

ggplot(aes(x=age,y = friend_count),data = subset(pf,!is.na(gender)))+
  geom_line(aes(color = gender),stat = 'summary', fun.y = median)

library('dplyr')
pf_by_age_gender <- group_by(pf,age,gender)

head(pf_by_age_gender)

pf.fc_by_age_gender <- summarise(subset(pf_by_age_gender,!is.na(gender)) ,
                                 mean_friend_count = mean(friend_count),
                                 median_friend_count = median(friend_count),
                                 n = n())

ggplot(aes(x = age, y = median_friend_count),data = subset(pf.fc_by_age_gender,!is.na(gender)))+
  geom_line(aes(color= gender))

install.packages('reshape2')

library('reshape2')


pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = 'median_friend_count')

head(pf.fc_by_age_gender.wide)


ggplot(aes(x=age,y=female/male),data = pf.fc_by_age_gender.wide)+
  geom_line()+
  geom_hline(yintercept = 1,alpha = 0.3,color = 'blue')

head(pf)

pf$year_joined <- floor(2014 - (pf$tenure / 365))

?cut

pf$year_joined.bucket <- cut(pf$year_joined,breaks = c(2004,2009,2011,2012,2014))
table(pf$year_joined.bucket)


ggplot(aes(x= age,y= friend_count),data = subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color = year_joined.bucket),stat = 'summary',fun.y = mean)+
  geom_line(stat = 'summary',fun.y = mean,linetype = 2)

pf_more_one_day <- subset(pf,tenure >1)

pf_more_one_day$rate <- pf_more_one_day$friend_count/pf_more_one_day$tenure

summary(pf_more_one_day$rate)

with(subset(pf,tenure >= 1),summary(friend_count/tenure))

ggplot(aes(x= (tenure) ,y = friendships_initiated/tenure),data = subset(pf,tenure>=1))+
  geom_line(aes(color= year_joined.bucket),stat = 'summary',fun.y = mean)+
  geom_line(stat = 'summary',fun.y = mean,linetype = 2)


ggplot(aes(x= 90*round(tenure/90) ,y = friendships_initiated/tenure),data = subset(pf,tenure>=1))+
  geom_line(aes(color= year_joined.bucket),stat = 'summary',fun.y = mean)+
  geom_line(stat = 'summary',fun.y = mean,linetype = 2)

ggplot(aes(x= tenure ,y = friendships_initiated/tenure),data = subset(pf,tenure>=1))+
  geom_smooth(aes(color= year_joined.bucket))
 



yo <- read.csv('yogurt.csv')

str(yo)

yo$id <- factor(yo$id)

ggplot(aes(x=price),data = yo)+
  geom_histogram


yo <- transform(yo,all.purchases = strawberry + blueberry 
                +pina.colada
                +plain
                +mixed.berry)
summary(yo$all.purchases)

ggplot(aes(x=time,y= price),data = yo)+
  geom_jitter(alpha = 1/4,shape = 21,color = 'orange')

set.seed(42)

sample.ids <- sample(levels(yo$id),16)

ggplot(aes(x = time, y = price),
       data = subset(yo, id %in% sample.ids))+
       facet_wrap(~id)+
       geom_line()+
       geom_point(aes(size = all.purchases),pch = 1)

install.packages("GGally")
library("GGally")

theme_set(theme_minimal(20))

set.seed(1836)

pf_subset <- pf[,c(2:15)]

names(pf_subset)

ggpairs(pf_subset[sample.int(nrows(pf_subset),1000),])