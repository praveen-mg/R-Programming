summary(diamonds)
library(ggplot2)
ggplot(aes(x = price, y = x),data = diamonds)+
  geom_point()

cor.test(diamonds$price,diamonds$x)

cor.test(diamonds$price,diamonds$y)

cor.test(diamonds$price,diamonds$z)

ggplot(aes(x= depth, y = price),data = diamonds)+
  geom_point(alpha = 1/100)+
  scale_x_continuous(limits = c(57,67), breaks = seq(57,67,2))

cor.test(diamonds$depth,diamonds$price)

ggplot(aes(x = carat, y = price),data = diamonds)+
  geom_point()+
  xlim(0,quantile(diamonds$carat,.99))+
  ylim(0,quantile(diamonds$price,.99))

diamonds$volume <- (diamonds$x * diamonds$y * diamonds$z)

summary(diamonds)

ggplot(aes(x=volume,y=price),data = diamonds)+
  geom_point()
  
detach("package:plyr", unload=TRUE)
library('plyr')
library('dplyr')
count(diamonds$volume == 0)
diamonds_good_volume <- subset(diamonds,volume !=0 & volume < 800)
cor.test(diamonds_good_volume$volume,diamonds_good_volume$price)

ggplot(aes(x=volume,y=price),data = diamonds_good_volume )+
  geom_point(alpha = 1/40)+
  geom_smooth(method = 'lm',color = 'red')



age_groups <- group_by(pf,age)

pf.fc_by_age <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())


diamonds_clarity <- group_by(diamonds,clarity)

diamondsByClarity <- summarise(diamonds_clarity,mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n())

head(diamondsByClarity)


diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


p1 <- ggplot(aes(x = clarity,y = mean_price),data = diamonds_mp_by_clarity )+
  geom_bar(stat="identity", color = "blue")

p2 <-  ggplot(aes(x = color,y = mean_price),data = diamonds_mp_by_color )+
  geom_bar(stat="identity", color = "red")


library(gridExtra)

grid.arrange(p1,p2,ncol=1)



library(tidyr)
female_marriage <- read.csv('indicator_age_of_marriage.csv',sep = ",",header=TRUE)
names(female_marriage)
female_marriage_gather <- gather(female_marriage,year,Age,-country)
names(female_marriage_year)
