names(diamonds)
unique(diamonds$color)
library(ggplot2)
ggplot(aes(x=price),data = diamonds)+
  facet_wrap(~color)+
  geom_histogram(aes(color = cut))
  

ggplot(aes(x = table ,y = price),data = diamonds)+
  geom_point(aes(color = cut))


ggplot(aes(x = (x*y*z),y = price),data = diamonds)+
  geom_point(aes(color = clarity))+
  xlim(0,quantile(diamonds$x * diamonds$y * diamonds$z ,.99))+
  scale_y_log10()+
  scale_color_brewer(type = 'div')

names(pf)
pf$prop_initiated <- ifelse(pf$friend_count >0,pf$friendships_initiated/pf$friend_count,0)


summary(pf$friendships_initiated)

ggplot(aes(x = tenure,y = prop_initiated),data = pf)+
  geom_line(aes(color = year_joined.bucket),stat = 'summary',fun.y = median)



ggplot(aes(x = tenure,y = prop_initiated),data = pf)+
  geom_smooth(aes(color= year_joined.bucket))
pf$year_joined.bucket

summary(pf$friendships_initiated)

summary(pf$year_joined.bucket)

unique(pf$year_joined.bucket)

table(pf$year_joined.bucket)

with(subset(pf, year_joined.bucket == "(2012,2014]"),summary(prop_initiated))

typeof(pf$year_joined.bucket)


ggplot(aes(x = cut, y = price/carat),data = diamonds)+
  facet_wrap(~clarity)+
  geom_point(aes(color = color),position = "jitter")+
  scale_color_brewer(type = 'div')
  
  
