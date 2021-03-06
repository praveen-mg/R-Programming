library(ggplot2)
data(diamonds)
summary(diamonds)
names(diamonds)
is.ordered(diamonds$z)

cost_plot <- ggplot(aes(x = price), data =diamonds ) + 
  geom_histogram(binwidth = 50)

cost_plot_less_250 <- cost_plot + 
  scale_x_continuous(limits = c(0,500)) 
 
cost_plot +
  scale_x_continuous()+
  facet_wrap(~cut,scales="free_y")
ggsave('priceHistogram.png')

summary(diamonds$price)

by(diamonds$price,diamonds$cut,min)

less_500 <- subset(diamonds, price < 500)
less_250 <- subset(diamonds,price< 250)
greater_15000 <- subset(diamonds,price >= 15000) 

ggplot(aes(x = price/carat), data =diamonds ) + 
  geom_histogram(binwidth =0.25)+
  facet_wrap(~cut,scales="free_y")+
  scale_x_log10()

ggplot(aes(x = price/carat), data =diamonds ) + 
  geom_histogram(binwidth =0.25)+
  facet_wrap(~cut,scales="free_y")+
  scale_x_log10()
ggplot(aes(x = color,y=price),data = diamonds)+
  geom_boxplot()+
  coord_cartesian(ylim= c(0,8500))

by(diamonds$price,diamonds$color,summary)

ggplot(aes(x = color,y=price/carat),data = diamonds)+
  geom_boxplot(aes(fill = color))+
  coord_cartesian(ylim= c(0,8500))


ggplot(aes(x = carat), data = diamonds) + 
  geom_freqpoly(binwidth=.2) + 
  scale_x_continuous(limits = c(2, 5), breaks =  seq(2, 5, .1))

  
table(diamonds$carat)


library("xlsx")

#employment <- read.xlsx('indicator_t_above_15_employ.xlsx',1)
library(tidyr)
library(dplyr)
library(ggplot2)
#employment$country <- row.names(employment)
#name_gather <- gather(employment,year, value, -Row.Labels)
tbl_df(employment)

employment <- read.csv('indicator_t_above_15_employ.csv',sep = ";",header=TRUE)


  