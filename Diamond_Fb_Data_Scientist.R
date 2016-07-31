library(ggplot2)

data("diamonds")
names(diamonds)
ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point(color = 'orange')+
  stat_smooth(method = 'lm')+
  xlim(0,quantile(diamonds$carat,.99))+
  ylim(0,quantile(diamonds$price,.99))

install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')

library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)
library(car)
library(reshape)
library(plyr)

set.seed(20022012)
cuberoot_trans = function(){
  trans_new('cuberoot',
            transform = function(x) {x^(1/3)},
            inverse = function(x){ x^3})
}
diamond_sample <- diamonds[sample(1:length(diamonds$price),10000),]

ggpairs(diamond_sample,  lower = list(continuous = wrap("points", shape = I('.'))), 
        upper = list(combo = wrap("box", outlier.shape = I('.'))))



library(gridExtra)

plot1 <- ggplot(aes(x= price),data = diamonds)+
          geom_histogram(binwidth = 0.01)

plot2 <- ggplot(aes(x= price),data = diamonds)+
  geom_histogram(binwidth = 0.01)+
  scale_y_log10()

grid.arrange(plot1,plot2,nrow = 1)

ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point()+
  scale_x_continuous(trans = cuberoot_trans(),limits = c(0.2,3),breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans = log10_trans(),limits = c(350,15000),breaks = c(350,1000,5000,10000,15000))
  
ggplot(aes(carat, price), data = diamonds) + 
  geom_point(aes(color = clarity),alpha = 0.5,size = 0.75,position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and clarity')

ggplot(aes(carat, price), data = diamonds) + 
  geom_point(aes(color = cut),alpha = 0.5,size = 0.75,position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and cut')



ggplot(aes(carat, price), data = diamonds) + 
  geom_point(aes(color = color),alpha = 0.5,size = 0.75,position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'color', reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and color')




##building linear model

m1 <- lm(I(log(price)) ~ I(carat^(1/3)),data = diamonds)

m2 <- update(m1,~ .+ carat)

m3 <- update(m2,~ . +cut)

m4 <- update(m3,~ .+ color)

m5 <- update(m4,~ .+ clarity)

mtable(m1,m2,m3,m4,m5)