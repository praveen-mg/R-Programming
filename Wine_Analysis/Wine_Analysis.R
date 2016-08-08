wine <- read.csv('wineQualityWhites.csv',header = TRUE)

head(wine)
names(wine)

library(ggplot2)
library("GGally")
ggplot(aes(x = alcohol, y = quality),data = subset(wine,quality >4))+
  geom_point(aes(color = citric.acid))+
  scale_fill_distiller()

summary(wine$residual.sugar)



ggplot(aes(x = quality),data = wine)+
  geom_histogram()


set.seed(1836)
wine_sample <- wine[sample.int(nrow(wine),1000),]
head(wine_sample)
ggpairs(wine_sample)

ggplot(aes(x = pH),data = wine)+
  geom_histogram(aes(color = residual.sugar))


