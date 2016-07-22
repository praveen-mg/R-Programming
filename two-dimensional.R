library(ggplot2)

pf <- read.csv('pseudo_facebook.tsv',sep = '\t')

qplot(x = age, y = friend_count, data = pf)

ggplot(aes(x = age, y = friend_count),data=pf)+
  geom_point(alpha = 1/20)+
  xlim(13,90)

ggplot(aes(x = age, y = friend_count),data=pf)+
  geom_jitter(alpha = 1/20)+
  xlim(13,90)+
  coord_trans(y = 'sqrt')