pf <- read.csv('pseudo_facebook.tsv',sep = '\t')
names(pf)
ggplot(aes(x = dob_day), data = pf) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~dob_month)
ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram()+ 
  scale_x_continuous(limits = c(0,500))

ggplot(aes(x = friend_count), data = subset(pf,!is.na(gender))) + 
  geom_histogram(binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  facet_wrap(~gender)


table(pf$gender)
by(pf$friend_count,pf$gender,summary)

ggplot(aes(x = tenure), data = pf) + 
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9')

ggplot(aes(x = tenure / 365), data = pf) + 
  geom_histogram(color = 'black', fill = '#F79420') + 
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) + 
  xlab('Number of years using Facebook') + 
  ylab('Number of users in sample')

##Age
ggplot(aes(x = age), data = pf) + 
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9')+ 
  scale_x_continuous(breaks = seq(10, 120, 10), limits = c(10, 120)) 


##friend count
p_count <- ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram(color = 'black', fill = '#099DD9') 

p_log <- ggplot(aes(x = friend_count+1), data = pf) + 
  geom_histogram(color = 'black', fill = '#F79420') +
  scale_x_log10()

p_sqr <- ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram() +
  scale_x_sqrt()

grid.arrange(p_count, p_log, p_sqr)

ggplot(aes(x = friend_count, y = ..count../sum(..count..)), data = subset(pf, !is.na(gender))) + 
  geom_freqpoly(aes(color = gender), binwidth=10) + 
  scale_x_continuous(limits = c(0, 1000), breaks =  seq(0, 1000, 50)) + 
  xlab('Friend Count') + 
  ylab('Percentage of users with that friend count')

##names

ggplot(aes(x = www_likes), data = subset(pf, !is.na(gender))) + 
  geom_freqpoly(aes(color = gender)) + 
  scale_x_continuous()+
  scale_x_log10()


by(pf$www_likes,pf$gender,sum)
by(pf$friend_count,summary)


ggplot(aes(x=gender,y = friend_count), data = subset(pf,!is.na(gender))) + 
  geom_boxplot() + 
  facet_wrap(~gender)+
  coord_cartesian(ylim= c(0,1000))

ggplot(aes(x=gender,y = friendships_initiated), data = subset(pf,!is.na(gender))) + 
  geom_boxplot() + 
  facet_wrap(~gender)+
  coord_cartesian(ylim= c(0,250))
by(pf$friendships_initiated,pf$gender,summary)

mobile_checkin <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes >0,1,0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)