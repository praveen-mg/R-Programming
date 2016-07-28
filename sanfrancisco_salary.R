salary <- read.csv('Salaries.csv',header = TRUE)

head(salary)

unique(salary$JobTitle)

library(ggplot2)

ggplot(aes(x= Year,y= TotalPay),data = salary)+
  geom_line(stat = 'summary',fun.y = mean)

summary(salary$TotalPay)

set.seed(41)

sample.JobTitles <- sample(levels(salary$JobTitle),12)

ggplot(aes(x = Year, y = TotalPay),
       data = subset(salary, JobTitle %in% sample.JobTitles))+
  facet_wrap(~JobTitle)+
  geom_line()
