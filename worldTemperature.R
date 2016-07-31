temperature <- read.csv('GlobalLandTemperaturesByCountry.csv',header = TRUE)

names(temperature)
temperature_india <- subset(temperature,Country == 'India')

head(temperature_india)
str(temperature_india)

sapply(temperature_india, typeof)

temperature_india$dt <- as.Date(temperature_india$dt,format = "%Y-%m-%d")

temperature_india$year <- as.numeric(format(temperature_india$dt,'%Y'))
#Average temperature variation for India every year from 1800
ggplot(aes(x=year,y = AverageTemperature),data = temperature_india,!is.na(AverageTemperature))+
  geom_line(stat = 'summary', fun.y = mean)

unique(temperature$Country)

countries <- c('India','Singapore','Sweden','United States')
temperature$dt <- as.Date(temperature$dt,format = "%Y-%m-%d")
temperature$year <- as.numeric(format(temperature$dt,'%Y'))
ggplot(aes(x= year, y = AverageTemperature),data = subset(temperature,Country %in% countries),!is.na(AverageTemperature))+
  geom_line(aes(color = Country),stat = 'summary',fun.y = mean)

ggplot(aes(x= year, y = AverageTemperatureUncertainty),data = subset(temperature,Country %in% countries),!is.na(AverageTemperatureUncertainty))+
  geom_line(aes(color = Country),stat = 'summary',fun.y = mean)

temperature_city <- read.csv('GlobalLandTemperaturesByMajorCity.csv',header = TRUE)

names(temperature_city)

temperature_city$dt <- as.Date(temperature_city$dt,format = "%Y-%m-%d")
temperature_city$year <- as.numeric(format(temperature_city$dt,'%Y'))

library(ggplot2)

ggplot(aes(x = year, y = AverageTemperature),data = subset(temperature_city,City == 'Los Angeles'),!is.na(AverageTemperature))+
  geom_line(stat = 'summary',fun.y = mean)