#https://www.kaggle.com/datasets/CooperUnion/cardataset\
library(tidyverse)
library(ggplot2)
test <- read.csv("data.csv")
tasr <- test %>% filter(MSRP > 2000 & MSRP <= 100000)
test %>% filter(MSRP > 2000) %>% ggplot(data = .) + geom_bar(mapping = aes(x = MSRP), width = 1000) + xlim(0, 100000)

#Avg fuel over time
gta <- test %>% group_by(Year) %>% summarise(highwayAVG = mean(highway.MPG),cityAVG = mean(city.mpg))
gta %>% ggplot(data = .) + geom_line(aes(x=Year, y=highwayAVG)) + geom_line(aes(x=Year, y=cityAVG))

#Avg fuel over time (Vehicle Size)
gta <- test %>% group_by(Year,Vehicle.Size) %>% summarise(highwayAVG = mean(highway.MPG),cityAVG = mean(city.mpg))
gta %>% ggplot(data = .) + geom_line(aes(x=Year, y=highwayAVG, color = Vehicle.Size)) + geom_line(aes(x=Year, y=cityAVG, color = Vehicle.Size))

#
ggplot(data = test) + geom_bar(mapping = aes(x = Year, fill=Vehicle.Size))
x <- test %>% group_by(Year, Vehicle.Size) %>% filter(Year > 2010) %>% summarise(count = n(), proportion = count / sum(count))

y <-  test %>%
  group_by(Year) %>%
  summarize(countT= n(),Vehicle.Size) %>%
  group_by(Vehicle.Size, .add=TRUE) %>%
  mutate(per=(round(n()/countT,2)))
y <- unique(y)

y %>% ggplot(data = .) + geom_point(aes(x=Year, y=per, color = Vehicle.Size)) + geom_line(aes(x=Year, y=per, color = Vehicle.Size)) 


#Given the exact same make and model, is the automatic,manual, automated_manual type more efficient in terms of fuel economy
sts <- test %>% group_by(Make,Model,Year,Transmission.Type)%>% summarise(highway.MPG,city.mpg,count = n()) %>% unique()
asd <- test %>% group_by(Make,Model,Year) %>% summarise(count =  n_distinct(Transmission.Type)) %>% filter(count > 1)
sts <- inner_join(asd,sts, c("Make","Model","Year"))
sum <- sts %>% group_by(Make,Transmission.Type) %>% summarize(avghighwayMpg = mean(highway.MPG),avgcityMpg = mean(city.mpg))
sts %>% ggplot(data = .) + geom_bar(mapping = aes(x=Make, y=highway.MPG)) 
