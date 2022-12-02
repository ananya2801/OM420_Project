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
sts <- test %>% group_by(Make,Model,Year,Transmission.Type)%>% filter( Transmission.Type %in% c("AUTOMATIC","MANUAL"))%>% summarise(highway.MPG,city.mpg,count = n()) %>% unique()
asd <- test %>% group_by(Make,Model,Year) %>% filter( Transmission.Type %in% c("AUTOMATIC","MANUAL"))%>% summarise(count =  n_distinct(Transmission.Type)) %>% filter(count > 1)
sts <- inner_join(asd,sts, c("Make","Model","Year"))
sum <- sts %>% group_by(Make,Model,Year,Transmission.Type) %>% summarise(avgHMPG = mean(highway.MPG),avgCMPG = mean(city.mpg))
#Compare older cars here: 1990-94
old_cars <- sum %>% filter(Year <1994, Transmission.Type %in% c("AUTOMATIC","MANUAL","AUTOMATED_MANUAL")) %>% arrange(Year)
old_cars %>% ggplot(data = .) + geom_bar(aes(x = Model,y = avgHMPG, fill=Transmission.Type), stat = "identity",position = "dodge",width = 0.4)
#Compare millennium cars here: 2000-2005
millenium_cars <- sum %>% filter(Year <=2005, Year >=2004,Transmission.Type %in% c("AUTOMATIC","MANUAL")) %>% arrange(Year)
millenium_cars %>% ggplot(data = .) + geom_bar(aes(x = Model,y = avgHMPG, fill=Transmission.Type), stat = "identity",position = "dodge",width = 0.4)
#Compare latest cars here: 2016-2017
new_cars <- sum %>% filter(Year ==2017,Transmission.Type %in% c("AUTOMATIC","MANUAL")) %>% arrange(Year)
new_cars_sample <- new_cars[1:32,]
new_cars_sample %>% ggplot(data = .) + geom_bar(aes(x = Model,y = avgHMPG, fill=Transmission.Type), stat = "identity",position = "dodge",width = 0.4)

asdf <- sum %>% group_by(Model) %>% summarize(count = n_distinct(Year)) %>% filter(count == 3)

proportion <- old_cars %>% group_by(Make,Model,Year) %>% summarize(highwayDiff = diff(avgHMPG), cityDiff = diff(avgCMPG)) %>% mutate(mostEfficientH = ifelse(highwayDiff < 0, "Auto","Man"))
#If automatic is more efficient than manual than diff is negative, otherwise vice versa
count = c(length(which(proportion$mostEfficientH == "Auto")),length(which(proportion$mostEfficientH == "Man")) )
label = c("Auto","Man")
pct <- round(count/sum(count)*100)
label <- paste(label, pct) # add percents to labels
label <- paste(label,"% ",sep="") # ad % to labels
label <- paste(label,"(",sep="") # ad % to labels
label <- paste(label,count,sep="") # ad % to labels
label <- paste(label,")",sep="") # ad % to labels
count %>% pie(labels = label, main="Which drivetrain is more efficient for cars from 1990-94?")

proportion <- millenium_cars %>% group_by(Make,Model,Year) %>% summarize(highwayDiff = diff(avgHMPG), cityDiff = diff(avgCMPG)) %>% mutate(mostEfficientH = ifelse(highwayDiff < 0, "Auto","Man"))
#If automatic is more efficient than manual than diff is negative, otherwise vice versa
count = c(length(which(proportion$mostEfficientH == "Auto")),length(which(proportion$mostEfficientH == "Man")) )
label = c("Auto","Man")
pct <- round(count/sum(count)*100)
label <- paste(label, pct) # add percents to labels
label <- paste(label,"% ",sep="") # ad % to labels
label <- paste(label,"(",sep="") # ad % to labels
label <- paste(label,count,sep="") # ad % to labels
label <- paste(label,")",sep="") # ad % to labels
count %>% pie(labels = label, main="Which drivetrain is more efficient for cars from 2004-05?")

proportion <- new_cars %>% group_by(Make,Model,Year) %>% summarize(highwayDiff = diff(avgHMPG), cityDiff = diff(avgCMPG)) %>% mutate(mostEfficientH = ifelse(highwayDiff < 0, "Auto","Man"))
#If automatic is more efficient than manual than diff is negative, otherwise vice versa
count = c(length(which(proportion$mostEfficientH == "Auto")),length(which(proportion$mostEfficientH == "Man")) )
label = c("Auto","Man")
pct <- round(count/sum(count)*100)
label <- paste(label, pct) # add percents to labels
label <- paste(label,"% ",sep="") # ad % to labels
label <- paste(label,"(",sep="") # ad % to labels
label <- paste(label,count,sep="") # ad % to labels
label <- paste(label,")",sep="") # ad % to labels
count %>% pie(labels = label, main="Which drivetrain is more efficient for cars from 2017?")


all_cars <- sum %>% filter(Transmission.Type %in% c("AUTOMATIC","MANUAL")) %>% arrange(Year)
proportion <- all_cars %>% group_by(Year,Transmission.Type) %>% summarise(avgHMPG = mean(avgHMPG),avgCMPG = mean(avgCMPG))
proportion %>% ggplot(data = .) + geom_point(mapping = aes(x = Year, y = avgHMPG, color = Transmission.Type)) + geom_line(mapping = aes(x = Year, y = avgHMPG, color = Transmission.Type))#If automatic is more efficient than manual than diff is negative, otherwise vice versa

#2008
newer_cars <- test %>% filter(Year >=2017)
set.seed(1234)
train_df <- newer_cars %>% sample_frac(0.25)
test_df <- setdiff(test,newer_cars) %>% filter(Make != "Tesla")
mod1 <- train_df %>% lm(MSRP ~ Make + Engine.HP  + Engine.Cylinders + Transmission.Type + Driven_Wheels + Number.of.Doors+ Vehicle.Size + highway.MPG + city.mpg, data = .)
mod2 <- test_df %>% lm(MSRP ~ Make + Engine.HP  + Engine.Cylinders + Transmission.Type + Driven_Wheels + Number.of.Doors+ Vehicle.Size + highway.MPG + city.mpg, data = .)
summary(mod1)
train_pred <- predict(
  mod1, 
  type = "response"
)

train_e <- train_df$MSRP - train_pred
train_mse <- mean(train_e ^ 2)

test_pred <- predict(
  mod2, 
  type = "response",
  newdata = test_df
)

test_e <- test_df$MSRP - test_pred
test_mse <- mean(test_e ^ 2)

set.seed(2)
indx <- sample(x = 1:nrow(newer_cars), size = floor(nrow(newer_cars) / 2))
df_train <- newer_cars[indx, ]
df_valid <- newer_cars[-indx, ]
mod2 <- train_df %>% lm(MSRP ~ Make + Engine.HP  + Engine.Cylinders + Transmission.Type + Driven_Wheels + Number.of.Doors+ Vehicle.Size + highway.MPG + city.mpg, data = .)

pred2 <- predict(object = mod2, type = "response", newdata = df_valid)
mean((pred2 - df_valid$MSRP)^2)

ggplot(newer_cars, aes(y = MSRP,x = Make)) + geom_point()


#CROSS VALIDATION
n <- nrow(newer_cars)
mse_i <- rep(0, n)
for (i in 1:n) {
  df_valid <- newer_cars[i, ]
  df_train <- newer_cars[-i, ]
  mod <- lm(MSRP ~ Make + Engine.HP  + Engine.Cylinders + Transmission.Type + Driven_Wheels + Number.of.Doors+ Vehicle.Size + highway.MPG + city.mpg, data = df_train)
  pred <- predict(object = mod, type = "response", newdata = df_valid)
  mse_i[i] <- mean((pred - df_valid$mpg)^2)
}

#LOOCV
loocv_mse <- mean(mse_i)
mse_mod <- rep(0, 10)
for (degree in 1:10) {
  mse_i <- rep(0, n)
  5
  for (row_i in 1:n) {
    df_valid <- newer_cars[row_i, ]
    df_train <- newer_cars[-row_i, ]
    mod <- lm(mpg ~ poly(horsepower, degree), data = df_train)
    pred <- predict(object = mod, type = "response", newdata = df_valid)
    mse_i[row_i] <- mean((pred - df_valid$mpg)^2)
  }
  mse_mod[degree] <- mean(mse_i)
}

