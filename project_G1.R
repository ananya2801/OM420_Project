#https://www.kaggle.com/datasets/CooperUnion/cardataset\

library(tidyverse)
library(ggplot2)

# --------------------------- DATA CLEANING -----------------------------------#

raw_data <- read_csv("data.csv")
names(raw_data)
# Rename columns with spaces
names(raw_data) <- str_replace_all(string=names(raw_data), pattern=" ", replacement="_")

# Separate multiple market Categories
separated <- raw_data
grouped <- raw_data %>% group_by(Market_Category) %>% summarise()
while (dim(grouped)[1] != 0) {
  category <- str_split(string=grouped$Market_Category[1],pattern=",", simplify=T)[1,1]
  separated <- separated %>% mutate("{category}" := str_detect(separated$Market_Category, category))
  grouped <- grouped %>% transmute(Market_Category = str_remove(Market_Category, paste(category, ',',sep="")))
  grouped <- grouped %>% transmute(Market_Category = str_remove(Market_Category, category))
  grouped <- grouped[grouped$Market_Category != "",]
  names(separated)
}
names(separated) <- str_replace_all(string=names(separated), pattern=" ", replacement="_")
test <- separated
# Changing spaces to underscores in the Make column. Without this, an error is produced
#during left join in finding 2.
# without this step. If this messes up other things let me know.
test$Make <- sub(" ", "_", test$Make)
# this is probably where we could add new columns

# -------------------------- FINDING 1 ----------------------------------------#
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


# -------------------------- FINDING 2 ----------------------------------------#
# Created a column with the country of origin for each make. Comparing performance 
# of cars made in leading countries, mainly england, Germany, Italy, Japan, US & South Korea.
# Comparison based on HP, MPG and MSRP for the last decade for mid range cars.

make_country <- read.csv("make_origin_country.csv")
# working on a copy of test
test2 <- test
test2 <- test2 %>% left_join(make_country, by = "Make")

# filter out the luxury,high performace, Exotic and Factory tuned cars to get a 
# better estimate for mid-range cars.
test2 <- test2 %>% 
  filter(Country %in% c("England","Germany","Italy","Japan","US","South Korea") &
           Exotic == FALSE & Luxury == FALSE & `High-Performance` == FALSE & 
           `Factory_Tuner` == FALSE & Year >= 2010 & 
           !is.na(highway_MPG) & !is.na(Engine_HP) & !is.na(MSRP))

# MPG and HP show an inverse relationship
ggplot(data = test2) + geom_point(mapping = aes(x = Engine_HP, y = highway_MPG))

# checking if data is normal
test2 %>% .$MSRP %>%
  hist(col='steelblue', main='Actual Data')

test2 %>% .$MSRP %>% log() %>%
  hist(col='steelblue', main='Normalized Data')

# ---------------- BOXPLOTS --------------------------------------
# for log normalized data
ggplot(data = test2) + 
  geom_boxplot(mapping = aes(x = Country, y = log(Engine_HP))) +
  coord_cartesian(ylim = c(4, 6.5))

# 100 ish outliers
test2 %>% filter(Country == "Germany"& (log(highway_MPG) < 3.25 | log(highway_MPG) > 3.71))
ggplot(data = test2) + 
  geom_boxplot(mapping = aes(x = Country, y = log(highway_MPG))) +
  coord_cartesian(ylim = c(2.5, 5))

test2 %>% filter(Country == "Germany" & (log(MSRP) < 9.81 | log(MSRP) > 10.665))
# 26 outliers for about 658 obs
ggplot(data = test2) + 
  geom_boxplot(mapping = aes(x = Country, y = log(MSRP))) +
  coord_cartesian(ylim = c(9.25, 11.25))

# for actual data ------------------------------
ggplot(data = test2) + 
  geom_boxplot(mapping = aes(x = Country, y = Engine_HP)) +
  coord_cartesian(ylim = c(0, 500))

# 69 outliers for 658
ggplot(data = test2) + 
  geom_boxplot(mapping = aes(x = Country, y = highway_MPG)) +
  coord_cartesian(ylim = c(0, 110))

test2 %>% filter(Country == "Germany" & (highway_MPG < 25 | highway_MPG > 41))

test2 %>% filter(Country == "Germany" & (MSRP< 18000 | MSRP > 40000))
ggplot(data = test2) + 
  geom_boxplot(mapping = aes(x = Country, y = MSRP)) +
  coord_cartesian(ylim = c(10000, 75000))



