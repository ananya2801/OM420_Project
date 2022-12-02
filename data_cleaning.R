##########
########## Remove outliers from the dataframe and add seperate columns for each vehicle type.
##########


# Load raw data from csv file
raw_data <- read_csv("data.csv")

# Rename columns with spaces
names(raw_data) <- str_replace(string=names(raw_data), pattern=" ", replacement="_")

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

#separated <- rename(Highway_MPG=highway_MPG,City_MPG=city_mpg)

# Distribution of MSRP's
separated %>% ggplot() + geom_bar(mapping=aes(x=MSRP), width=100000)

# Select vehicles with MSRP c (2000,100000]
cars_df <- separated[separated$MSRP <= 100000,]
cars_df <- cars_df[cars_df$MSRP > 2000,]

# This distribution looks much better (could maybe move right bound further)
cars_df %>% ggplot() + geom_bar(mapping=aes(x=MSRP), width=1000)


##########
########## Explore MSRPS
##########

# HOW DOES MSRP raise over many years?
raw_data %>% ggplot(data = .) + geom_point(mapping = aes(x=Year, y=MSRP, color=Vehicle_Style))
# This graph doesnt really show us much


raw_data %>% group_by(Year) %>% summarise(avg_msrp = mean(MSRP)) %>% ggplot(data = .) + geom_line(mapping = aes(x=Year, y=avg_msrp))
# average msrp increases per year, as expected.
# Slight rise over time but not very deffinative and very sporatic

# relationship between HP and fuel economy
ggplot(data = raw_data) + geom_point(mapping = aes(x=Engine_HP, y=highway_MPG)) + geom_smooth(mapping = aes(x=Engine_HP, y=highway_MPG))
# At a very basic level fuel economy decreases with more HP (this makes alot of sense)
# can we investigate this more.

# how cylinders effect fuiel economy
raw_data %>% ggplot() + geom_point(mapping=aes(x=Engine_Cylinders, y=highway_MPG))
raw_data %>% ggplot() + geom_boxplot(mapping=aes(x = Engine_Cylinders, y=highway_MPG, group=Engine_Cylinders)) + ylim(0,60) #+ facet_wrap(vars(Year), ncol=1)
# this is a really intereting plot
# why is the mean so close to the first quartile?

# Why
raw_data %>% ggplot() + geom_point(mapping=aes(x=Engine_Cylinders, y=highway_MPG, color=Year), position="jitter") + ylim(0,60) + xlim(2,16)
# Roughly newer cars have better fuel economy.

# Are there or enew cars?
raw_data %>% ggplot() + geom_bar(mapping=aes(x=Year))
# Yes there are... does this explain the weird distribvution...

#This looks a little more normal.


# This looks vagely liek an inverse function
# How about seperated by class
style_grouped <- raw_data %>% group_by(Vehicle_Style) %>% summarise(mpg = mean(highway_MPG, na.rm = T), hp = mean(Engine_HP, na.rm = T))
style_grouped %>% ggplot(data = .) + geom_line(mapping=aes(x=mpg, y=hp))








