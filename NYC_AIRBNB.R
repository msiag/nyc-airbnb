#libraries installed
install.packages("tidyverse")
install.packages("DataExplorer")
library(ggplot2)
library(tidyverse)

#Getting data
NYC_Airbnb = read.csv(file.choose())

#Exlporing the data
head(NYC_Airbnb)

tail(NYC_Airbnb)

str(NYC_Airbnb)

#Converting id, host_id columns to factor

NYC_Airbnb$id <- as.factor(NYC_Airbnb$id)
NYC_Airbnb$host_id<-as.factor(NYC_Airbnb$host_id)



#checking for duplicate
#48895 - for both commands so no duplicates

nrow(NYC_Airbnb[-1])
NROW(unique(NYC_Airbnb[-1]))

# check the missing data

plot_missing(NYC_Airbnb)

# check co relation

plot_correlation(NYC_Airbnb)

#checking data wrt price and number of listing

price_no_listing <- NYC_Airbnb %>% select(price) %>% group_by(price) %>% 
   count(price) %>% top_n(20)

#ploting data wrt price and number of listing

ggplot(price_no_listing,aes(price,n)) + geom_line(color="#FF2000")

# zooming uptp price 2000$

ggplot(price_no_listing,aes(price,n)) + 
  geom_line(color="#FF6500") + coord_cartesian(xlim=c(0,2000)) + xlab("Price in USD") + 
  ylab("number of listing")

#averagecost_neigbourhoodgroup 
#Manahattan seems to be most expensive

averagecost_neigbourhoodgroup <- NYC_Airbnb %>% group_by(neighbourhood_group) %>% 
  summarise(price = round(mean(price),2))



# Most booked neigbourhood 

ggplot(data = NYC_Airbnb, aes(x = neighbourhood_group, y=price)) + 
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Neighbourhood Group vs Average Price",
       x = "Neighbourhood Group", y = "Average Price of each Booking")

# Most preffered roomtype

ggplot(data = NYC_Airbnb, aes(room_type)) + geom_bar(stat = "count", fill = "green")

# type_of_room_vs_neighbourhood_group 

ggplot(NYC_Airbnb, aes(x = NYC_Airbnb$neighbourhood_group) )+ 
         geom_bar( aes(fill = NYC_Airbnb$room_type))

# Geo map wrt to listing and neighbourhood

ggplot(data=NYC_Airbnb)+
  geom_point(aes(x=latitude, y=longitude, color=neighbourhood_group))

#Mean,Median and Standard deviation

mean(NYC_Airbnb$price)

sd(NYC_Airbnb$price)

summary(NYC_Airbnb$price)

#distribution of room_type

table(NYC_Airbnb$room_type)

#distribution of neighbourhood_group

table(NYC_Airbnb$neighbourhood_group)

#difference in price across room types

ggplot(data = NYC_Airbnb) +
  geom_boxplot(mapping = aes(x=room_type, y=price)) + coord_cartesian(ylim=c(0,2000))

#difference in price across different neighbourhood group

ggplot(data = NYC_Airbnb) +
  geom_boxplot(mapping = aes(x=neighbourhood_group, y=price)) + coord_cartesian(ylim=c(0,2000))

# price of rooms for each room type

price_roomtype_count  <- NYC_Airbnb  %>% group_by(room_type,price) %>%  summarise(n_count = n())

ggplot(price_roomtype_count,aes(price,n_count)) + geom_point(color="#FF6500") + facet_wrap(~room_type) + coord_cartesian(xlim=c(0,799))




