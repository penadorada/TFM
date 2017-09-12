#--------------------------------------------------------------------------------------------------------------

## VISUALIZATION OF MADRID's NEIGHBOURHOODS MOST IMPORTANT FIGURES  
## AIRBNB PROPERTIES

#---------------------------------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggmap)


aib<-read.csv('airbnb1.csv')
aibs<-read.csv('aibsuma.csv')

# Outlook of Airbnb properties on Madrid map
Madr <- qmap('Madrid, Centro',zoom=14,color = 'bw')
Madr + geom_point(aes(x = longitude, y = latitude, colour = room_type),data=aib,size=0.5) + ggtitle('Airbnb listings in Madrid as of April 2017')

# Number of listings per District
ggplot(data=aib, aes(aib$neighbourhood_group_cleansed)) + 
  geom_histogram(stat = 'count', 
                 col="black", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low="green", high="red")+
  ggtitle('NUMBER OF AIRBNB LISTINGS IN MADRID')+
  xlab('DISTRICT')+ ylab('QUANTITY')+
  coord_flip()


# Airbnb Rooms Avg Price
aibs2 <- aibs[aibs$room_type=='Private room',]
ggplot(aibs2, aes(x=neighbourhood_cleansed,y = Median_price)) + 
  geom_bar(stat = 'identity',width = 0.4,fill='green',color='black') +
  ggtitle('Rooms Average Price')  +  xlab('AREAS') + ylab('Average Price (EUR)') +
  geom_hline(yintercept = 34,color='red') + 
  theme(axis.text.x = element_text(size = 6.7,angle = 90,colour = 'black'))


# Airbnb Apartments average price
aibs1 <- aibs[(aibs$room_type=='Entire home/apt'),]
ggplot(aibs1, aes(x=neighbourhood_cleansed,y = Median_price)) + 
  geom_bar(stat = 'identity',width = 0.4,fill='tomato',color='black') + 
  ggtitle('Apartments Average Price')  +  xlab('AREAS') + ylab('Average Price per Night(EUR)') +
  geom_hline(aes(yintercept = mean(aibs1$Median_price),color='blue')) +
  theme(axis.text.x = element_text(size = 6.7,angle = 90,colour = 'black'))


# Price Per Person on Airbnb Apartments
ggplot(aibs1, aes(x=neighbourhood_cleansed, y = Price_Person_Night)) + 
  geom_bar(stat = 'identity',width = 0.4,color='black',fill='tomato') +
  ggtitle('Average Price per Person/Night')  +  xlab('AREAS') + ylab('Price (EUR)') +
  geom_hline(aes(yintercept = 21.16),color='blue') + 
  theme(axis.text.x = element_text(size = 6.7,angle = 90,color='black'))


# Breakdown of type of properties
ggplot(aib, aes(x = "count", fill = factor(room_type))) + 
  geom_bar(width = 1) +
  coord_polar(theta = "y")+
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5))


# Number of rooms per area
ggplot(aibs2, aes(x=neighbourhood_cleansed, y = Quantity)) + 
  geom_bar(stat = 'identity',width = 0.2,fill='brown') +
  ggtitle('Number of Airbnb rooms in Madrid')  +  xlab('AREAS') + ylab('Quantity') +
  theme(axis.text.x = element_text(angle=90,size = 6.5,colour = 'black',face ='plain'))

# Average Room Availability in 30 days
ggplot(aibs1, aes(x=neighbourhood_cleansed, y = availability_30)) + 
  geom_bar(stat = 'identity',width = 0.2,fill='brown') +
  ggtitle('Average days available in a month')  +  xlab('AREAS') + ylab('DAYS') +
  theme(axis.text.x = element_text(angle=90,size = 6.5,colour = 'black',face ='plain'))

# Average Apartment/House Availability in 30 days
ggplot(aibs2, aes(x=neighbourhood_cleansed, y = availability_30)) + 
  geom_bar(stat = 'identity',width = 0.2,fill='brown') +
  ggtitle('Average days available in a month')  +  xlab('AREAS') + ylab('DAYS') +
  theme(axis.text.x = element_text(angle=90,size = 6.5,colour = 'black',face ='plain'))

# Cleaning fee
plot(aib$cleaning_fee)
aib[aib$cleaning_fee>200,]
