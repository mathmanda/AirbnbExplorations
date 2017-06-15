library(ggplot2) # graphics
library(dplyr) # data wrangling
library(cowplot) #plot grids
library(knitr) # fancy tables
library(GGally) # scatterplot matrices
library(rpart) #regression and classification trees
library(rpart.plot) #plots of classification trees
library(car) # marginal model plots 
seattle<-read.csv('Airbnb_Seattle.csv')
currency_to_numeric <- function(x){
  # Convert currencies to numeric i.e. strip dollar signs and commas
  x <- gsub('\\$','', as.character(x))
  x <- gsub('\\,','', as.character(x))
  x <- as.numeric(x)
  return(x)
}
seattle<-seattle %>% mutate(price = currency_to_numeric(price))
showcode<-F

#As usual, let's start by isolating the variables we care about: 
seattle1<-seattle %>% select(room_type, price, accommodates, beds, bathrooms,
                             guests_included ,review_scores_rating,
                             neighbourhood_cleansed,latitude, longitude)

#We've decided to use the following 5 neighbourhoods:  
nbd_list<-c('Broadway', 'Belltown', 'University District', 'Minor','Fremont')
#Let's eliminate all rentals that aren't in these neighborhoods: 
seattle1<-seattle1 %>% filter(neighbourhood_cleansed %in% nbd_list)
ggplot(seattle1, aes(x = longitude, y = latitude, color = neighbourhood_cleansed)) + 
  geom_point()

#Now we create a new data set with a dummy variable for 'entire home' and 
# elminating non-numeric variables
clusters<-seattle1 %>%
  mutate(Home = ifelse(room_type == 'Entire home/apt',1,0)) %>%
  select(-neighbourhood_cleansed,-room_type)%>%
  na.omit()

#K-means is a way to determining clusters in data by trying to minimize
# 'within-cluster-sum-of-squares'
#What if we try to detect just 2 clusters here 
km<-kmeans(cbind(clusters[,1],clusters[,7:8]),3)
clusters<-clusters %>% mutate(Predict = km$cluster)
ggplot(clusters, aes(x = longitude, y = latitude, color = as.factor(Predict))) + 
  geom_point()

#As you play around with the code above you may notice a few of the following things: # - Each time we xecute the code above we get potentially different clusters, as this method depends on randomly chosen starting locations
# - Some number of clusters work well, if we guess at too many or too few, we get bad results
# - Latitude and Longitude get almost ignored when any other variable is tossed into the model.  Is this because the variability in lat/long is so small compared to other variables? If so, can you think of a way to compat that? 

#Let's try using only lat/long, and trying to decide what the optimal number of clusters is: 
wss<-NULL
for(i in 2:10){
  wss<-c(wss,sum(kmeans(clusters[,7:8],i)$withinss))
}
df<-data.frame(x = 2:10, y = wss)
ggplot(df, aes(x = x, y = y)) + geom_line() + geom_point(shape = 7)+
  xlab('# of Clusters') + ylab('Within Sum of Squares') + ggtitle('Elbow Plot')
#In the 'elbow plot' above, we look for places where the slope changes drastically (elbows!)

#Suppose you choose 4 clusters: 
km<-kmeans(clusters[,7:8],5)
clusters<-clusters %>% mutate(Predict = km$cluster)
ggplot(clusters, aes(x = longitude, y = latitude, color = as.factor(Predict))) + 
  geom_point()

#How could you improve our clustering model? 
#Can you think of a way to not rely on latitude and longitude? 
