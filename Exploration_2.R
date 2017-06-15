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

#First we filter our data so that only includes the neighbourhood we care about: 
seattle1<-seattle %>% filter(neighbourhood_cleansed == 'Alki')

#Calculate the mean and standard deviation:
m = mean(seattle1$price)
s = sd(seattle1$price)

#Create a boxplot + violin plot to see what prices happen most: 
ggplot(seattle1, aes(x = '', y = price)) + geom_violin(fill = 'mediumorchid3')+
  geom_boxplot(width = .3, fill = 'mediumorchid1')+coord_flip() + xlab('') + ylab('Price')+
  ggtitle('Distribution of Prices in the Alki Neighborhood')

