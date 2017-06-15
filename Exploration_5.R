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

# Let's 
# - eliminate all variables except price and # of beds
# - eliminate missing data (... but worry about what missing data means...)
seattle1<-seattle %>% 
  select(price, beds) %>%
  na.omit()%>%
  filter(beds<=14)

#Create a jittered scatterplot which also shows the best fit line: 
p1<-ggplot(seattle1, aes(x = beds, y = price)) + geom_jitter(shape = 1) + xlim(0,11)+
  geom_smooth(method = 'lm',se = F) + xlab('# of Beds') + ylab('Price')

#Create a contour plot to show where data is most dense: 
p2<-ggplot(seattle1, aes(x = beds, y = price)) + 
  stat_density2d(na.rm=TRUE) + xlim(0.5,3.5) +ylim(0,250)+
  xlab('# of Beds') + ylab('Price')

plot_grid(p1, p2)

#Simple linear regression: 
m1<-with(lm(price~beds), data = seattle1)
s<-summary(m1)
#kable(s$coefficients)
