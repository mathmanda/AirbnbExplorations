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


#First, let's keep only the variables we're interested in:
seattle1<-seattle %>% 
  select(price, accommodates, beds, bathrooms,
         guests_included,review_scores_rating, neighbourhood_cleansed)

# Let's look at how these variables relate to each other
# by creating a scatterplot matrix:
ggpairs(seattle1[,1:6])
#It looks like price is correlated with accomodates, beds, and baths ...
# but these are also correlated with each other! 

# Does price seem to be related to neighbourhood? 
seattle1 %>% group_by(neighbourhood_cleansed) %>%
  summarise(mean = mean(price), median = median(price), n = n())%>%
  arrange(desc(n))

# Belltown seems to cost more than other neighborhoods.
# University District seems to cost less than other neighborhoods.
# Let's create dummy variables for Belltown and University District. 

seattle1<- seattle1 %>% mutate(
  BT=ifelse(neighbourhood_cleansed=='Belltown',1,0),
  UD=ifelse(neighbourhood_cleansed=='University District',1,0))

#================== Use Multiple Regression =====================

m1<-lm(price~ accommodates+beds+bathrooms+
         guests_included+review_scores_rating+BT+UD, data = seattle1 )
summary(m1)

#At first glance it appears that we can explain about 47% of the variation
#in price using these variables, and all variables seem to contribute to 
#price except 'beds'.  

#Does it seem weird to you that 'beds' doesn't seem to contribute significantly? 
#Can you explain why it's not? 

#There are many ways to decide which variables to include 
# (like stepwise regression, or contextual knowledge of the problem).  
# Let's naively just take the ones I mentioned above: 

seattle2<-seattle1 %>% select(-beds)
seattle2<-na.omit(seattle2)
m2<-lm(price~ accommodates+bathrooms+guests_included+
         review_scores_rating+BT+UD, data = seattle2 )
summary(m2)
stres<-rstandard(m2)
#Interestingly, we can still explain about 47% of the variation in price, 
#suggesting that eliminating 'beds' doesn't cost us any predictive power.
# If we choose to accept this model 
# (which, by the way, we shouldn't -- at least not until we assess its validity!), 
# a practical description of the model would be: 

# - For every person the rental accomodates, add $21.20 to the price
# - For every bathroom in the rental, add $35.15 to the price
# - For each guest included, add $6.52 o the rental price
# - For each point in the rating, add $.68 to the rental price
# - If the rental is in BellTown, add $30.47 to the price
# - If the rental is in University District, subtract $14.19 from the price

#What is the *practical* significance of the coefficients explained above?  
#For example, it seems like 'accommodates' and 'bathrooms' add a significant 
# amount of money to the price.  On the other hand, 'review_scores_rating' 
# *seems* statistically significant, adding 68 cents to the price of a rental 
# above $100 seems irrelevant (even if it is true). 

#If you were going to open an Airbnb in Seattle, 
# which of the variables above would you consider, in order to maximize your profits?

#If you are interested in assessing the validity of our model, ask: 
# Do the residuals appear to be normally distributed? 
p1<-ggplot(seattle2, aes(x = accommodates, y = rstandard(m2))) + geom_point(shape = 1)+ xlab('# Accommodated') + ylab('St. Residuals')
p2<-ggplot(seattle2, aes(x = bathrooms, y = rstandard(m2))) + geom_point(shape = 1)+
  xlab('# Bathrooms') + ylab('St. Residuals')
p3<-ggplot(seattle2, aes(x = guests_included, y = rstandard(m2))) + 
  geom_point(shape = 1)+ xlab('# Guests Included') + ylab('St. Residuals')
p4<-ggplot(seattle2, aes(x = lm.influence(m2)$hat, y = rstandard(m2))) + 
  geom_point(shape = 1) + 
  geom_vline(xintercept = 2*(5)/length(stres), color = 'cyan')+ 
  geom_hline(yintercept = -4, color = 'red')+ 
  geom_hline(yintercept = 4, color = 'red')+ xlab('Influence Score') + ylab('St. Residuals')
plot_grid(p1,p2,p3,p4)
ggplot(seattle2, aes(x = fitted(m2), y = rstandard(m2))) + geom_point(shape = 1)+
  xlab('Fitted Values') + ylab('St. Residuals')

#While there is no recognizable nonlinear shape in these residual plots, 
# we do have a number of 'outlier + leverage' points that are problematic.  
# Also, the variation of residuals appears to be growing as things get larger. 
# So, this model is not without its problems. 

#==================Use A Regression Tree=====================

#Produces a regression tree:
t<-rpart(price~ accommodates+beds+bathrooms +guests_included+review_scores_rating+BT+UD, data = seattle1 )
t
rpart.plot(t)

#It appears that the most important variable in the data is 'accomodates'.  
#If you just want to split the data into two groups, you should look to
# see if the rental accomodates 5 or more people.  
#     If so, the average price would be $227, 
#     If not the average price would be $104.  

#For the rentals which accomodate less than 5 people, 
# the next break occurs depending on 
#    if the rental accomodates four people (average price $140) 
#    or not (average price $91). 

#The next break occurs in the rentals that accommodate 5 or more people: 
#     rentals with two or fewer bathrooms (average price $192) 
#     or more than two (average price $324).

#We can keep following the 'breaks' in this regression tree until a good 'cut-off point'.  Finding the right cut-off point is an interesting problem which you should learn more about.  
# Further Questions: 
#1 - Can you interpret the meaning behind all of the breaks? 
#2 - How many 'breaks' in the tree would you consider necessary, 
#     before you might be 'overfitting' the data? Why?
#3 - Does this regression tree change your mind about which variables (in which cases) are practically significant? 
#4 - Which model we've produced so far do you think is better for predicting price?