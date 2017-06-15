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


#================== Use Multiple Logistic Regression =====================

# First, we consider only the variables we care about: 
seattle1<-seattle %>% select(room_type, price, accommodates, beds, bathrooms,
                             guests_included,review_scores_rating,
                             neighbourhood_cleansed)

#For simplicity, let's eliminate the Shared rooms, as they are irrelevant here
seattle1<- seattle1 %>% filter(room_type != 'Shared room')
seattle1<- seattle1 %>% mutate(Home = ifelse(room_type == 'Private room', 0, 1))


#Suppose we only use price to predict whether a rental is a private room (instead of an entire house)
m1<-glm(Home~ price, family = binomial, data = seattle1 )
c<-coefficients(m1)
#We create the logistic function that has been estimated: 
f<-function(x){1/(1 + exp(-c[[1]] -c[[2]]*x))}

#In order to create a visualization of what we've just done, we're going to bin our data into buckets of observations that have similar prices, and compare their proportion of 'entire home' rentals':
seattlebinned<-seattle1 %>% 
  mutate(pricebin = cut(price, breaks = 20*0:100, labels = 20*0.5:99.5))
seattleb<-seattlebinned %>% group_by(pricebin) %>% summarise(p = mean(Home))
ggplot(seattleb, aes(x = currency_to_numeric(pricebin), y = p)) + geom_point()+
  stat_function(fun = f, color = 'blue') + xlab('Price')+xlim(0,500)
#The graphic deomonstrates that an increase of price is linked to an increased likelihood that the rental is an entire home. 


summary(m1)
#The summary tells us that we estimate that the odds of a house being an entire home go up by .04 for every additional dollar in the price of the rental. 

#How well does this logistic regression do as a predictor? 
library(mclust)
seattle1<- seattle1 %>% mutate(Predict = ifelse(fitted(m1)>.5, 1, 0))
classError(seattle1$Home,seattle1$Predict )
#The classError function above tells us that our prediction gives us a 16% error rate. 

# Let's try adding more variables: 
seattle2<-na.omit(seattle1)
m2<-glm(Home~ accommodates+beds+bathrooms +guests_included+review_scores_rating + price, family = binomial, data = seattle2 )
summary(m2)

#Somewhat arbitrarily, I've decided to keep accomodates, bathrooms, and price: 
m3<-glm(Home~ accommodates+bathrooms  + price, family = binomial, data = seattle2 )
summary(m3)

#How effectively does this predict 'entire home's? 
seattle2<- seattle2 %>% mutate(Predict = ifelse(fitted(m3)>.5, 1, 0))
classError(seattle2$Home,seattle2$Predict )
#New error rate: 13.8%.  Not that much better? 


#================== Use Classification Tree =====================

#Now, let's produce a classification tree for this question: 
t<-rpart(Home~ accommodates+beds+bathrooms +guests_included+
           review_scores_rating + price, data = seattle2 )
t
rpart.plot(t, compress = TRUE)

#1 - Can you interpret the meaning behind all of the breaks in this tree? 
#2 - How many 'breaks' in the tree would you consider necessary, 
#     before you might be 'overfitting' the data? Why?
#3 - Does this classification tree change your mind about which variables 
#    (and in which cases) are practically significant? 
#4 - Which model we've produced so far do you think is better for 
#    predicting Homes vs. private rooms?