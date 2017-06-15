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

#For simplicity, let's eliminate all variables except cleanliness score and price
# and eliminate any missing data
# .... though .... we should consider what missing data might mean about a rental ...
seattle1<- seattle %>% 
  select(review_scores_cleanliness, price)%>%
  filter(review_scores_cleanliness != 'N/A')

# Make a bar graph of the cleanliness ratings
ggplot(seattle1, aes(x= review_scores_cleanliness))+
  ggtitle('Bar Graph of Cleanliness Ratings') +
  geom_bar(width=0.9, fill="steelblue") + 
  xlab('Cleanliness Score')

# We make a jittered scatterplot, and indicate a potential cut-off value for 
# cleanest vs least clean: 
ggplot(seattle1, aes(x = review_scores_cleanliness, y = price)) +
  geom_jitter(shape = 1) + 
  geom_segment(y = 0, yend = 1000, x = 7.5, xend = 7.5 , color = 'red')+
  ggtitle('Jittered Scatterplot')+ xlab('Cleanliness Score')+ ylab('Price')

#Let's create a new variable that separates the clean from the unclean!
seattle1 <- seattle1 %>% mutate(HiLo = ifelse(review_scores_cleanliness>=8, 'High', 'Low'))

#Now we make side by side violin and boxplots, so we can compare prices 
#of the two groups: 
ggplot(seattle1, aes(x = HiLo, y = price, fill = HiLo))+
  geom_violin()+geom_boxplot( width = .4)+coord_flip()+
  theme(legend.position = "none")+
  scale_y_log10() + 
  xlab('Cleanliness Rating') + ylab('Price')+ggtitle('Price on a Log Scale')