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

#Let's eliminate all variables except neighbourhood and cancellation policies,
# and filter out the irrelevant neighbourhoods.
seattle1<-seattle %>%
  select(cancellation_policy,neighbourhood_cleansed)%>%
  filter( (neighbourhood_cleansed == 'Alki' |  neighbourhood_cleansed == 'Belltown'))

#Create a visualization showing the different distributions of cancellation policies
ggplot(seattle1, aes(x = neighbourhood_cleansed, fill = cancellation_policy)) + 
  geom_bar(position = 'fill') + coord_flip() + ylab('Proportion')+ xlab('Neighborhood')+
  ggtitle('Cancellation Policy in Two Neighborhoods')

#Descriptive statistics: 
mytable1<-seattle1 %>% filter(cancellation_policy=='strict')%>%
  group_by(neighbourhood_cleansed) %>% tally()
mytable2<-seattle1 %>% group_by(neighbourhood_cleansed) %>% tally()
x1<-mytable1[[1,2]]
x2<-mytable1[[2,2]]
n1<-mytable2[[1,2]]
n2<-mytable2[[2,2]]
p1hat= x1/n1
p2hat= x2/n2


#Difference of proportions tests:

#Parametric (Z-test)
phatdiff = p1hat - p2hat
ppooled = (x1+x2)/(n1+n2)
stderror = sqrt(ppooled*(1-ppooled)*(1/n1+1/n2))
tstat = phatdiff/stderror
pvalue<- 2*pnorm(tstat)


#Nonparametric (Chi-Squared-test)
prop.test(x = c(x1,x2), n = c(n1,n2))
