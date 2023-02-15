
# Helpful Shortcuts -------------------------------------------------------

#ctrl s to save script

#ctrl shift r for inserting section 

# alt + dash is a short cut for <-

# cltr + enter will run a line of code

# ctrl shift m = pipe %>% 

#tip when doing new calculations, you can use vignettes to test calc out 
#and see how data is being formatted etc 


# Libraries ---------------------------------------------------------------

library(tidyverse) #ddplyr, pipes, ggplot
library(vegan) # community data / community ecology 
library(patchwork) # for putting together figures


# Data --------------------------------------------------------------------

invert_data<-read.csv("data/invert_data.csv")
View(invert_data) # always check to make sure data looks okay
# site= each study location
#columns are the family level of benthos observed 
#within each column there are observation counts
str(invert_data) # this shows you the overview of all the variables and what
#type they are 

colnames(invert_data) # shows all the column names in dataset



# Make into Univariate Dataframe ------------------------------------------

invert_data<-invert_data %>% 
  separate(site,c('site','sample')) #take the site column and separate it into
#site and sample number


#next step is to calc species data 

invert <- invert_data %>% select(Nemata:Unionidae) #subsetting data via as 
#nemata as first and unionidae as the last column, : means to 

View(invert)

env<-invert_data %>% select(site:sample)


richness <- specnumber(invert)
abundance <- rowSums(invert) #with inverts normally you are calculating this as
#density 

#vegan's help has a guide on each calc 

?diversity # this brings up help for how to cacl diversity in vegan

#Shannon 
H <- diversity(invert, index="shannon") #traditionally shannon is abbrev as H

#Simpsons 

D <- diversity(invert, index="simpson") #traditionally shannon is abbrev as D

#Pielous' evenness 

J<- H/log(richness) #traditionally Pielous' evenness is abbrev as J

#now we need to put all of these back into one dataset 

univariate <- env #renamed so it won't overwright env dataframe

univariate$richness <- richness #create a column for richness
univariate$abundance <- abundance
univariate$shannon <- H
univariate$simpson <- D
univariate$eveness <- J


#now let's save this as a csv 

write.csv(univariate, "data/univariate_data.csv", row.names = FALSE)
#row name false means  it won't name them 1,2,3,4

