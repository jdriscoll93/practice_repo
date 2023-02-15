
# Helpful Shortcuts -------------------------------------------------------

#ctrl s to save script

#ctrl shift r for inserting section 

# alt + dash is a short cut for <-

# cltr + enter will run a line of code

# ctrl shift m = pipe %>% 

#tip when doing new calculations, you can use vignettes to test calc out 
#and see how data is being formatted etc 

# can use this to get colour hex codes from images: https://coolors.co/
# can also look at trending palattes 

# Colorbrewer: https://colorbrewer2.org/

# Viridi colours: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html


#r shape codes for figures: http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

#ctrl shift c to add # to each line

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

#now upload to Git 
# go to Git in the environment box. select all files to commit and then push 


#group by site and summarize mean of species richness and it's standard.dev
univariate %>% 
  group_by(site) %>% 
  summarise(S_mean=mean(richness),
            S_sd = sd(richness))

#this code will do this across commmunity ecology calcs we did 
#so you do not need to do this individually, like above 

sum_table <- univariate %>% 
  group_by(site) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, min = min), na.rm = TRUE,
    .names = "{col}_{fn}"
  ))

sum_table <- sum_table %>% t %>% as.data.frame

write.csv(sum_table, "data/univariate_summary.csv", row.names=TRUE)


#need to calculate 1m2 density
invert_1m <- invert*10



# Figures -----------------------------------------------------------------

##Tip:
colour = c(station1 = "21") # if you want you can assign colours to be standard


# Richness plot

richness_plot <- ggplot(univariate, aes(x= site, y= richness)) +
  geom_boxplot(lwd = 0.75) + #lwd is line width
  geom_jitter(aes(shape=site, fill=site), #gitter lets you see the distribution
              size= 5, stroke =1.5, width = 0.5, height = 0.05) +
  theme_bw(14) + #14 is the font size
  scale_shape_manual(values = c(21, 24, 22)) +
  scale_fill_manual(values=c("#BA251E","#F2C73B", "#48484B")) + # 4 directions logo colours 
  theme(legend.position = "none") +
  ylim (0,10) +
  labs(x="",
       y= "Richness") + 
  scale_x_discrete(labels=c("station1" = "Station 1", 
                            "station2" = "Station 2",
                            "station3" = "Station 3"))
 
  
  y= (expression(paste("Richness per 1", " ", m^2))) # this will add in y axis label
  #as density per meter squared 
  
 
  

  # save figure at same size and dimension in the r window 
  ggsave("output/richness_plot.jpg", richness_plot,
         width = 5.28, 
         height = 4.3)
  
#Saving 5.28 x 4.3 in image

  
#Abundance plot: 
  
  
  density_plot <- ggplot(univariate, aes(x= site, y= abundance)) +
    geom_boxplot(lwd = 0.75) + #lwd is line width
    geom_jitter(aes(shape=site, fill=site), #gitter lets you see the distribution
                size= 5, stroke =1.5, width = 0.5, height = 0.05) +
    theme_bw(14) + #14 is the font size
    scale_shape_manual(values = c(21, 24, 22)) +
    scale_fill_manual(values=c("#BA251E","#F2C73B", "#48484B")) + # 4 directions logo colours 
    theme(legend.position = "none") +
    ylim (0,100) +
    labs(x="",
         y= (expression(paste("Richness per 1", " ", m^2)))) + 
    scale_x_discrete(labels=c("station1" = "Station 1", 
                              "station2" = "Station 2",
                              "station3" = "Station 3"))
 
practice_panel <- richness_plot + density_plot + plot_annotation(tag_levels = "A")
    
ggsave ('output/practice_panel.jpg', practice_panel)
 
 #or use " / " instead of "+" tp have them overtop 
  


# Multivariate ------------------------------------------------------------

invert_data


#facet wrap by taxa will show us individual figures for each taxa in a panel

# to do this we need to transpose invert data to have a column for taxa 
# column for count of each taxa
# station as a colum
#this allows wrapping 


#tidy has pivot long and pivot wide
#in this case our og invert data is wide but we want it long to have those
#columns mentioned above 

invert_long <- invert_data %>% 
  pivot_longer(Nemata:Unionidae,
               names_to = "species",
               values_to = "count")

ggplot(invert_long, aes(x = site, y= count, fill = site))+
  geom_col() +
  facet_wrap(.~species, scales = "free_y") # this is telling it to wrap by species
# scales is saying that each has it's on y axis 
