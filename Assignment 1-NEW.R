#-Assignment 1 New Version ------

#Zizhen Zhong 
#2024, October 03

#Packages Used: 

library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("maps")
library(maps)
install.packages("VennDiagram")
library(VennDiagram)



dfBOLD_Tardigrada <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Tardigrada&format=tsv")

dfBOLD_Interest <- dfBOLD_Tardigrada[,c(8,14,16,20,47,48,51,52)]

#Question 1 : 
# Tardigrades are known for their cryptobiosis and remarkable ability to endure extreme environmental stressors. Does exposure to such extreme environments correlate with greater genetic diversity within Tardigrade populations compared to those in more temperate habitats? 

#Question 2: 
# Find the highest abundant specie of Tardigrada, and where they inhabit compare to other species. 



#-----Question 1 Answer : ----

#Here I want to filter out the missing cells that does not have latitude values, and retaining recorded specimens that live in more extreme conditions. 

#dfBOLD_Interest <- dfBOLD_Interest %>% 
  #filter(!is.na(lat)) %>% 
  #group_by(lat >= 50 & lat <=-50)

#(>.<!!!) Seems like my code did not work, I still see latitude values with +-30s. 
#I learned that because latitude values cannot be both greater than 50 and smaller than -50 at the same time. So I will replace & with |.

#dfBOLD_Interest <- dfBOLD_Interest %>% 
  #filter(!is.na(lat)) %>% 
  #group_by(lat >= 50 | lat <=-50)

#(>.<!!!) Still not working, not filtering out what I wanted. I suddenly realized I accidentally created a new column with logical values of T/F corresponding to latitude range I assigned. Thus I think it is the group_by command that is misused. 

dfBOLD_Interest <- dfBOLD_Interest %>% 
  filter(!is.na(lat)) %>% 
  filter(lat >= 50 | lat <=-50)

#Here I renamed the bin_uri variable header to BIN, because R cannot recognize this variable sometimes for some reason. 
dfBOLD_Interest <- dfBOLD_Interest %>% 
  rename(BIN = bin_uri)

#I want to know how many unique species of Tardigrada live in more extreme conditions. 
length(unique(dfBOLD_Interest$BIN))

#I want to make another data frame for Tardigrada that lives in temperate environment to compare their genetic diversity with Tardigrada that lives in extreme conditions. 
dfBOLD_temperate <- dfBOLD_Tardigrada[, c(8,14,16,20,47,48,51,52)]

#Here I want to do the same thing to filter out and collect the species in Tardigrada that lives within temperate conditions.

#dfBOLD_temperate %>% 
  #filter(!is.na(lat)) %>% 
  #filter(lat <50 | lat >-50 )
#(>.<!!!) my code did not work again. So I alter the | to see what will happen. I learned that because this is a continuous range, between 50 and -50 latitude, therefore I can simply use & for a continuous range instead of | either.  

dfBOLD_temperate <- dfBOLD_temperate %>% 
  filter(!is.na(lat)) %>% 
  filter(lat <50 & lat >-50 )

dfBOLD_temperate <- dfBOLD_temperate %>% 
  rename(BIN = bin_uri)

length(unique(dfBOLD_temperate$BIN))
#===> There are 105 different species found in temperate condition, while 121 species found in extreme conditions. Therefore, the data suggest that Tardigrada that lives in more extreme environment are not genetically more diverse. 


#Here I want to list out all the unique BIN from each condition and compare the BINs to see if there is any common species. 


#Values_Genetic_Diversity <- list(length(unique(dfBOLD_temperate$BIN))), length(unique(dfBOLD_Interest$BIN)))
#Since they are in 2 lists, I want to transform the 2 list into 2 columns in a data frame allowing me to compare. 
#data_frame(unique(dfBOLD_temperate$BIN), unique(dfBOLD_Interest$BIN))
#(>.<!!!) Cannot do that because one list is longer than the other. Then my next step should be making the 2 list the same length and then replacing the empty cells with NA. 

#a <- list(unique(dfBOLD_Interest$BIN), unique(dfBOLD_temperate$BIN))
#a %>% 
 # replace(NA, nrow(1))
#(>.<!!!) This will replace first BIN cell into NA. 


#---Tried to create Venn Diagram, did not work. ---- 
#I extracted the unique BIN from each condition
#extreme.BIN <- as.list(unique(dfBOLD_Interest$BIN))
#temperate.BIN <- as.list(unique(dfBOLD_temperate$BIN))

#Here I try to find out which list has longer length, so I can later on replace NA for the list that have shorter length
#length.maxlist <- max(length(extreme.BIN), length(temperate.BIN))
#print(length.maxlist)

#new.list <- c(extreme.BIN, temperate.BIN)
#This was wrong in which it combined the 2 lists together into 1 list. 

#I replaced NA in cells that are empty while elonging the length of temperate list to equal to the list length of extreme condition. Got insight how to replace and extend the length of a list from this website (https://stackoverflow.com/questions/27995639/i-have-a-numeric-list-where-id-like-to-add-0-or-na-to-extend-the-length-of-the). 

#extreme.BIN <- c(extreme.BIN, rep(NA, length.maxlist - length(extreme.BIN)))
#temperate.BIN <- c(temperate.BIN, rep(NA, length.maxlist - length(temperate.BIN)))

#Now that both lists have the same length, I can combine them into 1 data frame for further investigation. 
#dfBIN <- data_frame(temperate.BIN, extreme.BIN)

#I want to find out what are the common species associated in both temperate and extreme conditions. 




#Thought to self, If Tardigrada is not genetically more diverse to handle extreme conditions in terms of temperature wise, maybe the depths of where they are located can give insight. Because some of these Tardigrada may be living in temperate region, but in extreme conditions such as very high in mountain or very deep in sea as anther type of extreme condition. 

#dfBOLD_temperate %>% 
  #filter(!is.na(elev)) %>% 
 # mean(elev)
#(>.<!!!) It is returning mean as NA because argument is not numerical. Now I have to change the class of the elevation column. 

#dfBOLD_temperate %>% 
  #as.numeric(elev)
#returning object not found. 

#dfBOLD_elevation <- dfBOLD_temperate %>% 
  #type.convert(as.is = TRUE)
#I learned to use this new command from Youtube (https://www.youtube.com/watch?v=pYu-gC6Q7gA&t=150s). By creating a new data frame using this new command so I will not be losing my data from dfBOLD_temperate if anything went wronng. 

#dfBOLD_elevation$elev <- as.numeric(dfBOLD_elevation$elev)
# However, the previous step did not give me a class of numeric, but instead it gave me integer. Thus, I am here changing the class of the elevation variable to numeric so I can run the mean. 

#class(dfBOLD_elevation$elev)
#Here I am checking to confirm if I have successfully changed the class to numeric. 

#mean(!is.na(dfBOLD_elevation$elev))
#===> The mean is 0.2526m. Which is not expected based on what I saw from the data frame list, I can see values of elevation between 72 - 2100. Thus, this mean given is inaccurate. 

mean(dfBOLD_temperate$elev,na.rm=TRUE)
#===> The mean is 596.04m. I do not consider 596m in elevation to be extreme condition. In which I think the possible reason for temperate condition Tardigrada to experience similar genetic diversity as extreme condition because they are not under extreme stress condition allowing them to .... 


#--Answer to Question 2 -----: 

#Here I created a new data frame, where I want to know how many unique BIN there are, and how many BINs reported. 
dfBOLD_species_pattern <- dfBOLD_Tardigrada[, c(8,20,47,48,55,56)] %>% 
  rename(BIN=bin_uri) %>% 
  filter(!is.na(BIN)) %>% 
  count(BIN) 
#===> From this tibble I know that species with BIN BOLD:AAV0429 is most abundant. 

#Here I want a data frame that only contain specie : BOLD:AAV0429 and its corresponding variables of information, and then create a map that allows me to visualize where all the BOLD:AAV0429 is located.  
dfBOLD_AAV0429 <- dfBOLD_Tardigrada %>% 
  filter(BIN=="BOLD:AAV0429")

#plot(x=dfBOLD_Country$lat, y=dfBOLD_Country$lon)
#Here I tried to plot the values to get an overall view of how the map will look like and where the data points are located. 


#When trying to create a simple world map, I found insight from https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/. 


#mapping <- map_data("world")

#ggplot()+
 # geom_polygon(data = mapping, aes(x=lat, y=long, group=group), fill = "lightblue" , colors = "lightgrey") +
  #geom_point(data = dfBOLD_AAV0429, aes(x=dfBOLD_AAV0429$lat, y=dfBOLD_AAV0429$lon))
#This map is looking a bit weird, I think it has been flipped. 


mapping <- map_data("world")

ggplot()+
  geom_polygon(data = mapping, aes(x=long, y=lat, group=group), fill = "lightblue" , colors = "lightgrey") +
  geom_point(data = dfBOLD_AAV0429, aes(x=dfBOLD_AAV0429$lon, y=dfBOLD_AAV0429$lat))+
  labs(title = "Distrubution Pattern of Specie BOLD:AAV0429", x= "Longitude", y="Latitude")
#===> Noticing all BOLD:AAV0429 were collected from the Southern Hemisphere where the location is considered to be extreme environment. 
  

 
#Now I want to make a same graph for all the species of Tardigrada. 
mapping <- map_data("world")

ggplot()+
  geom_polygon(data = mapping, aes(x=long, y=lat, group=group), fill = "lightblue" , colors = "lightgrey") +
  geom_point(data = dfBOLD_Country, aes(x=dfBOLD_Country$lon, y=dfBOLD_Country$lat))+
  labs(title = "Distribution Pattern of Tardigrada", x= "Longitude", y="Latitude")
#===> From this distribution graph, it shows that majority of the data was collected from Europe region. 



