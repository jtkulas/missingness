###########################################
#We have access to the 2020 data now! It's only available in Stata, but we can read that into R like this.

#install.packages("haven") #run if not installed already.
library(haven)
library(tidyverse)

#data <- read_sav(file.choose()) 

#install.packages("foreign")
library(foreign)

data <- read.spss(file.choose(), use.missings = FALSE)

data2 <- as.data.frame(data)  ## changes from list to dataframe

#Look for gss2020panel_r1.dta #Loads ALL of the data. 

#There's something fishy about the file...exploring 

table(data$samptype) #People were "re-interviewed" in 2020, having first been interviewed in 2016 and 2018.

#Let's focus just on people who did the 2018 and 2020 interviews. 

data_2018 <- data %>%
  filter(data$samptype == 2018)

count(data_2018) #Number of 2018 people in the whole sample.

#1,014 people were re-interviewed in 2020 from 2018! :)

#Ok, ready to go! Here's some info about what's what. 

#This codebook focuses on Wave 2 of the 2016-2020 GSS Panel - i.e. the panel reinterviews with 2018 GSS respondents and a randomly selected subset of 2016 GSS respondents. 

#In the 2016-2020 GSS Panel, variables only contain data from one of the three years. To differentiate between versions of each variable, they have been appended with suffixes. 

#Variables from 2016 (Wave 1a) have _1a appended, 
#Variables from 2018 (Wave 1b) have _1b appended, 
#Variables from 2020 (Wave 2) have _2 appended. 

#Users can also track cases from 2016 and 2018, and reinterviews from 2020 with the variable SAMPTYPE. Figure 1 shows the relationship of 2016, 2018, and 2020 rounds of data collection for the 2016-2020 GSS Panel.

